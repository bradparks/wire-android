/**
 * Wire
 * Copyright (C) 2017 Wire Swiss GmbH
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.waz.zclient

import android.content.Intent
import android.content.res.Configuration
import android.graphics.Bitmap
import android.net.Uri
import android.os.Bundle
import android.support.annotation.NonNull
import android.support.v4.app.{FragmentManager, FragmentTransaction}
import android.support.v4.content.ContextCompat
import android.widget.Toast
import com.localytics.android.Localytics
import com.waz.ZLog
import com.waz.ZLog.ImplicitTag._
import com.waz.api.{BitmapCallback, ImageAsset, ImageAssetFactory, LoadHandle}
import com.waz.service.{AccountsService, ZMessaging}
import com.waz.utils.wrappers.AndroidURIUtil
import com.waz.zclient.AppEntryController.{LoginEmail, _}
import com.waz.zclient.controllers.navigation.{NavigationControllerObserver, Page}
import com.waz.zclient.controllers.tracking.screens.ApplicationScreen
import com.waz.zclient.core.api.scala.AppEntryStore
import com.waz.zclient.core.controllers.tracking.attributes.{Attribute, RegistrationEventContext}
import com.waz.zclient.core.controllers.tracking.events.Event
import com.waz.zclient.core.controllers.tracking.events.registration.{OpenedPhoneRegistrationFromInviteEvent, SucceededWithRegistrationEvent}
import com.waz.zclient.core.stores.appentry.AppEntryState
import com.waz.zclient.newreg.fragments.SignUpPhotoFragment.UNSPLASH_API_URL
import com.waz.zclient.newreg.fragments._
import com.waz.zclient.newreg.fragments.country.{CountryController, CountryDialogFragment}
import com.waz.zclient.preferences.PreferencesController
import com.waz.zclient.tracking.GlobalTrackingController
import com.waz.zclient.ui.utils.KeyboardUtils
import com.waz.zclient.utils.{HockeyCrashReporting, ViewUtils, ZTimeFormatter}
import com.waz.zclient.views.LoadingIndicatorView
import net.hockeyapp.android.NativeCrashManager

object AppEntryActivity {
  val TAG: String = classOf[AppEntryActivity].getName
  private val HTTPS_PREFIX: String = "https://"
  private val HTTP_PREFIX: String = "http://"
  val PREFETCH_IMAGE_WIDTH: Int = 4
}

class AppEntryActivity extends BaseActivity
  with VerifyPhoneFragment.Container
  with PhoneRegisterFragment.Container
  with PhoneSignInFragment.Container
  with PhoneSetNameFragment.Container
  with PhoneAddEmailFragment.Container
  with OTRPhoneAddEmailFragment.Container
  with PhoneVerifyEmailFragment.Container
  with SignUpPhotoFragment.Container
  with EmailRegisterFragment.Container
  with EmailSignInFragment.Container
  with EmailVerifyEmailFragment.Container
  with WelcomeEmailFragment.Container
  with EmailInvitationFragment.Container
  with PhoneInvitationFragment.Container
  with InAppWebViewFragment.Container
  with CountryDialogFragment.Container
  with FirstLaunchAfterLoginFragment.Container
  with NavigationControllerObserver {

  private var unsplashInitImageAsset: ImageAsset = null
  private var unsplashInitLoadHandle: LoadHandle = null
  private var progressView: LoadingIndicatorView = null
  private lazy val countryController: CountryController = new CountryController(this)
  private var createdFromSavedInstance: Boolean = false
  private var isPaused: Boolean = false

  private lazy val appEntryController = inject[AppEntryController]

  ZMessaging.currentGlobal.blacklist.upToDate.onUi {
    case false =>
      startActivity(new Intent(this, classOf[ForceUpdateActivity]))
      finish()
    case _ =>
  }

  override def onBackPressed(): Unit = {
    val fragment: SignUpPhotoFragment = getSupportFragmentManager.findFragmentByTag(SignUpPhotoFragment.TAG).asInstanceOf[SignUpPhotoFragment]
    if (fragment != null && fragment.onBackPressed) {
      return
    }
    val otrPhoneAddEmailFragment: OTRPhoneAddEmailFragment = getSupportFragmentManager.findFragmentByTag(OTRPhoneAddEmailFragment.TAG).asInstanceOf[OTRPhoneAddEmailFragment]
    if (otrPhoneAddEmailFragment != null) {
      getSupportFragmentManager.popBackStackImmediate(R.id.fl_main_content, FragmentManager.POP_BACK_STACK_INCLUSIVE)
      getStoreFactory.getZMessagingApiStore.logout()
      getStoreFactory.getAppEntryStore.setState(AppEntryState.PHONE_SIGN_IN)
      return
    }
    if (!getStoreFactory.getAppEntryStore.onBackPressed) {
      super.onBackPressed()
    }
  }

  override def onCreate(savedInstanceState: Bundle): Unit = {
    if (getActionBar != null) getActionBar.hide()
    super.onCreate(savedInstanceState)
    ViewUtils.lockScreenOrientation(Configuration.ORIENTATION_PORTRAIT, this)
    setContentView(R.layout.activity_signup)
    progressView = ViewUtils.getView(this, R.id.liv__progress)
    enableProgress(false)
    createdFromSavedInstance = savedInstanceState != null

    if (unsplashInitLoadHandle == null && unsplashInitImageAsset == null) {
      unsplashInitImageAsset = ImageAssetFactory.getImageAsset(AndroidURIUtil.parse(UNSPLASH_API_URL))
      // This is just to force that SE will download the image so that it is probably ready when we are at the
      // set picture screen
      unsplashInitLoadHandle = unsplashInitImageAsset.getSingleBitmap(AppEntryActivity.PREFETCH_IMAGE_WIDTH, new BitmapCallback() {
        def onBitmapLoaded(b: Bitmap): Unit = {}
      })
    }

    appEntryController.uiSignInState ! LoginEmail

    appEntryController.entryStage.onUi {
      case EnterAppStage =>
        onEnterApplication(false)
      case LoginStage(LoginEmail) =>
        onShowEmailSignInPage()
      case LoginStage(LoginPhone) =>
        onShowPhoneSignInPage()
      case LoginStage(RegisterEmail) =>
        onShowEmailRegistrationPage()
      case DeviceLimitStage =>
        onEnterApplication(true)
      case AddNameStage =>
        onShowPhoneNamePage()
      case AddPictureStage =>
        onShowEmailSetPicturePage()
      case VerifyEmailStage =>
        onShowEmailVerifyEmailPage()
      case VerifyPhoneStage =>
        onShowPhoneVerifyEmailPage()
      case _ =>
    }
  }

  override def onStart(): Unit = {
    super.onStart()
    getControllerFactory.getNavigationController.addNavigationControllerObserver(this)
  }

  override protected def onResume(): Unit = {
    super.onResume()
    val trackingEnabled: Boolean = injectJava(classOf[PreferencesController]).isAnalyticsEnabled
    if (trackingEnabled) {
      HockeyCrashReporting.checkForCrashes(getApplicationContext, getControllerFactory.getUserPreferencesController.getDeviceId, injectJava(classOf[GlobalTrackingController]))
    }
    else {
      HockeyCrashReporting.deleteCrashReports(getApplicationContext)
      NativeCrashManager.deleteDumpFiles(getApplicationContext)
    }
  }

  override protected def onPostResume(): Unit = {
    super.onPostResume()
    isPaused = false
  }

  override protected def onPause(): Unit = {
    isPaused = true
    super.onPause()
  }

  override def onStop(): Unit = {
    getControllerFactory.getNavigationController.removeNavigationControllerObserver(this)
    if (unsplashInitLoadHandle != null) {
      unsplashInitLoadHandle.cancel()
      unsplashInitLoadHandle = null
    }
    super.onStop()
  }

  override protected def onActivityResult(requestCode: Int, resultCode: Int, data: Intent): Unit = {
    ZLog.info(s"OnActivity result: $requestCode, $resultCode")
    super.onActivityResult(requestCode, resultCode, data)
    getSupportFragmentManager.findFragmentById(R.id.fl_main_content).onActivityResult(requestCode, resultCode, data)
  }

  override def onDestroy(): Unit = {
    getStoreFactory.getAppEntryStore.clearCurrentState()
    super.onDestroy()
  }

  def enableProgress(enabled: Boolean): Unit = {
    if (enabled)
      progressView.show(LoadingIndicatorView.SPINNER_WITH_DIMMED_BACKGROUND, true)
    else
      progressView.hide()
  }

  def abortAddAccount(): Unit = {
    enableProgress(true)
    ZMessaging.currentAccounts.fallbackToLastAccount(new AccountsService.SwapAccountCallback() {
      def onSwapComplete(): Unit = {
        enableProgress(false)
        onEnterApplication(true)
      }

      def onSwapFailed(): Unit = enableProgress(false)
    })
  }

  def onOpenUrl(url: String): Unit = {
    try {
      val prefixedUrl =
        if (!url.startsWith(AppEntryActivity.HTTP_PREFIX) && !url.startsWith(AppEntryActivity.HTTPS_PREFIX))
          AppEntryActivity.HTTP_PREFIX + url
        else
          url
      val browserIntent: Intent = new Intent(Intent.ACTION_VIEW, Uri.parse(prefixedUrl))
      browserIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
      startActivity(browserIntent)
    }
    catch {
      case e: Exception => {
        ZLog.error(s"Failed to open URL: $url")
      }
    }
  }

  def getAccentColor: Int = ContextCompat.getColor(this, R.color.text__primary_dark)

  def onShowPhoneInvitationPage(): Unit = {
    val transaction: FragmentTransaction = getSupportFragmentManager.beginTransaction
    setDefaultAnimation(transaction).replace(R.id.fl_main_content, PhoneInvitationFragment.newInstance(getStoreFactory.getAppEntryStore.getInvitationName, getStoreFactory.getAppEntryStore.getInvitationPhone), PhoneInvitationFragment.TAG).commit
    enableProgress(false)
  }

  def onShowEmailInvitationPage(): Unit = {
    val transaction: FragmentTransaction = getSupportFragmentManager.beginTransaction
    setDefaultAnimation(transaction).replace(R.id.fl_main_content, EmailInvitationFragment.newInstance(getStoreFactory.getAppEntryStore.getInvitationName, getStoreFactory.getAppEntryStore.getInvitationEmail), EmailInvitationFragment.TAG).commit
    enableProgress(false)
  }

  def onInvitationFailed(): Unit = {
    Toast.makeText(this, getString(R.string.invitation__email__failed), Toast.LENGTH_SHORT).show()
  }

  def onInvitationSuccess(): Unit = {
    getControllerFactory.getUserPreferencesController.setPersonalInvitationToken(null)
  }

  def onShowPhoneRegistrationPage(): Unit = {
    if (isPaused) {
      return
    }
    val transaction: FragmentTransaction = getSupportFragmentManager.beginTransaction
    setDefaultAnimation(transaction).replace(R.id.fl_main_content, PhoneRegisterFragment.newInstance, PhoneRegisterFragment.TAG).commit
    enableProgress(false)
    if (fromGenericInvite) {
      getStoreFactory.getAppEntryStore.setRegistrationContext(RegistrationEventContext.GENERIC_INVITE_PHONE)
      // Temporary tracking to check on high number of invite registrations AN-4117
      val referralToken: String = getControllerFactory.getUserPreferencesController.getReferralToken
      val token: String = getControllerFactory.getUserPreferencesController.getGenericInvitationToken
      injectJava(classOf[GlobalTrackingController]).tagEvent(new OpenedPhoneRegistrationFromInviteEvent(referralToken, token))
    }
    else {
      getStoreFactory.getAppEntryStore.setRegistrationContext(RegistrationEventContext.PHONE)
    }
    getControllerFactory.getNavigationController.setLeftPage(Page.PHONE_REGISTRATION, AppEntryActivity.TAG)
  }

  def onShowPhoneSignInPage(): Unit = {
    val transaction: FragmentTransaction = getSupportFragmentManager.beginTransaction
    setDefaultAnimation(transaction).replace(R.id.fl_main_content, PhoneSignInFragment.newInstance, PhoneSignInFragment.TAG).commit
    enableProgress(false)
    getControllerFactory.getNavigationController.setLeftPage(Page.PHONE_LOGIN, AppEntryActivity.TAG)
  }

  def onShowPhoneCodePage(): Unit = {
    if (isPaused) {
      return
    }
    val transaction: FragmentTransaction = getSupportFragmentManager.beginTransaction
    setDefaultAnimation(transaction).replace(R.id.fl_main_content, VerifyPhoneFragment.newInstance(false), VerifyPhoneFragment.TAG).commit
    enableProgress(false)
    getControllerFactory.getNavigationController.setLeftPage(Page.PHONE_REGISTRATION_VERIFY_CODE, AppEntryActivity.TAG)
  }

  def onShowPhoneAddEmailPage(): Unit = {
    if (isPaused) {
      return
    }
    val transaction: FragmentTransaction = getSupportFragmentManager.beginTransaction
    setDefaultAnimation(transaction).replace(R.id.fl_main_content, OTRPhoneAddEmailFragment.newInstance, OTRPhoneAddEmailFragment.TAG).addToBackStack(OTRPhoneAddEmailFragment.TAG).commit
    enableProgress(false)
  }

  def onShowPhoneVerifyEmailPage(): Unit = {
    val transaction: FragmentTransaction = getSupportFragmentManager.beginTransaction
    setDefaultAnimation(transaction).replace(R.id.fl_main_content, PhoneVerifyEmailFragment.newInstance, PhoneVerifyEmailFragment.TAG).commit
    enableProgress(false)
  }

  def onShowPhoneSetPicturePage(): Unit = {
    if (getSupportFragmentManager.findFragmentByTag(SignUpPhotoFragment.TAG) != null) {
      return
    }
    val transaction: FragmentTransaction = getSupportFragmentManager.beginTransaction
    setDefaultAnimation(transaction).replace(R.id.fl_main_content, SignUpPhotoFragment.newInstance(SignUpPhotoFragment.RegistrationType.Phone), SignUpPhotoFragment.TAG).commit
    enableProgress(false)
    getControllerFactory.getNavigationController.setLeftPage(Page.PHONE_REGISTRATION_ADD_PHOTO, AppEntryActivity.TAG)
  }

  def onShowEmailWelcomePage(): Unit = {
    val transaction: FragmentTransaction = getSupportFragmentManager.beginTransaction
    setDefaultAnimation(transaction).replace(R.id.fl_main_content, WelcomeEmailFragment.newInstance, WelcomeEmailFragment.TAG).commit
    enableProgress(false)
    KeyboardUtils.closeKeyboardIfShown(this)
  }

  def onShowEmailRegistrationPage(): Unit = {
    val transaction: FragmentTransaction = getSupportFragmentManager.beginTransaction
    setDefaultAnimation(transaction).replace(R.id.fl_main_content, EmailRegisterFragment.newInstance, EmailRegisterFragment.TAG).commit
    enableProgress(false)
    if (fromGenericInvite) {
      getStoreFactory.getAppEntryStore.setRegistrationContext(RegistrationEventContext.GENERIC_INVITE_EMAIL)
    }
    else {
      getStoreFactory.getAppEntryStore.setRegistrationContext(RegistrationEventContext.EMAIL)
    }
  }

  def onShowEmailVerifyEmailPage(): Unit = {
    val transaction: FragmentTransaction = getSupportFragmentManager.beginTransaction
    setDefaultAnimation(transaction).replace(R.id.fl_main_content, EmailVerifyEmailFragment.newInstance, EmailVerifyEmailFragment.TAG).commit
    enableProgress(false)
  }

  def onShowEmailSignInPage(): Unit = {
    val transaction: FragmentTransaction = getSupportFragmentManager.beginTransaction
    setDefaultAnimation(transaction).replace(R.id.fl_main_content, EmailSignInFragment.newInstance, EmailSignInFragment.TAG).commit
    enableProgress(false)
    getControllerFactory.getNavigationController.setLeftPage(Page.EMAIL_LOGIN, AppEntryActivity.TAG)
  }

  def onShowEmailSetPicturePage(): Unit = {
    if (getSupportFragmentManager.findFragmentByTag(SignUpPhotoFragment.TAG) != null) {
      return
    }
    val transaction: FragmentTransaction = getSupportFragmentManager.beginTransaction
    setDefaultAnimation(transaction).replace(R.id.fl_main_content, SignUpPhotoFragment.newInstance(SignUpPhotoFragment.RegistrationType.Email), SignUpPhotoFragment.TAG).commit
    enableProgress(false)
  }

  def onShowEmailPhoneCodePage(): Unit = {
    val transaction: FragmentTransaction = getSupportFragmentManager.beginTransaction
    setDefaultAnimation(transaction).replace(R.id.fl_main_content, VerifyPhoneFragment.newInstance(true), VerifyPhoneFragment.TAG).commit
    enableProgress(false)
  }

  def onShowFirstLaunchPage(): Unit = {
    val id: String = getStoreFactory.getAppEntryStore.getUserId
    val hasUserLoggedIn: Boolean = getControllerFactory.getUserPreferencesController.hasUserLoggedIn(id)
    if (id != null && hasUserLoggedIn) {
      getStoreFactory.getAppEntryStore.setState(AppEntryState.LOGGED_IN)
    }
    else {
      if (id != null) {
        getControllerFactory.getUserPreferencesController.userLoggedIn(id)
      }
      val transaction: FragmentTransaction = getSupportFragmentManager.beginTransaction
      setDefaultAnimation(transaction).replace(R.id.fl_main_content, FirstLaunchAfterLoginFragment.newInstance, FirstLaunchAfterLoginFragment.TAG).commit
      enableProgress(false)
    }
  }

  def tagAppEntryEvent(event: Event): Unit = {
    injectJava(classOf[GlobalTrackingController]).tagEvent(event)
    if (event.isInstanceOf[SucceededWithRegistrationEvent]) {
      Localytics.setProfileAttribute(Attribute.REGISTRATION_WEEK.name, ZTimeFormatter.getCurrentWeek(this), Localytics.ProfileScope.APPLICATION)
    }
  }

  def onShowPhoneNamePage(): Unit = {
    val transaction: FragmentTransaction = getSupportFragmentManager.beginTransaction
    setDefaultAnimation(transaction).replace(R.id.fl_main_content, PhoneSetNameFragment.newInstance, PhoneSetNameFragment.TAG).commit
    enableProgress(false)
    getControllerFactory.getNavigationController.setLeftPage(Page.PHONE_REGISTRATION_ADD_NAME, AppEntryActivity.TAG)
  }

  def onEnterApplication(openSettings: Boolean): Unit = {
    getControllerFactory.getNavigationController.removeNavigationControllerObserver(this)
    getControllerFactory.getVerificationController.finishVerification()
    val intent = new Intent(this, classOf[MainActivity])
    val bundle = new Bundle
    bundle.putBoolean(MainActivity.OpenSettingsArg, openSettings)
    intent.putExtras(bundle)
    startActivity(intent)
    finish()
  }

  private def setDefaultAnimation(transaction: FragmentTransaction): FragmentTransaction = {
    transaction.setCustomAnimations(R.anim.new_reg_in, R.anim.new_reg_out)
    transaction
  }

  def openCountryBox(): Unit = {
    getSupportFragmentManager.beginTransaction.setCustomAnimations(R.anim.fade_in, R.anim.fade_out, R.anim.fade_in, R.anim.fade_out).add(R.id.container__country_box, new CountryDialogFragment, CountryDialogFragment.TAG).addToBackStack(CountryDialogFragment.TAG).commit
    KeyboardUtils.hideKeyboard(this)
  }

  override protected def onRestoreInstanceState(@NonNull savedInstanceState: Bundle): Unit = {
    super.onRestoreInstanceState(savedInstanceState)
    getStoreFactory.getAppEntryStore.onRestoreInstanceState(savedInstanceState, getStoreFactory.getZMessagingApiStore.getApi.getSelf)
  }

  override protected def onSaveInstanceState(outState: Bundle): Unit = {
    getStoreFactory.getAppEntryStore.onSaveInstanceState(outState)
    super.onSaveInstanceState(outState)
  }

  def onOpenUrlInApp(url: String, withCloseButton: Boolean): Unit = {
    val prefixedUrl =
      if (!url.startsWith(AppEntryActivity.HTTP_PREFIX) && !url.startsWith(AppEntryActivity.HTTPS_PREFIX))
        AppEntryActivity.HTTP_PREFIX + url
      else
        url
    val transaction = getSupportFragmentManager.beginTransaction
    transaction.setCustomAnimations(R.anim.new_reg_in, R.anim.new_reg_out)
    transaction.add(R.id.fl_main_web_view, InAppWebViewFragment.newInstance(prefixedUrl, withCloseButton), InAppWebViewFragment.TAG)
    transaction.addToBackStack(InAppWebViewFragment.TAG)
    transaction.commit
    KeyboardUtils.hideKeyboard(this)
  }

  def dismissInAppWebView(): Unit = getSupportFragmentManager.popBackStackImmediate

  def getCountryController: CountryController = countryController

  def dismissCountryBox(): Unit = {
    getSupportFragmentManager.popBackStackImmediate
    KeyboardUtils.showKeyboard(this)
  }

  private def fromGenericInvite: Boolean = {
    val referralToken: String = getControllerFactory.getUserPreferencesController.getReferralToken
    val token: String = getControllerFactory.getUserPreferencesController.getGenericInvitationToken
    token != null || AppEntryStore.GENERAL_GENERIC_INVITE_TOKEN == referralToken
  }

  def getUnsplashImageAsset: ImageAsset = unsplashInitImageAsset

  def onPageVisible(page: Page): Unit = {
    page match {
      case Page.PHONE_REGISTRATION =>
        injectJava(classOf[GlobalTrackingController]).onApplicationScreen(ApplicationScreen.PHONE_REGISTRATION)
      case Page.PHONE_REGISTRATION_VERIFY_CODE =>
        injectJava(classOf[GlobalTrackingController]).onApplicationScreen(ApplicationScreen.PHONE_REGISTRATION__VERIFY_CODE)
      case Page.PHONE_REGISTRATION_ADD_NAME =>
        injectJava(classOf[GlobalTrackingController]).onApplicationScreen(ApplicationScreen.PHONE_REGISTRATION__ADD_NAME)
      case Page.PHONE_REGISTRATION_ADD_PHOTO =>
        injectJava(classOf[GlobalTrackingController]).onApplicationScreen(ApplicationScreen.PHONE_REGISTRATION__ADD_PHOTO)
      case Page.EMAIL_LOGIN =>
        injectJava(classOf[GlobalTrackingController]).onApplicationScreen(ApplicationScreen.EMAIL_LOGIN)
      case Page.PHONE_LOGIN =>
        injectJava(classOf[GlobalTrackingController]).onApplicationScreen(ApplicationScreen.PHONE_LOGIN)
      case _ =>
    }
  }
}
