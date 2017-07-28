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

import com.waz.api.ClientRegistrationState
import com.waz.api.impl._
import com.waz.model._
import com.waz.service.ZMessaging
import com.waz.threading.Threading
import com.waz.utils.events.{EventContext, Signal}
import com.waz.zclient.AppEntryController._
import com.waz.zclient.core.stores.appentry.AppEntryError
import com.waz.zclient.core.stores.appentry.IAppEntryStore.ErrorCallback

import scala.concurrent.Future

//TODO: Invitation token!
class AppEntryController(implicit inj: Injector, eventContext: EventContext) extends Injectable {

  implicit val ec = Threading.Background

  val optZms = inject[Signal[Option[ZMessaging]]]

  val currentAccount = for {
    accountManager <- ZMessaging.currentAccounts.activeAccountManager
    accountData <- accountManager.fold(Signal.const(Option.empty[AccountData])){ _.accountData.map(Option(_)) }
    optZms <- optZms
    userData <- accountData.flatMap(_.userId).fold(Signal.const(Option.empty[UserData]))(u => optZms.fold(Signal.const(Option.empty[UserData]))(_.usersStorage.optSignal(u)))
  } yield (accountData, userData)

  //TODO: remove
  val uiSignInState = Signal[UiSignInState](LoginEmail)

  val entryStage = for {
    uiSignInState <- uiSignInState
    (account, user) <- currentAccount
  } yield stateForAccountAndUser(account, user, uiSignInState)

  def stateForAccountAndUser(account: Option[AccountData], user: Option[UserData], uiSignInState: UiSignInState): AppEntryStage = {
    account.fold[AppEntryStage] {
      LoginStage(uiSignInState)
    } { accountData =>
      if (accountData.clientRegState == ClientRegistrationState.LIMIT_REACHED) {
        return DeviceLimitStage
      }
      user.fold[AppEntryStage] {
        Unknown //Has account but has no user, should be temporary
      } { userData =>
        if (userData.name.isEmpty) {
          return AddNameStage
        }
        if (userData.picture.isEmpty) {
          return AddPictureStage
        }
        if (!accountData.verified) {
          if (accountData.pendingEmail.isDefined) {
            return VerifyEmailStage
          }
          if (accountData.pendingPhone.isDefined) {
            return VerifyPhoneStage
          }
          return Unknown //Unverified account with no pending stuff
        }
        EnterAppStage
      }
    }
  }

  def loginPhone(phone: String, errorCallback: ErrorCallback): Future[Unit] = {
    login(PhoneCredentials(PhoneNumber(phone), Option.empty[ConfirmationCode])) map {
      case Left(error) => errorCallback.onError(AppEntryError.EMAIL_GENERIC_ERROR)
      case _ =>
    }
  }

  def loginEmail(email: String, password: String, errorCallback: ErrorCallback): Future[Unit] = {
    login(EmailCredentials(EmailAddress(email), Some(password))) map {
      case Left(error) => errorCallback.onError(AppEntryError.EMAIL_GENERIC_ERROR)
      case _ =>
    }
  }

  def registerEmail(name: String, email: String, password: String, errorCallback: ErrorCallback): Future[Unit] = {
    register(EmailCredentials(EmailAddress(email), Some(password)), name) map {
      case Left(error) => errorCallback.onError(AppEntryError.EMAIL_GENERIC_ERROR)
      case _ =>
    }
  }

  def login(credentials: Credentials): Future[Either[ErrorResponse, AccountData]] = {
    ZMessaging.currentAccounts.login(credentials)
  }

  def register(credentials: Credentials, name: String): Future[Either[ErrorResponse, AccountData]] = {
    ZMessaging.currentAccounts.register(credentials, name, AccentColors.defaultColor)
  }

  //For Java access
  def goToLoginEmail(): Unit ={
    uiSignInState ! LoginEmail
  }

  def goToLoginPhone(): Unit ={
    uiSignInState ! LoginPhone
  }

  def goToRegisterEmail(): Unit ={
    uiSignInState ! RegisterEmail
  }

}

object AppEntryController {
  trait AppEntryStage
  object Unknown extends AppEntryStage
  object EnterAppStage extends AppEntryStage
  object DeviceLimitStage extends AppEntryStage
  object AddNameStage extends AppEntryStage
  object AddPictureStage extends AppEntryStage
  object VerifyEmailStage extends AppEntryStage
  object VerifyPhoneStage extends AppEntryStage

  case class LoginStage(uiSignInState: UiSignInState) extends AppEntryStage

  //TODO: remove -- UI State should be kept in the UI
  trait UiSignInState
  object LoginEmail extends UiSignInState
  object LoginPhone extends UiSignInState
  object RegisterEmail extends UiSignInState
}
