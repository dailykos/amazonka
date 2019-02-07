{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Chime.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Chime.Types.Product where

import Network.AWS.Chime.Internal
import Network.AWS.Chime.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Amazon Chime account details. An AWS account can have multiple Amazon Chime accounts.
--
--
--
-- /See:/ 'account' smart constructor.
data Account = Account'
  { _aDefaultLicense :: !(Maybe License)
  , _aSupportedLicenses :: !(Maybe [License])
  , _aCreatedTimestamp :: !(Maybe POSIX)
  , _aAccountType :: !(Maybe AccountType)
  , _aAWSAccountId :: !Text
  , _aAccountId :: !Text
  , _aName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Account' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aDefaultLicense' - The default license for the Amazon Chime account.
--
-- * 'aSupportedLicenses' - Supported licenses for the Amazon Chime account.
--
-- * 'aCreatedTimestamp' - The Amazon Chime account creation timestamp, in ISO 8601 format.
--
-- * 'aAccountType' - The Amazon Chime account type. For more information about different account types, see <http://docs.aws.amazon.com/chime/latest/ag/manage-chime-account.html Managing Your Amazon Chime Accounts> in the /Amazon Chime Administration Guide/ .
--
-- * 'aAWSAccountId' - The AWS account ID.
--
-- * 'aAccountId' - The Amazon Chime account ID.
--
-- * 'aName' - The Amazon Chime account name.
account
    :: Text -- ^ 'aAWSAccountId'
    -> Text -- ^ 'aAccountId'
    -> Text -- ^ 'aName'
    -> Account
account pAWSAccountId_ pAccountId_ pName_ =
  Account'
    { _aDefaultLicense = Nothing
    , _aSupportedLicenses = Nothing
    , _aCreatedTimestamp = Nothing
    , _aAccountType = Nothing
    , _aAWSAccountId = pAWSAccountId_
    , _aAccountId = pAccountId_
    , _aName = pName_
    }


-- | The default license for the Amazon Chime account.
aDefaultLicense :: Lens' Account (Maybe License)
aDefaultLicense = lens _aDefaultLicense (\ s a -> s{_aDefaultLicense = a})

-- | Supported licenses for the Amazon Chime account.
aSupportedLicenses :: Lens' Account [License]
aSupportedLicenses = lens _aSupportedLicenses (\ s a -> s{_aSupportedLicenses = a}) . _Default . _Coerce

-- | The Amazon Chime account creation timestamp, in ISO 8601 format.
aCreatedTimestamp :: Lens' Account (Maybe UTCTime)
aCreatedTimestamp = lens _aCreatedTimestamp (\ s a -> s{_aCreatedTimestamp = a}) . mapping _Time

-- | The Amazon Chime account type. For more information about different account types, see <http://docs.aws.amazon.com/chime/latest/ag/manage-chime-account.html Managing Your Amazon Chime Accounts> in the /Amazon Chime Administration Guide/ .
aAccountType :: Lens' Account (Maybe AccountType)
aAccountType = lens _aAccountType (\ s a -> s{_aAccountType = a})

-- | The AWS account ID.
aAWSAccountId :: Lens' Account Text
aAWSAccountId = lens _aAWSAccountId (\ s a -> s{_aAWSAccountId = a})

-- | The Amazon Chime account ID.
aAccountId :: Lens' Account Text
aAccountId = lens _aAccountId (\ s a -> s{_aAccountId = a})

-- | The Amazon Chime account name.
aName :: Lens' Account Text
aName = lens _aName (\ s a -> s{_aName = a})

instance FromJSON Account where
        parseJSON
          = withObject "Account"
              (\ x ->
                 Account' <$>
                   (x .:? "DefaultLicense") <*>
                     (x .:? "SupportedLicenses" .!= mempty)
                     <*> (x .:? "CreatedTimestamp")
                     <*> (x .:? "AccountType")
                     <*> (x .: "AwsAccountId")
                     <*> (x .: "AccountId")
                     <*> (x .: "Name"))

instance Hashable Account where

instance NFData Account where

-- | Settings related to the Amazon Chime account. This includes settings that start or stop remote control of shared screens, or start or stop the dial-out option in the Amazon Chime web application. For more information about these settings, see <http://docs.aws.amazon.com/chime/latest/ag/policies.html Use the Policies Page> in the /Amazon Chime Administration Guide/ .
--
--
--
-- /See:/ 'accountSettings' smart constructor.
data AccountSettings = AccountSettings'
  { _asEnableDialOut :: !(Maybe Bool)
  , _asDisableRemoteControl :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asEnableDialOut' - Setting that allows meeting participants to choose the __Call me at a phone number__ option. For more information, see <http://docs.aws.amazon.com/chime/latest/ug/chime-join-meeting.html Join a Meeting without the Amazon Chime App> .
--
-- * 'asDisableRemoteControl' - Setting that stops or starts remote control of shared screens during meetings.
accountSettings
    :: AccountSettings
accountSettings =
  AccountSettings'
    {_asEnableDialOut = Nothing, _asDisableRemoteControl = Nothing}


-- | Setting that allows meeting participants to choose the __Call me at a phone number__ option. For more information, see <http://docs.aws.amazon.com/chime/latest/ug/chime-join-meeting.html Join a Meeting without the Amazon Chime App> .
asEnableDialOut :: Lens' AccountSettings (Maybe Bool)
asEnableDialOut = lens _asEnableDialOut (\ s a -> s{_asEnableDialOut = a})

-- | Setting that stops or starts remote control of shared screens during meetings.
asDisableRemoteControl :: Lens' AccountSettings (Maybe Bool)
asDisableRemoteControl = lens _asDisableRemoteControl (\ s a -> s{_asDisableRemoteControl = a})

instance FromJSON AccountSettings where
        parseJSON
          = withObject "AccountSettings"
              (\ x ->
                 AccountSettings' <$>
                   (x .:? "EnableDialOut") <*>
                     (x .:? "DisableRemoteControl"))

instance Hashable AccountSettings where

instance NFData AccountSettings where

instance ToJSON AccountSettings where
        toJSON AccountSettings'{..}
          = object
              (catMaybes
                 [("EnableDialOut" .=) <$> _asEnableDialOut,
                  ("DisableRemoteControl" .=) <$>
                    _asDisableRemoteControl])

-- | Invitation object returned after emailing users to invite them to join the Amazon Chime @Team@ account.
--
--
--
-- /See:/ 'invite' smart constructor.
data Invite = Invite'
  { _iStatus :: !(Maybe InviteStatus)
  , _iEmailStatus :: !(Maybe EmailStatus)
  , _iInviteId :: !(Maybe Text)
  , _iEmailAddress :: !(Maybe (Sensitive Text))
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'Invite' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iStatus' - The status of the invite.
--
-- * 'iEmailStatus' - The status of the invite email.
--
-- * 'iInviteId' - The invite ID.
--
-- * 'iEmailAddress' - The email address to which the invite is sent.
invite
    :: Invite
invite =
  Invite'
    { _iStatus = Nothing
    , _iEmailStatus = Nothing
    , _iInviteId = Nothing
    , _iEmailAddress = Nothing
    }


-- | The status of the invite.
iStatus :: Lens' Invite (Maybe InviteStatus)
iStatus = lens _iStatus (\ s a -> s{_iStatus = a})

-- | The status of the invite email.
iEmailStatus :: Lens' Invite (Maybe EmailStatus)
iEmailStatus = lens _iEmailStatus (\ s a -> s{_iEmailStatus = a})

-- | The invite ID.
iInviteId :: Lens' Invite (Maybe Text)
iInviteId = lens _iInviteId (\ s a -> s{_iInviteId = a})

-- | The email address to which the invite is sent.
iEmailAddress :: Lens' Invite (Maybe Text)
iEmailAddress = lens _iEmailAddress (\ s a -> s{_iEmailAddress = a}) . mapping _Sensitive

instance FromJSON Invite where
        parseJSON
          = withObject "Invite"
              (\ x ->
                 Invite' <$>
                   (x .:? "Status") <*> (x .:? "EmailStatus") <*>
                     (x .:? "InviteId")
                     <*> (x .:? "EmailAddress"))

instance Hashable Invite where

instance NFData Invite where

-- | The user ID and user fields to update, used with the 'BatchUpdateUser' action.
--
--
--
-- /See:/ 'updateUserRequestItem' smart constructor.
data UpdateUserRequestItem = UpdateUserRequestItem'
  { _uuriLicenseType :: !(Maybe License)
  , _uuriUserId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUserRequestItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uuriLicenseType' - The user license type.
--
-- * 'uuriUserId' - The user ID.
updateUserRequestItem
    :: Text -- ^ 'uuriUserId'
    -> UpdateUserRequestItem
updateUserRequestItem pUserId_ =
  UpdateUserRequestItem' {_uuriLicenseType = Nothing, _uuriUserId = pUserId_}


-- | The user license type.
uuriLicenseType :: Lens' UpdateUserRequestItem (Maybe License)
uuriLicenseType = lens _uuriLicenseType (\ s a -> s{_uuriLicenseType = a})

-- | The user ID.
uuriUserId :: Lens' UpdateUserRequestItem Text
uuriUserId = lens _uuriUserId (\ s a -> s{_uuriUserId = a})

instance Hashable UpdateUserRequestItem where

instance NFData UpdateUserRequestItem where

instance ToJSON UpdateUserRequestItem where
        toJSON UpdateUserRequestItem'{..}
          = object
              (catMaybes
                 [("LicenseType" .=) <$> _uuriLicenseType,
                  Just ("UserId" .= _uuriUserId)])

-- | The user on the Amazon Chime account.
--
--
--
-- /See:/ 'user' smart constructor.
data User = User'
  { _uUserInvitationStatus :: !(Maybe InviteStatus)
  , _uPersonalPIN :: !(Maybe Text)
  , _uLicenseType :: !(Maybe License)
  , _uRegisteredOn :: !(Maybe POSIX)
  , _uAccountId :: !(Maybe Text)
  , _uUserRegistrationStatus :: !(Maybe RegistrationStatus)
  , _uInvitedOn :: !(Maybe POSIX)
  , _uDisplayName :: !(Maybe (Sensitive Text))
  , _uPrimaryEmail :: !(Maybe (Sensitive Text))
  , _uUserId :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uUserInvitationStatus' - The user invite status.
--
-- * 'uPersonalPIN' - The user's personal meeting PIN.
--
-- * 'uLicenseType' - The license type for the user.
--
-- * 'uRegisteredOn' - Date and time when the user is registered, in ISO 8601 format.
--
-- * 'uAccountId' - The Amazon Chime account ID.
--
-- * 'uUserRegistrationStatus' - The user registration status.
--
-- * 'uInvitedOn' - Date and time when the user is invited to the Amazon Chime account, in ISO 8601 format.
--
-- * 'uDisplayName' - The display name of the user.
--
-- * 'uPrimaryEmail' - The primary email address of the user.
--
-- * 'uUserId' - The user ID.
user
    :: Text -- ^ 'uUserId'
    -> User
user pUserId_ =
  User'
    { _uUserInvitationStatus = Nothing
    , _uPersonalPIN = Nothing
    , _uLicenseType = Nothing
    , _uRegisteredOn = Nothing
    , _uAccountId = Nothing
    , _uUserRegistrationStatus = Nothing
    , _uInvitedOn = Nothing
    , _uDisplayName = Nothing
    , _uPrimaryEmail = Nothing
    , _uUserId = pUserId_
    }


-- | The user invite status.
uUserInvitationStatus :: Lens' User (Maybe InviteStatus)
uUserInvitationStatus = lens _uUserInvitationStatus (\ s a -> s{_uUserInvitationStatus = a})

-- | The user's personal meeting PIN.
uPersonalPIN :: Lens' User (Maybe Text)
uPersonalPIN = lens _uPersonalPIN (\ s a -> s{_uPersonalPIN = a})

-- | The license type for the user.
uLicenseType :: Lens' User (Maybe License)
uLicenseType = lens _uLicenseType (\ s a -> s{_uLicenseType = a})

-- | Date and time when the user is registered, in ISO 8601 format.
uRegisteredOn :: Lens' User (Maybe UTCTime)
uRegisteredOn = lens _uRegisteredOn (\ s a -> s{_uRegisteredOn = a}) . mapping _Time

-- | The Amazon Chime account ID.
uAccountId :: Lens' User (Maybe Text)
uAccountId = lens _uAccountId (\ s a -> s{_uAccountId = a})

-- | The user registration status.
uUserRegistrationStatus :: Lens' User (Maybe RegistrationStatus)
uUserRegistrationStatus = lens _uUserRegistrationStatus (\ s a -> s{_uUserRegistrationStatus = a})

-- | Date and time when the user is invited to the Amazon Chime account, in ISO 8601 format.
uInvitedOn :: Lens' User (Maybe UTCTime)
uInvitedOn = lens _uInvitedOn (\ s a -> s{_uInvitedOn = a}) . mapping _Time

-- | The display name of the user.
uDisplayName :: Lens' User (Maybe Text)
uDisplayName = lens _uDisplayName (\ s a -> s{_uDisplayName = a}) . mapping _Sensitive

-- | The primary email address of the user.
uPrimaryEmail :: Lens' User (Maybe Text)
uPrimaryEmail = lens _uPrimaryEmail (\ s a -> s{_uPrimaryEmail = a}) . mapping _Sensitive

-- | The user ID.
uUserId :: Lens' User Text
uUserId = lens _uUserId (\ s a -> s{_uUserId = a})

instance FromJSON User where
        parseJSON
          = withObject "User"
              (\ x ->
                 User' <$>
                   (x .:? "UserInvitationStatus") <*>
                     (x .:? "PersonalPIN")
                     <*> (x .:? "LicenseType")
                     <*> (x .:? "RegisteredOn")
                     <*> (x .:? "AccountId")
                     <*> (x .:? "UserRegistrationStatus")
                     <*> (x .:? "InvitedOn")
                     <*> (x .:? "DisplayName")
                     <*> (x .:? "PrimaryEmail")
                     <*> (x .: "UserId"))

instance Hashable User where

instance NFData User where

-- | The list of errors returned when errors are encountered during the 'BatchSuspendUser' , 'BatchUnsuspendUser' , or 'BatchUpdateUser' actions. This includes user IDs, error codes, and error messages.
--
--
--
-- /See:/ 'userError' smart constructor.
data UserError = UserError'
  { _ueUserId :: !(Maybe Text)
  , _ueErrorCode :: !(Maybe ErrorCode)
  , _ueErrorMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ueUserId' - The user ID for which the action failed.
--
-- * 'ueErrorCode' - The error code.
--
-- * 'ueErrorMessage' - The error message.
userError
    :: UserError
userError =
  UserError'
    {_ueUserId = Nothing, _ueErrorCode = Nothing, _ueErrorMessage = Nothing}


-- | The user ID for which the action failed.
ueUserId :: Lens' UserError (Maybe Text)
ueUserId = lens _ueUserId (\ s a -> s{_ueUserId = a})

-- | The error code.
ueErrorCode :: Lens' UserError (Maybe ErrorCode)
ueErrorCode = lens _ueErrorCode (\ s a -> s{_ueErrorCode = a})

-- | The error message.
ueErrorMessage :: Lens' UserError (Maybe Text)
ueErrorMessage = lens _ueErrorMessage (\ s a -> s{_ueErrorMessage = a})

instance FromJSON UserError where
        parseJSON
          = withObject "UserError"
              (\ x ->
                 UserError' <$>
                   (x .:? "UserId") <*> (x .:? "ErrorCode") <*>
                     (x .:? "ErrorMessage"))

instance Hashable UserError where

instance NFData UserError where
