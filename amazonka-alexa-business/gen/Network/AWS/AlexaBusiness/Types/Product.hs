{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.Product where

import Network.AWS.AlexaBusiness.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An address book with attributes.
--
--
--
-- /See:/ 'addressBook' smart constructor.
data AddressBook = AddressBook'
  { _abAddressBookARN :: !(Maybe Text)
  , _abName :: !(Maybe Text)
  , _abDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddressBook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'abAddressBookARN' - The ARN of the address book.
--
-- * 'abName' - The name of the address book.
--
-- * 'abDescription' - The description of the address book.
addressBook
    :: AddressBook
addressBook =
  AddressBook'
    {_abAddressBookARN = Nothing, _abName = Nothing, _abDescription = Nothing}


-- | The ARN of the address book.
abAddressBookARN :: Lens' AddressBook (Maybe Text)
abAddressBookARN = lens _abAddressBookARN (\ s a -> s{_abAddressBookARN = a})

-- | The name of the address book.
abName :: Lens' AddressBook (Maybe Text)
abName = lens _abName (\ s a -> s{_abName = a})

-- | The description of the address book.
abDescription :: Lens' AddressBook (Maybe Text)
abDescription = lens _abDescription (\ s a -> s{_abDescription = a})

instance FromJSON AddressBook where
        parseJSON
          = withObject "AddressBook"
              (\ x ->
                 AddressBook' <$>
                   (x .:? "AddressBookArn") <*> (x .:? "Name") <*>
                     (x .:? "Description"))

instance Hashable AddressBook where

instance NFData AddressBook where

-- | Information related to an address book.
--
--
--
-- /See:/ 'addressBookData' smart constructor.
data AddressBookData = AddressBookData'
  { _abdAddressBookARN :: !(Maybe Text)
  , _abdName :: !(Maybe Text)
  , _abdDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddressBookData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'abdAddressBookARN' - The ARN of the address book.
--
-- * 'abdName' - The name of the address book.
--
-- * 'abdDescription' - The description of the address book.
addressBookData
    :: AddressBookData
addressBookData =
  AddressBookData'
    { _abdAddressBookARN = Nothing
    , _abdName = Nothing
    , _abdDescription = Nothing
    }


-- | The ARN of the address book.
abdAddressBookARN :: Lens' AddressBookData (Maybe Text)
abdAddressBookARN = lens _abdAddressBookARN (\ s a -> s{_abdAddressBookARN = a})

-- | The name of the address book.
abdName :: Lens' AddressBookData (Maybe Text)
abdName = lens _abdName (\ s a -> s{_abdName = a})

-- | The description of the address book.
abdDescription :: Lens' AddressBookData (Maybe Text)
abdDescription = lens _abdDescription (\ s a -> s{_abdDescription = a})

instance FromJSON AddressBookData where
        parseJSON
          = withObject "AddressBookData"
              (\ x ->
                 AddressBookData' <$>
                   (x .:? "AddressBookArn") <*> (x .:? "Name") <*>
                     (x .:? "Description"))

instance Hashable AddressBookData where

instance NFData AddressBookData where

-- | Usage report with specified parameters.
--
--
--
-- /See:/ 'businessReport' smart constructor.
data BusinessReport = BusinessReport'
  { _brStatus :: !(Maybe BusinessReportStatus)
  , _brFailureCode :: !(Maybe BusinessReportFailureCode)
  , _brDeliveryTime :: !(Maybe POSIX)
  , _brDownloadURL :: !(Maybe Text)
  , _brS3Location :: !(Maybe BusinessReportS3Location)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BusinessReport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brStatus' - The status of the report generation execution (RUNNING, SUCCEEDED, or FAILED).
--
-- * 'brFailureCode' - The failure code.
--
-- * 'brDeliveryTime' - The time of report delivery.
--
-- * 'brDownloadURL' - The download link where a user can download the report.
--
-- * 'brS3Location' - The S3 location of the output reports.
businessReport
    :: BusinessReport
businessReport =
  BusinessReport'
    { _brStatus = Nothing
    , _brFailureCode = Nothing
    , _brDeliveryTime = Nothing
    , _brDownloadURL = Nothing
    , _brS3Location = Nothing
    }


-- | The status of the report generation execution (RUNNING, SUCCEEDED, or FAILED).
brStatus :: Lens' BusinessReport (Maybe BusinessReportStatus)
brStatus = lens _brStatus (\ s a -> s{_brStatus = a})

-- | The failure code.
brFailureCode :: Lens' BusinessReport (Maybe BusinessReportFailureCode)
brFailureCode = lens _brFailureCode (\ s a -> s{_brFailureCode = a})

-- | The time of report delivery.
brDeliveryTime :: Lens' BusinessReport (Maybe UTCTime)
brDeliveryTime = lens _brDeliveryTime (\ s a -> s{_brDeliveryTime = a}) . mapping _Time

-- | The download link where a user can download the report.
brDownloadURL :: Lens' BusinessReport (Maybe Text)
brDownloadURL = lens _brDownloadURL (\ s a -> s{_brDownloadURL = a})

-- | The S3 location of the output reports.
brS3Location :: Lens' BusinessReport (Maybe BusinessReportS3Location)
brS3Location = lens _brS3Location (\ s a -> s{_brS3Location = a})

instance FromJSON BusinessReport where
        parseJSON
          = withObject "BusinessReport"
              (\ x ->
                 BusinessReport' <$>
                   (x .:? "Status") <*> (x .:? "FailureCode") <*>
                     (x .:? "DeliveryTime")
                     <*> (x .:? "DownloadUrl")
                     <*> (x .:? "S3Location"))

instance Hashable BusinessReport where

instance NFData BusinessReport where

-- | The content range of the report.
--
--
--
-- /See:/ 'businessReportContentRange' smart constructor.
newtype BusinessReportContentRange = BusinessReportContentRange'
  { _brcrInterval :: Maybe BusinessReportInterval
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BusinessReportContentRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brcrInterval' - The interval of the content range.
businessReportContentRange
    :: BusinessReportContentRange
businessReportContentRange =
  BusinessReportContentRange' {_brcrInterval = Nothing}


-- | The interval of the content range.
brcrInterval :: Lens' BusinessReportContentRange (Maybe BusinessReportInterval)
brcrInterval = lens _brcrInterval (\ s a -> s{_brcrInterval = a})

instance FromJSON BusinessReportContentRange where
        parseJSON
          = withObject "BusinessReportContentRange"
              (\ x ->
                 BusinessReportContentRange' <$> (x .:? "Interval"))

instance Hashable BusinessReportContentRange where

instance NFData BusinessReportContentRange where

instance ToJSON BusinessReportContentRange where
        toJSON BusinessReportContentRange'{..}
          = object
              (catMaybes [("Interval" .=) <$> _brcrInterval])

-- | The recurrence of the reports.
--
--
--
-- /See:/ 'businessReportRecurrence' smart constructor.
newtype BusinessReportRecurrence = BusinessReportRecurrence'
  { _brrStartDate :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BusinessReportRecurrence' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brrStartDate' - The start date.
businessReportRecurrence
    :: BusinessReportRecurrence
businessReportRecurrence = BusinessReportRecurrence' {_brrStartDate = Nothing}


-- | The start date.
brrStartDate :: Lens' BusinessReportRecurrence (Maybe Text)
brrStartDate = lens _brrStartDate (\ s a -> s{_brrStartDate = a})

instance FromJSON BusinessReportRecurrence where
        parseJSON
          = withObject "BusinessReportRecurrence"
              (\ x ->
                 BusinessReportRecurrence' <$> (x .:? "StartDate"))

instance Hashable BusinessReportRecurrence where

instance NFData BusinessReportRecurrence where

instance ToJSON BusinessReportRecurrence where
        toJSON BusinessReportRecurrence'{..}
          = object
              (catMaybes [("StartDate" .=) <$> _brrStartDate])

-- | The S3 location of the output reports.
--
--
--
-- /See:/ 'businessReportS3Location' smart constructor.
data BusinessReportS3Location = BusinessReportS3Location'
  { _brslPath :: !(Maybe Text)
  , _brslBucketName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BusinessReportS3Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brslPath' - The path of the business report.
--
-- * 'brslBucketName' - The S3 bucket name of the output reports.
businessReportS3Location
    :: BusinessReportS3Location
businessReportS3Location =
  BusinessReportS3Location' {_brslPath = Nothing, _brslBucketName = Nothing}


-- | The path of the business report.
brslPath :: Lens' BusinessReportS3Location (Maybe Text)
brslPath = lens _brslPath (\ s a -> s{_brslPath = a})

-- | The S3 bucket name of the output reports.
brslBucketName :: Lens' BusinessReportS3Location (Maybe Text)
brslBucketName = lens _brslBucketName (\ s a -> s{_brslBucketName = a})

instance FromJSON BusinessReportS3Location where
        parseJSON
          = withObject "BusinessReportS3Location"
              (\ x ->
                 BusinessReportS3Location' <$>
                   (x .:? "Path") <*> (x .:? "BucketName"))

instance Hashable BusinessReportS3Location where

instance NFData BusinessReportS3Location where

-- | The schedule of the usage report.
--
--
--
-- /See:/ 'businessReportSchedule' smart constructor.
data BusinessReportSchedule = BusinessReportSchedule'
  { _brsS3KeyPrefix :: !(Maybe Text)
  , _brsLastBusinessReport :: !(Maybe BusinessReport)
  , _brsFormat :: !(Maybe BusinessReportFormat)
  , _brsRecurrence :: !(Maybe BusinessReportRecurrence)
  , _brsScheduleName :: !(Maybe Text)
  , _brsScheduleARN :: !(Maybe Text)
  , _brsContentRange :: !(Maybe BusinessReportContentRange)
  , _brsS3BucketName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BusinessReportSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brsS3KeyPrefix' - The S3 key where the report is delivered.
--
-- * 'brsLastBusinessReport' - The details of the last business report delivery for a specified time interval.
--
-- * 'brsFormat' - The format of the generated report (individual CSV files or zipped files of individual files).
--
-- * 'brsRecurrence' - The recurrence of the reports.
--
-- * 'brsScheduleName' - The name identifier of the schedule.
--
-- * 'brsScheduleARN' - The ARN of the business report schedule.
--
-- * 'brsContentRange' - The content range of the reports.
--
-- * 'brsS3BucketName' - The S3 bucket name of the output reports.
businessReportSchedule
    :: BusinessReportSchedule
businessReportSchedule =
  BusinessReportSchedule'
    { _brsS3KeyPrefix = Nothing
    , _brsLastBusinessReport = Nothing
    , _brsFormat = Nothing
    , _brsRecurrence = Nothing
    , _brsScheduleName = Nothing
    , _brsScheduleARN = Nothing
    , _brsContentRange = Nothing
    , _brsS3BucketName = Nothing
    }


-- | The S3 key where the report is delivered.
brsS3KeyPrefix :: Lens' BusinessReportSchedule (Maybe Text)
brsS3KeyPrefix = lens _brsS3KeyPrefix (\ s a -> s{_brsS3KeyPrefix = a})

-- | The details of the last business report delivery for a specified time interval.
brsLastBusinessReport :: Lens' BusinessReportSchedule (Maybe BusinessReport)
brsLastBusinessReport = lens _brsLastBusinessReport (\ s a -> s{_brsLastBusinessReport = a})

-- | The format of the generated report (individual CSV files or zipped files of individual files).
brsFormat :: Lens' BusinessReportSchedule (Maybe BusinessReportFormat)
brsFormat = lens _brsFormat (\ s a -> s{_brsFormat = a})

-- | The recurrence of the reports.
brsRecurrence :: Lens' BusinessReportSchedule (Maybe BusinessReportRecurrence)
brsRecurrence = lens _brsRecurrence (\ s a -> s{_brsRecurrence = a})

-- | The name identifier of the schedule.
brsScheduleName :: Lens' BusinessReportSchedule (Maybe Text)
brsScheduleName = lens _brsScheduleName (\ s a -> s{_brsScheduleName = a})

-- | The ARN of the business report schedule.
brsScheduleARN :: Lens' BusinessReportSchedule (Maybe Text)
brsScheduleARN = lens _brsScheduleARN (\ s a -> s{_brsScheduleARN = a})

-- | The content range of the reports.
brsContentRange :: Lens' BusinessReportSchedule (Maybe BusinessReportContentRange)
brsContentRange = lens _brsContentRange (\ s a -> s{_brsContentRange = a})

-- | The S3 bucket name of the output reports.
brsS3BucketName :: Lens' BusinessReportSchedule (Maybe Text)
brsS3BucketName = lens _brsS3BucketName (\ s a -> s{_brsS3BucketName = a})

instance FromJSON BusinessReportSchedule where
        parseJSON
          = withObject "BusinessReportSchedule"
              (\ x ->
                 BusinessReportSchedule' <$>
                   (x .:? "S3KeyPrefix") <*>
                     (x .:? "LastBusinessReport")
                     <*> (x .:? "Format")
                     <*> (x .:? "Recurrence")
                     <*> (x .:? "ScheduleName")
                     <*> (x .:? "ScheduleArn")
                     <*> (x .:? "ContentRange")
                     <*> (x .:? "S3BucketName"))

instance Hashable BusinessReportSchedule where

instance NFData BusinessReportSchedule where

-- | The skill store category that is shown. Alexa skills are assigned a specific skill category during creation, such as News, Social, and Sports.
--
--
--
-- /See:/ 'category' smart constructor.
data Category = Category'
  { _cCategoryName :: !(Maybe Text)
  , _cCategoryId :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Category' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCategoryName' - The name of the skill store category.
--
-- * 'cCategoryId' - The ID of the skill store category.
category
    :: Category
category = Category' {_cCategoryName = Nothing, _cCategoryId = Nothing}


-- | The name of the skill store category.
cCategoryName :: Lens' Category (Maybe Text)
cCategoryName = lens _cCategoryName (\ s a -> s{_cCategoryName = a})

-- | The ID of the skill store category.
cCategoryId :: Lens' Category (Maybe Natural)
cCategoryId = lens _cCategoryId (\ s a -> s{_cCategoryId = a}) . mapping _Nat

instance FromJSON Category where
        parseJSON
          = withObject "Category"
              (\ x ->
                 Category' <$>
                   (x .:? "CategoryName") <*> (x .:? "CategoryId"))

instance Hashable Category where

instance NFData Category where

-- | The default conference provider that is used if no other scheduled meetings are detected.
--
--
--
-- /See:/ 'conferencePreference' smart constructor.
newtype ConferencePreference = ConferencePreference'
  { _cpDefaultConferenceProviderARN :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConferencePreference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpDefaultConferenceProviderARN' - The ARN of the default conference provider.
conferencePreference
    :: ConferencePreference
conferencePreference =
  ConferencePreference' {_cpDefaultConferenceProviderARN = Nothing}


-- | The ARN of the default conference provider.
cpDefaultConferenceProviderARN :: Lens' ConferencePreference (Maybe Text)
cpDefaultConferenceProviderARN = lens _cpDefaultConferenceProviderARN (\ s a -> s{_cpDefaultConferenceProviderARN = a})

instance FromJSON ConferencePreference where
        parseJSON
          = withObject "ConferencePreference"
              (\ x ->
                 ConferencePreference' <$>
                   (x .:? "DefaultConferenceProviderArn"))

instance Hashable ConferencePreference where

instance NFData ConferencePreference where

instance ToJSON ConferencePreference where
        toJSON ConferencePreference'{..}
          = object
              (catMaybes
                 [("DefaultConferenceProviderArn" .=) <$>
                    _cpDefaultConferenceProviderARN])

-- | An entity that provides a conferencing solution. Alexa for Business acts as the voice interface and mediator that connects users to their preferred conference provider. Examples of conference providers include Amazon Chime, Zoom, Cisco, and Polycom. 
--
--
--
-- /See:/ 'conferenceProvider' smart constructor.
data ConferenceProvider = ConferenceProvider'
  { _cpMeetingSetting :: !(Maybe MeetingSetting)
  , _cpARN :: !(Maybe Text)
  , _cpPSTNDialIn :: !(Maybe PSTNDialIn)
  , _cpName :: !(Maybe Text)
  , _cpType :: !(Maybe ConferenceProviderType)
  , _cpIPDialIn :: !(Maybe IPDialIn)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConferenceProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpMeetingSetting' - The meeting settings for the conference provider.
--
-- * 'cpARN' - The ARN of the newly created conference provider.
--
-- * 'cpPSTNDialIn' - The information for PSTN conferencing.
--
-- * 'cpName' - The name of the conference provider.
--
-- * 'cpType' - The type of conference providers.
--
-- * 'cpIPDialIn' - The IP endpoint and protocol for calling.
conferenceProvider
    :: ConferenceProvider
conferenceProvider =
  ConferenceProvider'
    { _cpMeetingSetting = Nothing
    , _cpARN = Nothing
    , _cpPSTNDialIn = Nothing
    , _cpName = Nothing
    , _cpType = Nothing
    , _cpIPDialIn = Nothing
    }


-- | The meeting settings for the conference provider.
cpMeetingSetting :: Lens' ConferenceProvider (Maybe MeetingSetting)
cpMeetingSetting = lens _cpMeetingSetting (\ s a -> s{_cpMeetingSetting = a})

-- | The ARN of the newly created conference provider.
cpARN :: Lens' ConferenceProvider (Maybe Text)
cpARN = lens _cpARN (\ s a -> s{_cpARN = a})

-- | The information for PSTN conferencing.
cpPSTNDialIn :: Lens' ConferenceProvider (Maybe PSTNDialIn)
cpPSTNDialIn = lens _cpPSTNDialIn (\ s a -> s{_cpPSTNDialIn = a})

-- | The name of the conference provider.
cpName :: Lens' ConferenceProvider (Maybe Text)
cpName = lens _cpName (\ s a -> s{_cpName = a})

-- | The type of conference providers.
cpType :: Lens' ConferenceProvider (Maybe ConferenceProviderType)
cpType = lens _cpType (\ s a -> s{_cpType = a})

-- | The IP endpoint and protocol for calling.
cpIPDialIn :: Lens' ConferenceProvider (Maybe IPDialIn)
cpIPDialIn = lens _cpIPDialIn (\ s a -> s{_cpIPDialIn = a})

instance FromJSON ConferenceProvider where
        parseJSON
          = withObject "ConferenceProvider"
              (\ x ->
                 ConferenceProvider' <$>
                   (x .:? "MeetingSetting") <*> (x .:? "Arn") <*>
                     (x .:? "PSTNDialIn")
                     <*> (x .:? "Name")
                     <*> (x .:? "Type")
                     <*> (x .:? "IPDialIn"))

instance Hashable ConferenceProvider where

instance NFData ConferenceProvider where

-- | A contact with attributes.
--
--
--
-- /See:/ 'contact' smart constructor.
data Contact = Contact'
  { _cLastName :: !(Maybe Text)
  , _cContactARN :: !(Maybe Text)
  , _cPhoneNumber :: !(Maybe (Sensitive Text))
  , _cFirstName :: !(Maybe Text)
  , _cDisplayName :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'Contact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cLastName' - The last name of the contact, used to call the contact on the device.
--
-- * 'cContactARN' - The ARN of the contact.
--
-- * 'cPhoneNumber' - The phone number of the contact.
--
-- * 'cFirstName' - The first name of the contact, used to call the contact on the device.
--
-- * 'cDisplayName' - The name of the contact to display on the console.
contact
    :: Contact
contact =
  Contact'
    { _cLastName = Nothing
    , _cContactARN = Nothing
    , _cPhoneNumber = Nothing
    , _cFirstName = Nothing
    , _cDisplayName = Nothing
    }


-- | The last name of the contact, used to call the contact on the device.
cLastName :: Lens' Contact (Maybe Text)
cLastName = lens _cLastName (\ s a -> s{_cLastName = a})

-- | The ARN of the contact.
cContactARN :: Lens' Contact (Maybe Text)
cContactARN = lens _cContactARN (\ s a -> s{_cContactARN = a})

-- | The phone number of the contact.
cPhoneNumber :: Lens' Contact (Maybe Text)
cPhoneNumber = lens _cPhoneNumber (\ s a -> s{_cPhoneNumber = a}) . mapping _Sensitive

-- | The first name of the contact, used to call the contact on the device.
cFirstName :: Lens' Contact (Maybe Text)
cFirstName = lens _cFirstName (\ s a -> s{_cFirstName = a})

-- | The name of the contact to display on the console.
cDisplayName :: Lens' Contact (Maybe Text)
cDisplayName = lens _cDisplayName (\ s a -> s{_cDisplayName = a})

instance FromJSON Contact where
        parseJSON
          = withObject "Contact"
              (\ x ->
                 Contact' <$>
                   (x .:? "LastName") <*> (x .:? "ContactArn") <*>
                     (x .:? "PhoneNumber")
                     <*> (x .:? "FirstName")
                     <*> (x .:? "DisplayName"))

instance Hashable Contact where

instance NFData Contact where

-- | Information related to a contact.
--
--
--
-- /See:/ 'contactData' smart constructor.
data ContactData = ContactData'
  { _cdLastName :: !(Maybe Text)
  , _cdContactARN :: !(Maybe Text)
  , _cdPhoneNumber :: !(Maybe (Sensitive Text))
  , _cdFirstName :: !(Maybe Text)
  , _cdDisplayName :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContactData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdLastName' - The last name of the contact, used to call the contact on the device.
--
-- * 'cdContactARN' - The ARN of the contact.
--
-- * 'cdPhoneNumber' - The phone number of the contact.
--
-- * 'cdFirstName' - The first name of the contact, used to call the contact on the device.
--
-- * 'cdDisplayName' - The name of the contact to display on the console.
contactData
    :: ContactData
contactData =
  ContactData'
    { _cdLastName = Nothing
    , _cdContactARN = Nothing
    , _cdPhoneNumber = Nothing
    , _cdFirstName = Nothing
    , _cdDisplayName = Nothing
    }


-- | The last name of the contact, used to call the contact on the device.
cdLastName :: Lens' ContactData (Maybe Text)
cdLastName = lens _cdLastName (\ s a -> s{_cdLastName = a})

-- | The ARN of the contact.
cdContactARN :: Lens' ContactData (Maybe Text)
cdContactARN = lens _cdContactARN (\ s a -> s{_cdContactARN = a})

-- | The phone number of the contact.
cdPhoneNumber :: Lens' ContactData (Maybe Text)
cdPhoneNumber = lens _cdPhoneNumber (\ s a -> s{_cdPhoneNumber = a}) . mapping _Sensitive

-- | The first name of the contact, used to call the contact on the device.
cdFirstName :: Lens' ContactData (Maybe Text)
cdFirstName = lens _cdFirstName (\ s a -> s{_cdFirstName = a})

-- | The name of the contact to display on the console.
cdDisplayName :: Lens' ContactData (Maybe Text)
cdDisplayName = lens _cdDisplayName (\ s a -> s{_cdDisplayName = a})

instance FromJSON ContactData where
        parseJSON
          = withObject "ContactData"
              (\ x ->
                 ContactData' <$>
                   (x .:? "LastName") <*> (x .:? "ContactArn") <*>
                     (x .:? "PhoneNumber")
                     <*> (x .:? "FirstName")
                     <*> (x .:? "DisplayName"))

instance Hashable ContactData where

instance NFData ContactData where

-- | The details about the developer that published the skill.
--
--
--
-- /See:/ 'developerInfo' smart constructor.
data DeveloperInfo = DeveloperInfo'
  { _diEmail :: !(Maybe Text)
  , _diURL :: !(Maybe Text)
  , _diPrivacyPolicy :: !(Maybe Text)
  , _diDeveloperName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeveloperInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diEmail' - The email of the developer.
--
-- * 'diURL' - The website of the developer.
--
-- * 'diPrivacyPolicy' - The URL of the privacy policy.
--
-- * 'diDeveloperName' - The name of the developer.
developerInfo
    :: DeveloperInfo
developerInfo =
  DeveloperInfo'
    { _diEmail = Nothing
    , _diURL = Nothing
    , _diPrivacyPolicy = Nothing
    , _diDeveloperName = Nothing
    }


-- | The email of the developer.
diEmail :: Lens' DeveloperInfo (Maybe Text)
diEmail = lens _diEmail (\ s a -> s{_diEmail = a})

-- | The website of the developer.
diURL :: Lens' DeveloperInfo (Maybe Text)
diURL = lens _diURL (\ s a -> s{_diURL = a})

-- | The URL of the privacy policy.
diPrivacyPolicy :: Lens' DeveloperInfo (Maybe Text)
diPrivacyPolicy = lens _diPrivacyPolicy (\ s a -> s{_diPrivacyPolicy = a})

-- | The name of the developer.
diDeveloperName :: Lens' DeveloperInfo (Maybe Text)
diDeveloperName = lens _diDeveloperName (\ s a -> s{_diDeveloperName = a})

instance FromJSON DeveloperInfo where
        parseJSON
          = withObject "DeveloperInfo"
              (\ x ->
                 DeveloperInfo' <$>
                   (x .:? "Email") <*> (x .:? "Url") <*>
                     (x .:? "PrivacyPolicy")
                     <*> (x .:? "DeveloperName"))

instance Hashable DeveloperInfo where

instance NFData DeveloperInfo where

-- | A device with attributes.
--
--
--
-- /See:/ 'device' smart constructor.
data Device = Device'
  { _dDeviceStatus :: !(Maybe DeviceStatus)
  , _dDeviceStatusInfo :: !(Maybe DeviceStatusInfo)
  , _dDeviceARN :: !(Maybe Text)
  , _dMACAddress :: !(Maybe Text)
  , _dDeviceName :: !(Maybe Text)
  , _dRoomARN :: !(Maybe Text)
  , _dSoftwareVersion :: !(Maybe Text)
  , _dDeviceType :: !(Maybe Text)
  , _dDeviceSerialNumber :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Device' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDeviceStatus' - The status of a device. If the status is not READY, check the DeviceStatusInfo value for details.
--
-- * 'dDeviceStatusInfo' - Detailed information about a device's status.
--
-- * 'dDeviceARN' - The ARN of a device.
--
-- * 'dMACAddress' - The MAC address of a device.
--
-- * 'dDeviceName' - The name of a device.
--
-- * 'dRoomARN' - The room ARN of a device.
--
-- * 'dSoftwareVersion' - The software version of a device.
--
-- * 'dDeviceType' - The type of a device.
--
-- * 'dDeviceSerialNumber' - The serial number of a device.
device
    :: Device
device =
  Device'
    { _dDeviceStatus = Nothing
    , _dDeviceStatusInfo = Nothing
    , _dDeviceARN = Nothing
    , _dMACAddress = Nothing
    , _dDeviceName = Nothing
    , _dRoomARN = Nothing
    , _dSoftwareVersion = Nothing
    , _dDeviceType = Nothing
    , _dDeviceSerialNumber = Nothing
    }


-- | The status of a device. If the status is not READY, check the DeviceStatusInfo value for details.
dDeviceStatus :: Lens' Device (Maybe DeviceStatus)
dDeviceStatus = lens _dDeviceStatus (\ s a -> s{_dDeviceStatus = a})

-- | Detailed information about a device's status.
dDeviceStatusInfo :: Lens' Device (Maybe DeviceStatusInfo)
dDeviceStatusInfo = lens _dDeviceStatusInfo (\ s a -> s{_dDeviceStatusInfo = a})

-- | The ARN of a device.
dDeviceARN :: Lens' Device (Maybe Text)
dDeviceARN = lens _dDeviceARN (\ s a -> s{_dDeviceARN = a})

-- | The MAC address of a device.
dMACAddress :: Lens' Device (Maybe Text)
dMACAddress = lens _dMACAddress (\ s a -> s{_dMACAddress = a})

-- | The name of a device.
dDeviceName :: Lens' Device (Maybe Text)
dDeviceName = lens _dDeviceName (\ s a -> s{_dDeviceName = a})

-- | The room ARN of a device.
dRoomARN :: Lens' Device (Maybe Text)
dRoomARN = lens _dRoomARN (\ s a -> s{_dRoomARN = a})

-- | The software version of a device.
dSoftwareVersion :: Lens' Device (Maybe Text)
dSoftwareVersion = lens _dSoftwareVersion (\ s a -> s{_dSoftwareVersion = a})

-- | The type of a device.
dDeviceType :: Lens' Device (Maybe Text)
dDeviceType = lens _dDeviceType (\ s a -> s{_dDeviceType = a})

-- | The serial number of a device.
dDeviceSerialNumber :: Lens' Device (Maybe Text)
dDeviceSerialNumber = lens _dDeviceSerialNumber (\ s a -> s{_dDeviceSerialNumber = a})

instance FromJSON Device where
        parseJSON
          = withObject "Device"
              (\ x ->
                 Device' <$>
                   (x .:? "DeviceStatus") <*> (x .:? "DeviceStatusInfo")
                     <*> (x .:? "DeviceArn")
                     <*> (x .:? "MacAddress")
                     <*> (x .:? "DeviceName")
                     <*> (x .:? "RoomArn")
                     <*> (x .:? "SoftwareVersion")
                     <*> (x .:? "DeviceType")
                     <*> (x .:? "DeviceSerialNumber"))

instance Hashable Device where

instance NFData Device where

-- | Device attributes.
--
--
--
-- /See:/ 'deviceData' smart constructor.
data DeviceData = DeviceData'
  { _ddDeviceStatus :: !(Maybe DeviceStatus)
  , _ddDeviceStatusInfo :: !(Maybe DeviceStatusInfo)
  , _ddDeviceARN :: !(Maybe Text)
  , _ddMACAddress :: !(Maybe Text)
  , _ddDeviceName :: !(Maybe Text)
  , _ddRoomARN :: !(Maybe Text)
  , _ddSoftwareVersion :: !(Maybe Text)
  , _ddDeviceType :: !(Maybe Text)
  , _ddRoomName :: !(Maybe Text)
  , _ddDeviceSerialNumber :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeviceData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddDeviceStatus' - The status of a device.
--
-- * 'ddDeviceStatusInfo' - Detailed information about a device's status.
--
-- * 'ddDeviceARN' - The ARN of a device.
--
-- * 'ddMACAddress' - The MAC address of a device.
--
-- * 'ddDeviceName' - The name of a device.
--
-- * 'ddRoomARN' - The room ARN associated with a device.
--
-- * 'ddSoftwareVersion' - The software version of a device.
--
-- * 'ddDeviceType' - The type of a device.
--
-- * 'ddRoomName' - The name of the room associated with a device.
--
-- * 'ddDeviceSerialNumber' - The serial number of a device.
deviceData
    :: DeviceData
deviceData =
  DeviceData'
    { _ddDeviceStatus = Nothing
    , _ddDeviceStatusInfo = Nothing
    , _ddDeviceARN = Nothing
    , _ddMACAddress = Nothing
    , _ddDeviceName = Nothing
    , _ddRoomARN = Nothing
    , _ddSoftwareVersion = Nothing
    , _ddDeviceType = Nothing
    , _ddRoomName = Nothing
    , _ddDeviceSerialNumber = Nothing
    }


-- | The status of a device.
ddDeviceStatus :: Lens' DeviceData (Maybe DeviceStatus)
ddDeviceStatus = lens _ddDeviceStatus (\ s a -> s{_ddDeviceStatus = a})

-- | Detailed information about a device's status.
ddDeviceStatusInfo :: Lens' DeviceData (Maybe DeviceStatusInfo)
ddDeviceStatusInfo = lens _ddDeviceStatusInfo (\ s a -> s{_ddDeviceStatusInfo = a})

-- | The ARN of a device.
ddDeviceARN :: Lens' DeviceData (Maybe Text)
ddDeviceARN = lens _ddDeviceARN (\ s a -> s{_ddDeviceARN = a})

-- | The MAC address of a device.
ddMACAddress :: Lens' DeviceData (Maybe Text)
ddMACAddress = lens _ddMACAddress (\ s a -> s{_ddMACAddress = a})

-- | The name of a device.
ddDeviceName :: Lens' DeviceData (Maybe Text)
ddDeviceName = lens _ddDeviceName (\ s a -> s{_ddDeviceName = a})

-- | The room ARN associated with a device.
ddRoomARN :: Lens' DeviceData (Maybe Text)
ddRoomARN = lens _ddRoomARN (\ s a -> s{_ddRoomARN = a})

-- | The software version of a device.
ddSoftwareVersion :: Lens' DeviceData (Maybe Text)
ddSoftwareVersion = lens _ddSoftwareVersion (\ s a -> s{_ddSoftwareVersion = a})

-- | The type of a device.
ddDeviceType :: Lens' DeviceData (Maybe Text)
ddDeviceType = lens _ddDeviceType (\ s a -> s{_ddDeviceType = a})

-- | The name of the room associated with a device.
ddRoomName :: Lens' DeviceData (Maybe Text)
ddRoomName = lens _ddRoomName (\ s a -> s{_ddRoomName = a})

-- | The serial number of a device.
ddDeviceSerialNumber :: Lens' DeviceData (Maybe Text)
ddDeviceSerialNumber = lens _ddDeviceSerialNumber (\ s a -> s{_ddDeviceSerialNumber = a})

instance FromJSON DeviceData where
        parseJSON
          = withObject "DeviceData"
              (\ x ->
                 DeviceData' <$>
                   (x .:? "DeviceStatus") <*> (x .:? "DeviceStatusInfo")
                     <*> (x .:? "DeviceArn")
                     <*> (x .:? "MacAddress")
                     <*> (x .:? "DeviceName")
                     <*> (x .:? "RoomArn")
                     <*> (x .:? "SoftwareVersion")
                     <*> (x .:? "DeviceType")
                     <*> (x .:? "RoomName")
                     <*> (x .:? "DeviceSerialNumber"))

instance Hashable DeviceData where

instance NFData DeviceData where

-- | The list of device events.
--
--
--
-- /See:/ 'deviceEvent' smart constructor.
data DeviceEvent = DeviceEvent'
  { _deValue :: !(Maybe Text)
  , _deType :: !(Maybe DeviceEventType)
  , _deTimestamp :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeviceEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deValue' - The value of the event.
--
-- * 'deType' - The type of device event.
--
-- * 'deTimestamp' - The time (in epoch) when the event occurred. 
deviceEvent
    :: DeviceEvent
deviceEvent =
  DeviceEvent' {_deValue = Nothing, _deType = Nothing, _deTimestamp = Nothing}


-- | The value of the event.
deValue :: Lens' DeviceEvent (Maybe Text)
deValue = lens _deValue (\ s a -> s{_deValue = a})

-- | The type of device event.
deType :: Lens' DeviceEvent (Maybe DeviceEventType)
deType = lens _deType (\ s a -> s{_deType = a})

-- | The time (in epoch) when the event occurred. 
deTimestamp :: Lens' DeviceEvent (Maybe UTCTime)
deTimestamp = lens _deTimestamp (\ s a -> s{_deTimestamp = a}) . mapping _Time

instance FromJSON DeviceEvent where
        parseJSON
          = withObject "DeviceEvent"
              (\ x ->
                 DeviceEvent' <$>
                   (x .:? "Value") <*> (x .:? "Type") <*>
                     (x .:? "Timestamp"))

instance Hashable DeviceEvent where

instance NFData DeviceEvent where

-- | Details of a device’s status.
--
--
--
-- /See:/ 'deviceStatusDetail' smart constructor.
newtype DeviceStatusDetail = DeviceStatusDetail'
  { _dsdCode :: Maybe DeviceStatusDetailCode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeviceStatusDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsdCode' - The device status detail code.
deviceStatusDetail
    :: DeviceStatusDetail
deviceStatusDetail = DeviceStatusDetail' {_dsdCode = Nothing}


-- | The device status detail code.
dsdCode :: Lens' DeviceStatusDetail (Maybe DeviceStatusDetailCode)
dsdCode = lens _dsdCode (\ s a -> s{_dsdCode = a})

instance FromJSON DeviceStatusDetail where
        parseJSON
          = withObject "DeviceStatusDetail"
              (\ x -> DeviceStatusDetail' <$> (x .:? "Code"))

instance Hashable DeviceStatusDetail where

instance NFData DeviceStatusDetail where

-- | Detailed information about a device's status.
--
--
--
-- /See:/ 'deviceStatusInfo' smart constructor.
data DeviceStatusInfo = DeviceStatusInfo'
  { _dsiDeviceStatusDetails :: !(Maybe [DeviceStatusDetail])
  , _dsiConnectionStatus :: !(Maybe ConnectionStatus)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeviceStatusInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsiDeviceStatusDetails' - One or more device status detail descriptions.
--
-- * 'dsiConnectionStatus' - The latest available information about the connection status of a device. 
deviceStatusInfo
    :: DeviceStatusInfo
deviceStatusInfo =
  DeviceStatusInfo'
    {_dsiDeviceStatusDetails = Nothing, _dsiConnectionStatus = Nothing}


-- | One or more device status detail descriptions.
dsiDeviceStatusDetails :: Lens' DeviceStatusInfo [DeviceStatusDetail]
dsiDeviceStatusDetails = lens _dsiDeviceStatusDetails (\ s a -> s{_dsiDeviceStatusDetails = a}) . _Default . _Coerce

-- | The latest available information about the connection status of a device. 
dsiConnectionStatus :: Lens' DeviceStatusInfo (Maybe ConnectionStatus)
dsiConnectionStatus = lens _dsiConnectionStatus (\ s a -> s{_dsiConnectionStatus = a})

instance FromJSON DeviceStatusInfo where
        parseJSON
          = withObject "DeviceStatusInfo"
              (\ x ->
                 DeviceStatusInfo' <$>
                   (x .:? "DeviceStatusDetails" .!= mempty) <*>
                     (x .:? "ConnectionStatus"))

instance Hashable DeviceStatusInfo where

instance NFData DeviceStatusInfo where

-- | A filter name and value pair that is used to return a more specific list of results. Filters can be used to match a set of resources by various criteria.
--
--
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter'
  { _fKey :: !Text
  , _fValues :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fKey' - The key of a filter.
--
-- * 'fValues' - The values of a filter.
filter'
    :: Text -- ^ 'fKey'
    -> Filter
filter' pKey_ = Filter' {_fKey = pKey_, _fValues = mempty}


-- | The key of a filter.
fKey :: Lens' Filter Text
fKey = lens _fKey (\ s a -> s{_fKey = a})

-- | The values of a filter.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\ s a -> s{_fValues = a}) . _Coerce

instance Hashable Filter where

instance NFData Filter where

instance ToJSON Filter where
        toJSON Filter'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _fKey), Just ("Values" .= _fValues)])

-- | The IP endpoint and protocol for calling.
--
--
--
-- /See:/ 'ipDialIn' smart constructor.
data IPDialIn = IPDialIn'
  { _idiEndpoint :: !Text
  , _idiCommsProtocol :: !CommsProtocol
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IPDialIn' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idiEndpoint' - The IP address.
--
-- * 'idiCommsProtocol' - The protocol, including SIP, SIPS, and H323.
ipDialIn
    :: Text -- ^ 'idiEndpoint'
    -> CommsProtocol -- ^ 'idiCommsProtocol'
    -> IPDialIn
ipDialIn pEndpoint_ pCommsProtocol_ =
  IPDialIn' {_idiEndpoint = pEndpoint_, _idiCommsProtocol = pCommsProtocol_}


-- | The IP address.
idiEndpoint :: Lens' IPDialIn Text
idiEndpoint = lens _idiEndpoint (\ s a -> s{_idiEndpoint = a})

-- | The protocol, including SIP, SIPS, and H323.
idiCommsProtocol :: Lens' IPDialIn CommsProtocol
idiCommsProtocol = lens _idiCommsProtocol (\ s a -> s{_idiCommsProtocol = a})

instance FromJSON IPDialIn where
        parseJSON
          = withObject "IPDialIn"
              (\ x ->
                 IPDialIn' <$>
                   (x .: "Endpoint") <*> (x .: "CommsProtocol"))

instance Hashable IPDialIn where

instance NFData IPDialIn where

instance ToJSON IPDialIn where
        toJSON IPDialIn'{..}
          = object
              (catMaybes
                 [Just ("Endpoint" .= _idiEndpoint),
                  Just ("CommsProtocol" .= _idiCommsProtocol)])

-- | The values that indicate whether a pin is always required (YES), never required (NO), or OPTIONAL.
--
--
--     * If YES, Alexa will always ask for a meeting pin.
--
--     * If NO, Alexa will never ask for a meeting pin.
--
--     * If OPTIONAL, Alexa will ask if you have a meeting pin and if the customer responds with yes, it will ask for the meeting pin.
--
--
--
--
-- /See:/ 'meetingSetting' smart constructor.
newtype MeetingSetting = MeetingSetting'
  { _msRequirePin :: RequirePin
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MeetingSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msRequirePin' - The values that indicate whether the pin is always required.
meetingSetting
    :: RequirePin -- ^ 'msRequirePin'
    -> MeetingSetting
meetingSetting pRequirePin_ = MeetingSetting' {_msRequirePin = pRequirePin_}


-- | The values that indicate whether the pin is always required.
msRequirePin :: Lens' MeetingSetting RequirePin
msRequirePin = lens _msRequirePin (\ s a -> s{_msRequirePin = a})

instance FromJSON MeetingSetting where
        parseJSON
          = withObject "MeetingSetting"
              (\ x -> MeetingSetting' <$> (x .: "RequirePin"))

instance Hashable MeetingSetting where

instance NFData MeetingSetting where

instance ToJSON MeetingSetting where
        toJSON MeetingSetting'{..}
          = object
              (catMaybes [Just ("RequirePin" .= _msRequirePin)])

-- | The information for public switched telephone network (PSTN) conferencing.
--
--
--
-- /See:/ 'pSTNDialIn' smart constructor.
data PSTNDialIn = PSTNDialIn'
  { _pstndiCountryCode :: !Text
  , _pstndiPhoneNumber :: !Text
  , _pstndiOneClickIdDelay :: !Text
  , _pstndiOneClickPinDelay :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PSTNDialIn' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pstndiCountryCode' - The zip code.
--
-- * 'pstndiPhoneNumber' - The phone number to call to join the conference.
--
-- * 'pstndiOneClickIdDelay' - The delay duration before Alexa enters the conference ID with dual-tone multi-frequency (DTMF). Each number on the dial pad corresponds to a DTMF tone, which is how we send data over the telephone network.
--
-- * 'pstndiOneClickPinDelay' - The delay duration before Alexa enters the conference pin with dual-tone multi-frequency (DTMF). Each number on the dial pad corresponds to a DTMF tone, which is how we send data over the telephone network.
pSTNDialIn
    :: Text -- ^ 'pstndiCountryCode'
    -> Text -- ^ 'pstndiPhoneNumber'
    -> Text -- ^ 'pstndiOneClickIdDelay'
    -> Text -- ^ 'pstndiOneClickPinDelay'
    -> PSTNDialIn
pSTNDialIn pCountryCode_ pPhoneNumber_ pOneClickIdDelay_ pOneClickPinDelay_ =
  PSTNDialIn'
    { _pstndiCountryCode = pCountryCode_
    , _pstndiPhoneNumber = pPhoneNumber_
    , _pstndiOneClickIdDelay = pOneClickIdDelay_
    , _pstndiOneClickPinDelay = pOneClickPinDelay_
    }


-- | The zip code.
pstndiCountryCode :: Lens' PSTNDialIn Text
pstndiCountryCode = lens _pstndiCountryCode (\ s a -> s{_pstndiCountryCode = a})

-- | The phone number to call to join the conference.
pstndiPhoneNumber :: Lens' PSTNDialIn Text
pstndiPhoneNumber = lens _pstndiPhoneNumber (\ s a -> s{_pstndiPhoneNumber = a})

-- | The delay duration before Alexa enters the conference ID with dual-tone multi-frequency (DTMF). Each number on the dial pad corresponds to a DTMF tone, which is how we send data over the telephone network.
pstndiOneClickIdDelay :: Lens' PSTNDialIn Text
pstndiOneClickIdDelay = lens _pstndiOneClickIdDelay (\ s a -> s{_pstndiOneClickIdDelay = a})

-- | The delay duration before Alexa enters the conference pin with dual-tone multi-frequency (DTMF). Each number on the dial pad corresponds to a DTMF tone, which is how we send data over the telephone network.
pstndiOneClickPinDelay :: Lens' PSTNDialIn Text
pstndiOneClickPinDelay = lens _pstndiOneClickPinDelay (\ s a -> s{_pstndiOneClickPinDelay = a})

instance FromJSON PSTNDialIn where
        parseJSON
          = withObject "PSTNDialIn"
              (\ x ->
                 PSTNDialIn' <$>
                   (x .: "CountryCode") <*> (x .: "PhoneNumber") <*>
                     (x .: "OneClickIdDelay")
                     <*> (x .: "OneClickPinDelay"))

instance Hashable PSTNDialIn where

instance NFData PSTNDialIn where

instance ToJSON PSTNDialIn where
        toJSON PSTNDialIn'{..}
          = object
              (catMaybes
                 [Just ("CountryCode" .= _pstndiCountryCode),
                  Just ("PhoneNumber" .= _pstndiPhoneNumber),
                  Just ("OneClickIdDelay" .= _pstndiOneClickIdDelay),
                  Just
                    ("OneClickPinDelay" .= _pstndiOneClickPinDelay)])

-- | A room profile with attributes.
--
--
--
-- /See:/ 'profile' smart constructor.
data Profile = Profile'
  { _pSetupModeDisabled :: !(Maybe Bool)
  , _pPSTNEnabled :: !(Maybe Bool)
  , _pAddressBookARN :: !(Maybe Text)
  , _pDistanceUnit :: !(Maybe DistanceUnit)
  , _pAddress :: !(Maybe Text)
  , _pProfileARN :: !(Maybe Text)
  , _pWakeWord :: !(Maybe WakeWord)
  , _pProfileName :: !(Maybe Text)
  , _pTemperatureUnit :: !(Maybe TemperatureUnit)
  , _pTimezone :: !(Maybe Text)
  , _pMaxVolumeLimit :: !(Maybe Int)
  , _pIsDefault :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Profile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pSetupModeDisabled' - The setup mode of a room profile.
--
-- * 'pPSTNEnabled' - The PSTN setting of a room profile.
--
-- * 'pAddressBookARN' - The ARN of the address book.
--
-- * 'pDistanceUnit' - The distance unit of a room profile.
--
-- * 'pAddress' - The address of a room profile.
--
-- * 'pProfileARN' - The ARN of a room profile.
--
-- * 'pWakeWord' - The wake word of a room profile.
--
-- * 'pProfileName' - The name of a room profile.
--
-- * 'pTemperatureUnit' - The temperature unit of a room profile.
--
-- * 'pTimezone' - The time zone of a room profile.
--
-- * 'pMaxVolumeLimit' - The max volume limit of a room profile.
--
-- * 'pIsDefault' - Retrieves if the profile is default or not.
profile
    :: Profile
profile =
  Profile'
    { _pSetupModeDisabled = Nothing
    , _pPSTNEnabled = Nothing
    , _pAddressBookARN = Nothing
    , _pDistanceUnit = Nothing
    , _pAddress = Nothing
    , _pProfileARN = Nothing
    , _pWakeWord = Nothing
    , _pProfileName = Nothing
    , _pTemperatureUnit = Nothing
    , _pTimezone = Nothing
    , _pMaxVolumeLimit = Nothing
    , _pIsDefault = Nothing
    }


-- | The setup mode of a room profile.
pSetupModeDisabled :: Lens' Profile (Maybe Bool)
pSetupModeDisabled = lens _pSetupModeDisabled (\ s a -> s{_pSetupModeDisabled = a})

-- | The PSTN setting of a room profile.
pPSTNEnabled :: Lens' Profile (Maybe Bool)
pPSTNEnabled = lens _pPSTNEnabled (\ s a -> s{_pPSTNEnabled = a})

-- | The ARN of the address book.
pAddressBookARN :: Lens' Profile (Maybe Text)
pAddressBookARN = lens _pAddressBookARN (\ s a -> s{_pAddressBookARN = a})

-- | The distance unit of a room profile.
pDistanceUnit :: Lens' Profile (Maybe DistanceUnit)
pDistanceUnit = lens _pDistanceUnit (\ s a -> s{_pDistanceUnit = a})

-- | The address of a room profile.
pAddress :: Lens' Profile (Maybe Text)
pAddress = lens _pAddress (\ s a -> s{_pAddress = a})

-- | The ARN of a room profile.
pProfileARN :: Lens' Profile (Maybe Text)
pProfileARN = lens _pProfileARN (\ s a -> s{_pProfileARN = a})

-- | The wake word of a room profile.
pWakeWord :: Lens' Profile (Maybe WakeWord)
pWakeWord = lens _pWakeWord (\ s a -> s{_pWakeWord = a})

-- | The name of a room profile.
pProfileName :: Lens' Profile (Maybe Text)
pProfileName = lens _pProfileName (\ s a -> s{_pProfileName = a})

-- | The temperature unit of a room profile.
pTemperatureUnit :: Lens' Profile (Maybe TemperatureUnit)
pTemperatureUnit = lens _pTemperatureUnit (\ s a -> s{_pTemperatureUnit = a})

-- | The time zone of a room profile.
pTimezone :: Lens' Profile (Maybe Text)
pTimezone = lens _pTimezone (\ s a -> s{_pTimezone = a})

-- | The max volume limit of a room profile.
pMaxVolumeLimit :: Lens' Profile (Maybe Int)
pMaxVolumeLimit = lens _pMaxVolumeLimit (\ s a -> s{_pMaxVolumeLimit = a})

-- | Retrieves if the profile is default or not.
pIsDefault :: Lens' Profile (Maybe Bool)
pIsDefault = lens _pIsDefault (\ s a -> s{_pIsDefault = a})

instance FromJSON Profile where
        parseJSON
          = withObject "Profile"
              (\ x ->
                 Profile' <$>
                   (x .:? "SetupModeDisabled") <*> (x .:? "PSTNEnabled")
                     <*> (x .:? "AddressBookArn")
                     <*> (x .:? "DistanceUnit")
                     <*> (x .:? "Address")
                     <*> (x .:? "ProfileArn")
                     <*> (x .:? "WakeWord")
                     <*> (x .:? "ProfileName")
                     <*> (x .:? "TemperatureUnit")
                     <*> (x .:? "Timezone")
                     <*> (x .:? "MaxVolumeLimit")
                     <*> (x .:? "IsDefault"))

instance Hashable Profile where

instance NFData Profile where

-- | The data of a room profile.
--
--
--
-- /See:/ 'profileData' smart constructor.
data ProfileData = ProfileData'
  { _pdDistanceUnit :: !(Maybe DistanceUnit)
  , _pdAddress :: !(Maybe Text)
  , _pdProfileARN :: !(Maybe Text)
  , _pdWakeWord :: !(Maybe WakeWord)
  , _pdProfileName :: !(Maybe Text)
  , _pdTemperatureUnit :: !(Maybe TemperatureUnit)
  , _pdTimezone :: !(Maybe Text)
  , _pdIsDefault :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProfileData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdDistanceUnit' - The distance unit of a room profile.
--
-- * 'pdAddress' - The address of a room profile.
--
-- * 'pdProfileARN' - The ARN of a room profile.
--
-- * 'pdWakeWord' - The wake word of a room profile.
--
-- * 'pdProfileName' - The name of a room profile.
--
-- * 'pdTemperatureUnit' - The temperature unit of a room profile.
--
-- * 'pdTimezone' - The timezone of a room profile.
--
-- * 'pdIsDefault' - Retrieves if the profile data is default or not.
profileData
    :: ProfileData
profileData =
  ProfileData'
    { _pdDistanceUnit = Nothing
    , _pdAddress = Nothing
    , _pdProfileARN = Nothing
    , _pdWakeWord = Nothing
    , _pdProfileName = Nothing
    , _pdTemperatureUnit = Nothing
    , _pdTimezone = Nothing
    , _pdIsDefault = Nothing
    }


-- | The distance unit of a room profile.
pdDistanceUnit :: Lens' ProfileData (Maybe DistanceUnit)
pdDistanceUnit = lens _pdDistanceUnit (\ s a -> s{_pdDistanceUnit = a})

-- | The address of a room profile.
pdAddress :: Lens' ProfileData (Maybe Text)
pdAddress = lens _pdAddress (\ s a -> s{_pdAddress = a})

-- | The ARN of a room profile.
pdProfileARN :: Lens' ProfileData (Maybe Text)
pdProfileARN = lens _pdProfileARN (\ s a -> s{_pdProfileARN = a})

-- | The wake word of a room profile.
pdWakeWord :: Lens' ProfileData (Maybe WakeWord)
pdWakeWord = lens _pdWakeWord (\ s a -> s{_pdWakeWord = a})

-- | The name of a room profile.
pdProfileName :: Lens' ProfileData (Maybe Text)
pdProfileName = lens _pdProfileName (\ s a -> s{_pdProfileName = a})

-- | The temperature unit of a room profile.
pdTemperatureUnit :: Lens' ProfileData (Maybe TemperatureUnit)
pdTemperatureUnit = lens _pdTemperatureUnit (\ s a -> s{_pdTemperatureUnit = a})

-- | The timezone of a room profile.
pdTimezone :: Lens' ProfileData (Maybe Text)
pdTimezone = lens _pdTimezone (\ s a -> s{_pdTimezone = a})

-- | Retrieves if the profile data is default or not.
pdIsDefault :: Lens' ProfileData (Maybe Bool)
pdIsDefault = lens _pdIsDefault (\ s a -> s{_pdIsDefault = a})

instance FromJSON ProfileData where
        parseJSON
          = withObject "ProfileData"
              (\ x ->
                 ProfileData' <$>
                   (x .:? "DistanceUnit") <*> (x .:? "Address") <*>
                     (x .:? "ProfileArn")
                     <*> (x .:? "WakeWord")
                     <*> (x .:? "ProfileName")
                     <*> (x .:? "TemperatureUnit")
                     <*> (x .:? "Timezone")
                     <*> (x .:? "IsDefault"))

instance Hashable ProfileData where

instance NFData ProfileData where

-- | A room with attributes.
--
--
--
-- /See:/ 'room' smart constructor.
data Room = Room'
  { _rProfileARN :: !(Maybe Text)
  , _rProviderCalendarId :: !(Maybe Text)
  , _rRoomARN :: !(Maybe Text)
  , _rRoomName :: !(Maybe Text)
  , _rDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Room' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rProfileARN' - The profile ARN of a room.
--
-- * 'rProviderCalendarId' - The provider calendar ARN of a room.
--
-- * 'rRoomARN' - The ARN of a room.
--
-- * 'rRoomName' - The name of a room.
--
-- * 'rDescription' - The description of a room.
room
    :: Room
room =
  Room'
    { _rProfileARN = Nothing
    , _rProviderCalendarId = Nothing
    , _rRoomARN = Nothing
    , _rRoomName = Nothing
    , _rDescription = Nothing
    }


-- | The profile ARN of a room.
rProfileARN :: Lens' Room (Maybe Text)
rProfileARN = lens _rProfileARN (\ s a -> s{_rProfileARN = a})

-- | The provider calendar ARN of a room.
rProviderCalendarId :: Lens' Room (Maybe Text)
rProviderCalendarId = lens _rProviderCalendarId (\ s a -> s{_rProviderCalendarId = a})

-- | The ARN of a room.
rRoomARN :: Lens' Room (Maybe Text)
rRoomARN = lens _rRoomARN (\ s a -> s{_rRoomARN = a})

-- | The name of a room.
rRoomName :: Lens' Room (Maybe Text)
rRoomName = lens _rRoomName (\ s a -> s{_rRoomName = a})

-- | The description of a room.
rDescription :: Lens' Room (Maybe Text)
rDescription = lens _rDescription (\ s a -> s{_rDescription = a})

instance FromJSON Room where
        parseJSON
          = withObject "Room"
              (\ x ->
                 Room' <$>
                   (x .:? "ProfileArn") <*> (x .:? "ProviderCalendarId")
                     <*> (x .:? "RoomArn")
                     <*> (x .:? "RoomName")
                     <*> (x .:? "Description"))

instance Hashable Room where

instance NFData Room where

-- | The data of a room.
--
--
--
-- /See:/ 'roomData' smart constructor.
data RoomData = RoomData'
  { _rdProfileARN :: !(Maybe Text)
  , _rdProviderCalendarId :: !(Maybe Text)
  , _rdProfileName :: !(Maybe Text)
  , _rdRoomARN :: !(Maybe Text)
  , _rdRoomName :: !(Maybe Text)
  , _rdDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RoomData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdProfileARN' - The profile ARN of a room.
--
-- * 'rdProviderCalendarId' - The provider calendar ARN of a room.
--
-- * 'rdProfileName' - The profile name of a room.
--
-- * 'rdRoomARN' - The ARN of a room.
--
-- * 'rdRoomName' - The name of a room.
--
-- * 'rdDescription' - The description of a room.
roomData
    :: RoomData
roomData =
  RoomData'
    { _rdProfileARN = Nothing
    , _rdProviderCalendarId = Nothing
    , _rdProfileName = Nothing
    , _rdRoomARN = Nothing
    , _rdRoomName = Nothing
    , _rdDescription = Nothing
    }


-- | The profile ARN of a room.
rdProfileARN :: Lens' RoomData (Maybe Text)
rdProfileARN = lens _rdProfileARN (\ s a -> s{_rdProfileARN = a})

-- | The provider calendar ARN of a room.
rdProviderCalendarId :: Lens' RoomData (Maybe Text)
rdProviderCalendarId = lens _rdProviderCalendarId (\ s a -> s{_rdProviderCalendarId = a})

-- | The profile name of a room.
rdProfileName :: Lens' RoomData (Maybe Text)
rdProfileName = lens _rdProfileName (\ s a -> s{_rdProfileName = a})

-- | The ARN of a room.
rdRoomARN :: Lens' RoomData (Maybe Text)
rdRoomARN = lens _rdRoomARN (\ s a -> s{_rdRoomARN = a})

-- | The name of a room.
rdRoomName :: Lens' RoomData (Maybe Text)
rdRoomName = lens _rdRoomName (\ s a -> s{_rdRoomName = a})

-- | The description of a room.
rdDescription :: Lens' RoomData (Maybe Text)
rdDescription = lens _rdDescription (\ s a -> s{_rdDescription = a})

instance FromJSON RoomData where
        parseJSON
          = withObject "RoomData"
              (\ x ->
                 RoomData' <$>
                   (x .:? "ProfileArn") <*> (x .:? "ProviderCalendarId")
                     <*> (x .:? "ProfileName")
                     <*> (x .:? "RoomArn")
                     <*> (x .:? "RoomName")
                     <*> (x .:? "Description"))

instance Hashable RoomData where

instance NFData RoomData where

-- | A skill parameter associated with a room.
--
--
--
-- /See:/ 'roomSkillParameter' smart constructor.
data RoomSkillParameter = RoomSkillParameter'
  { _rspParameterKey :: !Text
  , _rspParameterValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RoomSkillParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rspParameterKey' - The parameter key of a room skill parameter. ParameterKey is an enumerated type that only takes “DEFAULT” or “SCOPE” as valid values.
--
-- * 'rspParameterValue' - The parameter value of a room skill parameter.
roomSkillParameter
    :: Text -- ^ 'rspParameterKey'
    -> Text -- ^ 'rspParameterValue'
    -> RoomSkillParameter
roomSkillParameter pParameterKey_ pParameterValue_ =
  RoomSkillParameter'
    {_rspParameterKey = pParameterKey_, _rspParameterValue = pParameterValue_}


-- | The parameter key of a room skill parameter. ParameterKey is an enumerated type that only takes “DEFAULT” or “SCOPE” as valid values.
rspParameterKey :: Lens' RoomSkillParameter Text
rspParameterKey = lens _rspParameterKey (\ s a -> s{_rspParameterKey = a})

-- | The parameter value of a room skill parameter.
rspParameterValue :: Lens' RoomSkillParameter Text
rspParameterValue = lens _rspParameterValue (\ s a -> s{_rspParameterValue = a})

instance FromJSON RoomSkillParameter where
        parseJSON
          = withObject "RoomSkillParameter"
              (\ x ->
                 RoomSkillParameter' <$>
                   (x .: "ParameterKey") <*> (x .: "ParameterValue"))

instance Hashable RoomSkillParameter where

instance NFData RoomSkillParameter where

instance ToJSON RoomSkillParameter where
        toJSON RoomSkillParameter'{..}
          = object
              (catMaybes
                 [Just ("ParameterKey" .= _rspParameterKey),
                  Just ("ParameterValue" .= _rspParameterValue)])

-- | Granular information about the skill.
--
--
--
-- /See:/ 'skillDetails' smart constructor.
data SkillDetails = SkillDetails'
  { _sdSkillTypes :: !(Maybe [Text])
  , _sdProductDescription :: !(Maybe Text)
  , _sdInvocationPhrase :: !(Maybe Text)
  , _sdDeveloperInfo :: !(Maybe DeveloperInfo)
  , _sdEndUserLicenseAgreement :: !(Maybe Text)
  , _sdGenericKeywords :: !(Maybe [Text])
  , _sdReviews :: !(Maybe (Map Text Text))
  , _sdReleaseDate :: !(Maybe Text)
  , _sdNewInThisVersionBulletPoints :: !(Maybe [Text])
  , _sdBulletPoints :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SkillDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdSkillTypes' - The types of skills.
--
-- * 'sdProductDescription' - The description of the product.
--
-- * 'sdInvocationPhrase' - The phrase used to trigger the skill.
--
-- * 'sdDeveloperInfo' - The details about the developer that published the skill.
--
-- * 'sdEndUserLicenseAgreement' - The URL of the end user license agreement.
--
-- * 'sdGenericKeywords' - The generic keywords associated with the skill that can be used to find a skill.
--
-- * 'sdReviews' - The list of reviews for the skill, including Key and Value pair.
--
-- * 'sdReleaseDate' - The date when the skill was released.
--
-- * 'sdNewInThisVersionBulletPoints' - The updates added in bullet points.
--
-- * 'sdBulletPoints' - The details about what the skill supports organized as bullet points.
skillDetails
    :: SkillDetails
skillDetails =
  SkillDetails'
    { _sdSkillTypes = Nothing
    , _sdProductDescription = Nothing
    , _sdInvocationPhrase = Nothing
    , _sdDeveloperInfo = Nothing
    , _sdEndUserLicenseAgreement = Nothing
    , _sdGenericKeywords = Nothing
    , _sdReviews = Nothing
    , _sdReleaseDate = Nothing
    , _sdNewInThisVersionBulletPoints = Nothing
    , _sdBulletPoints = Nothing
    }


-- | The types of skills.
sdSkillTypes :: Lens' SkillDetails [Text]
sdSkillTypes = lens _sdSkillTypes (\ s a -> s{_sdSkillTypes = a}) . _Default . _Coerce

-- | The description of the product.
sdProductDescription :: Lens' SkillDetails (Maybe Text)
sdProductDescription = lens _sdProductDescription (\ s a -> s{_sdProductDescription = a})

-- | The phrase used to trigger the skill.
sdInvocationPhrase :: Lens' SkillDetails (Maybe Text)
sdInvocationPhrase = lens _sdInvocationPhrase (\ s a -> s{_sdInvocationPhrase = a})

-- | The details about the developer that published the skill.
sdDeveloperInfo :: Lens' SkillDetails (Maybe DeveloperInfo)
sdDeveloperInfo = lens _sdDeveloperInfo (\ s a -> s{_sdDeveloperInfo = a})

-- | The URL of the end user license agreement.
sdEndUserLicenseAgreement :: Lens' SkillDetails (Maybe Text)
sdEndUserLicenseAgreement = lens _sdEndUserLicenseAgreement (\ s a -> s{_sdEndUserLicenseAgreement = a})

-- | The generic keywords associated with the skill that can be used to find a skill.
sdGenericKeywords :: Lens' SkillDetails [Text]
sdGenericKeywords = lens _sdGenericKeywords (\ s a -> s{_sdGenericKeywords = a}) . _Default . _Coerce

-- | The list of reviews for the skill, including Key and Value pair.
sdReviews :: Lens' SkillDetails (HashMap Text Text)
sdReviews = lens _sdReviews (\ s a -> s{_sdReviews = a}) . _Default . _Map

-- | The date when the skill was released.
sdReleaseDate :: Lens' SkillDetails (Maybe Text)
sdReleaseDate = lens _sdReleaseDate (\ s a -> s{_sdReleaseDate = a})

-- | The updates added in bullet points.
sdNewInThisVersionBulletPoints :: Lens' SkillDetails [Text]
sdNewInThisVersionBulletPoints = lens _sdNewInThisVersionBulletPoints (\ s a -> s{_sdNewInThisVersionBulletPoints = a}) . _Default . _Coerce

-- | The details about what the skill supports organized as bullet points.
sdBulletPoints :: Lens' SkillDetails [Text]
sdBulletPoints = lens _sdBulletPoints (\ s a -> s{_sdBulletPoints = a}) . _Default . _Coerce

instance FromJSON SkillDetails where
        parseJSON
          = withObject "SkillDetails"
              (\ x ->
                 SkillDetails' <$>
                   (x .:? "SkillTypes" .!= mempty) <*>
                     (x .:? "ProductDescription")
                     <*> (x .:? "InvocationPhrase")
                     <*> (x .:? "DeveloperInfo")
                     <*> (x .:? "EndUserLicenseAgreement")
                     <*> (x .:? "GenericKeywords" .!= mempty)
                     <*> (x .:? "Reviews" .!= mempty)
                     <*> (x .:? "ReleaseDate")
                     <*> (x .:? "NewInThisVersionBulletPoints" .!= mempty)
                     <*> (x .:? "BulletPoints" .!= mempty))

instance Hashable SkillDetails where

instance NFData SkillDetails where

-- | A skill group with attributes.
--
--
--
-- /See:/ 'skillGroup' smart constructor.
data SkillGroup = SkillGroup'
  { _sgSkillGroupARN :: !(Maybe Text)
  , _sgDescription :: !(Maybe Text)
  , _sgSkillGroupName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SkillGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgSkillGroupARN' - The ARN of a skill group.
--
-- * 'sgDescription' - The description of a skill group.
--
-- * 'sgSkillGroupName' - The name of a skill group.
skillGroup
    :: SkillGroup
skillGroup =
  SkillGroup'
    { _sgSkillGroupARN = Nothing
    , _sgDescription = Nothing
    , _sgSkillGroupName = Nothing
    }


-- | The ARN of a skill group.
sgSkillGroupARN :: Lens' SkillGroup (Maybe Text)
sgSkillGroupARN = lens _sgSkillGroupARN (\ s a -> s{_sgSkillGroupARN = a})

-- | The description of a skill group.
sgDescription :: Lens' SkillGroup (Maybe Text)
sgDescription = lens _sgDescription (\ s a -> s{_sgDescription = a})

-- | The name of a skill group.
sgSkillGroupName :: Lens' SkillGroup (Maybe Text)
sgSkillGroupName = lens _sgSkillGroupName (\ s a -> s{_sgSkillGroupName = a})

instance FromJSON SkillGroup where
        parseJSON
          = withObject "SkillGroup"
              (\ x ->
                 SkillGroup' <$>
                   (x .:? "SkillGroupArn") <*> (x .:? "Description") <*>
                     (x .:? "SkillGroupName"))

instance Hashable SkillGroup where

instance NFData SkillGroup where

-- | The attributes of a skill group.
--
--
--
-- /See:/ 'skillGroupData' smart constructor.
data SkillGroupData = SkillGroupData'
  { _sgdSkillGroupARN :: !(Maybe Text)
  , _sgdDescription :: !(Maybe Text)
  , _sgdSkillGroupName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SkillGroupData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgdSkillGroupARN' - The skill group ARN of a skill group.
--
-- * 'sgdDescription' - The description of a skill group.
--
-- * 'sgdSkillGroupName' - The skill group name of a skill group.
skillGroupData
    :: SkillGroupData
skillGroupData =
  SkillGroupData'
    { _sgdSkillGroupARN = Nothing
    , _sgdDescription = Nothing
    , _sgdSkillGroupName = Nothing
    }


-- | The skill group ARN of a skill group.
sgdSkillGroupARN :: Lens' SkillGroupData (Maybe Text)
sgdSkillGroupARN = lens _sgdSkillGroupARN (\ s a -> s{_sgdSkillGroupARN = a})

-- | The description of a skill group.
sgdDescription :: Lens' SkillGroupData (Maybe Text)
sgdDescription = lens _sgdDescription (\ s a -> s{_sgdDescription = a})

-- | The skill group name of a skill group.
sgdSkillGroupName :: Lens' SkillGroupData (Maybe Text)
sgdSkillGroupName = lens _sgdSkillGroupName (\ s a -> s{_sgdSkillGroupName = a})

instance FromJSON SkillGroupData where
        parseJSON
          = withObject "SkillGroupData"
              (\ x ->
                 SkillGroupData' <$>
                   (x .:? "SkillGroupArn") <*> (x .:? "Description") <*>
                     (x .:? "SkillGroupName"))

instance Hashable SkillGroupData where

instance NFData SkillGroupData where

-- | The summary of skills.
--
--
--
-- /See:/ 'skillSummary' smart constructor.
data SkillSummary = SkillSummary'
  { _ssSkillId :: !(Maybe Text)
  , _ssSupportsLinking :: !(Maybe Bool)
  , _ssSkillType :: !(Maybe SkillType)
  , _ssEnablementType :: !(Maybe EnablementType)
  , _ssSkillName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SkillSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssSkillId' - The ARN of the skill summary.
--
-- * 'ssSupportsLinking' - Linking support for a skill.
--
-- * 'ssSkillType' - Whether the skill is publicly available or is a private skill.
--
-- * 'ssEnablementType' - Whether the skill is enabled under the user's account, or if it requires linking to be used.
--
-- * 'ssSkillName' - The name of the skill.
skillSummary
    :: SkillSummary
skillSummary =
  SkillSummary'
    { _ssSkillId = Nothing
    , _ssSupportsLinking = Nothing
    , _ssSkillType = Nothing
    , _ssEnablementType = Nothing
    , _ssSkillName = Nothing
    }


-- | The ARN of the skill summary.
ssSkillId :: Lens' SkillSummary (Maybe Text)
ssSkillId = lens _ssSkillId (\ s a -> s{_ssSkillId = a})

-- | Linking support for a skill.
ssSupportsLinking :: Lens' SkillSummary (Maybe Bool)
ssSupportsLinking = lens _ssSupportsLinking (\ s a -> s{_ssSupportsLinking = a})

-- | Whether the skill is publicly available or is a private skill.
ssSkillType :: Lens' SkillSummary (Maybe SkillType)
ssSkillType = lens _ssSkillType (\ s a -> s{_ssSkillType = a})

-- | Whether the skill is enabled under the user's account, or if it requires linking to be used.
ssEnablementType :: Lens' SkillSummary (Maybe EnablementType)
ssEnablementType = lens _ssEnablementType (\ s a -> s{_ssEnablementType = a})

-- | The name of the skill.
ssSkillName :: Lens' SkillSummary (Maybe Text)
ssSkillName = lens _ssSkillName (\ s a -> s{_ssSkillName = a})

instance FromJSON SkillSummary where
        parseJSON
          = withObject "SkillSummary"
              (\ x ->
                 SkillSummary' <$>
                   (x .:? "SkillId") <*> (x .:? "SupportsLinking") <*>
                     (x .:? "SkillType")
                     <*> (x .:? "EnablementType")
                     <*> (x .:? "SkillName"))

instance Hashable SkillSummary where

instance NFData SkillSummary where

-- | The detailed information about an Alexa skill.
--
--
--
-- /See:/ 'skillsStoreSkill' smart constructor.
data SkillsStoreSkill = SkillsStoreSkill'
  { _sssSkillId :: !(Maybe Text)
  , _sssSupportsLinking :: !(Maybe Bool)
  , _sssSampleUtterances :: !(Maybe [Text])
  , _sssShortDescription :: !(Maybe Text)
  , _sssIconURL :: !(Maybe Text)
  , _sssSkillDetails :: !(Maybe SkillDetails)
  , _sssSkillName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SkillsStoreSkill' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sssSkillId' - The ARN of the skill.
--
-- * 'sssSupportsLinking' - Linking support for a skill.
--
-- * 'sssSampleUtterances' - Sample utterances that interact with the skill.
--
-- * 'sssShortDescription' - Short description about the skill.
--
-- * 'sssIconURL' - The URL where the skill icon resides.
--
-- * 'sssSkillDetails' - Information about the skill.
--
-- * 'sssSkillName' - The name of the skill.
skillsStoreSkill
    :: SkillsStoreSkill
skillsStoreSkill =
  SkillsStoreSkill'
    { _sssSkillId = Nothing
    , _sssSupportsLinking = Nothing
    , _sssSampleUtterances = Nothing
    , _sssShortDescription = Nothing
    , _sssIconURL = Nothing
    , _sssSkillDetails = Nothing
    , _sssSkillName = Nothing
    }


-- | The ARN of the skill.
sssSkillId :: Lens' SkillsStoreSkill (Maybe Text)
sssSkillId = lens _sssSkillId (\ s a -> s{_sssSkillId = a})

-- | Linking support for a skill.
sssSupportsLinking :: Lens' SkillsStoreSkill (Maybe Bool)
sssSupportsLinking = lens _sssSupportsLinking (\ s a -> s{_sssSupportsLinking = a})

-- | Sample utterances that interact with the skill.
sssSampleUtterances :: Lens' SkillsStoreSkill [Text]
sssSampleUtterances = lens _sssSampleUtterances (\ s a -> s{_sssSampleUtterances = a}) . _Default . _Coerce

-- | Short description about the skill.
sssShortDescription :: Lens' SkillsStoreSkill (Maybe Text)
sssShortDescription = lens _sssShortDescription (\ s a -> s{_sssShortDescription = a})

-- | The URL where the skill icon resides.
sssIconURL :: Lens' SkillsStoreSkill (Maybe Text)
sssIconURL = lens _sssIconURL (\ s a -> s{_sssIconURL = a})

-- | Information about the skill.
sssSkillDetails :: Lens' SkillsStoreSkill (Maybe SkillDetails)
sssSkillDetails = lens _sssSkillDetails (\ s a -> s{_sssSkillDetails = a})

-- | The name of the skill.
sssSkillName :: Lens' SkillsStoreSkill (Maybe Text)
sssSkillName = lens _sssSkillName (\ s a -> s{_sssSkillName = a})

instance FromJSON SkillsStoreSkill where
        parseJSON
          = withObject "SkillsStoreSkill"
              (\ x ->
                 SkillsStoreSkill' <$>
                   (x .:? "SkillId") <*> (x .:? "SupportsLinking") <*>
                     (x .:? "SampleUtterances" .!= mempty)
                     <*> (x .:? "ShortDescription")
                     <*> (x .:? "IconUrl")
                     <*> (x .:? "SkillDetails")
                     <*> (x .:? "SkillName"))

instance Hashable SkillsStoreSkill where

instance NFData SkillsStoreSkill where

-- | A smart home appliance that can connect to a central system. Any domestic device can be a smart appliance. 
--
--
--
-- /See:/ 'smartHomeAppliance' smart constructor.
data SmartHomeAppliance = SmartHomeAppliance'
  { _shaFriendlyName :: !(Maybe Text)
  , _shaManufacturerName :: !(Maybe Text)
  , _shaDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SmartHomeAppliance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'shaFriendlyName' - The friendly name of the smart home appliance.
--
-- * 'shaManufacturerName' - The name of the manufacturer of the smart home appliance.
--
-- * 'shaDescription' - The description of the smart home appliance.
smartHomeAppliance
    :: SmartHomeAppliance
smartHomeAppliance =
  SmartHomeAppliance'
    { _shaFriendlyName = Nothing
    , _shaManufacturerName = Nothing
    , _shaDescription = Nothing
    }


-- | The friendly name of the smart home appliance.
shaFriendlyName :: Lens' SmartHomeAppliance (Maybe Text)
shaFriendlyName = lens _shaFriendlyName (\ s a -> s{_shaFriendlyName = a})

-- | The name of the manufacturer of the smart home appliance.
shaManufacturerName :: Lens' SmartHomeAppliance (Maybe Text)
shaManufacturerName = lens _shaManufacturerName (\ s a -> s{_shaManufacturerName = a})

-- | The description of the smart home appliance.
shaDescription :: Lens' SmartHomeAppliance (Maybe Text)
shaDescription = lens _shaDescription (\ s a -> s{_shaDescription = a})

instance FromJSON SmartHomeAppliance where
        parseJSON
          = withObject "SmartHomeAppliance"
              (\ x ->
                 SmartHomeAppliance' <$>
                   (x .:? "FriendlyName") <*> (x .:? "ManufacturerName")
                     <*> (x .:? "Description"))

instance Hashable SmartHomeAppliance where

instance NFData SmartHomeAppliance where

-- | An object representing a sort criteria. 
--
--
--
-- /See:/ 'sort' smart constructor.
data Sort = Sort'
  { _sKey :: !Text
  , _sValue :: !SortValue
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Sort' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sKey' - The sort key of a sort object.
--
-- * 'sValue' - The sort value of a sort object.
sort
    :: Text -- ^ 'sKey'
    -> SortValue -- ^ 'sValue'
    -> Sort
sort pKey_ pValue_ = Sort' {_sKey = pKey_, _sValue = pValue_}


-- | The sort key of a sort object.
sKey :: Lens' Sort Text
sKey = lens _sKey (\ s a -> s{_sKey = a})

-- | The sort value of a sort object.
sValue :: Lens' Sort SortValue
sValue = lens _sValue (\ s a -> s{_sValue = a})

instance Hashable Sort where

instance NFData Sort where

instance ToJSON Sort where
        toJSON Sort'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _sKey), Just ("Value" .= _sValue)])

-- | A key-value pair that can be associated with a resource. 
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagKey :: !Text
  , _tagValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - The key of a tag. Tag keys are case-sensitive. 
--
-- * 'tagValue' - The value of a tag. Tag values are case-sensitive and can be null.
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}


-- | The key of a tag. Tag keys are case-sensitive. 
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

-- | The value of a tag. Tag values are case-sensitive and can be null.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .: "Key") <*> (x .: "Value"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _tagKey),
                  Just ("Value" .= _tagValue)])

-- | Information related to a user.
--
--
--
-- /See:/ 'userData' smart constructor.
data UserData = UserData'
  { _udEmail :: !(Maybe Text)
  , _udLastName :: !(Maybe Text)
  , _udEnrollmentId :: !(Maybe Text)
  , _udUserARN :: !(Maybe Text)
  , _udFirstName :: !(Maybe Text)
  , _udEnrollmentStatus :: !(Maybe EnrollmentStatus)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udEmail' - The email of a user.
--
-- * 'udLastName' - The last name of a user.
--
-- * 'udEnrollmentId' - The enrollment ARN of a user.
--
-- * 'udUserARN' - The ARN of a user.
--
-- * 'udFirstName' - The first name of a user.
--
-- * 'udEnrollmentStatus' - The enrollment status of a user.
userData
    :: UserData
userData =
  UserData'
    { _udEmail = Nothing
    , _udLastName = Nothing
    , _udEnrollmentId = Nothing
    , _udUserARN = Nothing
    , _udFirstName = Nothing
    , _udEnrollmentStatus = Nothing
    }


-- | The email of a user.
udEmail :: Lens' UserData (Maybe Text)
udEmail = lens _udEmail (\ s a -> s{_udEmail = a})

-- | The last name of a user.
udLastName :: Lens' UserData (Maybe Text)
udLastName = lens _udLastName (\ s a -> s{_udLastName = a})

-- | The enrollment ARN of a user.
udEnrollmentId :: Lens' UserData (Maybe Text)
udEnrollmentId = lens _udEnrollmentId (\ s a -> s{_udEnrollmentId = a})

-- | The ARN of a user.
udUserARN :: Lens' UserData (Maybe Text)
udUserARN = lens _udUserARN (\ s a -> s{_udUserARN = a})

-- | The first name of a user.
udFirstName :: Lens' UserData (Maybe Text)
udFirstName = lens _udFirstName (\ s a -> s{_udFirstName = a})

-- | The enrollment status of a user.
udEnrollmentStatus :: Lens' UserData (Maybe EnrollmentStatus)
udEnrollmentStatus = lens _udEnrollmentStatus (\ s a -> s{_udEnrollmentStatus = a})

instance FromJSON UserData where
        parseJSON
          = withObject "UserData"
              (\ x ->
                 UserData' <$>
                   (x .:? "Email") <*> (x .:? "LastName") <*>
                     (x .:? "EnrollmentId")
                     <*> (x .:? "UserArn")
                     <*> (x .:? "FirstName")
                     <*> (x .:? "EnrollmentStatus"))

instance Hashable UserData where

instance NFData UserData where
