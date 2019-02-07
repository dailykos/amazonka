{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.Product where

import Network.AWS.AppStream.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an application in the application catalog.
--
--
--
-- /See:/ 'application' smart constructor.
data Application = Application'
  { _appEnabled :: !(Maybe Bool)
  , _appLaunchPath :: !(Maybe Text)
  , _appLaunchParameters :: !(Maybe Text)
  , _appName :: !(Maybe Text)
  , _appDisplayName :: !(Maybe Text)
  , _appMetadata :: !(Maybe (Map Text Text))
  , _appIconURL :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Application' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'appEnabled' - If there is a problem, the application can be disabled after image creation.
--
-- * 'appLaunchPath' - The path to the application executable in the instance.
--
-- * 'appLaunchParameters' - The arguments that are passed to the application at launch.
--
-- * 'appName' - The name of the application.
--
-- * 'appDisplayName' - The application name to display.
--
-- * 'appMetadata' - Additional attributes that describe the application.
--
-- * 'appIconURL' - The URL for the application icon. This URL might be time-limited.
application
    :: Application
application =
  Application'
    { _appEnabled = Nothing
    , _appLaunchPath = Nothing
    , _appLaunchParameters = Nothing
    , _appName = Nothing
    , _appDisplayName = Nothing
    , _appMetadata = Nothing
    , _appIconURL = Nothing
    }


-- | If there is a problem, the application can be disabled after image creation.
appEnabled :: Lens' Application (Maybe Bool)
appEnabled = lens _appEnabled (\ s a -> s{_appEnabled = a})

-- | The path to the application executable in the instance.
appLaunchPath :: Lens' Application (Maybe Text)
appLaunchPath = lens _appLaunchPath (\ s a -> s{_appLaunchPath = a})

-- | The arguments that are passed to the application at launch.
appLaunchParameters :: Lens' Application (Maybe Text)
appLaunchParameters = lens _appLaunchParameters (\ s a -> s{_appLaunchParameters = a})

-- | The name of the application.
appName :: Lens' Application (Maybe Text)
appName = lens _appName (\ s a -> s{_appName = a})

-- | The application name to display.
appDisplayName :: Lens' Application (Maybe Text)
appDisplayName = lens _appDisplayName (\ s a -> s{_appDisplayName = a})

-- | Additional attributes that describe the application.
appMetadata :: Lens' Application (HashMap Text Text)
appMetadata = lens _appMetadata (\ s a -> s{_appMetadata = a}) . _Default . _Map

-- | The URL for the application icon. This URL might be time-limited.
appIconURL :: Lens' Application (Maybe Text)
appIconURL = lens _appIconURL (\ s a -> s{_appIconURL = a})

instance FromJSON Application where
        parseJSON
          = withObject "Application"
              (\ x ->
                 Application' <$>
                   (x .:? "Enabled") <*> (x .:? "LaunchPath") <*>
                     (x .:? "LaunchParameters")
                     <*> (x .:? "Name")
                     <*> (x .:? "DisplayName")
                     <*> (x .:? "Metadata" .!= mempty)
                     <*> (x .:? "IconURL"))

instance Hashable Application where

instance NFData Application where

-- | The persistent application settings for users of a stack.
--
--
--
-- /See:/ 'applicationSettings' smart constructor.
data ApplicationSettings = ApplicationSettings'
  { _aSettingsGroup :: !(Maybe Text)
  , _aEnabled :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aSettingsGroup' - The path prefix for the S3 bucket where users’ persistent application settings are stored. You can allow the same persistent application settings to be used across multiple stacks by specifying the same settings group for each stack. 
--
-- * 'aEnabled' - Enables or disables persistent application settings for users during their streaming sessions. 
applicationSettings
    :: Bool -- ^ 'aEnabled'
    -> ApplicationSettings
applicationSettings pEnabled_ =
  ApplicationSettings' {_aSettingsGroup = Nothing, _aEnabled = pEnabled_}


-- | The path prefix for the S3 bucket where users’ persistent application settings are stored. You can allow the same persistent application settings to be used across multiple stacks by specifying the same settings group for each stack. 
aSettingsGroup :: Lens' ApplicationSettings (Maybe Text)
aSettingsGroup = lens _aSettingsGroup (\ s a -> s{_aSettingsGroup = a})

-- | Enables or disables persistent application settings for users during their streaming sessions. 
aEnabled :: Lens' ApplicationSettings Bool
aEnabled = lens _aEnabled (\ s a -> s{_aEnabled = a})

instance Hashable ApplicationSettings where

instance NFData ApplicationSettings where

instance ToJSON ApplicationSettings where
        toJSON ApplicationSettings'{..}
          = object
              (catMaybes
                 [("SettingsGroup" .=) <$> _aSettingsGroup,
                  Just ("Enabled" .= _aEnabled)])

-- | Describes the persistent application settings for users of a stack.
--
--
--
-- /See:/ 'applicationSettingsResponse' smart constructor.
data ApplicationSettingsResponse = ApplicationSettingsResponse'
  { _asEnabled :: !(Maybe Bool)
  , _asSettingsGroup :: !(Maybe Text)
  , _asS3BucketName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationSettingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asEnabled' - Specifies whether persistent application settings are enabled for users during their streaming sessions.
--
-- * 'asSettingsGroup' - The path prefix for the S3 bucket where users’ persistent application settings are stored.
--
-- * 'asS3BucketName' - The S3 bucket where users’ persistent application settings are stored. When persistent application settings are enabled for the first time for an account in an AWS Region, an S3 bucket is created. The bucket is unique to the AWS account and the Region. 
applicationSettingsResponse
    :: ApplicationSettingsResponse
applicationSettingsResponse =
  ApplicationSettingsResponse'
    { _asEnabled = Nothing
    , _asSettingsGroup = Nothing
    , _asS3BucketName = Nothing
    }


-- | Specifies whether persistent application settings are enabled for users during their streaming sessions.
asEnabled :: Lens' ApplicationSettingsResponse (Maybe Bool)
asEnabled = lens _asEnabled (\ s a -> s{_asEnabled = a})

-- | The path prefix for the S3 bucket where users’ persistent application settings are stored.
asSettingsGroup :: Lens' ApplicationSettingsResponse (Maybe Text)
asSettingsGroup = lens _asSettingsGroup (\ s a -> s{_asSettingsGroup = a})

-- | The S3 bucket where users’ persistent application settings are stored. When persistent application settings are enabled for the first time for an account in an AWS Region, an S3 bucket is created. The bucket is unique to the AWS account and the Region. 
asS3BucketName :: Lens' ApplicationSettingsResponse (Maybe Text)
asS3BucketName = lens _asS3BucketName (\ s a -> s{_asS3BucketName = a})

instance FromJSON ApplicationSettingsResponse where
        parseJSON
          = withObject "ApplicationSettingsResponse"
              (\ x ->
                 ApplicationSettingsResponse' <$>
                   (x .:? "Enabled") <*> (x .:? "SettingsGroup") <*>
                     (x .:? "S3BucketName"))

instance Hashable ApplicationSettingsResponse where

instance NFData ApplicationSettingsResponse where

-- | Describes the capacity for a fleet.
--
--
--
-- /See:/ 'computeCapacity' smart constructor.
newtype ComputeCapacity = ComputeCapacity'
  { _ccDesiredInstances :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ComputeCapacity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccDesiredInstances' - The desired number of streaming instances.
computeCapacity
    :: Int -- ^ 'ccDesiredInstances'
    -> ComputeCapacity
computeCapacity pDesiredInstances_ =
  ComputeCapacity' {_ccDesiredInstances = pDesiredInstances_}


-- | The desired number of streaming instances.
ccDesiredInstances :: Lens' ComputeCapacity Int
ccDesiredInstances = lens _ccDesiredInstances (\ s a -> s{_ccDesiredInstances = a})

instance Hashable ComputeCapacity where

instance NFData ComputeCapacity where

instance ToJSON ComputeCapacity where
        toJSON ComputeCapacity'{..}
          = object
              (catMaybes
                 [Just ("DesiredInstances" .= _ccDesiredInstances)])

-- | Describes the capacity status for a fleet.
--
--
--
-- /See:/ 'computeCapacityStatus' smart constructor.
data ComputeCapacityStatus = ComputeCapacityStatus'
  { _ccsInUse :: !(Maybe Int)
  , _ccsRunning :: !(Maybe Int)
  , _ccsAvailable :: !(Maybe Int)
  , _ccsDesired :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ComputeCapacityStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccsInUse' - The number of instances in use for streaming.
--
-- * 'ccsRunning' - The total number of simultaneous streaming instances that are running.
--
-- * 'ccsAvailable' - The number of currently available instances that can be used to stream sessions.
--
-- * 'ccsDesired' - The desired number of streaming instances.
computeCapacityStatus
    :: Int -- ^ 'ccsDesired'
    -> ComputeCapacityStatus
computeCapacityStatus pDesired_ =
  ComputeCapacityStatus'
    { _ccsInUse = Nothing
    , _ccsRunning = Nothing
    , _ccsAvailable = Nothing
    , _ccsDesired = pDesired_
    }


-- | The number of instances in use for streaming.
ccsInUse :: Lens' ComputeCapacityStatus (Maybe Int)
ccsInUse = lens _ccsInUse (\ s a -> s{_ccsInUse = a})

-- | The total number of simultaneous streaming instances that are running.
ccsRunning :: Lens' ComputeCapacityStatus (Maybe Int)
ccsRunning = lens _ccsRunning (\ s a -> s{_ccsRunning = a})

-- | The number of currently available instances that can be used to stream sessions.
ccsAvailable :: Lens' ComputeCapacityStatus (Maybe Int)
ccsAvailable = lens _ccsAvailable (\ s a -> s{_ccsAvailable = a})

-- | The desired number of streaming instances.
ccsDesired :: Lens' ComputeCapacityStatus Int
ccsDesired = lens _ccsDesired (\ s a -> s{_ccsDesired = a})

instance FromJSON ComputeCapacityStatus where
        parseJSON
          = withObject "ComputeCapacityStatus"
              (\ x ->
                 ComputeCapacityStatus' <$>
                   (x .:? "InUse") <*> (x .:? "Running") <*>
                     (x .:? "Available")
                     <*> (x .: "Desired"))

instance Hashable ComputeCapacityStatus where

instance NFData ComputeCapacityStatus where

-- | Describes the configuration information for the directory used to join a streaming instance to a Microsoft Active Directory domain.
--
--
--
-- /See:/ 'directoryConfig' smart constructor.
data DirectoryConfig = DirectoryConfig'
  { _dcCreatedTime :: !(Maybe POSIX)
  , _dcServiceAccountCredentials :: !(Maybe ServiceAccountCredentials)
  , _dcOrganizationalUnitDistinguishedNames :: !(Maybe [Text])
  , _dcDirectoryName :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DirectoryConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcCreatedTime' - The time the directory configuration was created.
--
-- * 'dcServiceAccountCredentials' - The credentials for the service account used by the streaming instance to connect to the directory.
--
-- * 'dcOrganizationalUnitDistinguishedNames' - The distinguished names of the organizational units for computer accounts.
--
-- * 'dcDirectoryName' - The fully qualified name of the directory (for example, corp.example.com).
directoryConfig
    :: Text -- ^ 'dcDirectoryName'
    -> DirectoryConfig
directoryConfig pDirectoryName_ =
  DirectoryConfig'
    { _dcCreatedTime = Nothing
    , _dcServiceAccountCredentials = Nothing
    , _dcOrganizationalUnitDistinguishedNames = Nothing
    , _dcDirectoryName = pDirectoryName_
    }


-- | The time the directory configuration was created.
dcCreatedTime :: Lens' DirectoryConfig (Maybe UTCTime)
dcCreatedTime = lens _dcCreatedTime (\ s a -> s{_dcCreatedTime = a}) . mapping _Time

-- | The credentials for the service account used by the streaming instance to connect to the directory.
dcServiceAccountCredentials :: Lens' DirectoryConfig (Maybe ServiceAccountCredentials)
dcServiceAccountCredentials = lens _dcServiceAccountCredentials (\ s a -> s{_dcServiceAccountCredentials = a})

-- | The distinguished names of the organizational units for computer accounts.
dcOrganizationalUnitDistinguishedNames :: Lens' DirectoryConfig [Text]
dcOrganizationalUnitDistinguishedNames = lens _dcOrganizationalUnitDistinguishedNames (\ s a -> s{_dcOrganizationalUnitDistinguishedNames = a}) . _Default . _Coerce

-- | The fully qualified name of the directory (for example, corp.example.com).
dcDirectoryName :: Lens' DirectoryConfig Text
dcDirectoryName = lens _dcDirectoryName (\ s a -> s{_dcDirectoryName = a})

instance FromJSON DirectoryConfig where
        parseJSON
          = withObject "DirectoryConfig"
              (\ x ->
                 DirectoryConfig' <$>
                   (x .:? "CreatedTime") <*>
                     (x .:? "ServiceAccountCredentials")
                     <*>
                     (x .:? "OrganizationalUnitDistinguishedNames" .!=
                        mempty)
                     <*> (x .: "DirectoryName"))

instance Hashable DirectoryConfig where

instance NFData DirectoryConfig where

-- | Describes the configuration information required to join fleets and image builders to Microsoft Active Directory domains.
--
--
--
-- /See:/ 'domainJoinInfo' smart constructor.
data DomainJoinInfo = DomainJoinInfo'
  { _djiOrganizationalUnitDistinguishedName :: !(Maybe Text)
  , _djiDirectoryName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DomainJoinInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djiOrganizationalUnitDistinguishedName' - The distinguished name of the organizational unit for computer accounts.
--
-- * 'djiDirectoryName' - The fully qualified name of the directory (for example, corp.example.com).
domainJoinInfo
    :: DomainJoinInfo
domainJoinInfo =
  DomainJoinInfo'
    { _djiOrganizationalUnitDistinguishedName = Nothing
    , _djiDirectoryName = Nothing
    }


-- | The distinguished name of the organizational unit for computer accounts.
djiOrganizationalUnitDistinguishedName :: Lens' DomainJoinInfo (Maybe Text)
djiOrganizationalUnitDistinguishedName = lens _djiOrganizationalUnitDistinguishedName (\ s a -> s{_djiOrganizationalUnitDistinguishedName = a})

-- | The fully qualified name of the directory (for example, corp.example.com).
djiDirectoryName :: Lens' DomainJoinInfo (Maybe Text)
djiDirectoryName = lens _djiDirectoryName (\ s a -> s{_djiDirectoryName = a})

instance FromJSON DomainJoinInfo where
        parseJSON
          = withObject "DomainJoinInfo"
              (\ x ->
                 DomainJoinInfo' <$>
                   (x .:? "OrganizationalUnitDistinguishedName") <*>
                     (x .:? "DirectoryName"))

instance Hashable DomainJoinInfo where

instance NFData DomainJoinInfo where

instance ToJSON DomainJoinInfo where
        toJSON DomainJoinInfo'{..}
          = object
              (catMaybes
                 [("OrganizationalUnitDistinguishedName" .=) <$>
                    _djiOrganizationalUnitDistinguishedName,
                  ("DirectoryName" .=) <$> _djiDirectoryName])

-- | Describes the parameters for a fleet.
--
--
--
-- /See:/ 'fleet' smart constructor.
data Fleet = Fleet'
  { _fDomainJoinInfo :: !(Maybe DomainJoinInfo)
  , _fDisconnectTimeoutInSeconds :: !(Maybe Int)
  , _fMaxUserDurationInSeconds :: !(Maybe Int)
  , _fCreatedTime :: !(Maybe POSIX)
  , _fFleetType :: !(Maybe FleetType)
  , _fVPCConfig :: !(Maybe VPCConfig)
  , _fImageARN :: !(Maybe Text)
  , _fFleetErrors :: !(Maybe [FleetError])
  , _fDisplayName :: !(Maybe Text)
  , _fEnableDefaultInternetAccess :: !(Maybe Bool)
  , _fImageName :: !(Maybe Text)
  , _fDescription :: !(Maybe Text)
  , _fARN :: !Text
  , _fName :: !Text
  , _fInstanceType :: !Text
  , _fComputeCapacityStatus :: !ComputeCapacityStatus
  , _fState :: !FleetState
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Fleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fDomainJoinInfo' - The name of the directory and organizational unit (OU) to use to join the fleet to a Microsoft Active Directory domain. 
--
-- * 'fDisconnectTimeoutInSeconds' - The time after disconnection when a session is considered to have ended, in seconds. If a user who was disconnected reconnects within this time interval, the user is connected to their previous session. Specify a value between 60 and 57600.
--
-- * 'fMaxUserDurationInSeconds' - The maximum time that a streaming session can run, in seconds. Specify a value between 600 and 57600.
--
-- * 'fCreatedTime' - The time the fleet was created.
--
-- * 'fFleetType' - The fleet type.     * ALWAYS_ON    * Provides users with instant-on access to their apps. You are charged for all running instances in your fleet, even if no users are streaming apps.     * ON_DEMAND    * Provide users with access to applications after they connect, which takes one to two minutes. You are charged for instance streaming when users are connected and a small hourly fee for instances that are not streaming apps.
--
-- * 'fVPCConfig' - The VPC configuration for the fleet.
--
-- * 'fImageARN' - The ARN for the public, private, or shared image.
--
-- * 'fFleetErrors' - The fleet errors.
--
-- * 'fDisplayName' - The fleet name to display.
--
-- * 'fEnableDefaultInternetAccess' - Indicates whether default internet access is enabled for the fleet.
--
-- * 'fImageName' - The name of the image used to create the fleet.
--
-- * 'fDescription' - The description to display.
--
-- * 'fARN' - The ARN for the fleet.
--
-- * 'fName' - The name of the fleet.
--
-- * 'fInstanceType' - The instance type to use when launching fleet instances.
--
-- * 'fComputeCapacityStatus' - The capacity status for the fleet.
--
-- * 'fState' - The current state for the fleet.
fleet
    :: Text -- ^ 'fARN'
    -> Text -- ^ 'fName'
    -> Text -- ^ 'fInstanceType'
    -> ComputeCapacityStatus -- ^ 'fComputeCapacityStatus'
    -> FleetState -- ^ 'fState'
    -> Fleet
fleet pARN_ pName_ pInstanceType_ pComputeCapacityStatus_ pState_ =
  Fleet'
    { _fDomainJoinInfo = Nothing
    , _fDisconnectTimeoutInSeconds = Nothing
    , _fMaxUserDurationInSeconds = Nothing
    , _fCreatedTime = Nothing
    , _fFleetType = Nothing
    , _fVPCConfig = Nothing
    , _fImageARN = Nothing
    , _fFleetErrors = Nothing
    , _fDisplayName = Nothing
    , _fEnableDefaultInternetAccess = Nothing
    , _fImageName = Nothing
    , _fDescription = Nothing
    , _fARN = pARN_
    , _fName = pName_
    , _fInstanceType = pInstanceType_
    , _fComputeCapacityStatus = pComputeCapacityStatus_
    , _fState = pState_
    }


-- | The name of the directory and organizational unit (OU) to use to join the fleet to a Microsoft Active Directory domain. 
fDomainJoinInfo :: Lens' Fleet (Maybe DomainJoinInfo)
fDomainJoinInfo = lens _fDomainJoinInfo (\ s a -> s{_fDomainJoinInfo = a})

-- | The time after disconnection when a session is considered to have ended, in seconds. If a user who was disconnected reconnects within this time interval, the user is connected to their previous session. Specify a value between 60 and 57600.
fDisconnectTimeoutInSeconds :: Lens' Fleet (Maybe Int)
fDisconnectTimeoutInSeconds = lens _fDisconnectTimeoutInSeconds (\ s a -> s{_fDisconnectTimeoutInSeconds = a})

-- | The maximum time that a streaming session can run, in seconds. Specify a value between 600 and 57600.
fMaxUserDurationInSeconds :: Lens' Fleet (Maybe Int)
fMaxUserDurationInSeconds = lens _fMaxUserDurationInSeconds (\ s a -> s{_fMaxUserDurationInSeconds = a})

-- | The time the fleet was created.
fCreatedTime :: Lens' Fleet (Maybe UTCTime)
fCreatedTime = lens _fCreatedTime (\ s a -> s{_fCreatedTime = a}) . mapping _Time

-- | The fleet type.     * ALWAYS_ON    * Provides users with instant-on access to their apps. You are charged for all running instances in your fleet, even if no users are streaming apps.     * ON_DEMAND    * Provide users with access to applications after they connect, which takes one to two minutes. You are charged for instance streaming when users are connected and a small hourly fee for instances that are not streaming apps.
fFleetType :: Lens' Fleet (Maybe FleetType)
fFleetType = lens _fFleetType (\ s a -> s{_fFleetType = a})

-- | The VPC configuration for the fleet.
fVPCConfig :: Lens' Fleet (Maybe VPCConfig)
fVPCConfig = lens _fVPCConfig (\ s a -> s{_fVPCConfig = a})

-- | The ARN for the public, private, or shared image.
fImageARN :: Lens' Fleet (Maybe Text)
fImageARN = lens _fImageARN (\ s a -> s{_fImageARN = a})

-- | The fleet errors.
fFleetErrors :: Lens' Fleet [FleetError]
fFleetErrors = lens _fFleetErrors (\ s a -> s{_fFleetErrors = a}) . _Default . _Coerce

-- | The fleet name to display.
fDisplayName :: Lens' Fleet (Maybe Text)
fDisplayName = lens _fDisplayName (\ s a -> s{_fDisplayName = a})

-- | Indicates whether default internet access is enabled for the fleet.
fEnableDefaultInternetAccess :: Lens' Fleet (Maybe Bool)
fEnableDefaultInternetAccess = lens _fEnableDefaultInternetAccess (\ s a -> s{_fEnableDefaultInternetAccess = a})

-- | The name of the image used to create the fleet.
fImageName :: Lens' Fleet (Maybe Text)
fImageName = lens _fImageName (\ s a -> s{_fImageName = a})

-- | The description to display.
fDescription :: Lens' Fleet (Maybe Text)
fDescription = lens _fDescription (\ s a -> s{_fDescription = a})

-- | The ARN for the fleet.
fARN :: Lens' Fleet Text
fARN = lens _fARN (\ s a -> s{_fARN = a})

-- | The name of the fleet.
fName :: Lens' Fleet Text
fName = lens _fName (\ s a -> s{_fName = a})

-- | The instance type to use when launching fleet instances.
fInstanceType :: Lens' Fleet Text
fInstanceType = lens _fInstanceType (\ s a -> s{_fInstanceType = a})

-- | The capacity status for the fleet.
fComputeCapacityStatus :: Lens' Fleet ComputeCapacityStatus
fComputeCapacityStatus = lens _fComputeCapacityStatus (\ s a -> s{_fComputeCapacityStatus = a})

-- | The current state for the fleet.
fState :: Lens' Fleet FleetState
fState = lens _fState (\ s a -> s{_fState = a})

instance FromJSON Fleet where
        parseJSON
          = withObject "Fleet"
              (\ x ->
                 Fleet' <$>
                   (x .:? "DomainJoinInfo") <*>
                     (x .:? "DisconnectTimeoutInSeconds")
                     <*> (x .:? "MaxUserDurationInSeconds")
                     <*> (x .:? "CreatedTime")
                     <*> (x .:? "FleetType")
                     <*> (x .:? "VpcConfig")
                     <*> (x .:? "ImageArn")
                     <*> (x .:? "FleetErrors" .!= mempty)
                     <*> (x .:? "DisplayName")
                     <*> (x .:? "EnableDefaultInternetAccess")
                     <*> (x .:? "ImageName")
                     <*> (x .:? "Description")
                     <*> (x .: "Arn")
                     <*> (x .: "Name")
                     <*> (x .: "InstanceType")
                     <*> (x .: "ComputeCapacityStatus")
                     <*> (x .: "State"))

instance Hashable Fleet where

instance NFData Fleet where

-- | Describes a fleet error.
--
--
--
-- /See:/ 'fleetError' smart constructor.
data FleetError = FleetError'
  { _feErrorCode :: !(Maybe FleetErrorCode)
  , _feErrorMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FleetError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'feErrorCode' - The error code.
--
-- * 'feErrorMessage' - The error message.
fleetError
    :: FleetError
fleetError = FleetError' {_feErrorCode = Nothing, _feErrorMessage = Nothing}


-- | The error code.
feErrorCode :: Lens' FleetError (Maybe FleetErrorCode)
feErrorCode = lens _feErrorCode (\ s a -> s{_feErrorCode = a})

-- | The error message.
feErrorMessage :: Lens' FleetError (Maybe Text)
feErrorMessage = lens _feErrorMessage (\ s a -> s{_feErrorMessage = a})

instance FromJSON FleetError where
        parseJSON
          = withObject "FleetError"
              (\ x ->
                 FleetError' <$>
                   (x .:? "ErrorCode") <*> (x .:? "ErrorMessage"))

instance Hashable FleetError where

instance NFData FleetError where

-- | Describes an image.
--
--
--
-- /See:/ 'image' smart constructor.
data Image = Image'
  { _iState :: !(Maybe ImageState)
  , _iImagePermissions :: !(Maybe ImagePermissions)
  , _iPlatform :: !(Maybe PlatformType)
  , _iPublicBaseImageReleasedDate :: !(Maybe POSIX)
  , _iStateChangeReason :: !(Maybe ImageStateChangeReason)
  , _iARN :: !(Maybe Text)
  , _iCreatedTime :: !(Maybe POSIX)
  , _iImageBuilderSupported :: !(Maybe Bool)
  , _iVisibility :: !(Maybe VisibilityType)
  , _iBaseImageARN :: !(Maybe Text)
  , _iDisplayName :: !(Maybe Text)
  , _iDescription :: !(Maybe Text)
  , _iAppstreamAgentVersion :: !(Maybe Text)
  , _iApplications :: !(Maybe [Application])
  , _iName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Image' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iState' - The image starts in the @PENDING@ state. If image creation succeeds, the state is @AVAILABLE@ . If image creation fails, the state is @FAILED@ .
--
-- * 'iImagePermissions' - The permissions to provide to the destination AWS account for the specified image.
--
-- * 'iPlatform' - The operating system platform of the image.
--
-- * 'iPublicBaseImageReleasedDate' - The release date of the public base image. For private images, this date is the release date of the base image from which the image was created.
--
-- * 'iStateChangeReason' - The reason why the last state change occurred.
--
-- * 'iARN' - The ARN of the image.
--
-- * 'iCreatedTime' - The time the image was created.
--
-- * 'iImageBuilderSupported' - Indicates whether an image builder can be launched from this image.
--
-- * 'iVisibility' - Indicates whether the image is public or private.
--
-- * 'iBaseImageARN' - The ARN of the image from which this image was created.
--
-- * 'iDisplayName' - The image name to display.
--
-- * 'iDescription' - The description to display.
--
-- * 'iAppstreamAgentVersion' - The version of the AppStream 2.0 agent to use for instances that are launched from this image. 
--
-- * 'iApplications' - The applications associated with the image.
--
-- * 'iName' - The name of the image.
image
    :: Text -- ^ 'iName'
    -> Image
image pName_ =
  Image'
    { _iState = Nothing
    , _iImagePermissions = Nothing
    , _iPlatform = Nothing
    , _iPublicBaseImageReleasedDate = Nothing
    , _iStateChangeReason = Nothing
    , _iARN = Nothing
    , _iCreatedTime = Nothing
    , _iImageBuilderSupported = Nothing
    , _iVisibility = Nothing
    , _iBaseImageARN = Nothing
    , _iDisplayName = Nothing
    , _iDescription = Nothing
    , _iAppstreamAgentVersion = Nothing
    , _iApplications = Nothing
    , _iName = pName_
    }


-- | The image starts in the @PENDING@ state. If image creation succeeds, the state is @AVAILABLE@ . If image creation fails, the state is @FAILED@ .
iState :: Lens' Image (Maybe ImageState)
iState = lens _iState (\ s a -> s{_iState = a})

-- | The permissions to provide to the destination AWS account for the specified image.
iImagePermissions :: Lens' Image (Maybe ImagePermissions)
iImagePermissions = lens _iImagePermissions (\ s a -> s{_iImagePermissions = a})

-- | The operating system platform of the image.
iPlatform :: Lens' Image (Maybe PlatformType)
iPlatform = lens _iPlatform (\ s a -> s{_iPlatform = a})

-- | The release date of the public base image. For private images, this date is the release date of the base image from which the image was created.
iPublicBaseImageReleasedDate :: Lens' Image (Maybe UTCTime)
iPublicBaseImageReleasedDate = lens _iPublicBaseImageReleasedDate (\ s a -> s{_iPublicBaseImageReleasedDate = a}) . mapping _Time

-- | The reason why the last state change occurred.
iStateChangeReason :: Lens' Image (Maybe ImageStateChangeReason)
iStateChangeReason = lens _iStateChangeReason (\ s a -> s{_iStateChangeReason = a})

-- | The ARN of the image.
iARN :: Lens' Image (Maybe Text)
iARN = lens _iARN (\ s a -> s{_iARN = a})

-- | The time the image was created.
iCreatedTime :: Lens' Image (Maybe UTCTime)
iCreatedTime = lens _iCreatedTime (\ s a -> s{_iCreatedTime = a}) . mapping _Time

-- | Indicates whether an image builder can be launched from this image.
iImageBuilderSupported :: Lens' Image (Maybe Bool)
iImageBuilderSupported = lens _iImageBuilderSupported (\ s a -> s{_iImageBuilderSupported = a})

-- | Indicates whether the image is public or private.
iVisibility :: Lens' Image (Maybe VisibilityType)
iVisibility = lens _iVisibility (\ s a -> s{_iVisibility = a})

-- | The ARN of the image from which this image was created.
iBaseImageARN :: Lens' Image (Maybe Text)
iBaseImageARN = lens _iBaseImageARN (\ s a -> s{_iBaseImageARN = a})

-- | The image name to display.
iDisplayName :: Lens' Image (Maybe Text)
iDisplayName = lens _iDisplayName (\ s a -> s{_iDisplayName = a})

-- | The description to display.
iDescription :: Lens' Image (Maybe Text)
iDescription = lens _iDescription (\ s a -> s{_iDescription = a})

-- | The version of the AppStream 2.0 agent to use for instances that are launched from this image. 
iAppstreamAgentVersion :: Lens' Image (Maybe Text)
iAppstreamAgentVersion = lens _iAppstreamAgentVersion (\ s a -> s{_iAppstreamAgentVersion = a})

-- | The applications associated with the image.
iApplications :: Lens' Image [Application]
iApplications = lens _iApplications (\ s a -> s{_iApplications = a}) . _Default . _Coerce

-- | The name of the image.
iName :: Lens' Image Text
iName = lens _iName (\ s a -> s{_iName = a})

instance FromJSON Image where
        parseJSON
          = withObject "Image"
              (\ x ->
                 Image' <$>
                   (x .:? "State") <*> (x .:? "ImagePermissions") <*>
                     (x .:? "Platform")
                     <*> (x .:? "PublicBaseImageReleasedDate")
                     <*> (x .:? "StateChangeReason")
                     <*> (x .:? "Arn")
                     <*> (x .:? "CreatedTime")
                     <*> (x .:? "ImageBuilderSupported")
                     <*> (x .:? "Visibility")
                     <*> (x .:? "BaseImageArn")
                     <*> (x .:? "DisplayName")
                     <*> (x .:? "Description")
                     <*> (x .:? "AppstreamAgentVersion")
                     <*> (x .:? "Applications" .!= mempty)
                     <*> (x .: "Name"))

instance Hashable Image where

instance NFData Image where

-- | Describes a virtual machine that is used to create an image. 
--
--
--
-- /See:/ 'imageBuilder' smart constructor.
data ImageBuilder = ImageBuilder'
  { _ibDomainJoinInfo :: !(Maybe DomainJoinInfo)
  , _ibState :: !(Maybe ImageBuilderState)
  , _ibPlatform :: !(Maybe PlatformType)
  , _ibStateChangeReason :: !(Maybe ImageBuilderStateChangeReason)
  , _ibARN :: !(Maybe Text)
  , _ibCreatedTime :: !(Maybe POSIX)
  , _ibImageBuilderErrors :: !(Maybe [ResourceError])
  , _ibInstanceType :: !(Maybe Text)
  , _ibVPCConfig :: !(Maybe VPCConfig)
  , _ibImageARN :: !(Maybe Text)
  , _ibDisplayName :: !(Maybe Text)
  , _ibEnableDefaultInternetAccess :: !(Maybe Bool)
  , _ibDescription :: !(Maybe Text)
  , _ibAppstreamAgentVersion :: !(Maybe Text)
  , _ibName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImageBuilder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ibDomainJoinInfo' - The name of the directory and organizational unit (OU) to use to join the image builder to a Microsoft Active Directory domain. 
--
-- * 'ibState' - The state of the image builder.
--
-- * 'ibPlatform' - The operating system platform of the image builder.
--
-- * 'ibStateChangeReason' - The reason why the last state change occurred.
--
-- * 'ibARN' - The ARN for the image builder.
--
-- * 'ibCreatedTime' - The time stamp when the image builder was created.
--
-- * 'ibImageBuilderErrors' - The image builder errors.
--
-- * 'ibInstanceType' - The instance type for the image builder.
--
-- * 'ibVPCConfig' - The VPC configuration of the image builder.
--
-- * 'ibImageARN' - The ARN of the image from which this builder was created.
--
-- * 'ibDisplayName' - The image builder name to display.
--
-- * 'ibEnableDefaultInternetAccess' - Enables or disables default internet access for the image builder.
--
-- * 'ibDescription' - The description to display.
--
-- * 'ibAppstreamAgentVersion' - The version of the AppStream 2.0 agent that is currently being used by the image builder. 
--
-- * 'ibName' - The name of the image builder.
imageBuilder
    :: Text -- ^ 'ibName'
    -> ImageBuilder
imageBuilder pName_ =
  ImageBuilder'
    { _ibDomainJoinInfo = Nothing
    , _ibState = Nothing
    , _ibPlatform = Nothing
    , _ibStateChangeReason = Nothing
    , _ibARN = Nothing
    , _ibCreatedTime = Nothing
    , _ibImageBuilderErrors = Nothing
    , _ibInstanceType = Nothing
    , _ibVPCConfig = Nothing
    , _ibImageARN = Nothing
    , _ibDisplayName = Nothing
    , _ibEnableDefaultInternetAccess = Nothing
    , _ibDescription = Nothing
    , _ibAppstreamAgentVersion = Nothing
    , _ibName = pName_
    }


-- | The name of the directory and organizational unit (OU) to use to join the image builder to a Microsoft Active Directory domain. 
ibDomainJoinInfo :: Lens' ImageBuilder (Maybe DomainJoinInfo)
ibDomainJoinInfo = lens _ibDomainJoinInfo (\ s a -> s{_ibDomainJoinInfo = a})

-- | The state of the image builder.
ibState :: Lens' ImageBuilder (Maybe ImageBuilderState)
ibState = lens _ibState (\ s a -> s{_ibState = a})

-- | The operating system platform of the image builder.
ibPlatform :: Lens' ImageBuilder (Maybe PlatformType)
ibPlatform = lens _ibPlatform (\ s a -> s{_ibPlatform = a})

-- | The reason why the last state change occurred.
ibStateChangeReason :: Lens' ImageBuilder (Maybe ImageBuilderStateChangeReason)
ibStateChangeReason = lens _ibStateChangeReason (\ s a -> s{_ibStateChangeReason = a})

-- | The ARN for the image builder.
ibARN :: Lens' ImageBuilder (Maybe Text)
ibARN = lens _ibARN (\ s a -> s{_ibARN = a})

-- | The time stamp when the image builder was created.
ibCreatedTime :: Lens' ImageBuilder (Maybe UTCTime)
ibCreatedTime = lens _ibCreatedTime (\ s a -> s{_ibCreatedTime = a}) . mapping _Time

-- | The image builder errors.
ibImageBuilderErrors :: Lens' ImageBuilder [ResourceError]
ibImageBuilderErrors = lens _ibImageBuilderErrors (\ s a -> s{_ibImageBuilderErrors = a}) . _Default . _Coerce

-- | The instance type for the image builder.
ibInstanceType :: Lens' ImageBuilder (Maybe Text)
ibInstanceType = lens _ibInstanceType (\ s a -> s{_ibInstanceType = a})

-- | The VPC configuration of the image builder.
ibVPCConfig :: Lens' ImageBuilder (Maybe VPCConfig)
ibVPCConfig = lens _ibVPCConfig (\ s a -> s{_ibVPCConfig = a})

-- | The ARN of the image from which this builder was created.
ibImageARN :: Lens' ImageBuilder (Maybe Text)
ibImageARN = lens _ibImageARN (\ s a -> s{_ibImageARN = a})

-- | The image builder name to display.
ibDisplayName :: Lens' ImageBuilder (Maybe Text)
ibDisplayName = lens _ibDisplayName (\ s a -> s{_ibDisplayName = a})

-- | Enables or disables default internet access for the image builder.
ibEnableDefaultInternetAccess :: Lens' ImageBuilder (Maybe Bool)
ibEnableDefaultInternetAccess = lens _ibEnableDefaultInternetAccess (\ s a -> s{_ibEnableDefaultInternetAccess = a})

-- | The description to display.
ibDescription :: Lens' ImageBuilder (Maybe Text)
ibDescription = lens _ibDescription (\ s a -> s{_ibDescription = a})

-- | The version of the AppStream 2.0 agent that is currently being used by the image builder. 
ibAppstreamAgentVersion :: Lens' ImageBuilder (Maybe Text)
ibAppstreamAgentVersion = lens _ibAppstreamAgentVersion (\ s a -> s{_ibAppstreamAgentVersion = a})

-- | The name of the image builder.
ibName :: Lens' ImageBuilder Text
ibName = lens _ibName (\ s a -> s{_ibName = a})

instance FromJSON ImageBuilder where
        parseJSON
          = withObject "ImageBuilder"
              (\ x ->
                 ImageBuilder' <$>
                   (x .:? "DomainJoinInfo") <*> (x .:? "State") <*>
                     (x .:? "Platform")
                     <*> (x .:? "StateChangeReason")
                     <*> (x .:? "Arn")
                     <*> (x .:? "CreatedTime")
                     <*> (x .:? "ImageBuilderErrors" .!= mempty)
                     <*> (x .:? "InstanceType")
                     <*> (x .:? "VpcConfig")
                     <*> (x .:? "ImageArn")
                     <*> (x .:? "DisplayName")
                     <*> (x .:? "EnableDefaultInternetAccess")
                     <*> (x .:? "Description")
                     <*> (x .:? "AppstreamAgentVersion")
                     <*> (x .: "Name"))

instance Hashable ImageBuilder where

instance NFData ImageBuilder where

-- | Describes the reason why the last image builder state change occurred.
--
--
--
-- /See:/ 'imageBuilderStateChangeReason' smart constructor.
data ImageBuilderStateChangeReason = ImageBuilderStateChangeReason'
  { _ibscrCode :: !(Maybe ImageBuilderStateChangeReasonCode)
  , _ibscrMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImageBuilderStateChangeReason' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ibscrCode' - The state change reason code.
--
-- * 'ibscrMessage' - The state change reason message.
imageBuilderStateChangeReason
    :: ImageBuilderStateChangeReason
imageBuilderStateChangeReason =
  ImageBuilderStateChangeReason' {_ibscrCode = Nothing, _ibscrMessage = Nothing}


-- | The state change reason code.
ibscrCode :: Lens' ImageBuilderStateChangeReason (Maybe ImageBuilderStateChangeReasonCode)
ibscrCode = lens _ibscrCode (\ s a -> s{_ibscrCode = a})

-- | The state change reason message.
ibscrMessage :: Lens' ImageBuilderStateChangeReason (Maybe Text)
ibscrMessage = lens _ibscrMessage (\ s a -> s{_ibscrMessage = a})

instance FromJSON ImageBuilderStateChangeReason where
        parseJSON
          = withObject "ImageBuilderStateChangeReason"
              (\ x ->
                 ImageBuilderStateChangeReason' <$>
                   (x .:? "Code") <*> (x .:? "Message"))

instance Hashable ImageBuilderStateChangeReason where

instance NFData ImageBuilderStateChangeReason where

-- | Describes the permissions for an image. 
--
--
--
-- /See:/ 'imagePermissions' smart constructor.
data ImagePermissions = ImagePermissions'
  { _ipAllowFleet :: !(Maybe Bool)
  , _ipAllowImageBuilder :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImagePermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipAllowFleet' - Indicates whether the image can be used for a fleet.
--
-- * 'ipAllowImageBuilder' - Indicates whether the image can be used for an image builder.
imagePermissions
    :: ImagePermissions
imagePermissions =
  ImagePermissions' {_ipAllowFleet = Nothing, _ipAllowImageBuilder = Nothing}


-- | Indicates whether the image can be used for a fleet.
ipAllowFleet :: Lens' ImagePermissions (Maybe Bool)
ipAllowFleet = lens _ipAllowFleet (\ s a -> s{_ipAllowFleet = a})

-- | Indicates whether the image can be used for an image builder.
ipAllowImageBuilder :: Lens' ImagePermissions (Maybe Bool)
ipAllowImageBuilder = lens _ipAllowImageBuilder (\ s a -> s{_ipAllowImageBuilder = a})

instance FromJSON ImagePermissions where
        parseJSON
          = withObject "ImagePermissions"
              (\ x ->
                 ImagePermissions' <$>
                   (x .:? "allowFleet") <*> (x .:? "allowImageBuilder"))

instance Hashable ImagePermissions where

instance NFData ImagePermissions where

instance ToJSON ImagePermissions where
        toJSON ImagePermissions'{..}
          = object
              (catMaybes
                 [("allowFleet" .=) <$> _ipAllowFleet,
                  ("allowImageBuilder" .=) <$> _ipAllowImageBuilder])

-- | Describes the reason why the last image state change occurred.
--
--
--
-- /See:/ 'imageStateChangeReason' smart constructor.
data ImageStateChangeReason = ImageStateChangeReason'
  { _iscrCode :: !(Maybe ImageStateChangeReasonCode)
  , _iscrMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImageStateChangeReason' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iscrCode' - The state change reason code.
--
-- * 'iscrMessage' - The state change reason message.
imageStateChangeReason
    :: ImageStateChangeReason
imageStateChangeReason =
  ImageStateChangeReason' {_iscrCode = Nothing, _iscrMessage = Nothing}


-- | The state change reason code.
iscrCode :: Lens' ImageStateChangeReason (Maybe ImageStateChangeReasonCode)
iscrCode = lens _iscrCode (\ s a -> s{_iscrCode = a})

-- | The state change reason message.
iscrMessage :: Lens' ImageStateChangeReason (Maybe Text)
iscrMessage = lens _iscrMessage (\ s a -> s{_iscrMessage = a})

instance FromJSON ImageStateChangeReason where
        parseJSON
          = withObject "ImageStateChangeReason"
              (\ x ->
                 ImageStateChangeReason' <$>
                   (x .:? "Code") <*> (x .:? "Message"))

instance Hashable ImageStateChangeReason where

instance NFData ImageStateChangeReason where

-- | Describes the network details of the fleet instance for the streaming session.
--
--
--
-- /See:/ 'networkAccessConfiguration' smart constructor.
data NetworkAccessConfiguration = NetworkAccessConfiguration'
  { _nacEniId :: !(Maybe Text)
  , _nacEniPrivateIPAddress :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NetworkAccessConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nacEniId' - The resource identifier of the elastic network interface that is attached to instances in your VPC. All network interfaces have the eni-xxxxxxxx resource identifier.
--
-- * 'nacEniPrivateIPAddress' - The private IP address of the elastic network interface that is attached to instances in your VPC.
networkAccessConfiguration
    :: NetworkAccessConfiguration
networkAccessConfiguration =
  NetworkAccessConfiguration'
    {_nacEniId = Nothing, _nacEniPrivateIPAddress = Nothing}


-- | The resource identifier of the elastic network interface that is attached to instances in your VPC. All network interfaces have the eni-xxxxxxxx resource identifier.
nacEniId :: Lens' NetworkAccessConfiguration (Maybe Text)
nacEniId = lens _nacEniId (\ s a -> s{_nacEniId = a})

-- | The private IP address of the elastic network interface that is attached to instances in your VPC.
nacEniPrivateIPAddress :: Lens' NetworkAccessConfiguration (Maybe Text)
nacEniPrivateIPAddress = lens _nacEniPrivateIPAddress (\ s a -> s{_nacEniPrivateIPAddress = a})

instance FromJSON NetworkAccessConfiguration where
        parseJSON
          = withObject "NetworkAccessConfiguration"
              (\ x ->
                 NetworkAccessConfiguration' <$>
                   (x .:? "EniId") <*> (x .:? "EniPrivateIpAddress"))

instance Hashable NetworkAccessConfiguration where

instance NFData NetworkAccessConfiguration where

-- | Describes a resource error.
--
--
--
-- /See:/ 'resourceError' smart constructor.
data ResourceError = ResourceError'
  { _reErrorCode :: !(Maybe FleetErrorCode)
  , _reErrorMessage :: !(Maybe Text)
  , _reErrorTimestamp :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'reErrorCode' - The error code.
--
-- * 'reErrorMessage' - The error message.
--
-- * 'reErrorTimestamp' - The time the error occurred.
resourceError
    :: ResourceError
resourceError =
  ResourceError'
    { _reErrorCode = Nothing
    , _reErrorMessage = Nothing
    , _reErrorTimestamp = Nothing
    }


-- | The error code.
reErrorCode :: Lens' ResourceError (Maybe FleetErrorCode)
reErrorCode = lens _reErrorCode (\ s a -> s{_reErrorCode = a})

-- | The error message.
reErrorMessage :: Lens' ResourceError (Maybe Text)
reErrorMessage = lens _reErrorMessage (\ s a -> s{_reErrorMessage = a})

-- | The time the error occurred.
reErrorTimestamp :: Lens' ResourceError (Maybe UTCTime)
reErrorTimestamp = lens _reErrorTimestamp (\ s a -> s{_reErrorTimestamp = a}) . mapping _Time

instance FromJSON ResourceError where
        parseJSON
          = withObject "ResourceError"
              (\ x ->
                 ResourceError' <$>
                   (x .:? "ErrorCode") <*> (x .:? "ErrorMessage") <*>
                     (x .:? "ErrorTimestamp"))

instance Hashable ResourceError where

instance NFData ResourceError where

-- | Describes the credentials for the service account used by the streaming instance to connect to the directory.
--
--
--
-- /See:/ 'serviceAccountCredentials' smart constructor.
data ServiceAccountCredentials = ServiceAccountCredentials'
  { _sacAccountName :: !(Sensitive Text)
  , _sacAccountPassword :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServiceAccountCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sacAccountName' - The user name of the account. This account must have the following privileges: create computer objects, join computers to the domain, and change/reset the password on descendant computer objects for the organizational units specified.
--
-- * 'sacAccountPassword' - The password for the account.
serviceAccountCredentials
    :: Text -- ^ 'sacAccountName'
    -> Text -- ^ 'sacAccountPassword'
    -> ServiceAccountCredentials
serviceAccountCredentials pAccountName_ pAccountPassword_ =
  ServiceAccountCredentials'
    { _sacAccountName = _Sensitive # pAccountName_
    , _sacAccountPassword = _Sensitive # pAccountPassword_
    }


-- | The user name of the account. This account must have the following privileges: create computer objects, join computers to the domain, and change/reset the password on descendant computer objects for the organizational units specified.
sacAccountName :: Lens' ServiceAccountCredentials Text
sacAccountName = lens _sacAccountName (\ s a -> s{_sacAccountName = a}) . _Sensitive

-- | The password for the account.
sacAccountPassword :: Lens' ServiceAccountCredentials Text
sacAccountPassword = lens _sacAccountPassword (\ s a -> s{_sacAccountPassword = a}) . _Sensitive

instance FromJSON ServiceAccountCredentials where
        parseJSON
          = withObject "ServiceAccountCredentials"
              (\ x ->
                 ServiceAccountCredentials' <$>
                   (x .: "AccountName") <*> (x .: "AccountPassword"))

instance Hashable ServiceAccountCredentials where

instance NFData ServiceAccountCredentials where

instance ToJSON ServiceAccountCredentials where
        toJSON ServiceAccountCredentials'{..}
          = object
              (catMaybes
                 [Just ("AccountName" .= _sacAccountName),
                  Just ("AccountPassword" .= _sacAccountPassword)])

-- | Describes a streaming session.
--
--
--
-- /See:/ 'session' smart constructor.
data Session = Session'
  { _sNetworkAccessConfiguration :: !(Maybe NetworkAccessConfiguration)
  , _sAuthenticationType :: !(Maybe AuthenticationType)
  , _sId :: !Text
  , _sUserId :: !Text
  , _sStackName :: !Text
  , _sFleetName :: !Text
  , _sState :: !SessionState
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Session' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sNetworkAccessConfiguration' - The network details for the streaming session.
--
-- * 'sAuthenticationType' - The authentication method. The user is authenticated using a streaming URL (@API@ ) or SAML federation (@SAML@ ).
--
-- * 'sId' - The identifier of the streaming session.
--
-- * 'sUserId' - The identifier of the user for whom the session was created.
--
-- * 'sStackName' - The name of the stack for the streaming session.
--
-- * 'sFleetName' - The name of the fleet for the streaming session.
--
-- * 'sState' - The current state of the streaming session.
session
    :: Text -- ^ 'sId'
    -> Text -- ^ 'sUserId'
    -> Text -- ^ 'sStackName'
    -> Text -- ^ 'sFleetName'
    -> SessionState -- ^ 'sState'
    -> Session
session pId_ pUserId_ pStackName_ pFleetName_ pState_ =
  Session'
    { _sNetworkAccessConfiguration = Nothing
    , _sAuthenticationType = Nothing
    , _sId = pId_
    , _sUserId = pUserId_
    , _sStackName = pStackName_
    , _sFleetName = pFleetName_
    , _sState = pState_
    }


-- | The network details for the streaming session.
sNetworkAccessConfiguration :: Lens' Session (Maybe NetworkAccessConfiguration)
sNetworkAccessConfiguration = lens _sNetworkAccessConfiguration (\ s a -> s{_sNetworkAccessConfiguration = a})

-- | The authentication method. The user is authenticated using a streaming URL (@API@ ) or SAML federation (@SAML@ ).
sAuthenticationType :: Lens' Session (Maybe AuthenticationType)
sAuthenticationType = lens _sAuthenticationType (\ s a -> s{_sAuthenticationType = a})

-- | The identifier of the streaming session.
sId :: Lens' Session Text
sId = lens _sId (\ s a -> s{_sId = a})

-- | The identifier of the user for whom the session was created.
sUserId :: Lens' Session Text
sUserId = lens _sUserId (\ s a -> s{_sUserId = a})

-- | The name of the stack for the streaming session.
sStackName :: Lens' Session Text
sStackName = lens _sStackName (\ s a -> s{_sStackName = a})

-- | The name of the fleet for the streaming session.
sFleetName :: Lens' Session Text
sFleetName = lens _sFleetName (\ s a -> s{_sFleetName = a})

-- | The current state of the streaming session.
sState :: Lens' Session SessionState
sState = lens _sState (\ s a -> s{_sState = a})

instance FromJSON Session where
        parseJSON
          = withObject "Session"
              (\ x ->
                 Session' <$>
                   (x .:? "NetworkAccessConfiguration") <*>
                     (x .:? "AuthenticationType")
                     <*> (x .: "Id")
                     <*> (x .: "UserId")
                     <*> (x .: "StackName")
                     <*> (x .: "FleetName")
                     <*> (x .: "State"))

instance Hashable Session where

instance NFData Session where

-- | Describes the permissions that are available to the specified AWS account for a shared image.
--
--
--
-- /See:/ 'sharedImagePermissions' smart constructor.
data SharedImagePermissions = SharedImagePermissions'
  { _sipSharedAccountId :: !Text
  , _sipImagePermissions :: !ImagePermissions
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SharedImagePermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sipSharedAccountId' - The 12-digit identifier of the AWS account with which the image is shared.
--
-- * 'sipImagePermissions' - Describes the permissions for a shared image.
sharedImagePermissions
    :: Text -- ^ 'sipSharedAccountId'
    -> ImagePermissions -- ^ 'sipImagePermissions'
    -> SharedImagePermissions
sharedImagePermissions pSharedAccountId_ pImagePermissions_ =
  SharedImagePermissions'
    { _sipSharedAccountId = pSharedAccountId_
    , _sipImagePermissions = pImagePermissions_
    }


-- | The 12-digit identifier of the AWS account with which the image is shared.
sipSharedAccountId :: Lens' SharedImagePermissions Text
sipSharedAccountId = lens _sipSharedAccountId (\ s a -> s{_sipSharedAccountId = a})

-- | Describes the permissions for a shared image.
sipImagePermissions :: Lens' SharedImagePermissions ImagePermissions
sipImagePermissions = lens _sipImagePermissions (\ s a -> s{_sipImagePermissions = a})

instance FromJSON SharedImagePermissions where
        parseJSON
          = withObject "SharedImagePermissions"
              (\ x ->
                 SharedImagePermissions' <$>
                   (x .: "sharedAccountId") <*>
                     (x .: "imagePermissions"))

instance Hashable SharedImagePermissions where

instance NFData SharedImagePermissions where

-- | Describes a stack.
--
--
--
-- /See:/ 'stack' smart constructor.
data Stack = Stack'
  { _sUserSettings :: !(Maybe (List1 UserSetting))
  , _sApplicationSettings :: !(Maybe ApplicationSettingsResponse)
  , _sFeedbackURL :: !(Maybe Text)
  , _sARN :: !(Maybe Text)
  , _sCreatedTime :: !(Maybe POSIX)
  , _sStorageConnectors :: !(Maybe [StorageConnector])
  , _sDisplayName :: !(Maybe Text)
  , _sStackErrors :: !(Maybe [StackError])
  , _sDescription :: !(Maybe Text)
  , _sRedirectURL :: !(Maybe Text)
  , _sName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Stack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sUserSettings' - The actions that are enabled or disabled for users during their streaming sessions. By default these actions are enabled.
--
-- * 'sApplicationSettings' - The persistent application settings for users of the stack.
--
-- * 'sFeedbackURL' - The URL that users are redirected to after they click the Send Feedback link. If no URL is specified, no Send Feedback link is displayed.
--
-- * 'sARN' - The ARN of the stack.
--
-- * 'sCreatedTime' - The time the stack was created.
--
-- * 'sStorageConnectors' - The storage connectors to enable.
--
-- * 'sDisplayName' - The stack name to display.
--
-- * 'sStackErrors' - The errors for the stack.
--
-- * 'sDescription' - The description to display.
--
-- * 'sRedirectURL' - The URL that users are redirected to after their streaming session ends.
--
-- * 'sName' - The name of the stack.
stack
    :: Text -- ^ 'sName'
    -> Stack
stack pName_ =
  Stack'
    { _sUserSettings = Nothing
    , _sApplicationSettings = Nothing
    , _sFeedbackURL = Nothing
    , _sARN = Nothing
    , _sCreatedTime = Nothing
    , _sStorageConnectors = Nothing
    , _sDisplayName = Nothing
    , _sStackErrors = Nothing
    , _sDescription = Nothing
    , _sRedirectURL = Nothing
    , _sName = pName_
    }


-- | The actions that are enabled or disabled for users during their streaming sessions. By default these actions are enabled.
sUserSettings :: Lens' Stack (Maybe (NonEmpty UserSetting))
sUserSettings = lens _sUserSettings (\ s a -> s{_sUserSettings = a}) . mapping _List1

-- | The persistent application settings for users of the stack.
sApplicationSettings :: Lens' Stack (Maybe ApplicationSettingsResponse)
sApplicationSettings = lens _sApplicationSettings (\ s a -> s{_sApplicationSettings = a})

-- | The URL that users are redirected to after they click the Send Feedback link. If no URL is specified, no Send Feedback link is displayed.
sFeedbackURL :: Lens' Stack (Maybe Text)
sFeedbackURL = lens _sFeedbackURL (\ s a -> s{_sFeedbackURL = a})

-- | The ARN of the stack.
sARN :: Lens' Stack (Maybe Text)
sARN = lens _sARN (\ s a -> s{_sARN = a})

-- | The time the stack was created.
sCreatedTime :: Lens' Stack (Maybe UTCTime)
sCreatedTime = lens _sCreatedTime (\ s a -> s{_sCreatedTime = a}) . mapping _Time

-- | The storage connectors to enable.
sStorageConnectors :: Lens' Stack [StorageConnector]
sStorageConnectors = lens _sStorageConnectors (\ s a -> s{_sStorageConnectors = a}) . _Default . _Coerce

-- | The stack name to display.
sDisplayName :: Lens' Stack (Maybe Text)
sDisplayName = lens _sDisplayName (\ s a -> s{_sDisplayName = a})

-- | The errors for the stack.
sStackErrors :: Lens' Stack [StackError]
sStackErrors = lens _sStackErrors (\ s a -> s{_sStackErrors = a}) . _Default . _Coerce

-- | The description to display.
sDescription :: Lens' Stack (Maybe Text)
sDescription = lens _sDescription (\ s a -> s{_sDescription = a})

-- | The URL that users are redirected to after their streaming session ends.
sRedirectURL :: Lens' Stack (Maybe Text)
sRedirectURL = lens _sRedirectURL (\ s a -> s{_sRedirectURL = a})

-- | The name of the stack.
sName :: Lens' Stack Text
sName = lens _sName (\ s a -> s{_sName = a})

instance FromJSON Stack where
        parseJSON
          = withObject "Stack"
              (\ x ->
                 Stack' <$>
                   (x .:? "UserSettings") <*>
                     (x .:? "ApplicationSettings")
                     <*> (x .:? "FeedbackURL")
                     <*> (x .:? "Arn")
                     <*> (x .:? "CreatedTime")
                     <*> (x .:? "StorageConnectors" .!= mempty)
                     <*> (x .:? "DisplayName")
                     <*> (x .:? "StackErrors" .!= mempty)
                     <*> (x .:? "Description")
                     <*> (x .:? "RedirectURL")
                     <*> (x .: "Name"))

instance Hashable Stack where

instance NFData Stack where

-- | Describes a stack error.
--
--
--
-- /See:/ 'stackError' smart constructor.
data StackError = StackError'
  { _seErrorCode :: !(Maybe StackErrorCode)
  , _seErrorMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StackError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seErrorCode' - The error code.
--
-- * 'seErrorMessage' - The error message.
stackError
    :: StackError
stackError = StackError' {_seErrorCode = Nothing, _seErrorMessage = Nothing}


-- | The error code.
seErrorCode :: Lens' StackError (Maybe StackErrorCode)
seErrorCode = lens _seErrorCode (\ s a -> s{_seErrorCode = a})

-- | The error message.
seErrorMessage :: Lens' StackError (Maybe Text)
seErrorMessage = lens _seErrorMessage (\ s a -> s{_seErrorMessage = a})

instance FromJSON StackError where
        parseJSON
          = withObject "StackError"
              (\ x ->
                 StackError' <$>
                   (x .:? "ErrorCode") <*> (x .:? "ErrorMessage"))

instance Hashable StackError where

instance NFData StackError where

-- | Describes a connector to enable persistent storage for users.
--
--
--
-- /See:/ 'storageConnector' smart constructor.
data StorageConnector = StorageConnector'
  { _scDomains :: !(Maybe [Text])
  , _scResourceIdentifier :: !(Maybe Text)
  , _scConnectorType :: !StorageConnectorType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StorageConnector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scDomains' - The names of the domains for the account.
--
-- * 'scResourceIdentifier' - The ARN of the storage connector.
--
-- * 'scConnectorType' - The type of storage connector.
storageConnector
    :: StorageConnectorType -- ^ 'scConnectorType'
    -> StorageConnector
storageConnector pConnectorType_ =
  StorageConnector'
    { _scDomains = Nothing
    , _scResourceIdentifier = Nothing
    , _scConnectorType = pConnectorType_
    }


-- | The names of the domains for the account.
scDomains :: Lens' StorageConnector [Text]
scDomains = lens _scDomains (\ s a -> s{_scDomains = a}) . _Default . _Coerce

-- | The ARN of the storage connector.
scResourceIdentifier :: Lens' StorageConnector (Maybe Text)
scResourceIdentifier = lens _scResourceIdentifier (\ s a -> s{_scResourceIdentifier = a})

-- | The type of storage connector.
scConnectorType :: Lens' StorageConnector StorageConnectorType
scConnectorType = lens _scConnectorType (\ s a -> s{_scConnectorType = a})

instance FromJSON StorageConnector where
        parseJSON
          = withObject "StorageConnector"
              (\ x ->
                 StorageConnector' <$>
                   (x .:? "Domains" .!= mempty) <*>
                     (x .:? "ResourceIdentifier")
                     <*> (x .: "ConnectorType"))

instance Hashable StorageConnector where

instance NFData StorageConnector where

instance ToJSON StorageConnector where
        toJSON StorageConnector'{..}
          = object
              (catMaybes
                 [("Domains" .=) <$> _scDomains,
                  ("ResourceIdentifier" .=) <$> _scResourceIdentifier,
                  Just ("ConnectorType" .= _scConnectorType)])

-- | Describes a user in the user pool.
--
--
--
-- /See:/ 'user' smart constructor.
data User = User'
  { _uStatus :: !(Maybe Text)
  , _uEnabled :: !(Maybe Bool)
  , _uLastName :: !(Maybe (Sensitive Text))
  , _uARN :: !(Maybe Text)
  , _uCreatedTime :: !(Maybe POSIX)
  , _uUserName :: !(Maybe (Sensitive Text))
  , _uFirstName :: !(Maybe (Sensitive Text))
  , _uAuthenticationType :: !AuthenticationType
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uStatus' - The status of the user in the user pool. The status can be one of the following:     * UNCONFIRMED – The user is created but not confirmed.     * CONFIRMED – The user is confirmed.     * ARCHIVED – The user is no longer active.     * COMPROMISED – The user is disabled because of a potential security threat.     * UNKNOWN – The user status is not known.
--
-- * 'uEnabled' - Specifies whether the user in the user pool is enabled.
--
-- * 'uLastName' - The last name, or surname, of the user.
--
-- * 'uARN' - The ARN of the user.
--
-- * 'uCreatedTime' - The date and time the user was created in the user pool.
--
-- * 'uUserName' - The email address of the user.
--
-- * 'uFirstName' - The first name, or given name, of the user.
--
-- * 'uAuthenticationType' - The authentication type for the user.
user
    :: AuthenticationType -- ^ 'uAuthenticationType'
    -> User
user pAuthenticationType_ =
  User'
    { _uStatus = Nothing
    , _uEnabled = Nothing
    , _uLastName = Nothing
    , _uARN = Nothing
    , _uCreatedTime = Nothing
    , _uUserName = Nothing
    , _uFirstName = Nothing
    , _uAuthenticationType = pAuthenticationType_
    }


-- | The status of the user in the user pool. The status can be one of the following:     * UNCONFIRMED – The user is created but not confirmed.     * CONFIRMED – The user is confirmed.     * ARCHIVED – The user is no longer active.     * COMPROMISED – The user is disabled because of a potential security threat.     * UNKNOWN – The user status is not known.
uStatus :: Lens' User (Maybe Text)
uStatus = lens _uStatus (\ s a -> s{_uStatus = a})

-- | Specifies whether the user in the user pool is enabled.
uEnabled :: Lens' User (Maybe Bool)
uEnabled = lens _uEnabled (\ s a -> s{_uEnabled = a})

-- | The last name, or surname, of the user.
uLastName :: Lens' User (Maybe Text)
uLastName = lens _uLastName (\ s a -> s{_uLastName = a}) . mapping _Sensitive

-- | The ARN of the user.
uARN :: Lens' User (Maybe Text)
uARN = lens _uARN (\ s a -> s{_uARN = a})

-- | The date and time the user was created in the user pool.
uCreatedTime :: Lens' User (Maybe UTCTime)
uCreatedTime = lens _uCreatedTime (\ s a -> s{_uCreatedTime = a}) . mapping _Time

-- | The email address of the user.
uUserName :: Lens' User (Maybe Text)
uUserName = lens _uUserName (\ s a -> s{_uUserName = a}) . mapping _Sensitive

-- | The first name, or given name, of the user.
uFirstName :: Lens' User (Maybe Text)
uFirstName = lens _uFirstName (\ s a -> s{_uFirstName = a}) . mapping _Sensitive

-- | The authentication type for the user.
uAuthenticationType :: Lens' User AuthenticationType
uAuthenticationType = lens _uAuthenticationType (\ s a -> s{_uAuthenticationType = a})

instance FromJSON User where
        parseJSON
          = withObject "User"
              (\ x ->
                 User' <$>
                   (x .:? "Status") <*> (x .:? "Enabled") <*>
                     (x .:? "LastName")
                     <*> (x .:? "Arn")
                     <*> (x .:? "CreatedTime")
                     <*> (x .:? "UserName")
                     <*> (x .:? "FirstName")
                     <*> (x .: "AuthenticationType"))

instance Hashable User where

instance NFData User where

-- | Describes an action and whether the action is enabled or disabled for users during their streaming sessions.
--
--
--
-- /See:/ 'userSetting' smart constructor.
data UserSetting = UserSetting'
  { _usAction :: !Action
  , _usPermission :: !Permission
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usAction' - The action that is enabled or disabled.
--
-- * 'usPermission' - Indicates whether the action is enabled or disabled.
userSetting
    :: Action -- ^ 'usAction'
    -> Permission -- ^ 'usPermission'
    -> UserSetting
userSetting pAction_ pPermission_ =
  UserSetting' {_usAction = pAction_, _usPermission = pPermission_}


-- | The action that is enabled or disabled.
usAction :: Lens' UserSetting Action
usAction = lens _usAction (\ s a -> s{_usAction = a})

-- | Indicates whether the action is enabled or disabled.
usPermission :: Lens' UserSetting Permission
usPermission = lens _usPermission (\ s a -> s{_usPermission = a})

instance FromJSON UserSetting where
        parseJSON
          = withObject "UserSetting"
              (\ x ->
                 UserSetting' <$>
                   (x .: "Action") <*> (x .: "Permission"))

instance Hashable UserSetting where

instance NFData UserSetting where

instance ToJSON UserSetting where
        toJSON UserSetting'{..}
          = object
              (catMaybes
                 [Just ("Action" .= _usAction),
                  Just ("Permission" .= _usPermission)])

-- | Describes a user in the user pool and the associated stack.
--
--
--
-- /See:/ 'userStackAssociation' smart constructor.
data UserStackAssociation = UserStackAssociation'
  { _usaSendEmailNotification :: !(Maybe Bool)
  , _usaStackName :: !Text
  , _usaUserName :: !(Sensitive Text)
  , _usaAuthenticationType :: !AuthenticationType
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserStackAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usaSendEmailNotification' - Specifies whether a welcome email is sent to a user after the user is created in the user pool.
--
-- * 'usaStackName' - The name of the stack that is associated with the user.
--
-- * 'usaUserName' - The email address of the user who is associated with the stack.
--
-- * 'usaAuthenticationType' - The authentication type for the user.
userStackAssociation
    :: Text -- ^ 'usaStackName'
    -> Text -- ^ 'usaUserName'
    -> AuthenticationType -- ^ 'usaAuthenticationType'
    -> UserStackAssociation
userStackAssociation pStackName_ pUserName_ pAuthenticationType_ =
  UserStackAssociation'
    { _usaSendEmailNotification = Nothing
    , _usaStackName = pStackName_
    , _usaUserName = _Sensitive # pUserName_
    , _usaAuthenticationType = pAuthenticationType_
    }


-- | Specifies whether a welcome email is sent to a user after the user is created in the user pool.
usaSendEmailNotification :: Lens' UserStackAssociation (Maybe Bool)
usaSendEmailNotification = lens _usaSendEmailNotification (\ s a -> s{_usaSendEmailNotification = a})

-- | The name of the stack that is associated with the user.
usaStackName :: Lens' UserStackAssociation Text
usaStackName = lens _usaStackName (\ s a -> s{_usaStackName = a})

-- | The email address of the user who is associated with the stack.
usaUserName :: Lens' UserStackAssociation Text
usaUserName = lens _usaUserName (\ s a -> s{_usaUserName = a}) . _Sensitive

-- | The authentication type for the user.
usaAuthenticationType :: Lens' UserStackAssociation AuthenticationType
usaAuthenticationType = lens _usaAuthenticationType (\ s a -> s{_usaAuthenticationType = a})

instance FromJSON UserStackAssociation where
        parseJSON
          = withObject "UserStackAssociation"
              (\ x ->
                 UserStackAssociation' <$>
                   (x .:? "SendEmailNotification") <*>
                     (x .: "StackName")
                     <*> (x .: "UserName")
                     <*> (x .: "AuthenticationType"))

instance Hashable UserStackAssociation where

instance NFData UserStackAssociation where

instance ToJSON UserStackAssociation where
        toJSON UserStackAssociation'{..}
          = object
              (catMaybes
                 [("SendEmailNotification" .=) <$>
                    _usaSendEmailNotification,
                  Just ("StackName" .= _usaStackName),
                  Just ("UserName" .= _usaUserName),
                  Just
                    ("AuthenticationType" .= _usaAuthenticationType)])

-- | Describes the error that is returned when a user can’t be associated with or disassociated from a stack. 
--
--
--
-- /See:/ 'userStackAssociationError' smart constructor.
data UserStackAssociationError = UserStackAssociationError'
  { _usaeUserStackAssociation :: !(Maybe UserStackAssociation)
  , _usaeErrorCode :: !(Maybe UserStackAssociationErrorCode)
  , _usaeErrorMessage :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserStackAssociationError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usaeUserStackAssociation' - Information about the user and associated stack.
--
-- * 'usaeErrorCode' - The error code for the error that is returned when a user can’t be associated with or disassociated from a stack.
--
-- * 'usaeErrorMessage' - The error message for the error that is returned when a user can’t be associated with or disassociated from a stack.
userStackAssociationError
    :: UserStackAssociationError
userStackAssociationError =
  UserStackAssociationError'
    { _usaeUserStackAssociation = Nothing
    , _usaeErrorCode = Nothing
    , _usaeErrorMessage = Nothing
    }


-- | Information about the user and associated stack.
usaeUserStackAssociation :: Lens' UserStackAssociationError (Maybe UserStackAssociation)
usaeUserStackAssociation = lens _usaeUserStackAssociation (\ s a -> s{_usaeUserStackAssociation = a})

-- | The error code for the error that is returned when a user can’t be associated with or disassociated from a stack.
usaeErrorCode :: Lens' UserStackAssociationError (Maybe UserStackAssociationErrorCode)
usaeErrorCode = lens _usaeErrorCode (\ s a -> s{_usaeErrorCode = a})

-- | The error message for the error that is returned when a user can’t be associated with or disassociated from a stack.
usaeErrorMessage :: Lens' UserStackAssociationError (Maybe Text)
usaeErrorMessage = lens _usaeErrorMessage (\ s a -> s{_usaeErrorMessage = a})

instance FromJSON UserStackAssociationError where
        parseJSON
          = withObject "UserStackAssociationError"
              (\ x ->
                 UserStackAssociationError' <$>
                   (x .:? "UserStackAssociation") <*>
                     (x .:? "ErrorCode")
                     <*> (x .:? "ErrorMessage"))

instance Hashable UserStackAssociationError where

instance NFData UserStackAssociationError where

-- | Describes VPC configuration information for fleets and image builders.
--
--
--
-- /See:/ 'vpcConfig' smart constructor.
data VPCConfig = VPCConfig'
  { _vcSecurityGroupIds :: !(Maybe [Text])
  , _vcSubnetIds :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcSecurityGroupIds' - The identifiers of the security groups for the fleet or image builder.
--
-- * 'vcSubnetIds' - The identifiers of the subnets to which a network interface is attached from the fleet instance or image builder instance. Fleet instances use one or two subnets. Image builder instances use one subnet.
vpcConfig
    :: VPCConfig
vpcConfig = VPCConfig' {_vcSecurityGroupIds = Nothing, _vcSubnetIds = Nothing}


-- | The identifiers of the security groups for the fleet or image builder.
vcSecurityGroupIds :: Lens' VPCConfig [Text]
vcSecurityGroupIds = lens _vcSecurityGroupIds (\ s a -> s{_vcSecurityGroupIds = a}) . _Default . _Coerce

-- | The identifiers of the subnets to which a network interface is attached from the fleet instance or image builder instance. Fleet instances use one or two subnets. Image builder instances use one subnet.
vcSubnetIds :: Lens' VPCConfig [Text]
vcSubnetIds = lens _vcSubnetIds (\ s a -> s{_vcSubnetIds = a}) . _Default . _Coerce

instance FromJSON VPCConfig where
        parseJSON
          = withObject "VPCConfig"
              (\ x ->
                 VPCConfig' <$>
                   (x .:? "SecurityGroupIds" .!= mempty) <*>
                     (x .:? "SubnetIds" .!= mempty))

instance Hashable VPCConfig where

instance NFData VPCConfig where

instance ToJSON VPCConfig where
        toJSON VPCConfig'{..}
          = object
              (catMaybes
                 [("SecurityGroupIds" .=) <$> _vcSecurityGroupIds,
                  ("SubnetIds" .=) <$> _vcSubnetIds])
