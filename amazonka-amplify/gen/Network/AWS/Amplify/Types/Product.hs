{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Amplify.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Amplify.Types.Product where

import Network.AWS.Amplify.Internal
import Network.AWS.Amplify.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Amplify App represents different branches of a repository for building, deploying, and hosting. 
--
--
--
-- /See:/ 'app' smart constructor.
data App = App'
  { _appBasicAuthCredentials :: !(Maybe Text)
  , _appBuildSpec :: !(Maybe Text)
  , _appCustomRules :: !(Maybe [CustomRule])
  , _appIamServiceRoleARN :: !(Maybe Text)
  , _appProductionBranch :: !(Maybe ProductionBranch)
  , _appTags :: !(Maybe (Map Text Text))
  , _appAppId :: !Text
  , _appAppARN :: !Text
  , _appName :: !Text
  , _appDescription :: !Text
  , _appRepository :: !Text
  , _appPlatform :: !Platform
  , _appCreateTime :: !POSIX
  , _appUpdateTime :: !POSIX
  , _appEnvironmentVariables :: !(Map Text Text)
  , _appDefaultDomain :: !Text
  , _appEnableBranchAutoBuild :: !Bool
  , _appEnableBasicAuth :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'App' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'appBasicAuthCredentials' - Basic Authorization credentials for branches for the Amplify App. 
--
-- * 'appBuildSpec' - BuildSpec content for Amplify App. 
--
-- * 'appCustomRules' - Custom redirect / rewrite rules for the Amplify App. 
--
-- * 'appIamServiceRoleARN' - IAM service role ARN for the Amplify App. 
--
-- * 'appProductionBranch' - Structure with Production Branch information. 
--
-- * 'appTags' - Tag for Amplify App. 
--
-- * 'appAppId' - Unique Id for the Amplify App. 
--
-- * 'appAppARN' - ARN for the Amplify App. 
--
-- * 'appName' - Name for the Amplify App. 
--
-- * 'appDescription' - Description for the Amplify App. 
--
-- * 'appRepository' - Repository for the Amplify App. 
--
-- * 'appPlatform' - Platform for the Amplify App. 
--
-- * 'appCreateTime' - Create date / time for the Amplify App. 
--
-- * 'appUpdateTime' - Update date / time for the Amplify App. 
--
-- * 'appEnvironmentVariables' - Environment Variables for the Amplify App. 
--
-- * 'appDefaultDomain' - Default domain for the Amplify App. 
--
-- * 'appEnableBranchAutoBuild' - Enables auto-building of branches for the Amplify App. 
--
-- * 'appEnableBasicAuth' - Enables Basic Authorization for branches for the Amplify App. 
app
    :: Text -- ^ 'appAppId'
    -> Text -- ^ 'appAppARN'
    -> Text -- ^ 'appName'
    -> Text -- ^ 'appDescription'
    -> Text -- ^ 'appRepository'
    -> Platform -- ^ 'appPlatform'
    -> UTCTime -- ^ 'appCreateTime'
    -> UTCTime -- ^ 'appUpdateTime'
    -> Text -- ^ 'appDefaultDomain'
    -> Bool -- ^ 'appEnableBranchAutoBuild'
    -> Bool -- ^ 'appEnableBasicAuth'
    -> App
app pAppId_ pAppARN_ pName_ pDescription_ pRepository_ pPlatform_ pCreateTime_ pUpdateTime_ pDefaultDomain_ pEnableBranchAutoBuild_ pEnableBasicAuth_ =
  App'
    { _appBasicAuthCredentials = Nothing
    , _appBuildSpec = Nothing
    , _appCustomRules = Nothing
    , _appIamServiceRoleARN = Nothing
    , _appProductionBranch = Nothing
    , _appTags = Nothing
    , _appAppId = pAppId_
    , _appAppARN = pAppARN_
    , _appName = pName_
    , _appDescription = pDescription_
    , _appRepository = pRepository_
    , _appPlatform = pPlatform_
    , _appCreateTime = _Time # pCreateTime_
    , _appUpdateTime = _Time # pUpdateTime_
    , _appEnvironmentVariables = mempty
    , _appDefaultDomain = pDefaultDomain_
    , _appEnableBranchAutoBuild = pEnableBranchAutoBuild_
    , _appEnableBasicAuth = pEnableBasicAuth_
    }


-- | Basic Authorization credentials for branches for the Amplify App. 
appBasicAuthCredentials :: Lens' App (Maybe Text)
appBasicAuthCredentials = lens _appBasicAuthCredentials (\ s a -> s{_appBasicAuthCredentials = a})

-- | BuildSpec content for Amplify App. 
appBuildSpec :: Lens' App (Maybe Text)
appBuildSpec = lens _appBuildSpec (\ s a -> s{_appBuildSpec = a})

-- | Custom redirect / rewrite rules for the Amplify App. 
appCustomRules :: Lens' App [CustomRule]
appCustomRules = lens _appCustomRules (\ s a -> s{_appCustomRules = a}) . _Default . _Coerce

-- | IAM service role ARN for the Amplify App. 
appIamServiceRoleARN :: Lens' App (Maybe Text)
appIamServiceRoleARN = lens _appIamServiceRoleARN (\ s a -> s{_appIamServiceRoleARN = a})

-- | Structure with Production Branch information. 
appProductionBranch :: Lens' App (Maybe ProductionBranch)
appProductionBranch = lens _appProductionBranch (\ s a -> s{_appProductionBranch = a})

-- | Tag for Amplify App. 
appTags :: Lens' App (HashMap Text Text)
appTags = lens _appTags (\ s a -> s{_appTags = a}) . _Default . _Map

-- | Unique Id for the Amplify App. 
appAppId :: Lens' App Text
appAppId = lens _appAppId (\ s a -> s{_appAppId = a})

-- | ARN for the Amplify App. 
appAppARN :: Lens' App Text
appAppARN = lens _appAppARN (\ s a -> s{_appAppARN = a})

-- | Name for the Amplify App. 
appName :: Lens' App Text
appName = lens _appName (\ s a -> s{_appName = a})

-- | Description for the Amplify App. 
appDescription :: Lens' App Text
appDescription = lens _appDescription (\ s a -> s{_appDescription = a})

-- | Repository for the Amplify App. 
appRepository :: Lens' App Text
appRepository = lens _appRepository (\ s a -> s{_appRepository = a})

-- | Platform for the Amplify App. 
appPlatform :: Lens' App Platform
appPlatform = lens _appPlatform (\ s a -> s{_appPlatform = a})

-- | Create date / time for the Amplify App. 
appCreateTime :: Lens' App UTCTime
appCreateTime = lens _appCreateTime (\ s a -> s{_appCreateTime = a}) . _Time

-- | Update date / time for the Amplify App. 
appUpdateTime :: Lens' App UTCTime
appUpdateTime = lens _appUpdateTime (\ s a -> s{_appUpdateTime = a}) . _Time

-- | Environment Variables for the Amplify App. 
appEnvironmentVariables :: Lens' App (HashMap Text Text)
appEnvironmentVariables = lens _appEnvironmentVariables (\ s a -> s{_appEnvironmentVariables = a}) . _Map

-- | Default domain for the Amplify App. 
appDefaultDomain :: Lens' App Text
appDefaultDomain = lens _appDefaultDomain (\ s a -> s{_appDefaultDomain = a})

-- | Enables auto-building of branches for the Amplify App. 
appEnableBranchAutoBuild :: Lens' App Bool
appEnableBranchAutoBuild = lens _appEnableBranchAutoBuild (\ s a -> s{_appEnableBranchAutoBuild = a})

-- | Enables Basic Authorization for branches for the Amplify App. 
appEnableBasicAuth :: Lens' App Bool
appEnableBasicAuth = lens _appEnableBasicAuth (\ s a -> s{_appEnableBasicAuth = a})

instance FromJSON App where
        parseJSON
          = withObject "App"
              (\ x ->
                 App' <$>
                   (x .:? "basicAuthCredentials") <*>
                     (x .:? "buildSpec")
                     <*> (x .:? "customRules" .!= mempty)
                     <*> (x .:? "iamServiceRoleArn")
                     <*> (x .:? "productionBranch")
                     <*> (x .:? "tags" .!= mempty)
                     <*> (x .: "appId")
                     <*> (x .: "appArn")
                     <*> (x .: "name")
                     <*> (x .: "description")
                     <*> (x .: "repository")
                     <*> (x .: "platform")
                     <*> (x .: "createTime")
                     <*> (x .: "updateTime")
                     <*> (x .:? "environmentVariables" .!= mempty)
                     <*> (x .: "defaultDomain")
                     <*> (x .: "enableBranchAutoBuild")
                     <*> (x .: "enableBasicAuth"))

instance Hashable App where

instance NFData App where

-- | Branch for an Amplify App, which maps to a 3rd party repository branch. 
--
--
--
-- /See:/ 'branch' smart constructor.
data Branch = Branch'
  { _bThumbnailURL :: !(Maybe Text)
  , _bBasicAuthCredentials :: !(Maybe Text)
  , _bBuildSpec :: !(Maybe Text)
  , _bDisplayName :: !(Maybe Text)
  , _bTags :: !(Maybe (Map Text Text))
  , _bBranchARN :: !Text
  , _bBranchName :: !Text
  , _bDescription :: !Text
  , _bStage :: !Stage
  , _bEnableNotification :: !Bool
  , _bCreateTime :: !POSIX
  , _bUpdateTime :: !POSIX
  , _bEnvironmentVariables :: !(Map Text Text)
  , _bEnableAutoBuild :: !Bool
  , _bCustomDomains :: ![Text]
  , _bFramework :: !Text
  , _bActiveJobId :: !Text
  , _bTotalNumberOfJobs :: !Text
  , _bEnableBasicAuth :: !Bool
  , _bTtl :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Branch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bThumbnailURL' - Thumbnail Url for the branch. 
--
-- * 'bBasicAuthCredentials' - Basic Authorization credentials for a branch, part of an Amplify App. 
--
-- * 'bBuildSpec' - BuildSpec content for branch for Amplify App. 
--
-- * 'bDisplayName' - Display name for a branch, part of an Amplify App. 
--
-- * 'bTags' - Tag for branch for Amplify App. 
--
-- * 'bBranchARN' - ARN for a branch, part of an Amplify App. 
--
-- * 'bBranchName' - Name for a branch, part of an Amplify App. 
--
-- * 'bDescription' - Description for a branch, part of an Amplify App. 
--
-- * 'bStage' - Stage for a branch, part of an Amplify App. 
--
-- * 'bEnableNotification' - Enables notifications for a branch, part of an Amplify App. 
--
-- * 'bCreateTime' - Creation date and time for a branch, part of an Amplify App. 
--
-- * 'bUpdateTime' - Last updated date and time for a branch, part of an Amplify App. 
--
-- * 'bEnvironmentVariables' - Environment Variables specific to a branch, part of an Amplify App. 
--
-- * 'bEnableAutoBuild' - Enables auto-building on push for a branch, part of an Amplify App. 
--
-- * 'bCustomDomains' - Custom domains for a branch, part of an Amplify App. 
--
-- * 'bFramework' - Framework for a branch, part of an Amplify App. 
--
-- * 'bActiveJobId' - Id of the active job for a branch, part of an Amplify App. 
--
-- * 'bTotalNumberOfJobs' - Total number of Jobs part of an Amplify App. 
--
-- * 'bEnableBasicAuth' - Enables Basic Authorization for a branch, part of an Amplify App. 
--
-- * 'bTtl' - The content TTL for the website in seconds. 
branch
    :: Text -- ^ 'bBranchARN'
    -> Text -- ^ 'bBranchName'
    -> Text -- ^ 'bDescription'
    -> Stage -- ^ 'bStage'
    -> Bool -- ^ 'bEnableNotification'
    -> UTCTime -- ^ 'bCreateTime'
    -> UTCTime -- ^ 'bUpdateTime'
    -> Bool -- ^ 'bEnableAutoBuild'
    -> Text -- ^ 'bFramework'
    -> Text -- ^ 'bActiveJobId'
    -> Text -- ^ 'bTotalNumberOfJobs'
    -> Bool -- ^ 'bEnableBasicAuth'
    -> Text -- ^ 'bTtl'
    -> Branch
branch pBranchARN_ pBranchName_ pDescription_ pStage_ pEnableNotification_ pCreateTime_ pUpdateTime_ pEnableAutoBuild_ pFramework_ pActiveJobId_ pTotalNumberOfJobs_ pEnableBasicAuth_ pTtl_ =
  Branch'
    { _bThumbnailURL = Nothing
    , _bBasicAuthCredentials = Nothing
    , _bBuildSpec = Nothing
    , _bDisplayName = Nothing
    , _bTags = Nothing
    , _bBranchARN = pBranchARN_
    , _bBranchName = pBranchName_
    , _bDescription = pDescription_
    , _bStage = pStage_
    , _bEnableNotification = pEnableNotification_
    , _bCreateTime = _Time # pCreateTime_
    , _bUpdateTime = _Time # pUpdateTime_
    , _bEnvironmentVariables = mempty
    , _bEnableAutoBuild = pEnableAutoBuild_
    , _bCustomDomains = mempty
    , _bFramework = pFramework_
    , _bActiveJobId = pActiveJobId_
    , _bTotalNumberOfJobs = pTotalNumberOfJobs_
    , _bEnableBasicAuth = pEnableBasicAuth_
    , _bTtl = pTtl_
    }


-- | Thumbnail Url for the branch. 
bThumbnailURL :: Lens' Branch (Maybe Text)
bThumbnailURL = lens _bThumbnailURL (\ s a -> s{_bThumbnailURL = a})

-- | Basic Authorization credentials for a branch, part of an Amplify App. 
bBasicAuthCredentials :: Lens' Branch (Maybe Text)
bBasicAuthCredentials = lens _bBasicAuthCredentials (\ s a -> s{_bBasicAuthCredentials = a})

-- | BuildSpec content for branch for Amplify App. 
bBuildSpec :: Lens' Branch (Maybe Text)
bBuildSpec = lens _bBuildSpec (\ s a -> s{_bBuildSpec = a})

-- | Display name for a branch, part of an Amplify App. 
bDisplayName :: Lens' Branch (Maybe Text)
bDisplayName = lens _bDisplayName (\ s a -> s{_bDisplayName = a})

-- | Tag for branch for Amplify App. 
bTags :: Lens' Branch (HashMap Text Text)
bTags = lens _bTags (\ s a -> s{_bTags = a}) . _Default . _Map

-- | ARN for a branch, part of an Amplify App. 
bBranchARN :: Lens' Branch Text
bBranchARN = lens _bBranchARN (\ s a -> s{_bBranchARN = a})

-- | Name for a branch, part of an Amplify App. 
bBranchName :: Lens' Branch Text
bBranchName = lens _bBranchName (\ s a -> s{_bBranchName = a})

-- | Description for a branch, part of an Amplify App. 
bDescription :: Lens' Branch Text
bDescription = lens _bDescription (\ s a -> s{_bDescription = a})

-- | Stage for a branch, part of an Amplify App. 
bStage :: Lens' Branch Stage
bStage = lens _bStage (\ s a -> s{_bStage = a})

-- | Enables notifications for a branch, part of an Amplify App. 
bEnableNotification :: Lens' Branch Bool
bEnableNotification = lens _bEnableNotification (\ s a -> s{_bEnableNotification = a})

-- | Creation date and time for a branch, part of an Amplify App. 
bCreateTime :: Lens' Branch UTCTime
bCreateTime = lens _bCreateTime (\ s a -> s{_bCreateTime = a}) . _Time

-- | Last updated date and time for a branch, part of an Amplify App. 
bUpdateTime :: Lens' Branch UTCTime
bUpdateTime = lens _bUpdateTime (\ s a -> s{_bUpdateTime = a}) . _Time

-- | Environment Variables specific to a branch, part of an Amplify App. 
bEnvironmentVariables :: Lens' Branch (HashMap Text Text)
bEnvironmentVariables = lens _bEnvironmentVariables (\ s a -> s{_bEnvironmentVariables = a}) . _Map

-- | Enables auto-building on push for a branch, part of an Amplify App. 
bEnableAutoBuild :: Lens' Branch Bool
bEnableAutoBuild = lens _bEnableAutoBuild (\ s a -> s{_bEnableAutoBuild = a})

-- | Custom domains for a branch, part of an Amplify App. 
bCustomDomains :: Lens' Branch [Text]
bCustomDomains = lens _bCustomDomains (\ s a -> s{_bCustomDomains = a}) . _Coerce

-- | Framework for a branch, part of an Amplify App. 
bFramework :: Lens' Branch Text
bFramework = lens _bFramework (\ s a -> s{_bFramework = a})

-- | Id of the active job for a branch, part of an Amplify App. 
bActiveJobId :: Lens' Branch Text
bActiveJobId = lens _bActiveJobId (\ s a -> s{_bActiveJobId = a})

-- | Total number of Jobs part of an Amplify App. 
bTotalNumberOfJobs :: Lens' Branch Text
bTotalNumberOfJobs = lens _bTotalNumberOfJobs (\ s a -> s{_bTotalNumberOfJobs = a})

-- | Enables Basic Authorization for a branch, part of an Amplify App. 
bEnableBasicAuth :: Lens' Branch Bool
bEnableBasicAuth = lens _bEnableBasicAuth (\ s a -> s{_bEnableBasicAuth = a})

-- | The content TTL for the website in seconds. 
bTtl :: Lens' Branch Text
bTtl = lens _bTtl (\ s a -> s{_bTtl = a})

instance FromJSON Branch where
        parseJSON
          = withObject "Branch"
              (\ x ->
                 Branch' <$>
                   (x .:? "thumbnailUrl") <*>
                     (x .:? "basicAuthCredentials")
                     <*> (x .:? "buildSpec")
                     <*> (x .:? "displayName")
                     <*> (x .:? "tags" .!= mempty)
                     <*> (x .: "branchArn")
                     <*> (x .: "branchName")
                     <*> (x .: "description")
                     <*> (x .: "stage")
                     <*> (x .: "enableNotification")
                     <*> (x .: "createTime")
                     <*> (x .: "updateTime")
                     <*> (x .:? "environmentVariables" .!= mempty)
                     <*> (x .: "enableAutoBuild")
                     <*> (x .:? "customDomains" .!= mempty)
                     <*> (x .: "framework")
                     <*> (x .: "activeJobId")
                     <*> (x .: "totalNumberOfJobs")
                     <*> (x .: "enableBasicAuth")
                     <*> (x .: "ttl"))

instance Hashable Branch where

instance NFData Branch where

-- | Custom rewrite / redirect rule. 
--
--
--
-- /See:/ 'customRule' smart constructor.
data CustomRule = CustomRule'
  { _crStatus :: !(Maybe Text)
  , _crCondition :: !(Maybe Text)
  , _crSource :: !Text
  , _crTarget :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CustomRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crStatus' - The status code for a URL rewrite or redirect rule. 
--
-- * 'crCondition' - The condition for a URL rewrite or redirect rule, e.g. country code. 
--
-- * 'crSource' - The source pattern for a URL rewrite or redirect rule. 
--
-- * 'crTarget' - The target pattern for a URL rewrite or redirect rule. 
customRule
    :: Text -- ^ 'crSource'
    -> Text -- ^ 'crTarget'
    -> CustomRule
customRule pSource_ pTarget_ =
  CustomRule'
    { _crStatus = Nothing
    , _crCondition = Nothing
    , _crSource = pSource_
    , _crTarget = pTarget_
    }


-- | The status code for a URL rewrite or redirect rule. 
crStatus :: Lens' CustomRule (Maybe Text)
crStatus = lens _crStatus (\ s a -> s{_crStatus = a})

-- | The condition for a URL rewrite or redirect rule, e.g. country code. 
crCondition :: Lens' CustomRule (Maybe Text)
crCondition = lens _crCondition (\ s a -> s{_crCondition = a})

-- | The source pattern for a URL rewrite or redirect rule. 
crSource :: Lens' CustomRule Text
crSource = lens _crSource (\ s a -> s{_crSource = a})

-- | The target pattern for a URL rewrite or redirect rule. 
crTarget :: Lens' CustomRule Text
crTarget = lens _crTarget (\ s a -> s{_crTarget = a})

instance FromJSON CustomRule where
        parseJSON
          = withObject "CustomRule"
              (\ x ->
                 CustomRule' <$>
                   (x .:? "status") <*> (x .:? "condition") <*>
                     (x .: "source")
                     <*> (x .: "target"))

instance Hashable CustomRule where

instance NFData CustomRule where

instance ToJSON CustomRule where
        toJSON CustomRule'{..}
          = object
              (catMaybes
                 [("status" .=) <$> _crStatus,
                  ("condition" .=) <$> _crCondition,
                  Just ("source" .= _crSource),
                  Just ("target" .= _crTarget)])

-- | Structure for Domain Association, which associates a custom domain with an Amplify App. 
--
--
--
-- /See:/ 'domainAssociation' smart constructor.
data DomainAssociation = DomainAssociation'
  { _daDomainAssociationARN :: !Text
  , _daDomainName :: !Text
  , _daEnableAutoSubDomain :: !Bool
  , _daDomainStatus :: !DomainStatus
  , _daStatusReason :: !Text
  , _daCertificateVerificationDNSRecord :: !Text
  , _daSubDomains :: ![SubDomain]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DomainAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daDomainAssociationARN' - ARN for the Domain Association. 
--
-- * 'daDomainName' - Name of the domain. 
--
-- * 'daEnableAutoSubDomain' - Enables automated creation of Subdomains for branches. 
--
-- * 'daDomainStatus' - Status fo the Domain Association. 
--
-- * 'daStatusReason' - Reason for the current status of the Domain Association. 
--
-- * 'daCertificateVerificationDNSRecord' - DNS Record for certificate verification. 
--
-- * 'daSubDomains' - Subdomains for the Domain Association. 
domainAssociation
    :: Text -- ^ 'daDomainAssociationARN'
    -> Text -- ^ 'daDomainName'
    -> Bool -- ^ 'daEnableAutoSubDomain'
    -> DomainStatus -- ^ 'daDomainStatus'
    -> Text -- ^ 'daStatusReason'
    -> Text -- ^ 'daCertificateVerificationDNSRecord'
    -> DomainAssociation
domainAssociation pDomainAssociationARN_ pDomainName_ pEnableAutoSubDomain_ pDomainStatus_ pStatusReason_ pCertificateVerificationDNSRecord_ =
  DomainAssociation'
    { _daDomainAssociationARN = pDomainAssociationARN_
    , _daDomainName = pDomainName_
    , _daEnableAutoSubDomain = pEnableAutoSubDomain_
    , _daDomainStatus = pDomainStatus_
    , _daStatusReason = pStatusReason_
    , _daCertificateVerificationDNSRecord = pCertificateVerificationDNSRecord_
    , _daSubDomains = mempty
    }


-- | ARN for the Domain Association. 
daDomainAssociationARN :: Lens' DomainAssociation Text
daDomainAssociationARN = lens _daDomainAssociationARN (\ s a -> s{_daDomainAssociationARN = a})

-- | Name of the domain. 
daDomainName :: Lens' DomainAssociation Text
daDomainName = lens _daDomainName (\ s a -> s{_daDomainName = a})

-- | Enables automated creation of Subdomains for branches. 
daEnableAutoSubDomain :: Lens' DomainAssociation Bool
daEnableAutoSubDomain = lens _daEnableAutoSubDomain (\ s a -> s{_daEnableAutoSubDomain = a})

-- | Status fo the Domain Association. 
daDomainStatus :: Lens' DomainAssociation DomainStatus
daDomainStatus = lens _daDomainStatus (\ s a -> s{_daDomainStatus = a})

-- | Reason for the current status of the Domain Association. 
daStatusReason :: Lens' DomainAssociation Text
daStatusReason = lens _daStatusReason (\ s a -> s{_daStatusReason = a})

-- | DNS Record for certificate verification. 
daCertificateVerificationDNSRecord :: Lens' DomainAssociation Text
daCertificateVerificationDNSRecord = lens _daCertificateVerificationDNSRecord (\ s a -> s{_daCertificateVerificationDNSRecord = a})

-- | Subdomains for the Domain Association. 
daSubDomains :: Lens' DomainAssociation [SubDomain]
daSubDomains = lens _daSubDomains (\ s a -> s{_daSubDomains = a}) . _Coerce

instance FromJSON DomainAssociation where
        parseJSON
          = withObject "DomainAssociation"
              (\ x ->
                 DomainAssociation' <$>
                   (x .: "domainAssociationArn") <*> (x .: "domainName")
                     <*> (x .: "enableAutoSubDomain")
                     <*> (x .: "domainStatus")
                     <*> (x .: "statusReason")
                     <*> (x .: "certificateVerificationDNSRecord")
                     <*> (x .:? "subDomains" .!= mempty))

instance Hashable DomainAssociation where

instance NFData DomainAssociation where

-- | Structure for an execution job for an Amplify App. 
--
--
--
-- /See:/ 'job' smart constructor.
data Job = Job'
  { _jSummary :: !JobSummary
  , _jSteps :: ![Step]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Job' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jSummary' - Summary for an execution job for an Amplify App. 
--
-- * 'jSteps' - Execution steps for an execution job, for an Amplify App. 
job
    :: JobSummary -- ^ 'jSummary'
    -> Job
job pSummary_ = Job' {_jSummary = pSummary_, _jSteps = mempty}


-- | Summary for an execution job for an Amplify App. 
jSummary :: Lens' Job JobSummary
jSummary = lens _jSummary (\ s a -> s{_jSummary = a})

-- | Execution steps for an execution job, for an Amplify App. 
jSteps :: Lens' Job [Step]
jSteps = lens _jSteps (\ s a -> s{_jSteps = a}) . _Coerce

instance FromJSON Job where
        parseJSON
          = withObject "Job"
              (\ x ->
                 Job' <$>
                   (x .: "summary") <*> (x .:? "steps" .!= mempty))

instance Hashable Job where

instance NFData Job where

-- | Structure for the summary of a Job. 
--
--
--
-- /See:/ 'jobSummary' smart constructor.
data JobSummary = JobSummary'
  { _jsEndTime :: !(Maybe POSIX)
  , _jsJobARN :: !Text
  , _jsJobId :: !Text
  , _jsCommitId :: !Text
  , _jsCommitMessage :: !Text
  , _jsCommitTime :: !POSIX
  , _jsStartTime :: !POSIX
  , _jsStatus :: !JobStatus
  , _jsJobType :: !JobType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jsEndTime' - End date / time for the Job. 
--
-- * 'jsJobARN' - Arn for the Job. 
--
-- * 'jsJobId' - Unique Id for the Job. 
--
-- * 'jsCommitId' - Commit Id from 3rd party repository provider for the Job. 
--
-- * 'jsCommitMessage' - Commit message from 3rd party repository provider for the Job. 
--
-- * 'jsCommitTime' - Commit date / time for the Job. 
--
-- * 'jsStartTime' - Start date / time for the Job. 
--
-- * 'jsStatus' - Status for the Job. 
--
-- * 'jsJobType' - Type for the Job. 
jobSummary
    :: Text -- ^ 'jsJobARN'
    -> Text -- ^ 'jsJobId'
    -> Text -- ^ 'jsCommitId'
    -> Text -- ^ 'jsCommitMessage'
    -> UTCTime -- ^ 'jsCommitTime'
    -> UTCTime -- ^ 'jsStartTime'
    -> JobStatus -- ^ 'jsStatus'
    -> JobType -- ^ 'jsJobType'
    -> JobSummary
jobSummary pJobARN_ pJobId_ pCommitId_ pCommitMessage_ pCommitTime_ pStartTime_ pStatus_ pJobType_ =
  JobSummary'
    { _jsEndTime = Nothing
    , _jsJobARN = pJobARN_
    , _jsJobId = pJobId_
    , _jsCommitId = pCommitId_
    , _jsCommitMessage = pCommitMessage_
    , _jsCommitTime = _Time # pCommitTime_
    , _jsStartTime = _Time # pStartTime_
    , _jsStatus = pStatus_
    , _jsJobType = pJobType_
    }


-- | End date / time for the Job. 
jsEndTime :: Lens' JobSummary (Maybe UTCTime)
jsEndTime = lens _jsEndTime (\ s a -> s{_jsEndTime = a}) . mapping _Time

-- | Arn for the Job. 
jsJobARN :: Lens' JobSummary Text
jsJobARN = lens _jsJobARN (\ s a -> s{_jsJobARN = a})

-- | Unique Id for the Job. 
jsJobId :: Lens' JobSummary Text
jsJobId = lens _jsJobId (\ s a -> s{_jsJobId = a})

-- | Commit Id from 3rd party repository provider for the Job. 
jsCommitId :: Lens' JobSummary Text
jsCommitId = lens _jsCommitId (\ s a -> s{_jsCommitId = a})

-- | Commit message from 3rd party repository provider for the Job. 
jsCommitMessage :: Lens' JobSummary Text
jsCommitMessage = lens _jsCommitMessage (\ s a -> s{_jsCommitMessage = a})

-- | Commit date / time for the Job. 
jsCommitTime :: Lens' JobSummary UTCTime
jsCommitTime = lens _jsCommitTime (\ s a -> s{_jsCommitTime = a}) . _Time

-- | Start date / time for the Job. 
jsStartTime :: Lens' JobSummary UTCTime
jsStartTime = lens _jsStartTime (\ s a -> s{_jsStartTime = a}) . _Time

-- | Status for the Job. 
jsStatus :: Lens' JobSummary JobStatus
jsStatus = lens _jsStatus (\ s a -> s{_jsStatus = a})

-- | Type for the Job. 
jsJobType :: Lens' JobSummary JobType
jsJobType = lens _jsJobType (\ s a -> s{_jsJobType = a})

instance FromJSON JobSummary where
        parseJSON
          = withObject "JobSummary"
              (\ x ->
                 JobSummary' <$>
                   (x .:? "endTime") <*> (x .: "jobArn") <*>
                     (x .: "jobId")
                     <*> (x .: "commitId")
                     <*> (x .: "commitMessage")
                     <*> (x .: "commitTime")
                     <*> (x .: "startTime")
                     <*> (x .: "status")
                     <*> (x .: "jobType"))

instance Hashable JobSummary where

instance NFData JobSummary where

-- | Structure with Production Branch information. 
--
--
--
-- /See:/ 'productionBranch' smart constructor.
data ProductionBranch = ProductionBranch'
  { _pbLastDeployTime :: !(Maybe POSIX)
  , _pbStatus :: !(Maybe Text)
  , _pbThumbnailURL :: !(Maybe Text)
  , _pbBranchName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProductionBranch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbLastDeployTime' - Last Deploy Time of Production Branch. 
--
-- * 'pbStatus' - Status of Production Branch. 
--
-- * 'pbThumbnailURL' - Thumbnail Url for Production Branch. 
--
-- * 'pbBranchName' - Branch Name for Production Branch. 
productionBranch
    :: ProductionBranch
productionBranch =
  ProductionBranch'
    { _pbLastDeployTime = Nothing
    , _pbStatus = Nothing
    , _pbThumbnailURL = Nothing
    , _pbBranchName = Nothing
    }


-- | Last Deploy Time of Production Branch. 
pbLastDeployTime :: Lens' ProductionBranch (Maybe UTCTime)
pbLastDeployTime = lens _pbLastDeployTime (\ s a -> s{_pbLastDeployTime = a}) . mapping _Time

-- | Status of Production Branch. 
pbStatus :: Lens' ProductionBranch (Maybe Text)
pbStatus = lens _pbStatus (\ s a -> s{_pbStatus = a})

-- | Thumbnail Url for Production Branch. 
pbThumbnailURL :: Lens' ProductionBranch (Maybe Text)
pbThumbnailURL = lens _pbThumbnailURL (\ s a -> s{_pbThumbnailURL = a})

-- | Branch Name for Production Branch. 
pbBranchName :: Lens' ProductionBranch (Maybe Text)
pbBranchName = lens _pbBranchName (\ s a -> s{_pbBranchName = a})

instance FromJSON ProductionBranch where
        parseJSON
          = withObject "ProductionBranch"
              (\ x ->
                 ProductionBranch' <$>
                   (x .:? "lastDeployTime") <*> (x .:? "status") <*>
                     (x .:? "thumbnailUrl")
                     <*> (x .:? "branchName"))

instance Hashable ProductionBranch where

instance NFData ProductionBranch where

-- | Structure for an execution step for an execution job, for an Amplify App. 
--
--
--
-- /See:/ 'step' smart constructor.
data Step = Step'
  { _sLogURL :: !(Maybe Text)
  , _sArtifactsURL :: !(Maybe Text)
  , _sScreenshots :: !(Maybe (Map Text Text))
  , _sStepName :: !Text
  , _sStartTime :: !POSIX
  , _sStatus :: !JobStatus
  , _sEndTime :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Step' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sLogURL' - Url to the logs for the execution step. 
--
-- * 'sArtifactsURL' - Url to teh artifact for the execution step. 
--
-- * 'sScreenshots' - List of screenshot Urls for the execution step, if relevant. 
--
-- * 'sStepName' - Name of the execution step. 
--
-- * 'sStartTime' - Start date/ time of the execution step. 
--
-- * 'sStatus' - Status of the execution step. 
--
-- * 'sEndTime' - End date/ time of the execution step. 
step
    :: Text -- ^ 'sStepName'
    -> UTCTime -- ^ 'sStartTime'
    -> JobStatus -- ^ 'sStatus'
    -> UTCTime -- ^ 'sEndTime'
    -> Step
step pStepName_ pStartTime_ pStatus_ pEndTime_ =
  Step'
    { _sLogURL = Nothing
    , _sArtifactsURL = Nothing
    , _sScreenshots = Nothing
    , _sStepName = pStepName_
    , _sStartTime = _Time # pStartTime_
    , _sStatus = pStatus_
    , _sEndTime = _Time # pEndTime_
    }


-- | Url to the logs for the execution step. 
sLogURL :: Lens' Step (Maybe Text)
sLogURL = lens _sLogURL (\ s a -> s{_sLogURL = a})

-- | Url to teh artifact for the execution step. 
sArtifactsURL :: Lens' Step (Maybe Text)
sArtifactsURL = lens _sArtifactsURL (\ s a -> s{_sArtifactsURL = a})

-- | List of screenshot Urls for the execution step, if relevant. 
sScreenshots :: Lens' Step (HashMap Text Text)
sScreenshots = lens _sScreenshots (\ s a -> s{_sScreenshots = a}) . _Default . _Map

-- | Name of the execution step. 
sStepName :: Lens' Step Text
sStepName = lens _sStepName (\ s a -> s{_sStepName = a})

-- | Start date/ time of the execution step. 
sStartTime :: Lens' Step UTCTime
sStartTime = lens _sStartTime (\ s a -> s{_sStartTime = a}) . _Time

-- | Status of the execution step. 
sStatus :: Lens' Step JobStatus
sStatus = lens _sStatus (\ s a -> s{_sStatus = a})

-- | End date/ time of the execution step. 
sEndTime :: Lens' Step UTCTime
sEndTime = lens _sEndTime (\ s a -> s{_sEndTime = a}) . _Time

instance FromJSON Step where
        parseJSON
          = withObject "Step"
              (\ x ->
                 Step' <$>
                   (x .:? "logUrl") <*> (x .:? "artifactsUrl") <*>
                     (x .:? "screenshots" .!= mempty)
                     <*> (x .: "stepName")
                     <*> (x .: "startTime")
                     <*> (x .: "status")
                     <*> (x .: "endTime"))

instance Hashable Step where

instance NFData Step where

-- | Subdomain for the Domain Association. 
--
--
--
-- /See:/ 'subDomain' smart constructor.
data SubDomain = SubDomain'
  { _sdSubDomainSetting :: !SubDomainSetting
  , _sdVerified :: !Bool
  , _sdDnsRecord :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SubDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdSubDomainSetting' - Setting structure for the Subdomain. 
--
-- * 'sdVerified' - Verified status of the Subdomain 
--
-- * 'sdDnsRecord' - DNS record for the Subdomain. 
subDomain
    :: SubDomainSetting -- ^ 'sdSubDomainSetting'
    -> Bool -- ^ 'sdVerified'
    -> Text -- ^ 'sdDnsRecord'
    -> SubDomain
subDomain pSubDomainSetting_ pVerified_ pDnsRecord_ =
  SubDomain'
    { _sdSubDomainSetting = pSubDomainSetting_
    , _sdVerified = pVerified_
    , _sdDnsRecord = pDnsRecord_
    }


-- | Setting structure for the Subdomain. 
sdSubDomainSetting :: Lens' SubDomain SubDomainSetting
sdSubDomainSetting = lens _sdSubDomainSetting (\ s a -> s{_sdSubDomainSetting = a})

-- | Verified status of the Subdomain 
sdVerified :: Lens' SubDomain Bool
sdVerified = lens _sdVerified (\ s a -> s{_sdVerified = a})

-- | DNS record for the Subdomain. 
sdDnsRecord :: Lens' SubDomain Text
sdDnsRecord = lens _sdDnsRecord (\ s a -> s{_sdDnsRecord = a})

instance FromJSON SubDomain where
        parseJSON
          = withObject "SubDomain"
              (\ x ->
                 SubDomain' <$>
                   (x .: "subDomainSetting") <*> (x .: "verified") <*>
                     (x .: "dnsRecord"))

instance Hashable SubDomain where

instance NFData SubDomain where

-- | Setting for the Subdomain. 
--
--
--
-- /See:/ 'subDomainSetting' smart constructor.
data SubDomainSetting = SubDomainSetting'
  { _sdsPrefix :: !Text
  , _sdsBranchName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SubDomainSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdsPrefix' - Prefix setting for the Subdomain. 
--
-- * 'sdsBranchName' - Branch name setting for the Subdomain. 
subDomainSetting
    :: Text -- ^ 'sdsPrefix'
    -> Text -- ^ 'sdsBranchName'
    -> SubDomainSetting
subDomainSetting pPrefix_ pBranchName_ =
  SubDomainSetting' {_sdsPrefix = pPrefix_, _sdsBranchName = pBranchName_}


-- | Prefix setting for the Subdomain. 
sdsPrefix :: Lens' SubDomainSetting Text
sdsPrefix = lens _sdsPrefix (\ s a -> s{_sdsPrefix = a})

-- | Branch name setting for the Subdomain. 
sdsBranchName :: Lens' SubDomainSetting Text
sdsBranchName = lens _sdsBranchName (\ s a -> s{_sdsBranchName = a})

instance FromJSON SubDomainSetting where
        parseJSON
          = withObject "SubDomainSetting"
              (\ x ->
                 SubDomainSetting' <$>
                   (x .: "prefix") <*> (x .: "branchName"))

instance Hashable SubDomainSetting where

instance NFData SubDomainSetting where

instance ToJSON SubDomainSetting where
        toJSON SubDomainSetting'{..}
          = object
              (catMaybes
                 [Just ("prefix" .= _sdsPrefix),
                  Just ("branchName" .= _sdsBranchName)])
