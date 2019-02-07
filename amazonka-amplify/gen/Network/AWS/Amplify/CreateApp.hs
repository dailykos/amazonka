{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Amplify.CreateApp
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amplify App. 
--
--
module Network.AWS.Amplify.CreateApp
    (
    -- * Creating a Request
      createApp
    , CreateApp
    -- * Request Lenses
    , caEnableBranchAutoBuild
    , caBasicAuthCredentials
    , caBuildSpec
    , caCustomRules
    , caIamServiceRoleARN
    , caEnvironmentVariables
    , caEnableBasicAuth
    , caDescription
    , caTags
    , caName
    , caRepository
    , caPlatform
    , caOauthToken

    -- * Destructuring the Response
    , createAppResponse
    , CreateAppResponse
    -- * Response Lenses
    , carsResponseStatus
    , carsApp
    ) where

import Network.AWS.Amplify.Types
import Network.AWS.Amplify.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure used to create Apps in Amplify. 
--
--
--
-- /See:/ 'createApp' smart constructor.
data CreateApp = CreateApp'
  { _caEnableBranchAutoBuild :: !(Maybe Bool)
  , _caBasicAuthCredentials :: !(Maybe Text)
  , _caBuildSpec :: !(Maybe Text)
  , _caCustomRules :: !(Maybe [CustomRule])
  , _caIamServiceRoleARN :: !(Maybe Text)
  , _caEnvironmentVariables :: !(Maybe (Map Text Text))
  , _caEnableBasicAuth :: !(Maybe Bool)
  , _caDescription :: !(Maybe Text)
  , _caTags :: !(Maybe (Map Text Text))
  , _caName :: !Text
  , _caRepository :: !Text
  , _caPlatform :: !Platform
  , _caOauthToken :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateApp' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caEnableBranchAutoBuild' - Enable the auto building of branches for an Amplify App. 
--
-- * 'caBasicAuthCredentials' - Credentials for Basic Authorization for an Amplify App. 
--
-- * 'caBuildSpec' - BuildSpec for an Amplify App 
--
-- * 'caCustomRules' - Custom rewrite / redirect rules for an Amplify App. 
--
-- * 'caIamServiceRoleARN' - AWS IAM service role for an Amplify App 
--
-- * 'caEnvironmentVariables' - Environment variables map for an Amplify App. 
--
-- * 'caEnableBasicAuth' - Enable Basic Authorization for an Amplify App, this will apply to all branches part of this App. 
--
-- * 'caDescription' - Description for an Amplify App 
--
-- * 'caTags' - Tag for an Amplify App 
--
-- * 'caName' - Name for the Amplify App 
--
-- * 'caRepository' - Repository for an Amplify App 
--
-- * 'caPlatform' - Platform / framework for an Amplify App 
--
-- * 'caOauthToken' - OAuth token for 3rd party source control system for an Amplify App, used to create webhook and read-only deploy key. OAuth token is not stored. 
createApp
    :: Text -- ^ 'caName'
    -> Text -- ^ 'caRepository'
    -> Platform -- ^ 'caPlatform'
    -> Text -- ^ 'caOauthToken'
    -> CreateApp
createApp pName_ pRepository_ pPlatform_ pOauthToken_ =
  CreateApp'
    { _caEnableBranchAutoBuild = Nothing
    , _caBasicAuthCredentials = Nothing
    , _caBuildSpec = Nothing
    , _caCustomRules = Nothing
    , _caIamServiceRoleARN = Nothing
    , _caEnvironmentVariables = Nothing
    , _caEnableBasicAuth = Nothing
    , _caDescription = Nothing
    , _caTags = Nothing
    , _caName = pName_
    , _caRepository = pRepository_
    , _caPlatform = pPlatform_
    , _caOauthToken = pOauthToken_
    }


-- | Enable the auto building of branches for an Amplify App. 
caEnableBranchAutoBuild :: Lens' CreateApp (Maybe Bool)
caEnableBranchAutoBuild = lens _caEnableBranchAutoBuild (\ s a -> s{_caEnableBranchAutoBuild = a})

-- | Credentials for Basic Authorization for an Amplify App. 
caBasicAuthCredentials :: Lens' CreateApp (Maybe Text)
caBasicAuthCredentials = lens _caBasicAuthCredentials (\ s a -> s{_caBasicAuthCredentials = a})

-- | BuildSpec for an Amplify App 
caBuildSpec :: Lens' CreateApp (Maybe Text)
caBuildSpec = lens _caBuildSpec (\ s a -> s{_caBuildSpec = a})

-- | Custom rewrite / redirect rules for an Amplify App. 
caCustomRules :: Lens' CreateApp [CustomRule]
caCustomRules = lens _caCustomRules (\ s a -> s{_caCustomRules = a}) . _Default . _Coerce

-- | AWS IAM service role for an Amplify App 
caIamServiceRoleARN :: Lens' CreateApp (Maybe Text)
caIamServiceRoleARN = lens _caIamServiceRoleARN (\ s a -> s{_caIamServiceRoleARN = a})

-- | Environment variables map for an Amplify App. 
caEnvironmentVariables :: Lens' CreateApp (HashMap Text Text)
caEnvironmentVariables = lens _caEnvironmentVariables (\ s a -> s{_caEnvironmentVariables = a}) . _Default . _Map

-- | Enable Basic Authorization for an Amplify App, this will apply to all branches part of this App. 
caEnableBasicAuth :: Lens' CreateApp (Maybe Bool)
caEnableBasicAuth = lens _caEnableBasicAuth (\ s a -> s{_caEnableBasicAuth = a})

-- | Description for an Amplify App 
caDescription :: Lens' CreateApp (Maybe Text)
caDescription = lens _caDescription (\ s a -> s{_caDescription = a})

-- | Tag for an Amplify App 
caTags :: Lens' CreateApp (HashMap Text Text)
caTags = lens _caTags (\ s a -> s{_caTags = a}) . _Default . _Map

-- | Name for the Amplify App 
caName :: Lens' CreateApp Text
caName = lens _caName (\ s a -> s{_caName = a})

-- | Repository for an Amplify App 
caRepository :: Lens' CreateApp Text
caRepository = lens _caRepository (\ s a -> s{_caRepository = a})

-- | Platform / framework for an Amplify App 
caPlatform :: Lens' CreateApp Platform
caPlatform = lens _caPlatform (\ s a -> s{_caPlatform = a})

-- | OAuth token for 3rd party source control system for an Amplify App, used to create webhook and read-only deploy key. OAuth token is not stored. 
caOauthToken :: Lens' CreateApp Text
caOauthToken = lens _caOauthToken (\ s a -> s{_caOauthToken = a})

instance AWSRequest CreateApp where
        type Rs CreateApp = CreateAppResponse
        request = postJSON amplify
        response
          = receiveJSON
              (\ s h x ->
                 CreateAppResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "app"))

instance Hashable CreateApp where

instance NFData CreateApp where

instance ToHeaders CreateApp where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateApp where
        toJSON CreateApp'{..}
          = object
              (catMaybes
                 [("enableBranchAutoBuild" .=) <$>
                    _caEnableBranchAutoBuild,
                  ("basicAuthCredentials" .=) <$>
                    _caBasicAuthCredentials,
                  ("buildSpec" .=) <$> _caBuildSpec,
                  ("customRules" .=) <$> _caCustomRules,
                  ("iamServiceRoleArn" .=) <$> _caIamServiceRoleARN,
                  ("environmentVariables" .=) <$>
                    _caEnvironmentVariables,
                  ("enableBasicAuth" .=) <$> _caEnableBasicAuth,
                  ("description" .=) <$> _caDescription,
                  ("tags" .=) <$> _caTags, Just ("name" .= _caName),
                  Just ("repository" .= _caRepository),
                  Just ("platform" .= _caPlatform),
                  Just ("oauthToken" .= _caOauthToken)])

instance ToPath CreateApp where
        toPath = const "/apps"

instance ToQuery CreateApp where
        toQuery = const mempty

-- | /See:/ 'createAppResponse' smart constructor.
data CreateAppResponse = CreateAppResponse'
  { _carsResponseStatus :: !Int
  , _carsApp :: !App
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAppResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carsResponseStatus' - -- | The response status code.
--
-- * 'carsApp' - Undocumented member.
createAppResponse
    :: Int -- ^ 'carsResponseStatus'
    -> App -- ^ 'carsApp'
    -> CreateAppResponse
createAppResponse pResponseStatus_ pApp_ =
  CreateAppResponse' {_carsResponseStatus = pResponseStatus_, _carsApp = pApp_}


-- | -- | The response status code.
carsResponseStatus :: Lens' CreateAppResponse Int
carsResponseStatus = lens _carsResponseStatus (\ s a -> s{_carsResponseStatus = a})

-- | Undocumented member.
carsApp :: Lens' CreateAppResponse App
carsApp = lens _carsApp (\ s a -> s{_carsApp = a})

instance NFData CreateAppResponse where
