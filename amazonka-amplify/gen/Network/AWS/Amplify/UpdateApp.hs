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
-- Module      : Network.AWS.Amplify.UpdateApp
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Amplify App. 
--
--
module Network.AWS.Amplify.UpdateApp
    (
    -- * Creating a Request
      updateApp
    , UpdateApp
    -- * Request Lenses
    , uaEnableBranchAutoBuild
    , uaPlatform
    , uaBasicAuthCredentials
    , uaBuildSpec
    , uaCustomRules
    , uaIamServiceRoleARN
    , uaName
    , uaEnvironmentVariables
    , uaEnableBasicAuth
    , uaDescription
    , uaAppId

    -- * Destructuring the Response
    , updateAppResponse
    , UpdateAppResponse
    -- * Response Lenses
    , uarsResponseStatus
    , uarsApp
    ) where

import Network.AWS.Amplify.Types
import Network.AWS.Amplify.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure for update App request. 
--
--
--
-- /See:/ 'updateApp' smart constructor.
data UpdateApp = UpdateApp'
  { _uaEnableBranchAutoBuild :: !(Maybe Bool)
  , _uaPlatform :: !(Maybe Platform)
  , _uaBasicAuthCredentials :: !(Maybe Text)
  , _uaBuildSpec :: !(Maybe Text)
  , _uaCustomRules :: !(Maybe [CustomRule])
  , _uaIamServiceRoleARN :: !(Maybe Text)
  , _uaName :: !(Maybe Text)
  , _uaEnvironmentVariables :: !(Maybe (Map Text Text))
  , _uaEnableBasicAuth :: !(Maybe Bool)
  , _uaDescription :: !(Maybe Text)
  , _uaAppId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateApp' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaEnableBranchAutoBuild' - Enables branch auto-building for an Amplify App. 
--
-- * 'uaPlatform' - Platform for an Amplify App. 
--
-- * 'uaBasicAuthCredentials' - Basic Authorization credentials for an Amplify App. 
--
-- * 'uaBuildSpec' - BuildSpec for an Amplify App. 
--
-- * 'uaCustomRules' - Custom redirect / rewrite rules for an Amplify App. 
--
-- * 'uaIamServiceRoleARN' - IAM service role for an Amplify App. 
--
-- * 'uaName' - Name for an Amplify App. 
--
-- * 'uaEnvironmentVariables' - Environment Variables for an Amplify App. 
--
-- * 'uaEnableBasicAuth' - Enables Basic Authorization for an Amplify App. 
--
-- * 'uaDescription' - Description for an Amplify App. 
--
-- * 'uaAppId' - Unique Id for an Amplify App. 
updateApp
    :: Text -- ^ 'uaAppId'
    -> UpdateApp
updateApp pAppId_ =
  UpdateApp'
    { _uaEnableBranchAutoBuild = Nothing
    , _uaPlatform = Nothing
    , _uaBasicAuthCredentials = Nothing
    , _uaBuildSpec = Nothing
    , _uaCustomRules = Nothing
    , _uaIamServiceRoleARN = Nothing
    , _uaName = Nothing
    , _uaEnvironmentVariables = Nothing
    , _uaEnableBasicAuth = Nothing
    , _uaDescription = Nothing
    , _uaAppId = pAppId_
    }


-- | Enables branch auto-building for an Amplify App. 
uaEnableBranchAutoBuild :: Lens' UpdateApp (Maybe Bool)
uaEnableBranchAutoBuild = lens _uaEnableBranchAutoBuild (\ s a -> s{_uaEnableBranchAutoBuild = a})

-- | Platform for an Amplify App. 
uaPlatform :: Lens' UpdateApp (Maybe Platform)
uaPlatform = lens _uaPlatform (\ s a -> s{_uaPlatform = a})

-- | Basic Authorization credentials for an Amplify App. 
uaBasicAuthCredentials :: Lens' UpdateApp (Maybe Text)
uaBasicAuthCredentials = lens _uaBasicAuthCredentials (\ s a -> s{_uaBasicAuthCredentials = a})

-- | BuildSpec for an Amplify App. 
uaBuildSpec :: Lens' UpdateApp (Maybe Text)
uaBuildSpec = lens _uaBuildSpec (\ s a -> s{_uaBuildSpec = a})

-- | Custom redirect / rewrite rules for an Amplify App. 
uaCustomRules :: Lens' UpdateApp [CustomRule]
uaCustomRules = lens _uaCustomRules (\ s a -> s{_uaCustomRules = a}) . _Default . _Coerce

-- | IAM service role for an Amplify App. 
uaIamServiceRoleARN :: Lens' UpdateApp (Maybe Text)
uaIamServiceRoleARN = lens _uaIamServiceRoleARN (\ s a -> s{_uaIamServiceRoleARN = a})

-- | Name for an Amplify App. 
uaName :: Lens' UpdateApp (Maybe Text)
uaName = lens _uaName (\ s a -> s{_uaName = a})

-- | Environment Variables for an Amplify App. 
uaEnvironmentVariables :: Lens' UpdateApp (HashMap Text Text)
uaEnvironmentVariables = lens _uaEnvironmentVariables (\ s a -> s{_uaEnvironmentVariables = a}) . _Default . _Map

-- | Enables Basic Authorization for an Amplify App. 
uaEnableBasicAuth :: Lens' UpdateApp (Maybe Bool)
uaEnableBasicAuth = lens _uaEnableBasicAuth (\ s a -> s{_uaEnableBasicAuth = a})

-- | Description for an Amplify App. 
uaDescription :: Lens' UpdateApp (Maybe Text)
uaDescription = lens _uaDescription (\ s a -> s{_uaDescription = a})

-- | Unique Id for an Amplify App. 
uaAppId :: Lens' UpdateApp Text
uaAppId = lens _uaAppId (\ s a -> s{_uaAppId = a})

instance AWSRequest UpdateApp where
        type Rs UpdateApp = UpdateAppResponse
        request = postJSON amplify
        response
          = receiveJSON
              (\ s h x ->
                 UpdateAppResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "app"))

instance Hashable UpdateApp where

instance NFData UpdateApp where

instance ToHeaders UpdateApp where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateApp where
        toJSON UpdateApp'{..}
          = object
              (catMaybes
                 [("enableBranchAutoBuild" .=) <$>
                    _uaEnableBranchAutoBuild,
                  ("platform" .=) <$> _uaPlatform,
                  ("basicAuthCredentials" .=) <$>
                    _uaBasicAuthCredentials,
                  ("buildSpec" .=) <$> _uaBuildSpec,
                  ("customRules" .=) <$> _uaCustomRules,
                  ("iamServiceRoleArn" .=) <$> _uaIamServiceRoleARN,
                  ("name" .=) <$> _uaName,
                  ("environmentVariables" .=) <$>
                    _uaEnvironmentVariables,
                  ("enableBasicAuth" .=) <$> _uaEnableBasicAuth,
                  ("description" .=) <$> _uaDescription])

instance ToPath UpdateApp where
        toPath UpdateApp'{..}
          = mconcat ["/apps/", toBS _uaAppId]

instance ToQuery UpdateApp where
        toQuery = const mempty

-- | Result structure for an Amplify App update request. 
--
--
--
-- /See:/ 'updateAppResponse' smart constructor.
data UpdateAppResponse = UpdateAppResponse'
  { _uarsResponseStatus :: !Int
  , _uarsApp :: !App
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAppResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uarsResponseStatus' - -- | The response status code.
--
-- * 'uarsApp' - App structure for the updated App. 
updateAppResponse
    :: Int -- ^ 'uarsResponseStatus'
    -> App -- ^ 'uarsApp'
    -> UpdateAppResponse
updateAppResponse pResponseStatus_ pApp_ =
  UpdateAppResponse' {_uarsResponseStatus = pResponseStatus_, _uarsApp = pApp_}


-- | -- | The response status code.
uarsResponseStatus :: Lens' UpdateAppResponse Int
uarsResponseStatus = lens _uarsResponseStatus (\ s a -> s{_uarsResponseStatus = a})

-- | App structure for the updated App. 
uarsApp :: Lens' UpdateAppResponse App
uarsApp = lens _uarsApp (\ s a -> s{_uarsApp = a})

instance NFData UpdateAppResponse where
