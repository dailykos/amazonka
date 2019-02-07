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
-- Module      : Network.AWS.Amplify.UpdateBranch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a branch for an Amplify App. 
--
--
module Network.AWS.Amplify.UpdateBranch
    (
    -- * Creating a Request
      updateBranch
    , UpdateBranch
    -- * Request Lenses
    , ubFramework
    , ubTtl
    , ubEnableNotification
    , ubStage
    , ubBasicAuthCredentials
    , ubBuildSpec
    , ubEnvironmentVariables
    , ubEnableAutoBuild
    , ubEnableBasicAuth
    , ubDescription
    , ubAppId
    , ubBranchName

    -- * Destructuring the Response
    , updateBranchResponse
    , UpdateBranchResponse
    -- * Response Lenses
    , ubrsResponseStatus
    , ubrsBranch
    ) where

import Network.AWS.Amplify.Types
import Network.AWS.Amplify.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure for update branch request. 
--
--
--
-- /See:/ 'updateBranch' smart constructor.
data UpdateBranch = UpdateBranch'
  { _ubFramework :: !(Maybe Text)
  , _ubTtl :: !(Maybe Text)
  , _ubEnableNotification :: !(Maybe Bool)
  , _ubStage :: !(Maybe Stage)
  , _ubBasicAuthCredentials :: !(Maybe Text)
  , _ubBuildSpec :: !(Maybe Text)
  , _ubEnvironmentVariables :: !(Maybe (Map Text Text))
  , _ubEnableAutoBuild :: !(Maybe Bool)
  , _ubEnableBasicAuth :: !(Maybe Bool)
  , _ubDescription :: !(Maybe Text)
  , _ubAppId :: !Text
  , _ubBranchName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateBranch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubFramework' - Framework for the branch. 
--
-- * 'ubTtl' - The content TTL for the website in seconds. 
--
-- * 'ubEnableNotification' - Enables notifications for the branch. 
--
-- * 'ubStage' - Stage for the branch. 
--
-- * 'ubBasicAuthCredentials' - Basic Authorization credentials for the branch. 
--
-- * 'ubBuildSpec' - BuildSpec for the branch. 
--
-- * 'ubEnvironmentVariables' - Environment Variables for the branch. 
--
-- * 'ubEnableAutoBuild' - Enables auto building for the branch. 
--
-- * 'ubEnableBasicAuth' - Enables Basic Auth for the branch. 
--
-- * 'ubDescription' - Description for the branch. 
--
-- * 'ubAppId' - Unique Id for an Amplify App. 
--
-- * 'ubBranchName' - Name for the branch. 
updateBranch
    :: Text -- ^ 'ubAppId'
    -> Text -- ^ 'ubBranchName'
    -> UpdateBranch
updateBranch pAppId_ pBranchName_ =
  UpdateBranch'
    { _ubFramework = Nothing
    , _ubTtl = Nothing
    , _ubEnableNotification = Nothing
    , _ubStage = Nothing
    , _ubBasicAuthCredentials = Nothing
    , _ubBuildSpec = Nothing
    , _ubEnvironmentVariables = Nothing
    , _ubEnableAutoBuild = Nothing
    , _ubEnableBasicAuth = Nothing
    , _ubDescription = Nothing
    , _ubAppId = pAppId_
    , _ubBranchName = pBranchName_
    }


-- | Framework for the branch. 
ubFramework :: Lens' UpdateBranch (Maybe Text)
ubFramework = lens _ubFramework (\ s a -> s{_ubFramework = a})

-- | The content TTL for the website in seconds. 
ubTtl :: Lens' UpdateBranch (Maybe Text)
ubTtl = lens _ubTtl (\ s a -> s{_ubTtl = a})

-- | Enables notifications for the branch. 
ubEnableNotification :: Lens' UpdateBranch (Maybe Bool)
ubEnableNotification = lens _ubEnableNotification (\ s a -> s{_ubEnableNotification = a})

-- | Stage for the branch. 
ubStage :: Lens' UpdateBranch (Maybe Stage)
ubStage = lens _ubStage (\ s a -> s{_ubStage = a})

-- | Basic Authorization credentials for the branch. 
ubBasicAuthCredentials :: Lens' UpdateBranch (Maybe Text)
ubBasicAuthCredentials = lens _ubBasicAuthCredentials (\ s a -> s{_ubBasicAuthCredentials = a})

-- | BuildSpec for the branch. 
ubBuildSpec :: Lens' UpdateBranch (Maybe Text)
ubBuildSpec = lens _ubBuildSpec (\ s a -> s{_ubBuildSpec = a})

-- | Environment Variables for the branch. 
ubEnvironmentVariables :: Lens' UpdateBranch (HashMap Text Text)
ubEnvironmentVariables = lens _ubEnvironmentVariables (\ s a -> s{_ubEnvironmentVariables = a}) . _Default . _Map

-- | Enables auto building for the branch. 
ubEnableAutoBuild :: Lens' UpdateBranch (Maybe Bool)
ubEnableAutoBuild = lens _ubEnableAutoBuild (\ s a -> s{_ubEnableAutoBuild = a})

-- | Enables Basic Auth for the branch. 
ubEnableBasicAuth :: Lens' UpdateBranch (Maybe Bool)
ubEnableBasicAuth = lens _ubEnableBasicAuth (\ s a -> s{_ubEnableBasicAuth = a})

-- | Description for the branch. 
ubDescription :: Lens' UpdateBranch (Maybe Text)
ubDescription = lens _ubDescription (\ s a -> s{_ubDescription = a})

-- | Unique Id for an Amplify App. 
ubAppId :: Lens' UpdateBranch Text
ubAppId = lens _ubAppId (\ s a -> s{_ubAppId = a})

-- | Name for the branch. 
ubBranchName :: Lens' UpdateBranch Text
ubBranchName = lens _ubBranchName (\ s a -> s{_ubBranchName = a})

instance AWSRequest UpdateBranch where
        type Rs UpdateBranch = UpdateBranchResponse
        request = postJSON amplify
        response
          = receiveJSON
              (\ s h x ->
                 UpdateBranchResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "branch"))

instance Hashable UpdateBranch where

instance NFData UpdateBranch where

instance ToHeaders UpdateBranch where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateBranch where
        toJSON UpdateBranch'{..}
          = object
              (catMaybes
                 [("framework" .=) <$> _ubFramework,
                  ("ttl" .=) <$> _ubTtl,
                  ("enableNotification" .=) <$> _ubEnableNotification,
                  ("stage" .=) <$> _ubStage,
                  ("basicAuthCredentials" .=) <$>
                    _ubBasicAuthCredentials,
                  ("buildSpec" .=) <$> _ubBuildSpec,
                  ("environmentVariables" .=) <$>
                    _ubEnvironmentVariables,
                  ("enableAutoBuild" .=) <$> _ubEnableAutoBuild,
                  ("enableBasicAuth" .=) <$> _ubEnableBasicAuth,
                  ("description" .=) <$> _ubDescription])

instance ToPath UpdateBranch where
        toPath UpdateBranch'{..}
          = mconcat
              ["/apps/", toBS _ubAppId, "/branches/",
               toBS _ubBranchName]

instance ToQuery UpdateBranch where
        toQuery = const mempty

-- | Result structure for update branch request. 
--
--
--
-- /See:/ 'updateBranchResponse' smart constructor.
data UpdateBranchResponse = UpdateBranchResponse'
  { _ubrsResponseStatus :: !Int
  , _ubrsBranch :: !Branch
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateBranchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubrsResponseStatus' - -- | The response status code.
--
-- * 'ubrsBranch' - Branch structure for an Amplify App. 
updateBranchResponse
    :: Int -- ^ 'ubrsResponseStatus'
    -> Branch -- ^ 'ubrsBranch'
    -> UpdateBranchResponse
updateBranchResponse pResponseStatus_ pBranch_ =
  UpdateBranchResponse'
    {_ubrsResponseStatus = pResponseStatus_, _ubrsBranch = pBranch_}


-- | -- | The response status code.
ubrsResponseStatus :: Lens' UpdateBranchResponse Int
ubrsResponseStatus = lens _ubrsResponseStatus (\ s a -> s{_ubrsResponseStatus = a})

-- | Branch structure for an Amplify App. 
ubrsBranch :: Lens' UpdateBranchResponse Branch
ubrsBranch = lens _ubrsBranch (\ s a -> s{_ubrsBranch = a})

instance NFData UpdateBranchResponse where
