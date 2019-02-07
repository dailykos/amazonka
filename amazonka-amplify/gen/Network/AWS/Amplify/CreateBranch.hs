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
-- Module      : Network.AWS.Amplify.CreateBranch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Branch for an Amplify App. 
--
--
module Network.AWS.Amplify.CreateBranch
    (
    -- * Creating a Request
      createBranch
    , CreateBranch
    -- * Request Lenses
    , cbFramework
    , cbTtl
    , cbEnableNotification
    , cbStage
    , cbBasicAuthCredentials
    , cbBuildSpec
    , cbEnvironmentVariables
    , cbEnableAutoBuild
    , cbEnableBasicAuth
    , cbDescription
    , cbTags
    , cbAppId
    , cbBranchName

    -- * Destructuring the Response
    , createBranchResponse
    , CreateBranchResponse
    -- * Response Lenses
    , cbrsResponseStatus
    , cbrsBranch
    ) where

import Network.AWS.Amplify.Types
import Network.AWS.Amplify.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure for a branch create request. 
--
--
--
-- /See:/ 'createBranch' smart constructor.
data CreateBranch = CreateBranch'
  { _cbFramework :: !(Maybe Text)
  , _cbTtl :: !(Maybe Text)
  , _cbEnableNotification :: !(Maybe Bool)
  , _cbStage :: !(Maybe Stage)
  , _cbBasicAuthCredentials :: !(Maybe Text)
  , _cbBuildSpec :: !(Maybe Text)
  , _cbEnvironmentVariables :: !(Maybe (Map Text Text))
  , _cbEnableAutoBuild :: !(Maybe Bool)
  , _cbEnableBasicAuth :: !(Maybe Bool)
  , _cbDescription :: !(Maybe Text)
  , _cbTags :: !(Maybe (Map Text Text))
  , _cbAppId :: !Text
  , _cbBranchName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBranch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbFramework' - Framework for the branch. 
--
-- * 'cbTtl' - The content TTL for the website in seconds. 
--
-- * 'cbEnableNotification' - Enables notifications for the branch. 
--
-- * 'cbStage' - Stage for the branch. 
--
-- * 'cbBasicAuthCredentials' - Basic Authorization credentials for the branch. 
--
-- * 'cbBuildSpec' - BuildSpec for the branch. 
--
-- * 'cbEnvironmentVariables' - Environment Variables for the branch. 
--
-- * 'cbEnableAutoBuild' - Enables auto building for the branch. 
--
-- * 'cbEnableBasicAuth' - Enables Basic Auth for the branch. 
--
-- * 'cbDescription' - Description for the branch. 
--
-- * 'cbTags' - Tag for the branch. 
--
-- * 'cbAppId' - Unique Id for an Amplify App. 
--
-- * 'cbBranchName' - Name for the branch. 
createBranch
    :: Text -- ^ 'cbAppId'
    -> Text -- ^ 'cbBranchName'
    -> CreateBranch
createBranch pAppId_ pBranchName_ =
  CreateBranch'
    { _cbFramework = Nothing
    , _cbTtl = Nothing
    , _cbEnableNotification = Nothing
    , _cbStage = Nothing
    , _cbBasicAuthCredentials = Nothing
    , _cbBuildSpec = Nothing
    , _cbEnvironmentVariables = Nothing
    , _cbEnableAutoBuild = Nothing
    , _cbEnableBasicAuth = Nothing
    , _cbDescription = Nothing
    , _cbTags = Nothing
    , _cbAppId = pAppId_
    , _cbBranchName = pBranchName_
    }


-- | Framework for the branch. 
cbFramework :: Lens' CreateBranch (Maybe Text)
cbFramework = lens _cbFramework (\ s a -> s{_cbFramework = a})

-- | The content TTL for the website in seconds. 
cbTtl :: Lens' CreateBranch (Maybe Text)
cbTtl = lens _cbTtl (\ s a -> s{_cbTtl = a})

-- | Enables notifications for the branch. 
cbEnableNotification :: Lens' CreateBranch (Maybe Bool)
cbEnableNotification = lens _cbEnableNotification (\ s a -> s{_cbEnableNotification = a})

-- | Stage for the branch. 
cbStage :: Lens' CreateBranch (Maybe Stage)
cbStage = lens _cbStage (\ s a -> s{_cbStage = a})

-- | Basic Authorization credentials for the branch. 
cbBasicAuthCredentials :: Lens' CreateBranch (Maybe Text)
cbBasicAuthCredentials = lens _cbBasicAuthCredentials (\ s a -> s{_cbBasicAuthCredentials = a})

-- | BuildSpec for the branch. 
cbBuildSpec :: Lens' CreateBranch (Maybe Text)
cbBuildSpec = lens _cbBuildSpec (\ s a -> s{_cbBuildSpec = a})

-- | Environment Variables for the branch. 
cbEnvironmentVariables :: Lens' CreateBranch (HashMap Text Text)
cbEnvironmentVariables = lens _cbEnvironmentVariables (\ s a -> s{_cbEnvironmentVariables = a}) . _Default . _Map

-- | Enables auto building for the branch. 
cbEnableAutoBuild :: Lens' CreateBranch (Maybe Bool)
cbEnableAutoBuild = lens _cbEnableAutoBuild (\ s a -> s{_cbEnableAutoBuild = a})

-- | Enables Basic Auth for the branch. 
cbEnableBasicAuth :: Lens' CreateBranch (Maybe Bool)
cbEnableBasicAuth = lens _cbEnableBasicAuth (\ s a -> s{_cbEnableBasicAuth = a})

-- | Description for the branch. 
cbDescription :: Lens' CreateBranch (Maybe Text)
cbDescription = lens _cbDescription (\ s a -> s{_cbDescription = a})

-- | Tag for the branch. 
cbTags :: Lens' CreateBranch (HashMap Text Text)
cbTags = lens _cbTags (\ s a -> s{_cbTags = a}) . _Default . _Map

-- | Unique Id for an Amplify App. 
cbAppId :: Lens' CreateBranch Text
cbAppId = lens _cbAppId (\ s a -> s{_cbAppId = a})

-- | Name for the branch. 
cbBranchName :: Lens' CreateBranch Text
cbBranchName = lens _cbBranchName (\ s a -> s{_cbBranchName = a})

instance AWSRequest CreateBranch where
        type Rs CreateBranch = CreateBranchResponse
        request = postJSON amplify
        response
          = receiveJSON
              (\ s h x ->
                 CreateBranchResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "branch"))

instance Hashable CreateBranch where

instance NFData CreateBranch where

instance ToHeaders CreateBranch where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateBranch where
        toJSON CreateBranch'{..}
          = object
              (catMaybes
                 [("framework" .=) <$> _cbFramework,
                  ("ttl" .=) <$> _cbTtl,
                  ("enableNotification" .=) <$> _cbEnableNotification,
                  ("stage" .=) <$> _cbStage,
                  ("basicAuthCredentials" .=) <$>
                    _cbBasicAuthCredentials,
                  ("buildSpec" .=) <$> _cbBuildSpec,
                  ("environmentVariables" .=) <$>
                    _cbEnvironmentVariables,
                  ("enableAutoBuild" .=) <$> _cbEnableAutoBuild,
                  ("enableBasicAuth" .=) <$> _cbEnableBasicAuth,
                  ("description" .=) <$> _cbDescription,
                  ("tags" .=) <$> _cbTags,
                  Just ("branchName" .= _cbBranchName)])

instance ToPath CreateBranch where
        toPath CreateBranch'{..}
          = mconcat ["/apps/", toBS _cbAppId, "/branches"]

instance ToQuery CreateBranch where
        toQuery = const mempty

-- | Result structure for create branch request. 
--
--
--
-- /See:/ 'createBranchResponse' smart constructor.
data CreateBranchResponse = CreateBranchResponse'
  { _cbrsResponseStatus :: !Int
  , _cbrsBranch :: !Branch
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBranchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbrsResponseStatus' - -- | The response status code.
--
-- * 'cbrsBranch' - Branch structure for an Amplify App. 
createBranchResponse
    :: Int -- ^ 'cbrsResponseStatus'
    -> Branch -- ^ 'cbrsBranch'
    -> CreateBranchResponse
createBranchResponse pResponseStatus_ pBranch_ =
  CreateBranchResponse'
    {_cbrsResponseStatus = pResponseStatus_, _cbrsBranch = pBranch_}


-- | -- | The response status code.
cbrsResponseStatus :: Lens' CreateBranchResponse Int
cbrsResponseStatus = lens _cbrsResponseStatus (\ s a -> s{_cbrsResponseStatus = a})

-- | Branch structure for an Amplify App. 
cbrsBranch :: Lens' CreateBranchResponse Branch
cbrsBranch = lens _cbrsBranch (\ s a -> s{_cbrsBranch = a})

instance NFData CreateBranchResponse where
