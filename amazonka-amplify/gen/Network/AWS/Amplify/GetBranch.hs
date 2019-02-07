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
-- Module      : Network.AWS.Amplify.GetBranch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a branch for an Amplify App. 
--
--
module Network.AWS.Amplify.GetBranch
    (
    -- * Creating a Request
      getBranch
    , GetBranch
    -- * Request Lenses
    , gbAppId
    , gbBranchName

    -- * Destructuring the Response
    , getBranchResponse
    , GetBranchResponse
    -- * Response Lenses
    , gbrsResponseStatus
    , gbrsBranch
    ) where

import Network.AWS.Amplify.Types
import Network.AWS.Amplify.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Result structure for get branch request. 
--
--
--
-- /See:/ 'getBranch' smart constructor.
data GetBranch = GetBranch'
  { _gbAppId :: !Text
  , _gbBranchName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBranch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbAppId' - Unique Id for an Amplify App. 
--
-- * 'gbBranchName' - Name for the branch. 
getBranch
    :: Text -- ^ 'gbAppId'
    -> Text -- ^ 'gbBranchName'
    -> GetBranch
getBranch pAppId_ pBranchName_ =
  GetBranch' {_gbAppId = pAppId_, _gbBranchName = pBranchName_}


-- | Unique Id for an Amplify App. 
gbAppId :: Lens' GetBranch Text
gbAppId = lens _gbAppId (\ s a -> s{_gbAppId = a})

-- | Name for the branch. 
gbBranchName :: Lens' GetBranch Text
gbBranchName = lens _gbBranchName (\ s a -> s{_gbBranchName = a})

instance AWSRequest GetBranch where
        type Rs GetBranch = GetBranchResponse
        request = get amplify
        response
          = receiveJSON
              (\ s h x ->
                 GetBranchResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "branch"))

instance Hashable GetBranch where

instance NFData GetBranch where

instance ToHeaders GetBranch where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetBranch where
        toPath GetBranch'{..}
          = mconcat
              ["/apps/", toBS _gbAppId, "/branches/",
               toBS _gbBranchName]

instance ToQuery GetBranch where
        toQuery = const mempty

-- | /See:/ 'getBranchResponse' smart constructor.
data GetBranchResponse = GetBranchResponse'
  { _gbrsResponseStatus :: !Int
  , _gbrsBranch :: !Branch
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBranchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbrsResponseStatus' - -- | The response status code.
--
-- * 'gbrsBranch' - Undocumented member.
getBranchResponse
    :: Int -- ^ 'gbrsResponseStatus'
    -> Branch -- ^ 'gbrsBranch'
    -> GetBranchResponse
getBranchResponse pResponseStatus_ pBranch_ =
  GetBranchResponse'
    {_gbrsResponseStatus = pResponseStatus_, _gbrsBranch = pBranch_}


-- | -- | The response status code.
gbrsResponseStatus :: Lens' GetBranchResponse Int
gbrsResponseStatus = lens _gbrsResponseStatus (\ s a -> s{_gbrsResponseStatus = a})

-- | Undocumented member.
gbrsBranch :: Lens' GetBranchResponse Branch
gbrsBranch = lens _gbrsBranch (\ s a -> s{_gbrsBranch = a})

instance NFData GetBranchResponse where
