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
-- Module      : Network.AWS.Amplify.DeleteBranch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a branch for an Amplify App. 
--
--
module Network.AWS.Amplify.DeleteBranch
    (
    -- * Creating a Request
      deleteBranch
    , DeleteBranch
    -- * Request Lenses
    , dbAppId
    , dbBranchName

    -- * Destructuring the Response
    , deleteBranchResponse
    , DeleteBranchResponse
    -- * Response Lenses
    , dbrsResponseStatus
    , dbrsBranch
    ) where

import Network.AWS.Amplify.Types
import Network.AWS.Amplify.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure for delete branch request. 
--
--
--
-- /See:/ 'deleteBranch' smart constructor.
data DeleteBranch = DeleteBranch'
  { _dbAppId :: !Text
  , _dbBranchName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBranch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbAppId' - Unique Id for an Amplify App. 
--
-- * 'dbBranchName' - Name for the branch. 
deleteBranch
    :: Text -- ^ 'dbAppId'
    -> Text -- ^ 'dbBranchName'
    -> DeleteBranch
deleteBranch pAppId_ pBranchName_ =
  DeleteBranch' {_dbAppId = pAppId_, _dbBranchName = pBranchName_}


-- | Unique Id for an Amplify App. 
dbAppId :: Lens' DeleteBranch Text
dbAppId = lens _dbAppId (\ s a -> s{_dbAppId = a})

-- | Name for the branch. 
dbBranchName :: Lens' DeleteBranch Text
dbBranchName = lens _dbBranchName (\ s a -> s{_dbBranchName = a})

instance AWSRequest DeleteBranch where
        type Rs DeleteBranch = DeleteBranchResponse
        request = delete amplify
        response
          = receiveJSON
              (\ s h x ->
                 DeleteBranchResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "branch"))

instance Hashable DeleteBranch where

instance NFData DeleteBranch where

instance ToHeaders DeleteBranch where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteBranch where
        toPath DeleteBranch'{..}
          = mconcat
              ["/apps/", toBS _dbAppId, "/branches/",
               toBS _dbBranchName]

instance ToQuery DeleteBranch where
        toQuery = const mempty

-- | Result structure for delete branch request. 
--
--
--
-- /See:/ 'deleteBranchResponse' smart constructor.
data DeleteBranchResponse = DeleteBranchResponse'
  { _dbrsResponseStatus :: !Int
  , _dbrsBranch :: !Branch
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBranchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbrsResponseStatus' - -- | The response status code.
--
-- * 'dbrsBranch' - Branch structure for an Amplify App. 
deleteBranchResponse
    :: Int -- ^ 'dbrsResponseStatus'
    -> Branch -- ^ 'dbrsBranch'
    -> DeleteBranchResponse
deleteBranchResponse pResponseStatus_ pBranch_ =
  DeleteBranchResponse'
    {_dbrsResponseStatus = pResponseStatus_, _dbrsBranch = pBranch_}


-- | -- | The response status code.
dbrsResponseStatus :: Lens' DeleteBranchResponse Int
dbrsResponseStatus = lens _dbrsResponseStatus (\ s a -> s{_dbrsResponseStatus = a})

-- | Branch structure for an Amplify App. 
dbrsBranch :: Lens' DeleteBranchResponse Branch
dbrsBranch = lens _dbrsBranch (\ s a -> s{_dbrsBranch = a})

instance NFData DeleteBranchResponse where
