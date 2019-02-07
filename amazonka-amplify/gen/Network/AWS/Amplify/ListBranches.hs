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
-- Module      : Network.AWS.Amplify.ListBranches
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists branches for an Amplify App. 
--
--
--
-- This operation returns paginated results.
module Network.AWS.Amplify.ListBranches
    (
    -- * Creating a Request
      listBranches
    , ListBranches
    -- * Request Lenses
    , lbNextToken
    , lbMaxResults
    , lbAppId

    -- * Destructuring the Response
    , listBranchesResponse
    , ListBranchesResponse
    -- * Response Lenses
    , lbrsNextToken
    , lbrsResponseStatus
    , lbrsBranches
    ) where

import Network.AWS.Amplify.Types
import Network.AWS.Amplify.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure for list branches request. 
--
--
--
-- /See:/ 'listBranches' smart constructor.
data ListBranches = ListBranches'
  { _lbNextToken :: !(Maybe Text)
  , _lbMaxResults :: !(Maybe Nat)
  , _lbAppId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBranches' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbNextToken' - Pagination token. Set to null to start listing branches from start. If a non-null pagination token is returned in a result, then pass its value in here to list more branches. 
--
-- * 'lbMaxResults' - Maximum number of records to list in a single response. 
--
-- * 'lbAppId' - Unique Id for an Amplify App. 
listBranches
    :: Text -- ^ 'lbAppId'
    -> ListBranches
listBranches pAppId_ =
  ListBranches'
    {_lbNextToken = Nothing, _lbMaxResults = Nothing, _lbAppId = pAppId_}


-- | Pagination token. Set to null to start listing branches from start. If a non-null pagination token is returned in a result, then pass its value in here to list more branches. 
lbNextToken :: Lens' ListBranches (Maybe Text)
lbNextToken = lens _lbNextToken (\ s a -> s{_lbNextToken = a})

-- | Maximum number of records to list in a single response. 
lbMaxResults :: Lens' ListBranches (Maybe Natural)
lbMaxResults = lens _lbMaxResults (\ s a -> s{_lbMaxResults = a}) . mapping _Nat

-- | Unique Id for an Amplify App. 
lbAppId :: Lens' ListBranches Text
lbAppId = lens _lbAppId (\ s a -> s{_lbAppId = a})

instance AWSPager ListBranches where
        page rq rs
          | stop (rs ^. lbrsNextToken) = Nothing
          | stop (rs ^. lbrsBranches) = Nothing
          | otherwise =
            Just $ rq & lbNextToken .~ rs ^. lbrsNextToken

instance AWSRequest ListBranches where
        type Rs ListBranches = ListBranchesResponse
        request = get amplify
        response
          = receiveJSON
              (\ s h x ->
                 ListBranchesResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "branches" .!@ mempty))

instance Hashable ListBranches where

instance NFData ListBranches where

instance ToHeaders ListBranches where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListBranches where
        toPath ListBranches'{..}
          = mconcat ["/apps/", toBS _lbAppId, "/branches"]

instance ToQuery ListBranches where
        toQuery ListBranches'{..}
          = mconcat
              ["nextToken" =: _lbNextToken,
               "maxResults" =: _lbMaxResults]

-- | Result structure for list branches request. 
--
--
--
-- /See:/ 'listBranchesResponse' smart constructor.
data ListBranchesResponse = ListBranchesResponse'
  { _lbrsNextToken :: !(Maybe Text)
  , _lbrsResponseStatus :: !Int
  , _lbrsBranches :: ![Branch]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBranchesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbrsNextToken' - Pagination token. If non-null pagination token is returned in a result, then pass its value in another request to fetch more entries. 
--
-- * 'lbrsResponseStatus' - -- | The response status code.
--
-- * 'lbrsBranches' - List of branches for an Amplify App. 
listBranchesResponse
    :: Int -- ^ 'lbrsResponseStatus'
    -> ListBranchesResponse
listBranchesResponse pResponseStatus_ =
  ListBranchesResponse'
    { _lbrsNextToken = Nothing
    , _lbrsResponseStatus = pResponseStatus_
    , _lbrsBranches = mempty
    }


-- | Pagination token. If non-null pagination token is returned in a result, then pass its value in another request to fetch more entries. 
lbrsNextToken :: Lens' ListBranchesResponse (Maybe Text)
lbrsNextToken = lens _lbrsNextToken (\ s a -> s{_lbrsNextToken = a})

-- | -- | The response status code.
lbrsResponseStatus :: Lens' ListBranchesResponse Int
lbrsResponseStatus = lens _lbrsResponseStatus (\ s a -> s{_lbrsResponseStatus = a})

-- | List of branches for an Amplify App. 
lbrsBranches :: Lens' ListBranchesResponse [Branch]
lbrsBranches = lens _lbrsBranches (\ s a -> s{_lbrsBranches = a}) . _Coerce

instance NFData ListBranchesResponse where
