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
-- Module      : Network.AWS.Amplify.ListJobs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List Jobs for a branch, part of an Amplify App. 
--
--
--
-- This operation returns paginated results.
module Network.AWS.Amplify.ListJobs
    (
    -- * Creating a Request
      listJobs
    , ListJobs
    -- * Request Lenses
    , ljNextToken
    , ljMaxResults
    , ljAppId
    , ljBranchName

    -- * Destructuring the Response
    , listJobsResponse
    , ListJobsResponse
    -- * Response Lenses
    , ljrsNextToken
    , ljrsResponseStatus
    , ljrsJobSummaries
    ) where

import Network.AWS.Amplify.Types
import Network.AWS.Amplify.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure for list job request. 
--
--
--
-- /See:/ 'listJobs' smart constructor.
data ListJobs = ListJobs'
  { _ljNextToken :: !(Maybe Text)
  , _ljMaxResults :: !(Maybe Nat)
  , _ljAppId :: !Text
  , _ljBranchName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljNextToken' - Pagination token. Set to null to start listing steps from start. If a non-null pagination token is returned in a result, then pass its value in here to list more steps. 
--
-- * 'ljMaxResults' - Maximum number of records to list in a single response. 
--
-- * 'ljAppId' - Unique Id for an Amplify App. 
--
-- * 'ljBranchName' - Name for a branch. 
listJobs
    :: Text -- ^ 'ljAppId'
    -> Text -- ^ 'ljBranchName'
    -> ListJobs
listJobs pAppId_ pBranchName_ =
  ListJobs'
    { _ljNextToken = Nothing
    , _ljMaxResults = Nothing
    , _ljAppId = pAppId_
    , _ljBranchName = pBranchName_
    }


-- | Pagination token. Set to null to start listing steps from start. If a non-null pagination token is returned in a result, then pass its value in here to list more steps. 
ljNextToken :: Lens' ListJobs (Maybe Text)
ljNextToken = lens _ljNextToken (\ s a -> s{_ljNextToken = a})

-- | Maximum number of records to list in a single response. 
ljMaxResults :: Lens' ListJobs (Maybe Natural)
ljMaxResults = lens _ljMaxResults (\ s a -> s{_ljMaxResults = a}) . mapping _Nat

-- | Unique Id for an Amplify App. 
ljAppId :: Lens' ListJobs Text
ljAppId = lens _ljAppId (\ s a -> s{_ljAppId = a})

-- | Name for a branch. 
ljBranchName :: Lens' ListJobs Text
ljBranchName = lens _ljBranchName (\ s a -> s{_ljBranchName = a})

instance AWSPager ListJobs where
        page rq rs
          | stop (rs ^. ljrsNextToken) = Nothing
          | stop (rs ^. ljrsJobSummaries) = Nothing
          | otherwise =
            Just $ rq & ljNextToken .~ rs ^. ljrsNextToken

instance AWSRequest ListJobs where
        type Rs ListJobs = ListJobsResponse
        request = get amplify
        response
          = receiveJSON
              (\ s h x ->
                 ListJobsResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "jobSummaries" .!@ mempty))

instance Hashable ListJobs where

instance NFData ListJobs where

instance ToHeaders ListJobs where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListJobs where
        toPath ListJobs'{..}
          = mconcat
              ["/apps/", toBS _ljAppId, "/branches/",
               toBS _ljBranchName, "/jobs"]

instance ToQuery ListJobs where
        toQuery ListJobs'{..}
          = mconcat
              ["nextToken" =: _ljNextToken,
               "maxResults" =: _ljMaxResults]

-- | Maximum number of records to list in a single response. 
--
--
--
-- /See:/ 'listJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { _ljrsNextToken :: !(Maybe Text)
  , _ljrsResponseStatus :: !Int
  , _ljrsJobSummaries :: ![JobSummary]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljrsNextToken' - Pagination token. If non-null pagination token is returned in a result, then pass its value in another request to fetch more entries. 
--
-- * 'ljrsResponseStatus' - -- | The response status code.
--
-- * 'ljrsJobSummaries' - Result structure for list job result request. 
listJobsResponse
    :: Int -- ^ 'ljrsResponseStatus'
    -> ListJobsResponse
listJobsResponse pResponseStatus_ =
  ListJobsResponse'
    { _ljrsNextToken = Nothing
    , _ljrsResponseStatus = pResponseStatus_
    , _ljrsJobSummaries = mempty
    }


-- | Pagination token. If non-null pagination token is returned in a result, then pass its value in another request to fetch more entries. 
ljrsNextToken :: Lens' ListJobsResponse (Maybe Text)
ljrsNextToken = lens _ljrsNextToken (\ s a -> s{_ljrsNextToken = a})

-- | -- | The response status code.
ljrsResponseStatus :: Lens' ListJobsResponse Int
ljrsResponseStatus = lens _ljrsResponseStatus (\ s a -> s{_ljrsResponseStatus = a})

-- | Result structure for list job result request. 
ljrsJobSummaries :: Lens' ListJobsResponse [JobSummary]
ljrsJobSummaries = lens _ljrsJobSummaries (\ s a -> s{_ljrsJobSummaries = a}) . _Coerce

instance NFData ListJobsResponse where
