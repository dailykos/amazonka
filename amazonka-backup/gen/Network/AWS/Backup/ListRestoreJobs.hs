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
-- Module      : Network.AWS.Backup.ListRestoreJobs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of jobs that AWS Backup initiated to restore a saved resource, including metadata about the recovery process.
--
--
module Network.AWS.Backup.ListRestoreJobs
    (
    -- * Creating a Request
      listRestoreJobs
    , ListRestoreJobs
    -- * Request Lenses
    , lrjNextToken
    , lrjMaxResults

    -- * Destructuring the Response
    , listRestoreJobsResponse
    , ListRestoreJobsResponse
    -- * Response Lenses
    , lrjrsNextToken
    , lrjrsRestoreJobs
    , lrjrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listRestoreJobs' smart constructor.
data ListRestoreJobs = ListRestoreJobs'
  { _lrjNextToken :: !(Maybe Text)
  , _lrjMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRestoreJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrjNextToken' - The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
--
-- * 'lrjMaxResults' - The maximum number of items to be returned.
listRestoreJobs
    :: ListRestoreJobs
listRestoreJobs =
  ListRestoreJobs' {_lrjNextToken = Nothing, _lrjMaxResults = Nothing}


-- | The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
lrjNextToken :: Lens' ListRestoreJobs (Maybe Text)
lrjNextToken = lens _lrjNextToken (\ s a -> s{_lrjNextToken = a})

-- | The maximum number of items to be returned.
lrjMaxResults :: Lens' ListRestoreJobs (Maybe Natural)
lrjMaxResults = lens _lrjMaxResults (\ s a -> s{_lrjMaxResults = a}) . mapping _Nat

instance AWSRequest ListRestoreJobs where
        type Rs ListRestoreJobs = ListRestoreJobsResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 ListRestoreJobsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "RestoreJobs" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListRestoreJobs where

instance NFData ListRestoreJobs where

instance ToHeaders ListRestoreJobs where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListRestoreJobs where
        toPath = const "/restore-jobs/"

instance ToQuery ListRestoreJobs where
        toQuery ListRestoreJobs'{..}
          = mconcat
              ["nextToken" =: _lrjNextToken,
               "maxResults" =: _lrjMaxResults]

-- | /See:/ 'listRestoreJobsResponse' smart constructor.
data ListRestoreJobsResponse = ListRestoreJobsResponse'
  { _lrjrsNextToken :: !(Maybe Text)
  , _lrjrsRestoreJobs :: !(Maybe [RestoreJobsListMember])
  , _lrjrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRestoreJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrjrsNextToken' - The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
--
-- * 'lrjrsRestoreJobs' - An array of objects that contain detailed information about jobs to restore saved resources.
--
-- * 'lrjrsResponseStatus' - -- | The response status code.
listRestoreJobsResponse
    :: Int -- ^ 'lrjrsResponseStatus'
    -> ListRestoreJobsResponse
listRestoreJobsResponse pResponseStatus_ =
  ListRestoreJobsResponse'
    { _lrjrsNextToken = Nothing
    , _lrjrsRestoreJobs = Nothing
    , _lrjrsResponseStatus = pResponseStatus_
    }


-- | The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
lrjrsNextToken :: Lens' ListRestoreJobsResponse (Maybe Text)
lrjrsNextToken = lens _lrjrsNextToken (\ s a -> s{_lrjrsNextToken = a})

-- | An array of objects that contain detailed information about jobs to restore saved resources.
lrjrsRestoreJobs :: Lens' ListRestoreJobsResponse [RestoreJobsListMember]
lrjrsRestoreJobs = lens _lrjrsRestoreJobs (\ s a -> s{_lrjrsRestoreJobs = a}) . _Default . _Coerce

-- | -- | The response status code.
lrjrsResponseStatus :: Lens' ListRestoreJobsResponse Int
lrjrsResponseStatus = lens _lrjrsResponseStatus (\ s a -> s{_lrjrsResponseStatus = a})

instance NFData ListRestoreJobsResponse where
