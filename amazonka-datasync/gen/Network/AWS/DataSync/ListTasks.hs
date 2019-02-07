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
-- Module      : Network.AWS.DataSync.ListTasks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all the tasks.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DataSync.ListTasks
    (
    -- * Creating a Request
      listTasks
    , ListTasks
    -- * Request Lenses
    , ltNextToken
    , ltMaxResults

    -- * Destructuring the Response
    , listTasksResponse
    , ListTasksResponse
    -- * Response Lenses
    , ltrsTasks
    , ltrsNextToken
    , ltrsResponseStatus
    ) where

import Network.AWS.DataSync.Types
import Network.AWS.DataSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | ListTasksRequest
--
--
--
-- /See:/ 'listTasks' smart constructor.
data ListTasks = ListTasks'
  { _ltNextToken :: !(Maybe Text)
  , _ltMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltNextToken' - An opaque string that indicates the position at which to begin the next list of tasks.
--
-- * 'ltMaxResults' - The maximum number of tasks to return.
listTasks
    :: ListTasks
listTasks = ListTasks' {_ltNextToken = Nothing, _ltMaxResults = Nothing}


-- | An opaque string that indicates the position at which to begin the next list of tasks.
ltNextToken :: Lens' ListTasks (Maybe Text)
ltNextToken = lens _ltNextToken (\ s a -> s{_ltNextToken = a})

-- | The maximum number of tasks to return.
ltMaxResults :: Lens' ListTasks (Maybe Natural)
ltMaxResults = lens _ltMaxResults (\ s a -> s{_ltMaxResults = a}) . mapping _Nat

instance AWSPager ListTasks where
        page rq rs
          | stop (rs ^. ltrsNextToken) = Nothing
          | stop (rs ^. ltrsTasks) = Nothing
          | otherwise =
            Just $ rq & ltNextToken .~ rs ^. ltrsNextToken

instance AWSRequest ListTasks where
        type Rs ListTasks = ListTasksResponse
        request = postJSON dataSync
        response
          = receiveJSON
              (\ s h x ->
                 ListTasksResponse' <$>
                   (x .?> "Tasks" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListTasks where

instance NFData ListTasks where

instance ToHeaders ListTasks where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("FmrsService.ListTasks" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTasks where
        toJSON ListTasks'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _ltNextToken,
                  ("MaxResults" .=) <$> _ltMaxResults])

instance ToPath ListTasks where
        toPath = const "/"

instance ToQuery ListTasks where
        toQuery = const mempty

-- | ListTasksResponse
--
--
--
-- /See:/ 'listTasksResponse' smart constructor.
data ListTasksResponse = ListTasksResponse'
  { _ltrsTasks :: !(Maybe [TaskListEntry])
  , _ltrsNextToken :: !(Maybe Text)
  , _ltrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTasksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrsTasks' - A list of all the tasks that are returned.
--
-- * 'ltrsNextToken' - An opaque string that indicates the position at which to begin returning the next list of tasks.
--
-- * 'ltrsResponseStatus' - -- | The response status code.
listTasksResponse
    :: Int -- ^ 'ltrsResponseStatus'
    -> ListTasksResponse
listTasksResponse pResponseStatus_ =
  ListTasksResponse'
    { _ltrsTasks = Nothing
    , _ltrsNextToken = Nothing
    , _ltrsResponseStatus = pResponseStatus_
    }


-- | A list of all the tasks that are returned.
ltrsTasks :: Lens' ListTasksResponse [TaskListEntry]
ltrsTasks = lens _ltrsTasks (\ s a -> s{_ltrsTasks = a}) . _Default . _Coerce

-- | An opaque string that indicates the position at which to begin returning the next list of tasks.
ltrsNextToken :: Lens' ListTasksResponse (Maybe Text)
ltrsNextToken = lens _ltrsNextToken (\ s a -> s{_ltrsNextToken = a})

-- | -- | The response status code.
ltrsResponseStatus :: Lens' ListTasksResponse Int
ltrsResponseStatus = lens _ltrsResponseStatus (\ s a -> s{_ltrsResponseStatus = a})

instance NFData ListTasksResponse where
