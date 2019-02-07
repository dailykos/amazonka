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
-- Module      : Network.AWS.DataSync.ListTaskExecutions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of executed tasks.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DataSync.ListTaskExecutions
    (
    -- * Creating a Request
      listTaskExecutions
    , ListTaskExecutions
    -- * Request Lenses
    , lteTaskARN
    , lteNextToken
    , lteMaxResults

    -- * Destructuring the Response
    , listTaskExecutionsResponse
    , ListTaskExecutionsResponse
    -- * Response Lenses
    , ltersNextToken
    , ltersTaskExecutions
    , ltersResponseStatus
    ) where

import Network.AWS.DataSync.Types
import Network.AWS.DataSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | ListTaskExecutions
--
--
--
-- /See:/ 'listTaskExecutions' smart constructor.
data ListTaskExecutions = ListTaskExecutions'
  { _lteTaskARN :: !(Maybe Text)
  , _lteNextToken :: !(Maybe Text)
  , _lteMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTaskExecutions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lteTaskARN' - The Amazon Resource Name (ARN) of the task whose tasks you want to list.
--
-- * 'lteNextToken' - An opaque string that indicates the position at which to begin the next list of the executed tasks.
--
-- * 'lteMaxResults' - The maximum number of executed tasks to list.
listTaskExecutions
    :: ListTaskExecutions
listTaskExecutions =
  ListTaskExecutions'
    {_lteTaskARN = Nothing, _lteNextToken = Nothing, _lteMaxResults = Nothing}


-- | The Amazon Resource Name (ARN) of the task whose tasks you want to list.
lteTaskARN :: Lens' ListTaskExecutions (Maybe Text)
lteTaskARN = lens _lteTaskARN (\ s a -> s{_lteTaskARN = a})

-- | An opaque string that indicates the position at which to begin the next list of the executed tasks.
lteNextToken :: Lens' ListTaskExecutions (Maybe Text)
lteNextToken = lens _lteNextToken (\ s a -> s{_lteNextToken = a})

-- | The maximum number of executed tasks to list.
lteMaxResults :: Lens' ListTaskExecutions (Maybe Natural)
lteMaxResults = lens _lteMaxResults (\ s a -> s{_lteMaxResults = a}) . mapping _Nat

instance AWSPager ListTaskExecutions where
        page rq rs
          | stop (rs ^. ltersNextToken) = Nothing
          | stop (rs ^. ltersTaskExecutions) = Nothing
          | otherwise =
            Just $ rq & lteNextToken .~ rs ^. ltersNextToken

instance AWSRequest ListTaskExecutions where
        type Rs ListTaskExecutions =
             ListTaskExecutionsResponse
        request = postJSON dataSync
        response
          = receiveJSON
              (\ s h x ->
                 ListTaskExecutionsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "TaskExecutions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListTaskExecutions where

instance NFData ListTaskExecutions where

instance ToHeaders ListTaskExecutions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("FmrsService.ListTaskExecutions" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTaskExecutions where
        toJSON ListTaskExecutions'{..}
          = object
              (catMaybes
                 [("TaskArn" .=) <$> _lteTaskARN,
                  ("NextToken" .=) <$> _lteNextToken,
                  ("MaxResults" .=) <$> _lteMaxResults])

instance ToPath ListTaskExecutions where
        toPath = const "/"

instance ToQuery ListTaskExecutions where
        toQuery = const mempty

-- | ListTaskExecutionsResponse
--
--
--
-- /See:/ 'listTaskExecutionsResponse' smart constructor.
data ListTaskExecutionsResponse = ListTaskExecutionsResponse'
  { _ltersNextToken :: !(Maybe Text)
  , _ltersTaskExecutions :: !(Maybe [TaskExecutionListEntry])
  , _ltersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTaskExecutionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltersNextToken' - An opaque string that indicates the position at which to begin returning the next list of executed tasks.
--
-- * 'ltersTaskExecutions' - A list of executed tasks.
--
-- * 'ltersResponseStatus' - -- | The response status code.
listTaskExecutionsResponse
    :: Int -- ^ 'ltersResponseStatus'
    -> ListTaskExecutionsResponse
listTaskExecutionsResponse pResponseStatus_ =
  ListTaskExecutionsResponse'
    { _ltersNextToken = Nothing
    , _ltersTaskExecutions = Nothing
    , _ltersResponseStatus = pResponseStatus_
    }


-- | An opaque string that indicates the position at which to begin returning the next list of executed tasks.
ltersNextToken :: Lens' ListTaskExecutionsResponse (Maybe Text)
ltersNextToken = lens _ltersNextToken (\ s a -> s{_ltersNextToken = a})

-- | A list of executed tasks.
ltersTaskExecutions :: Lens' ListTaskExecutionsResponse [TaskExecutionListEntry]
ltersTaskExecutions = lens _ltersTaskExecutions (\ s a -> s{_ltersTaskExecutions = a}) . _Default . _Coerce

-- | -- | The response status code.
ltersResponseStatus :: Lens' ListTaskExecutionsResponse Int
ltersResponseStatus = lens _ltersResponseStatus (\ s a -> s{_ltersResponseStatus = a})

instance NFData ListTaskExecutionsResponse where
