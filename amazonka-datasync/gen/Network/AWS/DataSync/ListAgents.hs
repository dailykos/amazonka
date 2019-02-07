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
-- Module      : Network.AWS.DataSync.ListAgents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of agents owned by an AWS account in the AWS Region specified in the request. The returned list is ordered by agent Amazon Resource Name (ARN).
--
--
-- By default, this operation returns a maximum of 100 agents. This operation supports pagination that enables you to optionally reduce the number of agents returned in a response.
--
-- If you have more agents than are returned in a response (that is, the response returns only a truncated list of your agents), the response contains a marker that you can specify in your next request to fetch the next page of agents.
--
--
-- This operation returns paginated results.
module Network.AWS.DataSync.ListAgents
    (
    -- * Creating a Request
      listAgents
    , ListAgents
    -- * Request Lenses
    , laNextToken
    , laMaxResults

    -- * Destructuring the Response
    , listAgentsResponse
    , ListAgentsResponse
    -- * Response Lenses
    , larsAgents
    , larsNextToken
    , larsResponseStatus
    ) where

import Network.AWS.DataSync.Types
import Network.AWS.DataSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | ListAgentsRequest
--
--
--
-- /See:/ 'listAgents' smart constructor.
data ListAgents = ListAgents'
  { _laNextToken :: !(Maybe Text)
  , _laMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAgents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laNextToken' - An opaque string that indicates the position at which to begin the next list of agents.
--
-- * 'laMaxResults' - The maximum number of agents to list.
listAgents
    :: ListAgents
listAgents = ListAgents' {_laNextToken = Nothing, _laMaxResults = Nothing}


-- | An opaque string that indicates the position at which to begin the next list of agents.
laNextToken :: Lens' ListAgents (Maybe Text)
laNextToken = lens _laNextToken (\ s a -> s{_laNextToken = a})

-- | The maximum number of agents to list.
laMaxResults :: Lens' ListAgents (Maybe Natural)
laMaxResults = lens _laMaxResults (\ s a -> s{_laMaxResults = a}) . mapping _Nat

instance AWSPager ListAgents where
        page rq rs
          | stop (rs ^. larsNextToken) = Nothing
          | stop (rs ^. larsAgents) = Nothing
          | otherwise =
            Just $ rq & laNextToken .~ rs ^. larsNextToken

instance AWSRequest ListAgents where
        type Rs ListAgents = ListAgentsResponse
        request = postJSON dataSync
        response
          = receiveJSON
              (\ s h x ->
                 ListAgentsResponse' <$>
                   (x .?> "Agents" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListAgents where

instance NFData ListAgents where

instance ToHeaders ListAgents where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("FmrsService.ListAgents" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListAgents where
        toJSON ListAgents'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _laNextToken,
                  ("MaxResults" .=) <$> _laMaxResults])

instance ToPath ListAgents where
        toPath = const "/"

instance ToQuery ListAgents where
        toQuery = const mempty

-- | ListAgentsResponse
--
--
--
-- /See:/ 'listAgentsResponse' smart constructor.
data ListAgentsResponse = ListAgentsResponse'
  { _larsAgents :: !(Maybe [AgentListEntry])
  , _larsNextToken :: !(Maybe Text)
  , _larsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAgentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larsAgents' - A list of agents in your account.
--
-- * 'larsNextToken' - An opaque string that indicates the position at which to begin returning the next list of agents.
--
-- * 'larsResponseStatus' - -- | The response status code.
listAgentsResponse
    :: Int -- ^ 'larsResponseStatus'
    -> ListAgentsResponse
listAgentsResponse pResponseStatus_ =
  ListAgentsResponse'
    { _larsAgents = Nothing
    , _larsNextToken = Nothing
    , _larsResponseStatus = pResponseStatus_
    }


-- | A list of agents in your account.
larsAgents :: Lens' ListAgentsResponse [AgentListEntry]
larsAgents = lens _larsAgents (\ s a -> s{_larsAgents = a}) . _Default . _Coerce

-- | An opaque string that indicates the position at which to begin returning the next list of agents.
larsNextToken :: Lens' ListAgentsResponse (Maybe Text)
larsNextToken = lens _larsNextToken (\ s a -> s{_larsNextToken = a})

-- | -- | The response status code.
larsResponseStatus :: Lens' ListAgentsResponse Int
larsResponseStatus = lens _larsResponseStatus (\ s a -> s{_larsResponseStatus = a})

instance NFData ListAgentsResponse where
