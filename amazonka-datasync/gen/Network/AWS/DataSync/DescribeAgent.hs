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
-- Module      : Network.AWS.DataSync.DescribeAgent
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata such as the name, the network interfaces, and the status (that is, whether the agent is running or not) for an agent. To specify which agent to describe, use the Amazon Resource Name (ARN) of the agent in your request. 
--
--
module Network.AWS.DataSync.DescribeAgent
    (
    -- * Creating a Request
      describeAgent
    , DescribeAgent
    -- * Request Lenses
    , daAgentARN

    -- * Destructuring the Response
    , describeAgentResponse
    , DescribeAgentResponse
    -- * Response Lenses
    , darsCreationTime
    , darsStatus
    , darsLastConnectionTime
    , darsAgentARN
    , darsName
    , darsResponseStatus
    ) where

import Network.AWS.DataSync.Types
import Network.AWS.DataSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | DescribeAgent
--
--
--
-- /See:/ 'describeAgent' smart constructor.
newtype DescribeAgent = DescribeAgent'
  { _daAgentARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAgent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daAgentARN' - The Amazon Resource Name (ARN) of the agent to describe.
describeAgent
    :: Text -- ^ 'daAgentARN'
    -> DescribeAgent
describeAgent pAgentARN_ = DescribeAgent' {_daAgentARN = pAgentARN_}


-- | The Amazon Resource Name (ARN) of the agent to describe.
daAgentARN :: Lens' DescribeAgent Text
daAgentARN = lens _daAgentARN (\ s a -> s{_daAgentARN = a})

instance AWSRequest DescribeAgent where
        type Rs DescribeAgent = DescribeAgentResponse
        request = postJSON dataSync
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAgentResponse' <$>
                   (x .?> "CreationTime") <*> (x .?> "Status") <*>
                     (x .?> "LastConnectionTime")
                     <*> (x .?> "AgentArn")
                     <*> (x .?> "Name")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeAgent where

instance NFData DescribeAgent where

instance ToHeaders DescribeAgent where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("FmrsService.DescribeAgent" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeAgent where
        toJSON DescribeAgent'{..}
          = object
              (catMaybes [Just ("AgentArn" .= _daAgentARN)])

instance ToPath DescribeAgent where
        toPath = const "/"

instance ToQuery DescribeAgent where
        toQuery = const mempty

-- | DescribeAgentResponse
--
--
--
-- /See:/ 'describeAgentResponse' smart constructor.
data DescribeAgentResponse = DescribeAgentResponse'
  { _darsCreationTime :: !(Maybe POSIX)
  , _darsStatus :: !(Maybe AgentStatus)
  , _darsLastConnectionTime :: !(Maybe POSIX)
  , _darsAgentARN :: !(Maybe Text)
  , _darsName :: !(Maybe Text)
  , _darsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAgentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsCreationTime' - The time that the agent was activated (that is, created in your account).
--
-- * 'darsStatus' - The status of the agent. If the status is ONLINE, then the agent is configured properly and is available to use. The Running status is the normal running status for an agent. If the status is OFFLINE, the agent's VM is turned off or the agent is in an unhealthy state. When the issue that caused the unhealthy state is resolved, the agent returns to ONLINE status.
--
-- * 'darsLastConnectionTime' - The time that the agent was last connected.
--
-- * 'darsAgentARN' - The Amazon Resource Name (ARN) of the agent.
--
-- * 'darsName' - The name of the agent.
--
-- * 'darsResponseStatus' - -- | The response status code.
describeAgentResponse
    :: Int -- ^ 'darsResponseStatus'
    -> DescribeAgentResponse
describeAgentResponse pResponseStatus_ =
  DescribeAgentResponse'
    { _darsCreationTime = Nothing
    , _darsStatus = Nothing
    , _darsLastConnectionTime = Nothing
    , _darsAgentARN = Nothing
    , _darsName = Nothing
    , _darsResponseStatus = pResponseStatus_
    }


-- | The time that the agent was activated (that is, created in your account).
darsCreationTime :: Lens' DescribeAgentResponse (Maybe UTCTime)
darsCreationTime = lens _darsCreationTime (\ s a -> s{_darsCreationTime = a}) . mapping _Time

-- | The status of the agent. If the status is ONLINE, then the agent is configured properly and is available to use. The Running status is the normal running status for an agent. If the status is OFFLINE, the agent's VM is turned off or the agent is in an unhealthy state. When the issue that caused the unhealthy state is resolved, the agent returns to ONLINE status.
darsStatus :: Lens' DescribeAgentResponse (Maybe AgentStatus)
darsStatus = lens _darsStatus (\ s a -> s{_darsStatus = a})

-- | The time that the agent was last connected.
darsLastConnectionTime :: Lens' DescribeAgentResponse (Maybe UTCTime)
darsLastConnectionTime = lens _darsLastConnectionTime (\ s a -> s{_darsLastConnectionTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the agent.
darsAgentARN :: Lens' DescribeAgentResponse (Maybe Text)
darsAgentARN = lens _darsAgentARN (\ s a -> s{_darsAgentARN = a})

-- | The name of the agent.
darsName :: Lens' DescribeAgentResponse (Maybe Text)
darsName = lens _darsName (\ s a -> s{_darsName = a})

-- | -- | The response status code.
darsResponseStatus :: Lens' DescribeAgentResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a})

instance NFData DescribeAgentResponse where
