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
-- Module      : Network.AWS.DataSync.CreateAgent
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates an AWS DataSync agent that you have deployed on your host. The activation process associates your agent with your account. In the activation process, you specify information such as the AWS Region that you want to activate the agent in. You activate the agent in the AWS Region where your target locations (in Amazon S3 or Amazon EFS) reside. Your tasks are created in this AWS Region. 
--
--
-- You can use an agent for more than one location. If a task uses multiple agents, all of them need to have status AVAILABLE for the task to run. If you use multiple agents for a source location, the status of all the agents must be AVAILABLE for the task to run. For more information, see <https://docs.aws.amazon.com/sync-service/latest/userguide/working-with-sync-agents.html#activating-sync-agent Activating a Sync Agent> in the /AWS DataSync User Guide./ 
--
-- Agents are automatically updated by AWS on a regular basis, using a mechanism that ensures minimal interruption to your tasks.
--
--
--
module Network.AWS.DataSync.CreateAgent
    (
    -- * Creating a Request
      createAgent
    , CreateAgent
    -- * Request Lenses
    , caAgentName
    , caTags
    , caActivationKey

    -- * Destructuring the Response
    , createAgentResponse
    , CreateAgentResponse
    -- * Response Lenses
    , carsAgentARN
    , carsResponseStatus
    ) where

import Network.AWS.DataSync.Types
import Network.AWS.DataSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | CreateAgentRequest
--
--
--
-- /See:/ 'createAgent' smart constructor.
data CreateAgent = CreateAgent'
  { _caAgentName :: !(Maybe Text)
  , _caTags :: !(Maybe [TagListEntry])
  , _caActivationKey :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAgent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caAgentName' - The name you configured for your agent. This value is a text reference that is used to identify the agent in the console.
--
-- * 'caTags' - The key-value pair that represents the tag you want to associate with the agent. The value can be an empty string. This value helps you manage, filter, and search for your agents.
--
-- * 'caActivationKey' - Your agent activation key. You can get the activation key either by sending an HTTP GET request with redirects that enable you to get the agent IP address (port 80). Alternatively, you can get it from the AWS DataSync console.  The redirect URL returned in the response provides you the activation key for your agent in the query string parameter @activationKey@ . It might also include other activation-related parameters; however, these are merely defaults. The arguments you pass to this API call determine the actual configuration of your agent. For more information, see <https://docs.aws.amazon.com/sync-service/latest/userguide/working-with-sync-agents.html#activating-sync-agent Activating a Sync Agent> in the /AWS DataSync User Guide./ 
createAgent
    :: Text -- ^ 'caActivationKey'
    -> CreateAgent
createAgent pActivationKey_ =
  CreateAgent'
    { _caAgentName = Nothing
    , _caTags = Nothing
    , _caActivationKey = pActivationKey_
    }


-- | The name you configured for your agent. This value is a text reference that is used to identify the agent in the console.
caAgentName :: Lens' CreateAgent (Maybe Text)
caAgentName = lens _caAgentName (\ s a -> s{_caAgentName = a})

-- | The key-value pair that represents the tag you want to associate with the agent. The value can be an empty string. This value helps you manage, filter, and search for your agents.
caTags :: Lens' CreateAgent [TagListEntry]
caTags = lens _caTags (\ s a -> s{_caTags = a}) . _Default . _Coerce

-- | Your agent activation key. You can get the activation key either by sending an HTTP GET request with redirects that enable you to get the agent IP address (port 80). Alternatively, you can get it from the AWS DataSync console.  The redirect URL returned in the response provides you the activation key for your agent in the query string parameter @activationKey@ . It might also include other activation-related parameters; however, these are merely defaults. The arguments you pass to this API call determine the actual configuration of your agent. For more information, see <https://docs.aws.amazon.com/sync-service/latest/userguide/working-with-sync-agents.html#activating-sync-agent Activating a Sync Agent> in the /AWS DataSync User Guide./ 
caActivationKey :: Lens' CreateAgent Text
caActivationKey = lens _caActivationKey (\ s a -> s{_caActivationKey = a})

instance AWSRequest CreateAgent where
        type Rs CreateAgent = CreateAgentResponse
        request = postJSON dataSync
        response
          = receiveJSON
              (\ s h x ->
                 CreateAgentResponse' <$>
                   (x .?> "AgentArn") <*> (pure (fromEnum s)))

instance Hashable CreateAgent where

instance NFData CreateAgent where

instance ToHeaders CreateAgent where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("FmrsService.CreateAgent" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateAgent where
        toJSON CreateAgent'{..}
          = object
              (catMaybes
                 [("AgentName" .=) <$> _caAgentName,
                  ("Tags" .=) <$> _caTags,
                  Just ("ActivationKey" .= _caActivationKey)])

instance ToPath CreateAgent where
        toPath = const "/"

instance ToQuery CreateAgent where
        toQuery = const mempty

-- | CreateAgentResponse
--
--
--
-- /See:/ 'createAgentResponse' smart constructor.
data CreateAgentResponse = CreateAgentResponse'
  { _carsAgentARN :: !(Maybe Text)
  , _carsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAgentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carsAgentARN' - The Amazon Resource Name (ARN) of the agent. Use the @ListAgents@ operation to return a list of agents for your account and AWS Region.
--
-- * 'carsResponseStatus' - -- | The response status code.
createAgentResponse
    :: Int -- ^ 'carsResponseStatus'
    -> CreateAgentResponse
createAgentResponse pResponseStatus_ =
  CreateAgentResponse'
    {_carsAgentARN = Nothing, _carsResponseStatus = pResponseStatus_}


-- | The Amazon Resource Name (ARN) of the agent. Use the @ListAgents@ operation to return a list of agents for your account and AWS Region.
carsAgentARN :: Lens' CreateAgentResponse (Maybe Text)
carsAgentARN = lens _carsAgentARN (\ s a -> s{_carsAgentARN = a})

-- | -- | The response status code.
carsResponseStatus :: Lens' CreateAgentResponse Int
carsResponseStatus = lens _carsResponseStatus (\ s a -> s{_carsResponseStatus = a})

instance NFData CreateAgentResponse where
