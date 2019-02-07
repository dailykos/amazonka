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
-- Module      : Network.AWS.DataSync.DeleteAgent
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an agent. To specify which agent to delete, use the Amazon Resource Name (ARN) of the agent in your request. The operation disassociates the agent from your AWS account. However, it doesn't delete the agent virtual machine (VM) from your on-premises environment.
--
--
module Network.AWS.DataSync.DeleteAgent
    (
    -- * Creating a Request
      deleteAgent
    , DeleteAgent
    -- * Request Lenses
    , dAgentARN

    -- * Destructuring the Response
    , deleteAgentResponse
    , DeleteAgentResponse
    -- * Response Lenses
    , delrsResponseStatus
    ) where

import Network.AWS.DataSync.Types
import Network.AWS.DataSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | DeleteAgentRequest
--
--
--
-- /See:/ 'deleteAgent' smart constructor.
newtype DeleteAgent = DeleteAgent'
  { _dAgentARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAgent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAgentARN' - The Amazon Resource Name (ARN) of the agent to delete. Use the @ListAgents@ operation to return a list of agents for your account and AWS Region.
deleteAgent
    :: Text -- ^ 'dAgentARN'
    -> DeleteAgent
deleteAgent pAgentARN_ = DeleteAgent' {_dAgentARN = pAgentARN_}


-- | The Amazon Resource Name (ARN) of the agent to delete. Use the @ListAgents@ operation to return a list of agents for your account and AWS Region.
dAgentARN :: Lens' DeleteAgent Text
dAgentARN = lens _dAgentARN (\ s a -> s{_dAgentARN = a})

instance AWSRequest DeleteAgent where
        type Rs DeleteAgent = DeleteAgentResponse
        request = postJSON dataSync
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteAgentResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteAgent where

instance NFData DeleteAgent where

instance ToHeaders DeleteAgent where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("FmrsService.DeleteAgent" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteAgent where
        toJSON DeleteAgent'{..}
          = object
              (catMaybes [Just ("AgentArn" .= _dAgentARN)])

instance ToPath DeleteAgent where
        toPath = const "/"

instance ToQuery DeleteAgent where
        toQuery = const mempty

-- | /See:/ 'deleteAgentResponse' smart constructor.
newtype DeleteAgentResponse = DeleteAgentResponse'
  { _delrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAgentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteAgentResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeleteAgentResponse
deleteAgentResponse pResponseStatus_ =
  DeleteAgentResponse' {_delrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteAgentResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a})

instance NFData DeleteAgentResponse where
