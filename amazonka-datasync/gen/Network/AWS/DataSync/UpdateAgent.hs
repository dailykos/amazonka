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
-- Module      : Network.AWS.DataSync.UpdateAgent
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name of an agent.
--
--
module Network.AWS.DataSync.UpdateAgent
    (
    -- * Creating a Request
      updateAgent
    , UpdateAgent
    -- * Request Lenses
    , uaName
    , uaAgentARN

    -- * Destructuring the Response
    , updateAgentResponse
    , UpdateAgentResponse
    -- * Response Lenses
    , uarsResponseStatus
    ) where

import Network.AWS.DataSync.Types
import Network.AWS.DataSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | UpdateAgentRequest
--
--
--
-- /See:/ 'updateAgent' smart constructor.
data UpdateAgent = UpdateAgent'
  { _uaName :: !(Maybe Text)
  , _uaAgentARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAgent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaName' - The name that you want to use to configure the agent.
--
-- * 'uaAgentARN' - The Amazon Resource Name (ARN) of the agent to update.
updateAgent
    :: Text -- ^ 'uaAgentARN'
    -> UpdateAgent
updateAgent pAgentARN_ =
  UpdateAgent' {_uaName = Nothing, _uaAgentARN = pAgentARN_}


-- | The name that you want to use to configure the agent.
uaName :: Lens' UpdateAgent (Maybe Text)
uaName = lens _uaName (\ s a -> s{_uaName = a})

-- | The Amazon Resource Name (ARN) of the agent to update.
uaAgentARN :: Lens' UpdateAgent Text
uaAgentARN = lens _uaAgentARN (\ s a -> s{_uaAgentARN = a})

instance AWSRequest UpdateAgent where
        type Rs UpdateAgent = UpdateAgentResponse
        request = postJSON dataSync
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateAgentResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateAgent where

instance NFData UpdateAgent where

instance ToHeaders UpdateAgent where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("FmrsService.UpdateAgent" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateAgent where
        toJSON UpdateAgent'{..}
          = object
              (catMaybes
                 [("Name" .=) <$> _uaName,
                  Just ("AgentArn" .= _uaAgentARN)])

instance ToPath UpdateAgent where
        toPath = const "/"

instance ToQuery UpdateAgent where
        toQuery = const mempty

-- | /See:/ 'updateAgentResponse' smart constructor.
newtype UpdateAgentResponse = UpdateAgentResponse'
  { _uarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAgentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uarsResponseStatus' - -- | The response status code.
updateAgentResponse
    :: Int -- ^ 'uarsResponseStatus'
    -> UpdateAgentResponse
updateAgentResponse pResponseStatus_ =
  UpdateAgentResponse' {_uarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uarsResponseStatus :: Lens' UpdateAgentResponse Int
uarsResponseStatus = lens _uarsResponseStatus (\ s a -> s{_uarsResponseStatus = a})

instance NFData UpdateAgentResponse where
