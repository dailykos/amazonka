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
-- Module      : Network.AWS.APIGatewayV2.CreateAPIMapping
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an API mapping.
--
--
module Network.AWS.APIGatewayV2.CreateAPIMapping
    (
    -- * Creating a Request
      createAPIMapping
    , CreateAPIMapping
    -- * Request Lenses
    , camAPIMappingKey
    , camDomainName
    , camStage
    , camAPIId

    -- * Destructuring the Response
    , createAPIMappingResponse
    , CreateAPIMappingResponse
    -- * Response Lenses
    , camrsStage
    , camrsAPIId
    , camrsAPIMappingKey
    , camrsAPIMappingId
    , camrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createAPIMapping' smart constructor.
data CreateAPIMapping = CreateAPIMapping'
  { _camAPIMappingKey :: !(Maybe Text)
  , _camDomainName :: !Text
  , _camStage :: !Text
  , _camAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAPIMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'camAPIMappingKey' - 
--
-- * 'camDomainName' - The domain name.
--
-- * 'camStage' - The API stage.
--
-- * 'camAPIId' - The API identifier.
createAPIMapping
    :: Text -- ^ 'camDomainName'
    -> Text -- ^ 'camStage'
    -> Text -- ^ 'camAPIId'
    -> CreateAPIMapping
createAPIMapping pDomainName_ pStage_ pAPIId_ =
  CreateAPIMapping'
    { _camAPIMappingKey = Nothing
    , _camDomainName = pDomainName_
    , _camStage = pStage_
    , _camAPIId = pAPIId_
    }


-- | 
camAPIMappingKey :: Lens' CreateAPIMapping (Maybe Text)
camAPIMappingKey = lens _camAPIMappingKey (\ s a -> s{_camAPIMappingKey = a})

-- | The domain name.
camDomainName :: Lens' CreateAPIMapping Text
camDomainName = lens _camDomainName (\ s a -> s{_camDomainName = a})

-- | The API stage.
camStage :: Lens' CreateAPIMapping Text
camStage = lens _camStage (\ s a -> s{_camStage = a})

-- | The API identifier.
camAPIId :: Lens' CreateAPIMapping Text
camAPIId = lens _camAPIId (\ s a -> s{_camAPIId = a})

instance AWSRequest CreateAPIMapping where
        type Rs CreateAPIMapping = CreateAPIMappingResponse
        request = postJSON apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 CreateAPIMappingResponse' <$>
                   (x .?> "stage") <*> (x .?> "apiId") <*>
                     (x .?> "apiMappingKey")
                     <*> (x .?> "apiMappingId")
                     <*> (pure (fromEnum s)))

instance Hashable CreateAPIMapping where

instance NFData CreateAPIMapping where

instance ToHeaders CreateAPIMapping where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateAPIMapping where
        toJSON CreateAPIMapping'{..}
          = object
              (catMaybes
                 [("apiMappingKey" .=) <$> _camAPIMappingKey,
                  Just ("stage" .= _camStage),
                  Just ("apiId" .= _camAPIId)])

instance ToPath CreateAPIMapping where
        toPath CreateAPIMapping'{..}
          = mconcat
              ["/v2/domainnames/", toBS _camDomainName,
               "/apimappings"]

instance ToQuery CreateAPIMapping where
        toQuery = const mempty

-- | /See:/ 'createAPIMappingResponse' smart constructor.
data CreateAPIMappingResponse = CreateAPIMappingResponse'
  { _camrsStage :: !(Maybe Text)
  , _camrsAPIId :: !(Maybe Text)
  , _camrsAPIMappingKey :: !(Maybe Text)
  , _camrsAPIMappingId :: !(Maybe Text)
  , _camrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAPIMappingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'camrsStage' - The API stage.
--
-- * 'camrsAPIId' - The API identifier.
--
-- * 'camrsAPIMappingKey' - The API mapping key.
--
-- * 'camrsAPIMappingId' - The API mapping identifier.
--
-- * 'camrsResponseStatus' - -- | The response status code.
createAPIMappingResponse
    :: Int -- ^ 'camrsResponseStatus'
    -> CreateAPIMappingResponse
createAPIMappingResponse pResponseStatus_ =
  CreateAPIMappingResponse'
    { _camrsStage = Nothing
    , _camrsAPIId = Nothing
    , _camrsAPIMappingKey = Nothing
    , _camrsAPIMappingId = Nothing
    , _camrsResponseStatus = pResponseStatus_
    }


-- | The API stage.
camrsStage :: Lens' CreateAPIMappingResponse (Maybe Text)
camrsStage = lens _camrsStage (\ s a -> s{_camrsStage = a})

-- | The API identifier.
camrsAPIId :: Lens' CreateAPIMappingResponse (Maybe Text)
camrsAPIId = lens _camrsAPIId (\ s a -> s{_camrsAPIId = a})

-- | The API mapping key.
camrsAPIMappingKey :: Lens' CreateAPIMappingResponse (Maybe Text)
camrsAPIMappingKey = lens _camrsAPIMappingKey (\ s a -> s{_camrsAPIMappingKey = a})

-- | The API mapping identifier.
camrsAPIMappingId :: Lens' CreateAPIMappingResponse (Maybe Text)
camrsAPIMappingId = lens _camrsAPIMappingId (\ s a -> s{_camrsAPIMappingId = a})

-- | -- | The response status code.
camrsResponseStatus :: Lens' CreateAPIMappingResponse Int
camrsResponseStatus = lens _camrsResponseStatus (\ s a -> s{_camrsResponseStatus = a})

instance NFData CreateAPIMappingResponse where
