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
-- Module      : Network.AWS.APIGatewayV2.GetRouteResponse
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a RouteResponse.
--
--
module Network.AWS.APIGatewayV2.GetRouteResponse
    (
    -- * Creating a Request
      getRouteResponse
    , GetRouteResponse
    -- * Request Lenses
    , grRouteResponseId
    , grAPIId
    , grRouteId

    -- * Destructuring the Response
    , getRouteResponseResponse
    , GetRouteResponseResponse
    -- * Response Lenses
    , grrrrsModelSelectionExpression
    , grrrrsResponseModels
    , grrrrsRouteResponseId
    , grrrrsRouteResponseKey
    , grrrrsResponseParameters
    , grrrrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRouteResponse' smart constructor.
data GetRouteResponse = GetRouteResponse'
  { _grRouteResponseId :: !Text
  , _grAPIId :: !Text
  , _grRouteId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRouteResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grRouteResponseId' - The route response ID.
--
-- * 'grAPIId' - The API identifier.
--
-- * 'grRouteId' - The route ID.
getRouteResponse
    :: Text -- ^ 'grRouteResponseId'
    -> Text -- ^ 'grAPIId'
    -> Text -- ^ 'grRouteId'
    -> GetRouteResponse
getRouteResponse pRouteResponseId_ pAPIId_ pRouteId_ =
  GetRouteResponse'
    { _grRouteResponseId = pRouteResponseId_
    , _grAPIId = pAPIId_
    , _grRouteId = pRouteId_
    }


-- | The route response ID.
grRouteResponseId :: Lens' GetRouteResponse Text
grRouteResponseId = lens _grRouteResponseId (\ s a -> s{_grRouteResponseId = a})

-- | The API identifier.
grAPIId :: Lens' GetRouteResponse Text
grAPIId = lens _grAPIId (\ s a -> s{_grAPIId = a})

-- | The route ID.
grRouteId :: Lens' GetRouteResponse Text
grRouteId = lens _grRouteId (\ s a -> s{_grRouteId = a})

instance AWSRequest GetRouteResponse where
        type Rs GetRouteResponse = GetRouteResponseResponse
        request = get apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 GetRouteResponseResponse' <$>
                   (x .?> "modelSelectionExpression") <*>
                     (x .?> "responseModels" .!@ mempty)
                     <*> (x .?> "routeResponseId")
                     <*> (x .?> "routeResponseKey")
                     <*> (x .?> "responseParameters" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetRouteResponse where

instance NFData GetRouteResponse where

instance ToHeaders GetRouteResponse where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetRouteResponse where
        toPath GetRouteResponse'{..}
          = mconcat
              ["/v2/apis/", toBS _grAPIId, "/routes/",
               toBS _grRouteId, "/routeresponses/",
               toBS _grRouteResponseId]

instance ToQuery GetRouteResponse where
        toQuery = const mempty

-- | /See:/ 'getRouteResponseResponse' smart constructor.
data GetRouteResponseResponse = GetRouteResponseResponse'
  { _grrrrsModelSelectionExpression :: !(Maybe Text)
  , _grrrrsResponseModels :: !(Maybe (Map Text Text))
  , _grrrrsRouteResponseId :: !(Maybe Text)
  , _grrrrsRouteResponseKey :: !(Maybe Text)
  , _grrrrsResponseParameters :: !(Maybe (Map Text ParameterConstraints))
  , _grrrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRouteResponseResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrrrsModelSelectionExpression' - Represents the model selection expression of a route response.
--
-- * 'grrrrsResponseModels' - Represents the response models of a route response.
--
-- * 'grrrrsRouteResponseId' - Represents the identifier of a route response.
--
-- * 'grrrrsRouteResponseKey' - Represents the route response key of a route response.
--
-- * 'grrrrsResponseParameters' - Represents the response parameters of a route response.
--
-- * 'grrrrsResponseStatus' - -- | The response status code.
getRouteResponseResponse
    :: Int -- ^ 'grrrrsResponseStatus'
    -> GetRouteResponseResponse
getRouteResponseResponse pResponseStatus_ =
  GetRouteResponseResponse'
    { _grrrrsModelSelectionExpression = Nothing
    , _grrrrsResponseModels = Nothing
    , _grrrrsRouteResponseId = Nothing
    , _grrrrsRouteResponseKey = Nothing
    , _grrrrsResponseParameters = Nothing
    , _grrrrsResponseStatus = pResponseStatus_
    }


-- | Represents the model selection expression of a route response.
grrrrsModelSelectionExpression :: Lens' GetRouteResponseResponse (Maybe Text)
grrrrsModelSelectionExpression = lens _grrrrsModelSelectionExpression (\ s a -> s{_grrrrsModelSelectionExpression = a})

-- | Represents the response models of a route response.
grrrrsResponseModels :: Lens' GetRouteResponseResponse (HashMap Text Text)
grrrrsResponseModels = lens _grrrrsResponseModels (\ s a -> s{_grrrrsResponseModels = a}) . _Default . _Map

-- | Represents the identifier of a route response.
grrrrsRouteResponseId :: Lens' GetRouteResponseResponse (Maybe Text)
grrrrsRouteResponseId = lens _grrrrsRouteResponseId (\ s a -> s{_grrrrsRouteResponseId = a})

-- | Represents the route response key of a route response.
grrrrsRouteResponseKey :: Lens' GetRouteResponseResponse (Maybe Text)
grrrrsRouteResponseKey = lens _grrrrsRouteResponseKey (\ s a -> s{_grrrrsRouteResponseKey = a})

-- | Represents the response parameters of a route response.
grrrrsResponseParameters :: Lens' GetRouteResponseResponse (HashMap Text ParameterConstraints)
grrrrsResponseParameters = lens _grrrrsResponseParameters (\ s a -> s{_grrrrsResponseParameters = a}) . _Default . _Map

-- | -- | The response status code.
grrrrsResponseStatus :: Lens' GetRouteResponseResponse Int
grrrrsResponseStatus = lens _grrrrsResponseStatus (\ s a -> s{_grrrrsResponseStatus = a})

instance NFData GetRouteResponseResponse where
