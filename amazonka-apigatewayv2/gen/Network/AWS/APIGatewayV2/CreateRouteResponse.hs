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
-- Module      : Network.AWS.APIGatewayV2.CreateRouteResponse
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a RouteResponse for a Route.
--
--
module Network.AWS.APIGatewayV2.CreateRouteResponse
    (
    -- * Creating a Request
      createRouteResponse
    , CreateRouteResponse
    -- * Request Lenses
    , crModelSelectionExpression
    , crResponseModels
    , crResponseParameters
    , crAPIId
    , crRouteId
    , crRouteResponseKey

    -- * Destructuring the Response
    , createRouteResponseResponse
    , CreateRouteResponseResponse
    -- * Response Lenses
    , crrrsModelSelectionExpression
    , crrrsResponseModels
    , crrrsRouteResponseId
    , crrrsRouteResponseKey
    , crrrsResponseParameters
    , crrrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createRouteResponse' smart constructor.
data CreateRouteResponse = CreateRouteResponse'
  { _crModelSelectionExpression :: !(Maybe Text)
  , _crResponseModels :: !(Maybe (Map Text Text))
  , _crResponseParameters :: !(Maybe (Map Text ParameterConstraints))
  , _crAPIId :: !Text
  , _crRouteId :: !Text
  , _crRouteResponseKey :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRouteResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crModelSelectionExpression' - The model selection expression for the route response.
--
-- * 'crResponseModels' - The response models for the route response.
--
-- * 'crResponseParameters' - The route response parameters.
--
-- * 'crAPIId' - The API identifier.
--
-- * 'crRouteId' - The route ID.
--
-- * 'crRouteResponseKey' - The route response key.
createRouteResponse
    :: Text -- ^ 'crAPIId'
    -> Text -- ^ 'crRouteId'
    -> Text -- ^ 'crRouteResponseKey'
    -> CreateRouteResponse
createRouteResponse pAPIId_ pRouteId_ pRouteResponseKey_ =
  CreateRouteResponse'
    { _crModelSelectionExpression = Nothing
    , _crResponseModels = Nothing
    , _crResponseParameters = Nothing
    , _crAPIId = pAPIId_
    , _crRouteId = pRouteId_
    , _crRouteResponseKey = pRouteResponseKey_
    }


-- | The model selection expression for the route response.
crModelSelectionExpression :: Lens' CreateRouteResponse (Maybe Text)
crModelSelectionExpression = lens _crModelSelectionExpression (\ s a -> s{_crModelSelectionExpression = a})

-- | The response models for the route response.
crResponseModels :: Lens' CreateRouteResponse (HashMap Text Text)
crResponseModels = lens _crResponseModels (\ s a -> s{_crResponseModels = a}) . _Default . _Map

-- | The route response parameters.
crResponseParameters :: Lens' CreateRouteResponse (HashMap Text ParameterConstraints)
crResponseParameters = lens _crResponseParameters (\ s a -> s{_crResponseParameters = a}) . _Default . _Map

-- | The API identifier.
crAPIId :: Lens' CreateRouteResponse Text
crAPIId = lens _crAPIId (\ s a -> s{_crAPIId = a})

-- | The route ID.
crRouteId :: Lens' CreateRouteResponse Text
crRouteId = lens _crRouteId (\ s a -> s{_crRouteId = a})

-- | The route response key.
crRouteResponseKey :: Lens' CreateRouteResponse Text
crRouteResponseKey = lens _crRouteResponseKey (\ s a -> s{_crRouteResponseKey = a})

instance AWSRequest CreateRouteResponse where
        type Rs CreateRouteResponse =
             CreateRouteResponseResponse
        request = postJSON apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 CreateRouteResponseResponse' <$>
                   (x .?> "modelSelectionExpression") <*>
                     (x .?> "responseModels" .!@ mempty)
                     <*> (x .?> "routeResponseId")
                     <*> (x .?> "routeResponseKey")
                     <*> (x .?> "responseParameters" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable CreateRouteResponse where

instance NFData CreateRouteResponse where

instance ToHeaders CreateRouteResponse where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateRouteResponse where
        toJSON CreateRouteResponse'{..}
          = object
              (catMaybes
                 [("modelSelectionExpression" .=) <$>
                    _crModelSelectionExpression,
                  ("responseModels" .=) <$> _crResponseModels,
                  ("responseParameters" .=) <$> _crResponseParameters,
                  Just ("routeResponseKey" .= _crRouteResponseKey)])

instance ToPath CreateRouteResponse where
        toPath CreateRouteResponse'{..}
          = mconcat
              ["/v2/apis/", toBS _crAPIId, "/routes/",
               toBS _crRouteId, "/routeresponses"]

instance ToQuery CreateRouteResponse where
        toQuery = const mempty

-- | /See:/ 'createRouteResponseResponse' smart constructor.
data CreateRouteResponseResponse = CreateRouteResponseResponse'
  { _crrrsModelSelectionExpression :: !(Maybe Text)
  , _crrrsResponseModels :: !(Maybe (Map Text Text))
  , _crrrsRouteResponseId :: !(Maybe Text)
  , _crrrsRouteResponseKey :: !(Maybe Text)
  , _crrrsResponseParameters :: !(Maybe (Map Text ParameterConstraints))
  , _crrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRouteResponseResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crrrsModelSelectionExpression' - Represents the model selection expression of a route response.
--
-- * 'crrrsResponseModels' - Represents the response models of a route response.
--
-- * 'crrrsRouteResponseId' - Represents the identifier of a route response.
--
-- * 'crrrsRouteResponseKey' - Represents the route response key of a route response.
--
-- * 'crrrsResponseParameters' - Represents the response parameters of a route response.
--
-- * 'crrrsResponseStatus' - -- | The response status code.
createRouteResponseResponse
    :: Int -- ^ 'crrrsResponseStatus'
    -> CreateRouteResponseResponse
createRouteResponseResponse pResponseStatus_ =
  CreateRouteResponseResponse'
    { _crrrsModelSelectionExpression = Nothing
    , _crrrsResponseModels = Nothing
    , _crrrsRouteResponseId = Nothing
    , _crrrsRouteResponseKey = Nothing
    , _crrrsResponseParameters = Nothing
    , _crrrsResponseStatus = pResponseStatus_
    }


-- | Represents the model selection expression of a route response.
crrrsModelSelectionExpression :: Lens' CreateRouteResponseResponse (Maybe Text)
crrrsModelSelectionExpression = lens _crrrsModelSelectionExpression (\ s a -> s{_crrrsModelSelectionExpression = a})

-- | Represents the response models of a route response.
crrrsResponseModels :: Lens' CreateRouteResponseResponse (HashMap Text Text)
crrrsResponseModels = lens _crrrsResponseModels (\ s a -> s{_crrrsResponseModels = a}) . _Default . _Map

-- | Represents the identifier of a route response.
crrrsRouteResponseId :: Lens' CreateRouteResponseResponse (Maybe Text)
crrrsRouteResponseId = lens _crrrsRouteResponseId (\ s a -> s{_crrrsRouteResponseId = a})

-- | Represents the route response key of a route response.
crrrsRouteResponseKey :: Lens' CreateRouteResponseResponse (Maybe Text)
crrrsRouteResponseKey = lens _crrrsRouteResponseKey (\ s a -> s{_crrrsRouteResponseKey = a})

-- | Represents the response parameters of a route response.
crrrsResponseParameters :: Lens' CreateRouteResponseResponse (HashMap Text ParameterConstraints)
crrrsResponseParameters = lens _crrrsResponseParameters (\ s a -> s{_crrrsResponseParameters = a}) . _Default . _Map

-- | -- | The response status code.
crrrsResponseStatus :: Lens' CreateRouteResponseResponse Int
crrrsResponseStatus = lens _crrrsResponseStatus (\ s a -> s{_crrrsResponseStatus = a})

instance NFData CreateRouteResponseResponse where
