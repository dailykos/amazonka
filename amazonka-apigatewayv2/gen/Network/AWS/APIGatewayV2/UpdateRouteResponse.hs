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
-- Module      : Network.AWS.APIGatewayV2.UpdateRouteResponse
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a RouteResponse.
--
--
module Network.AWS.APIGatewayV2.UpdateRouteResponse
    (
    -- * Creating a Request
      updateRouteResponse
    , UpdateRouteResponse
    -- * Request Lenses
    , uModelSelectionExpression
    , uResponseModels
    , uRouteResponseKey
    , uResponseParameters
    , uRouteResponseId
    , uAPIId
    , uRouteId

    -- * Destructuring the Response
    , updateRouteResponseResponse
    , UpdateRouteResponseResponse
    -- * Response Lenses
    , urrrsModelSelectionExpression
    , urrrsResponseModels
    , urrrsRouteResponseId
    , urrrsRouteResponseKey
    , urrrsResponseParameters
    , urrrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateRouteResponse' smart constructor.
data UpdateRouteResponse = UpdateRouteResponse'
  { _uModelSelectionExpression :: !(Maybe Text)
  , _uResponseModels :: !(Maybe (Map Text Text))
  , _uRouteResponseKey :: !(Maybe Text)
  , _uResponseParameters :: !(Maybe (Map Text ParameterConstraints))
  , _uRouteResponseId :: !Text
  , _uAPIId :: !Text
  , _uRouteId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRouteResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uModelSelectionExpression' - The model selection expression for the route response.
--
-- * 'uResponseModels' - The response models for the route response.
--
-- * 'uRouteResponseKey' - The route response key.
--
-- * 'uResponseParameters' - The route response parameters.
--
-- * 'uRouteResponseId' - The route response ID.
--
-- * 'uAPIId' - The API identifier.
--
-- * 'uRouteId' - The route ID.
updateRouteResponse
    :: Text -- ^ 'uRouteResponseId'
    -> Text -- ^ 'uAPIId'
    -> Text -- ^ 'uRouteId'
    -> UpdateRouteResponse
updateRouteResponse pRouteResponseId_ pAPIId_ pRouteId_ =
  UpdateRouteResponse'
    { _uModelSelectionExpression = Nothing
    , _uResponseModels = Nothing
    , _uRouteResponseKey = Nothing
    , _uResponseParameters = Nothing
    , _uRouteResponseId = pRouteResponseId_
    , _uAPIId = pAPIId_
    , _uRouteId = pRouteId_
    }


-- | The model selection expression for the route response.
uModelSelectionExpression :: Lens' UpdateRouteResponse (Maybe Text)
uModelSelectionExpression = lens _uModelSelectionExpression (\ s a -> s{_uModelSelectionExpression = a})

-- | The response models for the route response.
uResponseModels :: Lens' UpdateRouteResponse (HashMap Text Text)
uResponseModels = lens _uResponseModels (\ s a -> s{_uResponseModels = a}) . _Default . _Map

-- | The route response key.
uRouteResponseKey :: Lens' UpdateRouteResponse (Maybe Text)
uRouteResponseKey = lens _uRouteResponseKey (\ s a -> s{_uRouteResponseKey = a})

-- | The route response parameters.
uResponseParameters :: Lens' UpdateRouteResponse (HashMap Text ParameterConstraints)
uResponseParameters = lens _uResponseParameters (\ s a -> s{_uResponseParameters = a}) . _Default . _Map

-- | The route response ID.
uRouteResponseId :: Lens' UpdateRouteResponse Text
uRouteResponseId = lens _uRouteResponseId (\ s a -> s{_uRouteResponseId = a})

-- | The API identifier.
uAPIId :: Lens' UpdateRouteResponse Text
uAPIId = lens _uAPIId (\ s a -> s{_uAPIId = a})

-- | The route ID.
uRouteId :: Lens' UpdateRouteResponse Text
uRouteId = lens _uRouteId (\ s a -> s{_uRouteId = a})

instance AWSRequest UpdateRouteResponse where
        type Rs UpdateRouteResponse =
             UpdateRouteResponseResponse
        request = patchJSON apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 UpdateRouteResponseResponse' <$>
                   (x .?> "modelSelectionExpression") <*>
                     (x .?> "responseModels" .!@ mempty)
                     <*> (x .?> "routeResponseId")
                     <*> (x .?> "routeResponseKey")
                     <*> (x .?> "responseParameters" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable UpdateRouteResponse where

instance NFData UpdateRouteResponse where

instance ToHeaders UpdateRouteResponse where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateRouteResponse where
        toJSON UpdateRouteResponse'{..}
          = object
              (catMaybes
                 [("modelSelectionExpression" .=) <$>
                    _uModelSelectionExpression,
                  ("responseModels" .=) <$> _uResponseModels,
                  ("routeResponseKey" .=) <$> _uRouteResponseKey,
                  ("responseParameters" .=) <$> _uResponseParameters])

instance ToPath UpdateRouteResponse where
        toPath UpdateRouteResponse'{..}
          = mconcat
              ["/v2/apis/", toBS _uAPIId, "/routes/",
               toBS _uRouteId, "/routeresponses/",
               toBS _uRouteResponseId]

instance ToQuery UpdateRouteResponse where
        toQuery = const mempty

-- | /See:/ 'updateRouteResponseResponse' smart constructor.
data UpdateRouteResponseResponse = UpdateRouteResponseResponse'
  { _urrrsModelSelectionExpression :: !(Maybe Text)
  , _urrrsResponseModels :: !(Maybe (Map Text Text))
  , _urrrsRouteResponseId :: !(Maybe Text)
  , _urrrsRouteResponseKey :: !(Maybe Text)
  , _urrrsResponseParameters :: !(Maybe (Map Text ParameterConstraints))
  , _urrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRouteResponseResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urrrsModelSelectionExpression' - Represents the model selection expression of a route response.
--
-- * 'urrrsResponseModels' - Represents the response models of a route response.
--
-- * 'urrrsRouteResponseId' - Represents the identifier of a route response.
--
-- * 'urrrsRouteResponseKey' - Represents the route response key of a route response.
--
-- * 'urrrsResponseParameters' - Represents the response parameters of a route response.
--
-- * 'urrrsResponseStatus' - -- | The response status code.
updateRouteResponseResponse
    :: Int -- ^ 'urrrsResponseStatus'
    -> UpdateRouteResponseResponse
updateRouteResponseResponse pResponseStatus_ =
  UpdateRouteResponseResponse'
    { _urrrsModelSelectionExpression = Nothing
    , _urrrsResponseModels = Nothing
    , _urrrsRouteResponseId = Nothing
    , _urrrsRouteResponseKey = Nothing
    , _urrrsResponseParameters = Nothing
    , _urrrsResponseStatus = pResponseStatus_
    }


-- | Represents the model selection expression of a route response.
urrrsModelSelectionExpression :: Lens' UpdateRouteResponseResponse (Maybe Text)
urrrsModelSelectionExpression = lens _urrrsModelSelectionExpression (\ s a -> s{_urrrsModelSelectionExpression = a})

-- | Represents the response models of a route response.
urrrsResponseModels :: Lens' UpdateRouteResponseResponse (HashMap Text Text)
urrrsResponseModels = lens _urrrsResponseModels (\ s a -> s{_urrrsResponseModels = a}) . _Default . _Map

-- | Represents the identifier of a route response.
urrrsRouteResponseId :: Lens' UpdateRouteResponseResponse (Maybe Text)
urrrsRouteResponseId = lens _urrrsRouteResponseId (\ s a -> s{_urrrsRouteResponseId = a})

-- | Represents the route response key of a route response.
urrrsRouteResponseKey :: Lens' UpdateRouteResponseResponse (Maybe Text)
urrrsRouteResponseKey = lens _urrrsRouteResponseKey (\ s a -> s{_urrrsRouteResponseKey = a})

-- | Represents the response parameters of a route response.
urrrsResponseParameters :: Lens' UpdateRouteResponseResponse (HashMap Text ParameterConstraints)
urrrsResponseParameters = lens _urrrsResponseParameters (\ s a -> s{_urrrsResponseParameters = a}) . _Default . _Map

-- | -- | The response status code.
urrrsResponseStatus :: Lens' UpdateRouteResponseResponse Int
urrrsResponseStatus = lens _urrrsResponseStatus (\ s a -> s{_urrrsResponseStatus = a})

instance NFData UpdateRouteResponseResponse where
