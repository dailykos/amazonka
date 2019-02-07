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
-- Module      : Network.AWS.APIGatewayV2.GetRoute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a Route.
--
--
module Network.AWS.APIGatewayV2.GetRoute
    (
    -- * Creating a Request
      getRoute
    , GetRoute
    -- * Request Lenses
    , geteAPIId
    , geteRouteId

    -- * Destructuring the Response
    , getRouteResponse'
    , GetRouteResponse'
    -- * Response Lenses
    , ggrsAuthorizationScopes
    , ggrsModelSelectionExpression
    , ggrsRequestModels
    , ggrsRouteResponseSelectionExpression
    , ggrsRequestParameters
    , ggrsRouteId
    , ggrsAuthorizerId
    , ggrsOperationName
    , ggrsAuthorizationType
    , ggrsAPIKeyRequired
    , ggrsRouteKey
    , ggrsTarget
    , ggrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRoute' smart constructor.
data GetRoute = GetRoute'
  { _geteAPIId :: !Text
  , _geteRouteId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'geteAPIId' - The API identifier.
--
-- * 'geteRouteId' - The route ID.
getRoute
    :: Text -- ^ 'geteAPIId'
    -> Text -- ^ 'geteRouteId'
    -> GetRoute
getRoute pAPIId_ pRouteId_ =
  GetRoute' {_geteAPIId = pAPIId_, _geteRouteId = pRouteId_}


-- | The API identifier.
geteAPIId :: Lens' GetRoute Text
geteAPIId = lens _geteAPIId (\ s a -> s{_geteAPIId = a})

-- | The route ID.
geteRouteId :: Lens' GetRoute Text
geteRouteId = lens _geteRouteId (\ s a -> s{_geteRouteId = a})

instance AWSRequest GetRoute where
        type Rs GetRoute = GetRouteResponse'
        request = get apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 GetRouteResponse'' <$>
                   (x .?> "authorizationScopes" .!@ mempty) <*>
                     (x .?> "modelSelectionExpression")
                     <*> (x .?> "requestModels" .!@ mempty)
                     <*> (x .?> "routeResponseSelectionExpression")
                     <*> (x .?> "requestParameters" .!@ mempty)
                     <*> (x .?> "routeId")
                     <*> (x .?> "authorizerId")
                     <*> (x .?> "operationName")
                     <*> (x .?> "authorizationType")
                     <*> (x .?> "apiKeyRequired")
                     <*> (x .?> "routeKey")
                     <*> (x .?> "target")
                     <*> (pure (fromEnum s)))

instance Hashable GetRoute where

instance NFData GetRoute where

instance ToHeaders GetRoute where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetRoute where
        toPath GetRoute'{..}
          = mconcat
              ["/v2/apis/", toBS _geteAPIId, "/routes/",
               toBS _geteRouteId]

instance ToQuery GetRoute where
        toQuery = const mempty

-- | /See:/ 'getRouteResponse'' smart constructor.
data GetRouteResponse' = GetRouteResponse''
  { _ggrsAuthorizationScopes :: !(Maybe [Text])
  , _ggrsModelSelectionExpression :: !(Maybe Text)
  , _ggrsRequestModels :: !(Maybe (Map Text Text))
  , _ggrsRouteResponseSelectionExpression :: !(Maybe Text)
  , _ggrsRequestParameters :: !(Maybe (Map Text ParameterConstraints))
  , _ggrsRouteId :: !(Maybe Text)
  , _ggrsAuthorizerId :: !(Maybe Text)
  , _ggrsOperationName :: !(Maybe Text)
  , _ggrsAuthorizationType :: !(Maybe AuthorizationType)
  , _ggrsAPIKeyRequired :: !(Maybe Bool)
  , _ggrsRouteKey :: !(Maybe Text)
  , _ggrsTarget :: !(Maybe Text)
  , _ggrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRouteResponse'' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggrsAuthorizationScopes' - The authorization scopes supported by this route. 
--
-- * 'ggrsModelSelectionExpression' - The model selection expression for the route.
--
-- * 'ggrsRequestModels' - The request models for the route.
--
-- * 'ggrsRouteResponseSelectionExpression' - The route response selection expression for the route.
--
-- * 'ggrsRequestParameters' - The request parameters for the route.
--
-- * 'ggrsRouteId' - The route ID.
--
-- * 'ggrsAuthorizerId' - The identifier of the Authorizer resource to be associated with this route.
--
-- * 'ggrsOperationName' - The operation name for the route.
--
-- * 'ggrsAuthorizationType' - The authorization type for the route. Valid values are NONE for open access, AWS_IAM for using AWS IAM permissions.
--
-- * 'ggrsAPIKeyRequired' - Specifies whether an API key is required for this route.
--
-- * 'ggrsRouteKey' - The route key for the route.
--
-- * 'ggrsTarget' - The target for the route.
--
-- * 'ggrsResponseStatus' - -- | The response status code.
getRouteResponse'
    :: Int -- ^ 'ggrsResponseStatus'
    -> GetRouteResponse'
getRouteResponse' pResponseStatus_ =
  GetRouteResponse''
    { _ggrsAuthorizationScopes = Nothing
    , _ggrsModelSelectionExpression = Nothing
    , _ggrsRequestModels = Nothing
    , _ggrsRouteResponseSelectionExpression = Nothing
    , _ggrsRequestParameters = Nothing
    , _ggrsRouteId = Nothing
    , _ggrsAuthorizerId = Nothing
    , _ggrsOperationName = Nothing
    , _ggrsAuthorizationType = Nothing
    , _ggrsAPIKeyRequired = Nothing
    , _ggrsRouteKey = Nothing
    , _ggrsTarget = Nothing
    , _ggrsResponseStatus = pResponseStatus_
    }


-- | The authorization scopes supported by this route. 
ggrsAuthorizationScopes :: Lens' GetRouteResponse' [Text]
ggrsAuthorizationScopes = lens _ggrsAuthorizationScopes (\ s a -> s{_ggrsAuthorizationScopes = a}) . _Default . _Coerce

-- | The model selection expression for the route.
ggrsModelSelectionExpression :: Lens' GetRouteResponse' (Maybe Text)
ggrsModelSelectionExpression = lens _ggrsModelSelectionExpression (\ s a -> s{_ggrsModelSelectionExpression = a})

-- | The request models for the route.
ggrsRequestModels :: Lens' GetRouteResponse' (HashMap Text Text)
ggrsRequestModels = lens _ggrsRequestModels (\ s a -> s{_ggrsRequestModels = a}) . _Default . _Map

-- | The route response selection expression for the route.
ggrsRouteResponseSelectionExpression :: Lens' GetRouteResponse' (Maybe Text)
ggrsRouteResponseSelectionExpression = lens _ggrsRouteResponseSelectionExpression (\ s a -> s{_ggrsRouteResponseSelectionExpression = a})

-- | The request parameters for the route.
ggrsRequestParameters :: Lens' GetRouteResponse' (HashMap Text ParameterConstraints)
ggrsRequestParameters = lens _ggrsRequestParameters (\ s a -> s{_ggrsRequestParameters = a}) . _Default . _Map

-- | The route ID.
ggrsRouteId :: Lens' GetRouteResponse' (Maybe Text)
ggrsRouteId = lens _ggrsRouteId (\ s a -> s{_ggrsRouteId = a})

-- | The identifier of the Authorizer resource to be associated with this route.
ggrsAuthorizerId :: Lens' GetRouteResponse' (Maybe Text)
ggrsAuthorizerId = lens _ggrsAuthorizerId (\ s a -> s{_ggrsAuthorizerId = a})

-- | The operation name for the route.
ggrsOperationName :: Lens' GetRouteResponse' (Maybe Text)
ggrsOperationName = lens _ggrsOperationName (\ s a -> s{_ggrsOperationName = a})

-- | The authorization type for the route. Valid values are NONE for open access, AWS_IAM for using AWS IAM permissions.
ggrsAuthorizationType :: Lens' GetRouteResponse' (Maybe AuthorizationType)
ggrsAuthorizationType = lens _ggrsAuthorizationType (\ s a -> s{_ggrsAuthorizationType = a})

-- | Specifies whether an API key is required for this route.
ggrsAPIKeyRequired :: Lens' GetRouteResponse' (Maybe Bool)
ggrsAPIKeyRequired = lens _ggrsAPIKeyRequired (\ s a -> s{_ggrsAPIKeyRequired = a})

-- | The route key for the route.
ggrsRouteKey :: Lens' GetRouteResponse' (Maybe Text)
ggrsRouteKey = lens _ggrsRouteKey (\ s a -> s{_ggrsRouteKey = a})

-- | The target for the route.
ggrsTarget :: Lens' GetRouteResponse' (Maybe Text)
ggrsTarget = lens _ggrsTarget (\ s a -> s{_ggrsTarget = a})

-- | -- | The response status code.
ggrsResponseStatus :: Lens' GetRouteResponse' Int
ggrsResponseStatus = lens _ggrsResponseStatus (\ s a -> s{_ggrsResponseStatus = a})

instance NFData GetRouteResponse' where
