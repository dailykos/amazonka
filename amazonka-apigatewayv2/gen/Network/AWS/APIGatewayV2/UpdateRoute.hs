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
-- Module      : Network.AWS.APIGatewayV2.UpdateRoute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Route.
--
--
module Network.AWS.APIGatewayV2.UpdateRoute
    (
    -- * Creating a Request
      updateRoute
    , UpdateRoute
    -- * Request Lenses
    , urAuthorizationScopes
    , urModelSelectionExpression
    , urRequestModels
    , urRouteResponseSelectionExpression
    , urRequestParameters
    , urAuthorizerId
    , urOperationName
    , urAuthorizationType
    , urAPIKeyRequired
    , urRouteKey
    , urTarget
    , urAPIId
    , urRouteId

    -- * Destructuring the Response
    , updateRouteResponse'
    , UpdateRouteResponse'
    -- * Response Lenses
    , ursAuthorizationScopes
    , ursModelSelectionExpression
    , ursRequestModels
    , ursRouteResponseSelectionExpression
    , ursRequestParameters
    , ursRouteId
    , ursAuthorizerId
    , ursOperationName
    , ursAuthorizationType
    , ursAPIKeyRequired
    , ursRouteKey
    , ursTarget
    , ursResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateRoute' smart constructor.
data UpdateRoute = UpdateRoute'
  { _urAuthorizationScopes :: !(Maybe [Text])
  , _urModelSelectionExpression :: !(Maybe Text)
  , _urRequestModels :: !(Maybe (Map Text Text))
  , _urRouteResponseSelectionExpression :: !(Maybe Text)
  , _urRequestParameters :: !(Maybe (Map Text ParameterConstraints))
  , _urAuthorizerId :: !(Maybe Text)
  , _urOperationName :: !(Maybe Text)
  , _urAuthorizationType :: !(Maybe AuthorizationType)
  , _urAPIKeyRequired :: !(Maybe Bool)
  , _urRouteKey :: !(Maybe Text)
  , _urTarget :: !(Maybe Text)
  , _urAPIId :: !Text
  , _urRouteId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urAuthorizationScopes' - The authorization scopes supported by this route.
--
-- * 'urModelSelectionExpression' - The model selection expression for the route.
--
-- * 'urRequestModels' - The request models for the route.
--
-- * 'urRouteResponseSelectionExpression' - The route response selection expression for the route.
--
-- * 'urRequestParameters' - The request parameters for the route.
--
-- * 'urAuthorizerId' - The identifier of the Authorizer resource to be associated with this route.
--
-- * 'urOperationName' - The operation name for the route.
--
-- * 'urAuthorizationType' - The authorization type for the route. Valid values are NONE for open access, AWS_IAM for using AWS IAM permissions.
--
-- * 'urAPIKeyRequired' - Specifies whether an API key is required for the route.
--
-- * 'urRouteKey' - The route key for the route.
--
-- * 'urTarget' - The target for the route.
--
-- * 'urAPIId' - The API identifier.
--
-- * 'urRouteId' - The route ID.
updateRoute
    :: Text -- ^ 'urAPIId'
    -> Text -- ^ 'urRouteId'
    -> UpdateRoute
updateRoute pAPIId_ pRouteId_ =
  UpdateRoute'
    { _urAuthorizationScopes = Nothing
    , _urModelSelectionExpression = Nothing
    , _urRequestModels = Nothing
    , _urRouteResponseSelectionExpression = Nothing
    , _urRequestParameters = Nothing
    , _urAuthorizerId = Nothing
    , _urOperationName = Nothing
    , _urAuthorizationType = Nothing
    , _urAPIKeyRequired = Nothing
    , _urRouteKey = Nothing
    , _urTarget = Nothing
    , _urAPIId = pAPIId_
    , _urRouteId = pRouteId_
    }


-- | The authorization scopes supported by this route.
urAuthorizationScopes :: Lens' UpdateRoute [Text]
urAuthorizationScopes = lens _urAuthorizationScopes (\ s a -> s{_urAuthorizationScopes = a}) . _Default . _Coerce

-- | The model selection expression for the route.
urModelSelectionExpression :: Lens' UpdateRoute (Maybe Text)
urModelSelectionExpression = lens _urModelSelectionExpression (\ s a -> s{_urModelSelectionExpression = a})

-- | The request models for the route.
urRequestModels :: Lens' UpdateRoute (HashMap Text Text)
urRequestModels = lens _urRequestModels (\ s a -> s{_urRequestModels = a}) . _Default . _Map

-- | The route response selection expression for the route.
urRouteResponseSelectionExpression :: Lens' UpdateRoute (Maybe Text)
urRouteResponseSelectionExpression = lens _urRouteResponseSelectionExpression (\ s a -> s{_urRouteResponseSelectionExpression = a})

-- | The request parameters for the route.
urRequestParameters :: Lens' UpdateRoute (HashMap Text ParameterConstraints)
urRequestParameters = lens _urRequestParameters (\ s a -> s{_urRequestParameters = a}) . _Default . _Map

-- | The identifier of the Authorizer resource to be associated with this route.
urAuthorizerId :: Lens' UpdateRoute (Maybe Text)
urAuthorizerId = lens _urAuthorizerId (\ s a -> s{_urAuthorizerId = a})

-- | The operation name for the route.
urOperationName :: Lens' UpdateRoute (Maybe Text)
urOperationName = lens _urOperationName (\ s a -> s{_urOperationName = a})

-- | The authorization type for the route. Valid values are NONE for open access, AWS_IAM for using AWS IAM permissions.
urAuthorizationType :: Lens' UpdateRoute (Maybe AuthorizationType)
urAuthorizationType = lens _urAuthorizationType (\ s a -> s{_urAuthorizationType = a})

-- | Specifies whether an API key is required for the route.
urAPIKeyRequired :: Lens' UpdateRoute (Maybe Bool)
urAPIKeyRequired = lens _urAPIKeyRequired (\ s a -> s{_urAPIKeyRequired = a})

-- | The route key for the route.
urRouteKey :: Lens' UpdateRoute (Maybe Text)
urRouteKey = lens _urRouteKey (\ s a -> s{_urRouteKey = a})

-- | The target for the route.
urTarget :: Lens' UpdateRoute (Maybe Text)
urTarget = lens _urTarget (\ s a -> s{_urTarget = a})

-- | The API identifier.
urAPIId :: Lens' UpdateRoute Text
urAPIId = lens _urAPIId (\ s a -> s{_urAPIId = a})

-- | The route ID.
urRouteId :: Lens' UpdateRoute Text
urRouteId = lens _urRouteId (\ s a -> s{_urRouteId = a})

instance AWSRequest UpdateRoute where
        type Rs UpdateRoute = UpdateRouteResponse'
        request = patchJSON apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 UpdateRouteResponse'' <$>
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

instance Hashable UpdateRoute where

instance NFData UpdateRoute where

instance ToHeaders UpdateRoute where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateRoute where
        toJSON UpdateRoute'{..}
          = object
              (catMaybes
                 [("authorizationScopes" .=) <$>
                    _urAuthorizationScopes,
                  ("modelSelectionExpression" .=) <$>
                    _urModelSelectionExpression,
                  ("requestModels" .=) <$> _urRequestModels,
                  ("routeResponseSelectionExpression" .=) <$>
                    _urRouteResponseSelectionExpression,
                  ("requestParameters" .=) <$> _urRequestParameters,
                  ("authorizerId" .=) <$> _urAuthorizerId,
                  ("operationName" .=) <$> _urOperationName,
                  ("authorizationType" .=) <$> _urAuthorizationType,
                  ("apiKeyRequired" .=) <$> _urAPIKeyRequired,
                  ("routeKey" .=) <$> _urRouteKey,
                  ("target" .=) <$> _urTarget])

instance ToPath UpdateRoute where
        toPath UpdateRoute'{..}
          = mconcat
              ["/v2/apis/", toBS _urAPIId, "/routes/",
               toBS _urRouteId]

instance ToQuery UpdateRoute where
        toQuery = const mempty

-- | /See:/ 'updateRouteResponse'' smart constructor.
data UpdateRouteResponse' = UpdateRouteResponse''
  { _ursAuthorizationScopes :: !(Maybe [Text])
  , _ursModelSelectionExpression :: !(Maybe Text)
  , _ursRequestModels :: !(Maybe (Map Text Text))
  , _ursRouteResponseSelectionExpression :: !(Maybe Text)
  , _ursRequestParameters :: !(Maybe (Map Text ParameterConstraints))
  , _ursRouteId :: !(Maybe Text)
  , _ursAuthorizerId :: !(Maybe Text)
  , _ursOperationName :: !(Maybe Text)
  , _ursAuthorizationType :: !(Maybe AuthorizationType)
  , _ursAPIKeyRequired :: !(Maybe Bool)
  , _ursRouteKey :: !(Maybe Text)
  , _ursTarget :: !(Maybe Text)
  , _ursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRouteResponse'' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ursAuthorizationScopes' - The authorization scopes supported by this route. 
--
-- * 'ursModelSelectionExpression' - The model selection expression for the route.
--
-- * 'ursRequestModels' - The request models for the route.
--
-- * 'ursRouteResponseSelectionExpression' - The route response selection expression for the route.
--
-- * 'ursRequestParameters' - The request parameters for the route.
--
-- * 'ursRouteId' - The route ID.
--
-- * 'ursAuthorizerId' - The identifier of the Authorizer resource to be associated with this route.
--
-- * 'ursOperationName' - The operation name for the route.
--
-- * 'ursAuthorizationType' - The authorization type for the route. Valid values are NONE for open access, AWS_IAM for using AWS IAM permissions.
--
-- * 'ursAPIKeyRequired' - Specifies whether an API key is required for this route.
--
-- * 'ursRouteKey' - The route key for the route.
--
-- * 'ursTarget' - The target for the route.
--
-- * 'ursResponseStatus' - -- | The response status code.
updateRouteResponse'
    :: Int -- ^ 'ursResponseStatus'
    -> UpdateRouteResponse'
updateRouteResponse' pResponseStatus_ =
  UpdateRouteResponse''
    { _ursAuthorizationScopes = Nothing
    , _ursModelSelectionExpression = Nothing
    , _ursRequestModels = Nothing
    , _ursRouteResponseSelectionExpression = Nothing
    , _ursRequestParameters = Nothing
    , _ursRouteId = Nothing
    , _ursAuthorizerId = Nothing
    , _ursOperationName = Nothing
    , _ursAuthorizationType = Nothing
    , _ursAPIKeyRequired = Nothing
    , _ursRouteKey = Nothing
    , _ursTarget = Nothing
    , _ursResponseStatus = pResponseStatus_
    }


-- | The authorization scopes supported by this route. 
ursAuthorizationScopes :: Lens' UpdateRouteResponse' [Text]
ursAuthorizationScopes = lens _ursAuthorizationScopes (\ s a -> s{_ursAuthorizationScopes = a}) . _Default . _Coerce

-- | The model selection expression for the route.
ursModelSelectionExpression :: Lens' UpdateRouteResponse' (Maybe Text)
ursModelSelectionExpression = lens _ursModelSelectionExpression (\ s a -> s{_ursModelSelectionExpression = a})

-- | The request models for the route.
ursRequestModels :: Lens' UpdateRouteResponse' (HashMap Text Text)
ursRequestModels = lens _ursRequestModels (\ s a -> s{_ursRequestModels = a}) . _Default . _Map

-- | The route response selection expression for the route.
ursRouteResponseSelectionExpression :: Lens' UpdateRouteResponse' (Maybe Text)
ursRouteResponseSelectionExpression = lens _ursRouteResponseSelectionExpression (\ s a -> s{_ursRouteResponseSelectionExpression = a})

-- | The request parameters for the route.
ursRequestParameters :: Lens' UpdateRouteResponse' (HashMap Text ParameterConstraints)
ursRequestParameters = lens _ursRequestParameters (\ s a -> s{_ursRequestParameters = a}) . _Default . _Map

-- | The route ID.
ursRouteId :: Lens' UpdateRouteResponse' (Maybe Text)
ursRouteId = lens _ursRouteId (\ s a -> s{_ursRouteId = a})

-- | The identifier of the Authorizer resource to be associated with this route.
ursAuthorizerId :: Lens' UpdateRouteResponse' (Maybe Text)
ursAuthorizerId = lens _ursAuthorizerId (\ s a -> s{_ursAuthorizerId = a})

-- | The operation name for the route.
ursOperationName :: Lens' UpdateRouteResponse' (Maybe Text)
ursOperationName = lens _ursOperationName (\ s a -> s{_ursOperationName = a})

-- | The authorization type for the route. Valid values are NONE for open access, AWS_IAM for using AWS IAM permissions.
ursAuthorizationType :: Lens' UpdateRouteResponse' (Maybe AuthorizationType)
ursAuthorizationType = lens _ursAuthorizationType (\ s a -> s{_ursAuthorizationType = a})

-- | Specifies whether an API key is required for this route.
ursAPIKeyRequired :: Lens' UpdateRouteResponse' (Maybe Bool)
ursAPIKeyRequired = lens _ursAPIKeyRequired (\ s a -> s{_ursAPIKeyRequired = a})

-- | The route key for the route.
ursRouteKey :: Lens' UpdateRouteResponse' (Maybe Text)
ursRouteKey = lens _ursRouteKey (\ s a -> s{_ursRouteKey = a})

-- | The target for the route.
ursTarget :: Lens' UpdateRouteResponse' (Maybe Text)
ursTarget = lens _ursTarget (\ s a -> s{_ursTarget = a})

-- | -- | The response status code.
ursResponseStatus :: Lens' UpdateRouteResponse' Int
ursResponseStatus = lens _ursResponseStatus (\ s a -> s{_ursResponseStatus = a})

instance NFData UpdateRouteResponse' where
