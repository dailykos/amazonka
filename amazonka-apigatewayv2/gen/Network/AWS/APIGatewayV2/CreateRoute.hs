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
-- Module      : Network.AWS.APIGatewayV2.CreateRoute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Route for an API.
--
--
module Network.AWS.APIGatewayV2.CreateRoute
    (
    -- * Creating a Request
      createRoute
    , CreateRoute
    -- * Request Lenses
    , creAuthorizationScopes
    , creModelSelectionExpression
    , creRequestModels
    , creRouteResponseSelectionExpression
    , creRequestParameters
    , creAuthorizerId
    , creOperationName
    , creAuthorizationType
    , creAPIKeyRequired
    , creTarget
    , creAPIId
    , creRouteKey

    -- * Destructuring the Response
    , createRouteResponse'
    , CreateRouteResponse'
    -- * Response Lenses
    , crrrrsAuthorizationScopes
    , crrrrsModelSelectionExpression
    , crrrrsRequestModels
    , crrrrsRouteResponseSelectionExpression
    , crrrrsRequestParameters
    , crrrrsRouteId
    , crrrrsAuthorizerId
    , crrrrsOperationName
    , crrrrsAuthorizationType
    , crrrrsAPIKeyRequired
    , crrrrsRouteKey
    , crrrrsTarget
    , crrrrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createRoute' smart constructor.
data CreateRoute = CreateRoute'
  { _creAuthorizationScopes :: !(Maybe [Text])
  , _creModelSelectionExpression :: !(Maybe Text)
  , _creRequestModels :: !(Maybe (Map Text Text))
  , _creRouteResponseSelectionExpression :: !(Maybe Text)
  , _creRequestParameters :: !(Maybe (Map Text ParameterConstraints))
  , _creAuthorizerId :: !(Maybe Text)
  , _creOperationName :: !(Maybe Text)
  , _creAuthorizationType :: !(Maybe AuthorizationType)
  , _creAPIKeyRequired :: !(Maybe Bool)
  , _creTarget :: !(Maybe Text)
  , _creAPIId :: !Text
  , _creRouteKey :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'creAuthorizationScopes' - The authorization scopes supported by this route.
--
-- * 'creModelSelectionExpression' - The model selection expression for the route.
--
-- * 'creRequestModels' - The request models for the route.
--
-- * 'creRouteResponseSelectionExpression' - The route response selection expression for the route.
--
-- * 'creRequestParameters' - The request parameters for the route.
--
-- * 'creAuthorizerId' - The identifier of the Authorizer resource to be associated with this route.
--
-- * 'creOperationName' - The operation name for the route.
--
-- * 'creAuthorizationType' - The authorization type for the route. Valid values are NONE for open access, AWS_IAM for using AWS IAM permissions.
--
-- * 'creAPIKeyRequired' - Specifies whether an API key is required for the route.
--
-- * 'creTarget' - The target for the route.
--
-- * 'creAPIId' - The API identifier.
--
-- * 'creRouteKey' - The route key for the route.
createRoute
    :: Text -- ^ 'creAPIId'
    -> Text -- ^ 'creRouteKey'
    -> CreateRoute
createRoute pAPIId_ pRouteKey_ =
  CreateRoute'
    { _creAuthorizationScopes = Nothing
    , _creModelSelectionExpression = Nothing
    , _creRequestModels = Nothing
    , _creRouteResponseSelectionExpression = Nothing
    , _creRequestParameters = Nothing
    , _creAuthorizerId = Nothing
    , _creOperationName = Nothing
    , _creAuthorizationType = Nothing
    , _creAPIKeyRequired = Nothing
    , _creTarget = Nothing
    , _creAPIId = pAPIId_
    , _creRouteKey = pRouteKey_
    }


-- | The authorization scopes supported by this route.
creAuthorizationScopes :: Lens' CreateRoute [Text]
creAuthorizationScopes = lens _creAuthorizationScopes (\ s a -> s{_creAuthorizationScopes = a}) . _Default . _Coerce

-- | The model selection expression for the route.
creModelSelectionExpression :: Lens' CreateRoute (Maybe Text)
creModelSelectionExpression = lens _creModelSelectionExpression (\ s a -> s{_creModelSelectionExpression = a})

-- | The request models for the route.
creRequestModels :: Lens' CreateRoute (HashMap Text Text)
creRequestModels = lens _creRequestModels (\ s a -> s{_creRequestModels = a}) . _Default . _Map

-- | The route response selection expression for the route.
creRouteResponseSelectionExpression :: Lens' CreateRoute (Maybe Text)
creRouteResponseSelectionExpression = lens _creRouteResponseSelectionExpression (\ s a -> s{_creRouteResponseSelectionExpression = a})

-- | The request parameters for the route.
creRequestParameters :: Lens' CreateRoute (HashMap Text ParameterConstraints)
creRequestParameters = lens _creRequestParameters (\ s a -> s{_creRequestParameters = a}) . _Default . _Map

-- | The identifier of the Authorizer resource to be associated with this route.
creAuthorizerId :: Lens' CreateRoute (Maybe Text)
creAuthorizerId = lens _creAuthorizerId (\ s a -> s{_creAuthorizerId = a})

-- | The operation name for the route.
creOperationName :: Lens' CreateRoute (Maybe Text)
creOperationName = lens _creOperationName (\ s a -> s{_creOperationName = a})

-- | The authorization type for the route. Valid values are NONE for open access, AWS_IAM for using AWS IAM permissions.
creAuthorizationType :: Lens' CreateRoute (Maybe AuthorizationType)
creAuthorizationType = lens _creAuthorizationType (\ s a -> s{_creAuthorizationType = a})

-- | Specifies whether an API key is required for the route.
creAPIKeyRequired :: Lens' CreateRoute (Maybe Bool)
creAPIKeyRequired = lens _creAPIKeyRequired (\ s a -> s{_creAPIKeyRequired = a})

-- | The target for the route.
creTarget :: Lens' CreateRoute (Maybe Text)
creTarget = lens _creTarget (\ s a -> s{_creTarget = a})

-- | The API identifier.
creAPIId :: Lens' CreateRoute Text
creAPIId = lens _creAPIId (\ s a -> s{_creAPIId = a})

-- | The route key for the route.
creRouteKey :: Lens' CreateRoute Text
creRouteKey = lens _creRouteKey (\ s a -> s{_creRouteKey = a})

instance AWSRequest CreateRoute where
        type Rs CreateRoute = CreateRouteResponse'
        request = postJSON apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 CreateRouteResponse'' <$>
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

instance Hashable CreateRoute where

instance NFData CreateRoute where

instance ToHeaders CreateRoute where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateRoute where
        toJSON CreateRoute'{..}
          = object
              (catMaybes
                 [("authorizationScopes" .=) <$>
                    _creAuthorizationScopes,
                  ("modelSelectionExpression" .=) <$>
                    _creModelSelectionExpression,
                  ("requestModels" .=) <$> _creRequestModels,
                  ("routeResponseSelectionExpression" .=) <$>
                    _creRouteResponseSelectionExpression,
                  ("requestParameters" .=) <$> _creRequestParameters,
                  ("authorizerId" .=) <$> _creAuthorizerId,
                  ("operationName" .=) <$> _creOperationName,
                  ("authorizationType" .=) <$> _creAuthorizationType,
                  ("apiKeyRequired" .=) <$> _creAPIKeyRequired,
                  ("target" .=) <$> _creTarget,
                  Just ("routeKey" .= _creRouteKey)])

instance ToPath CreateRoute where
        toPath CreateRoute'{..}
          = mconcat ["/v2/apis/", toBS _creAPIId, "/routes"]

instance ToQuery CreateRoute where
        toQuery = const mempty

-- | /See:/ 'createRouteResponse'' smart constructor.
data CreateRouteResponse' = CreateRouteResponse''
  { _crrrrsAuthorizationScopes :: !(Maybe [Text])
  , _crrrrsModelSelectionExpression :: !(Maybe Text)
  , _crrrrsRequestModels :: !(Maybe (Map Text Text))
  , _crrrrsRouteResponseSelectionExpression :: !(Maybe Text)
  , _crrrrsRequestParameters :: !(Maybe (Map Text ParameterConstraints))
  , _crrrrsRouteId :: !(Maybe Text)
  , _crrrrsAuthorizerId :: !(Maybe Text)
  , _crrrrsOperationName :: !(Maybe Text)
  , _crrrrsAuthorizationType :: !(Maybe AuthorizationType)
  , _crrrrsAPIKeyRequired :: !(Maybe Bool)
  , _crrrrsRouteKey :: !(Maybe Text)
  , _crrrrsTarget :: !(Maybe Text)
  , _crrrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRouteResponse'' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crrrrsAuthorizationScopes' - The authorization scopes supported by this route. 
--
-- * 'crrrrsModelSelectionExpression' - The model selection expression for the route.
--
-- * 'crrrrsRequestModels' - The request models for the route.
--
-- * 'crrrrsRouteResponseSelectionExpression' - The route response selection expression for the route.
--
-- * 'crrrrsRequestParameters' - The request parameters for the route.
--
-- * 'crrrrsRouteId' - The route ID.
--
-- * 'crrrrsAuthorizerId' - The identifier of the Authorizer resource to be associated with this route.
--
-- * 'crrrrsOperationName' - The operation name for the route.
--
-- * 'crrrrsAuthorizationType' - The authorization type for the route. Valid values are NONE for open access, AWS_IAM for using AWS IAM permissions.
--
-- * 'crrrrsAPIKeyRequired' - Specifies whether an API key is required for this route.
--
-- * 'crrrrsRouteKey' - The route key for the route.
--
-- * 'crrrrsTarget' - The target for the route.
--
-- * 'crrrrsResponseStatus' - -- | The response status code.
createRouteResponse'
    :: Int -- ^ 'crrrrsResponseStatus'
    -> CreateRouteResponse'
createRouteResponse' pResponseStatus_ =
  CreateRouteResponse''
    { _crrrrsAuthorizationScopes = Nothing
    , _crrrrsModelSelectionExpression = Nothing
    , _crrrrsRequestModels = Nothing
    , _crrrrsRouteResponseSelectionExpression = Nothing
    , _crrrrsRequestParameters = Nothing
    , _crrrrsRouteId = Nothing
    , _crrrrsAuthorizerId = Nothing
    , _crrrrsOperationName = Nothing
    , _crrrrsAuthorizationType = Nothing
    , _crrrrsAPIKeyRequired = Nothing
    , _crrrrsRouteKey = Nothing
    , _crrrrsTarget = Nothing
    , _crrrrsResponseStatus = pResponseStatus_
    }


-- | The authorization scopes supported by this route. 
crrrrsAuthorizationScopes :: Lens' CreateRouteResponse' [Text]
crrrrsAuthorizationScopes = lens _crrrrsAuthorizationScopes (\ s a -> s{_crrrrsAuthorizationScopes = a}) . _Default . _Coerce

-- | The model selection expression for the route.
crrrrsModelSelectionExpression :: Lens' CreateRouteResponse' (Maybe Text)
crrrrsModelSelectionExpression = lens _crrrrsModelSelectionExpression (\ s a -> s{_crrrrsModelSelectionExpression = a})

-- | The request models for the route.
crrrrsRequestModels :: Lens' CreateRouteResponse' (HashMap Text Text)
crrrrsRequestModels = lens _crrrrsRequestModels (\ s a -> s{_crrrrsRequestModels = a}) . _Default . _Map

-- | The route response selection expression for the route.
crrrrsRouteResponseSelectionExpression :: Lens' CreateRouteResponse' (Maybe Text)
crrrrsRouteResponseSelectionExpression = lens _crrrrsRouteResponseSelectionExpression (\ s a -> s{_crrrrsRouteResponseSelectionExpression = a})

-- | The request parameters for the route.
crrrrsRequestParameters :: Lens' CreateRouteResponse' (HashMap Text ParameterConstraints)
crrrrsRequestParameters = lens _crrrrsRequestParameters (\ s a -> s{_crrrrsRequestParameters = a}) . _Default . _Map

-- | The route ID.
crrrrsRouteId :: Lens' CreateRouteResponse' (Maybe Text)
crrrrsRouteId = lens _crrrrsRouteId (\ s a -> s{_crrrrsRouteId = a})

-- | The identifier of the Authorizer resource to be associated with this route.
crrrrsAuthorizerId :: Lens' CreateRouteResponse' (Maybe Text)
crrrrsAuthorizerId = lens _crrrrsAuthorizerId (\ s a -> s{_crrrrsAuthorizerId = a})

-- | The operation name for the route.
crrrrsOperationName :: Lens' CreateRouteResponse' (Maybe Text)
crrrrsOperationName = lens _crrrrsOperationName (\ s a -> s{_crrrrsOperationName = a})

-- | The authorization type for the route. Valid values are NONE for open access, AWS_IAM for using AWS IAM permissions.
crrrrsAuthorizationType :: Lens' CreateRouteResponse' (Maybe AuthorizationType)
crrrrsAuthorizationType = lens _crrrrsAuthorizationType (\ s a -> s{_crrrrsAuthorizationType = a})

-- | Specifies whether an API key is required for this route.
crrrrsAPIKeyRequired :: Lens' CreateRouteResponse' (Maybe Bool)
crrrrsAPIKeyRequired = lens _crrrrsAPIKeyRequired (\ s a -> s{_crrrrsAPIKeyRequired = a})

-- | The route key for the route.
crrrrsRouteKey :: Lens' CreateRouteResponse' (Maybe Text)
crrrrsRouteKey = lens _crrrrsRouteKey (\ s a -> s{_crrrrsRouteKey = a})

-- | The target for the route.
crrrrsTarget :: Lens' CreateRouteResponse' (Maybe Text)
crrrrsTarget = lens _crrrrsTarget (\ s a -> s{_crrrrsTarget = a})

-- | -- | The response status code.
crrrrsResponseStatus :: Lens' CreateRouteResponse' Int
crrrrsResponseStatus = lens _crrrrsResponseStatus (\ s a -> s{_crrrrsResponseStatus = a})

instance NFData CreateRouteResponse' where
