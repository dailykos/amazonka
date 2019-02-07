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
-- Module      : Network.AWS.APIGatewayV2.UpdateAPI
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Api resource.
--
--
module Network.AWS.APIGatewayV2.UpdateAPI
    (
    -- * Creating a Request
      updateAPI
    , UpdateAPI
    -- * Request Lenses
    , uapiName
    , uapiVersion
    , uapiAPIKeySelectionExpression
    , uapiRouteSelectionExpression
    , uapiDisableSchemaValidation
    , uapiDescription
    , uapiAPIId

    -- * Destructuring the Response
    , updateAPIResponse
    , UpdateAPIResponse
    -- * Response Lenses
    , uarsAPIId
    , uarsAPIEndpoint
    , uarsWarnings
    , uarsCreatedDate
    , uarsName
    , uarsVersion
    , uarsAPIKeySelectionExpression
    , uarsRouteSelectionExpression
    , uarsDisableSchemaValidation
    , uarsDescription
    , uarsProtocolType
    , uarsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateAPI' smart constructor.
data UpdateAPI = UpdateAPI'
  { _uapiName :: !(Maybe Text)
  , _uapiVersion :: !(Maybe Text)
  , _uapiAPIKeySelectionExpression :: !(Maybe Text)
  , _uapiRouteSelectionExpression :: !(Maybe Text)
  , _uapiDisableSchemaValidation :: !(Maybe Bool)
  , _uapiDescription :: !(Maybe Text)
  , _uapiAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAPI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uapiName' - The name of the API.
--
-- * 'uapiVersion' - A version identifier for the API.
--
-- * 'uapiAPIKeySelectionExpression' - An API key selection expression. See <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions> .
--
-- * 'uapiRouteSelectionExpression' - The route selection expression for the API.
--
-- * 'uapiDisableSchemaValidation' - Avoid validating models when creating a deployment.
--
-- * 'uapiDescription' - The description of the API.
--
-- * 'uapiAPIId' - The API identifier.
updateAPI
    :: Text -- ^ 'uapiAPIId'
    -> UpdateAPI
updateAPI pAPIId_ =
  UpdateAPI'
    { _uapiName = Nothing
    , _uapiVersion = Nothing
    , _uapiAPIKeySelectionExpression = Nothing
    , _uapiRouteSelectionExpression = Nothing
    , _uapiDisableSchemaValidation = Nothing
    , _uapiDescription = Nothing
    , _uapiAPIId = pAPIId_
    }


-- | The name of the API.
uapiName :: Lens' UpdateAPI (Maybe Text)
uapiName = lens _uapiName (\ s a -> s{_uapiName = a})

-- | A version identifier for the API.
uapiVersion :: Lens' UpdateAPI (Maybe Text)
uapiVersion = lens _uapiVersion (\ s a -> s{_uapiVersion = a})

-- | An API key selection expression. See <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions> .
uapiAPIKeySelectionExpression :: Lens' UpdateAPI (Maybe Text)
uapiAPIKeySelectionExpression = lens _uapiAPIKeySelectionExpression (\ s a -> s{_uapiAPIKeySelectionExpression = a})

-- | The route selection expression for the API.
uapiRouteSelectionExpression :: Lens' UpdateAPI (Maybe Text)
uapiRouteSelectionExpression = lens _uapiRouteSelectionExpression (\ s a -> s{_uapiRouteSelectionExpression = a})

-- | Avoid validating models when creating a deployment.
uapiDisableSchemaValidation :: Lens' UpdateAPI (Maybe Bool)
uapiDisableSchemaValidation = lens _uapiDisableSchemaValidation (\ s a -> s{_uapiDisableSchemaValidation = a})

-- | The description of the API.
uapiDescription :: Lens' UpdateAPI (Maybe Text)
uapiDescription = lens _uapiDescription (\ s a -> s{_uapiDescription = a})

-- | The API identifier.
uapiAPIId :: Lens' UpdateAPI Text
uapiAPIId = lens _uapiAPIId (\ s a -> s{_uapiAPIId = a})

instance AWSRequest UpdateAPI where
        type Rs UpdateAPI = UpdateAPIResponse
        request = patchJSON apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 UpdateAPIResponse' <$>
                   (x .?> "apiId") <*> (x .?> "apiEndpoint") <*>
                     (x .?> "warnings" .!@ mempty)
                     <*> (x .?> "createdDate")
                     <*> (x .?> "name")
                     <*> (x .?> "version")
                     <*> (x .?> "apiKeySelectionExpression")
                     <*> (x .?> "routeSelectionExpression")
                     <*> (x .?> "disableSchemaValidation")
                     <*> (x .?> "description")
                     <*> (x .?> "protocolType")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateAPI where

instance NFData UpdateAPI where

instance ToHeaders UpdateAPI where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateAPI where
        toJSON UpdateAPI'{..}
          = object
              (catMaybes
                 [("name" .=) <$> _uapiName,
                  ("version" .=) <$> _uapiVersion,
                  ("apiKeySelectionExpression" .=) <$>
                    _uapiAPIKeySelectionExpression,
                  ("routeSelectionExpression" .=) <$>
                    _uapiRouteSelectionExpression,
                  ("disableSchemaValidation" .=) <$>
                    _uapiDisableSchemaValidation,
                  ("description" .=) <$> _uapiDescription])

instance ToPath UpdateAPI where
        toPath UpdateAPI'{..}
          = mconcat ["/v2/apis/", toBS _uapiAPIId]

instance ToQuery UpdateAPI where
        toQuery = const mempty

-- | /See:/ 'updateAPIResponse' smart constructor.
data UpdateAPIResponse = UpdateAPIResponse'
  { _uarsAPIId :: !(Maybe Text)
  , _uarsAPIEndpoint :: !(Maybe Text)
  , _uarsWarnings :: !(Maybe [Text])
  , _uarsCreatedDate :: !(Maybe POSIX)
  , _uarsName :: !(Maybe Text)
  , _uarsVersion :: !(Maybe Text)
  , _uarsAPIKeySelectionExpression :: !(Maybe Text)
  , _uarsRouteSelectionExpression :: !(Maybe Text)
  , _uarsDisableSchemaValidation :: !(Maybe Bool)
  , _uarsDescription :: !(Maybe Text)
  , _uarsProtocolType :: !(Maybe ProtocolType)
  , _uarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAPIResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uarsAPIId' - The API ID.
--
-- * 'uarsAPIEndpoint' - The URI of the API, of the form {api-id}.execute-api.{region}.amazonaws.com. The stage name is typically appended to this URI to form a complete path to a deployed API stage.
--
-- * 'uarsWarnings' - The warning messages reported when failonwarnings is turned on during API import.
--
-- * 'uarsCreatedDate' - The timestamp when the API was created.
--
-- * 'uarsName' - The name of the API.
--
-- * 'uarsVersion' - A version identifier for the API.
--
-- * 'uarsAPIKeySelectionExpression' - An API key selection expression. See <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions> .
--
-- * 'uarsRouteSelectionExpression' - The route selection expression for the API.
--
-- * 'uarsDisableSchemaValidation' - Avoid validating models when creating a deployment.
--
-- * 'uarsDescription' - The description of the API.
--
-- * 'uarsProtocolType' - The API protocol: HTTP or WEBSOCKET.
--
-- * 'uarsResponseStatus' - -- | The response status code.
updateAPIResponse
    :: Int -- ^ 'uarsResponseStatus'
    -> UpdateAPIResponse
updateAPIResponse pResponseStatus_ =
  UpdateAPIResponse'
    { _uarsAPIId = Nothing
    , _uarsAPIEndpoint = Nothing
    , _uarsWarnings = Nothing
    , _uarsCreatedDate = Nothing
    , _uarsName = Nothing
    , _uarsVersion = Nothing
    , _uarsAPIKeySelectionExpression = Nothing
    , _uarsRouteSelectionExpression = Nothing
    , _uarsDisableSchemaValidation = Nothing
    , _uarsDescription = Nothing
    , _uarsProtocolType = Nothing
    , _uarsResponseStatus = pResponseStatus_
    }


-- | The API ID.
uarsAPIId :: Lens' UpdateAPIResponse (Maybe Text)
uarsAPIId = lens _uarsAPIId (\ s a -> s{_uarsAPIId = a})

-- | The URI of the API, of the form {api-id}.execute-api.{region}.amazonaws.com. The stage name is typically appended to this URI to form a complete path to a deployed API stage.
uarsAPIEndpoint :: Lens' UpdateAPIResponse (Maybe Text)
uarsAPIEndpoint = lens _uarsAPIEndpoint (\ s a -> s{_uarsAPIEndpoint = a})

-- | The warning messages reported when failonwarnings is turned on during API import.
uarsWarnings :: Lens' UpdateAPIResponse [Text]
uarsWarnings = lens _uarsWarnings (\ s a -> s{_uarsWarnings = a}) . _Default . _Coerce

-- | The timestamp when the API was created.
uarsCreatedDate :: Lens' UpdateAPIResponse (Maybe UTCTime)
uarsCreatedDate = lens _uarsCreatedDate (\ s a -> s{_uarsCreatedDate = a}) . mapping _Time

-- | The name of the API.
uarsName :: Lens' UpdateAPIResponse (Maybe Text)
uarsName = lens _uarsName (\ s a -> s{_uarsName = a})

-- | A version identifier for the API.
uarsVersion :: Lens' UpdateAPIResponse (Maybe Text)
uarsVersion = lens _uarsVersion (\ s a -> s{_uarsVersion = a})

-- | An API key selection expression. See <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions> .
uarsAPIKeySelectionExpression :: Lens' UpdateAPIResponse (Maybe Text)
uarsAPIKeySelectionExpression = lens _uarsAPIKeySelectionExpression (\ s a -> s{_uarsAPIKeySelectionExpression = a})

-- | The route selection expression for the API.
uarsRouteSelectionExpression :: Lens' UpdateAPIResponse (Maybe Text)
uarsRouteSelectionExpression = lens _uarsRouteSelectionExpression (\ s a -> s{_uarsRouteSelectionExpression = a})

-- | Avoid validating models when creating a deployment.
uarsDisableSchemaValidation :: Lens' UpdateAPIResponse (Maybe Bool)
uarsDisableSchemaValidation = lens _uarsDisableSchemaValidation (\ s a -> s{_uarsDisableSchemaValidation = a})

-- | The description of the API.
uarsDescription :: Lens' UpdateAPIResponse (Maybe Text)
uarsDescription = lens _uarsDescription (\ s a -> s{_uarsDescription = a})

-- | The API protocol: HTTP or WEBSOCKET.
uarsProtocolType :: Lens' UpdateAPIResponse (Maybe ProtocolType)
uarsProtocolType = lens _uarsProtocolType (\ s a -> s{_uarsProtocolType = a})

-- | -- | The response status code.
uarsResponseStatus :: Lens' UpdateAPIResponse Int
uarsResponseStatus = lens _uarsResponseStatus (\ s a -> s{_uarsResponseStatus = a})

instance NFData UpdateAPIResponse where
