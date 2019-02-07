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
-- Module      : Network.AWS.APIGatewayV2.CreateAPI
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Api resource.
--
--
module Network.AWS.APIGatewayV2.CreateAPI
    (
    -- * Creating a Request
      createAPI
    , CreateAPI
    -- * Request Lenses
    , caVersion
    , caAPIKeySelectionExpression
    , caDisableSchemaValidation
    , caDescription
    , caRouteSelectionExpression
    , caProtocolType
    , caName

    -- * Destructuring the Response
    , createAPIResponse
    , CreateAPIResponse
    -- * Response Lenses
    , carsAPIId
    , carsAPIEndpoint
    , carsWarnings
    , carsCreatedDate
    , carsName
    , carsVersion
    , carsAPIKeySelectionExpression
    , carsRouteSelectionExpression
    , carsDisableSchemaValidation
    , carsDescription
    , carsProtocolType
    , carsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createAPI' smart constructor.
data CreateAPI = CreateAPI'
  { _caVersion :: !(Maybe Text)
  , _caAPIKeySelectionExpression :: !(Maybe Text)
  , _caDisableSchemaValidation :: !(Maybe Bool)
  , _caDescription :: !(Maybe Text)
  , _caRouteSelectionExpression :: !Text
  , _caProtocolType :: !ProtocolType
  , _caName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAPI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caVersion' - A version identifier for the API.
--
-- * 'caAPIKeySelectionExpression' - An API key selection expression. See <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions> .
--
-- * 'caDisableSchemaValidation' - Avoid validating models when creating a deployment.
--
-- * 'caDescription' - The description of the API.
--
-- * 'caRouteSelectionExpression' - The route selection expression for the API.
--
-- * 'caProtocolType' - The API protocol: HTTP or WEBSOCKET.
--
-- * 'caName' - The name of the API.
createAPI
    :: Text -- ^ 'caRouteSelectionExpression'
    -> ProtocolType -- ^ 'caProtocolType'
    -> Text -- ^ 'caName'
    -> CreateAPI
createAPI pRouteSelectionExpression_ pProtocolType_ pName_ =
  CreateAPI'
    { _caVersion = Nothing
    , _caAPIKeySelectionExpression = Nothing
    , _caDisableSchemaValidation = Nothing
    , _caDescription = Nothing
    , _caRouteSelectionExpression = pRouteSelectionExpression_
    , _caProtocolType = pProtocolType_
    , _caName = pName_
    }


-- | A version identifier for the API.
caVersion :: Lens' CreateAPI (Maybe Text)
caVersion = lens _caVersion (\ s a -> s{_caVersion = a})

-- | An API key selection expression. See <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions> .
caAPIKeySelectionExpression :: Lens' CreateAPI (Maybe Text)
caAPIKeySelectionExpression = lens _caAPIKeySelectionExpression (\ s a -> s{_caAPIKeySelectionExpression = a})

-- | Avoid validating models when creating a deployment.
caDisableSchemaValidation :: Lens' CreateAPI (Maybe Bool)
caDisableSchemaValidation = lens _caDisableSchemaValidation (\ s a -> s{_caDisableSchemaValidation = a})

-- | The description of the API.
caDescription :: Lens' CreateAPI (Maybe Text)
caDescription = lens _caDescription (\ s a -> s{_caDescription = a})

-- | The route selection expression for the API.
caRouteSelectionExpression :: Lens' CreateAPI Text
caRouteSelectionExpression = lens _caRouteSelectionExpression (\ s a -> s{_caRouteSelectionExpression = a})

-- | The API protocol: HTTP or WEBSOCKET.
caProtocolType :: Lens' CreateAPI ProtocolType
caProtocolType = lens _caProtocolType (\ s a -> s{_caProtocolType = a})

-- | The name of the API.
caName :: Lens' CreateAPI Text
caName = lens _caName (\ s a -> s{_caName = a})

instance AWSRequest CreateAPI where
        type Rs CreateAPI = CreateAPIResponse
        request = postJSON apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 CreateAPIResponse' <$>
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

instance Hashable CreateAPI where

instance NFData CreateAPI where

instance ToHeaders CreateAPI where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateAPI where
        toJSON CreateAPI'{..}
          = object
              (catMaybes
                 [("version" .=) <$> _caVersion,
                  ("apiKeySelectionExpression" .=) <$>
                    _caAPIKeySelectionExpression,
                  ("disableSchemaValidation" .=) <$>
                    _caDisableSchemaValidation,
                  ("description" .=) <$> _caDescription,
                  Just
                    ("routeSelectionExpression" .=
                       _caRouteSelectionExpression),
                  Just ("protocolType" .= _caProtocolType),
                  Just ("name" .= _caName)])

instance ToPath CreateAPI where
        toPath = const "/v2/apis"

instance ToQuery CreateAPI where
        toQuery = const mempty

-- | /See:/ 'createAPIResponse' smart constructor.
data CreateAPIResponse = CreateAPIResponse'
  { _carsAPIId :: !(Maybe Text)
  , _carsAPIEndpoint :: !(Maybe Text)
  , _carsWarnings :: !(Maybe [Text])
  , _carsCreatedDate :: !(Maybe POSIX)
  , _carsName :: !(Maybe Text)
  , _carsVersion :: !(Maybe Text)
  , _carsAPIKeySelectionExpression :: !(Maybe Text)
  , _carsRouteSelectionExpression :: !(Maybe Text)
  , _carsDisableSchemaValidation :: !(Maybe Bool)
  , _carsDescription :: !(Maybe Text)
  , _carsProtocolType :: !(Maybe ProtocolType)
  , _carsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAPIResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carsAPIId' - The API ID.
--
-- * 'carsAPIEndpoint' - The URI of the API, of the form {api-id}.execute-api.{region}.amazonaws.com. The stage name is typically appended to this URI to form a complete path to a deployed API stage.
--
-- * 'carsWarnings' - The warning messages reported when failonwarnings is turned on during API import.
--
-- * 'carsCreatedDate' - The timestamp when the API was created.
--
-- * 'carsName' - The name of the API.
--
-- * 'carsVersion' - A version identifier for the API.
--
-- * 'carsAPIKeySelectionExpression' - An API key selection expression. See <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions> .
--
-- * 'carsRouteSelectionExpression' - The route selection expression for the API.
--
-- * 'carsDisableSchemaValidation' - Avoid validating models when creating a deployment.
--
-- * 'carsDescription' - The description of the API.
--
-- * 'carsProtocolType' - The API protocol: HTTP or WEBSOCKET.
--
-- * 'carsResponseStatus' - -- | The response status code.
createAPIResponse
    :: Int -- ^ 'carsResponseStatus'
    -> CreateAPIResponse
createAPIResponse pResponseStatus_ =
  CreateAPIResponse'
    { _carsAPIId = Nothing
    , _carsAPIEndpoint = Nothing
    , _carsWarnings = Nothing
    , _carsCreatedDate = Nothing
    , _carsName = Nothing
    , _carsVersion = Nothing
    , _carsAPIKeySelectionExpression = Nothing
    , _carsRouteSelectionExpression = Nothing
    , _carsDisableSchemaValidation = Nothing
    , _carsDescription = Nothing
    , _carsProtocolType = Nothing
    , _carsResponseStatus = pResponseStatus_
    }


-- | The API ID.
carsAPIId :: Lens' CreateAPIResponse (Maybe Text)
carsAPIId = lens _carsAPIId (\ s a -> s{_carsAPIId = a})

-- | The URI of the API, of the form {api-id}.execute-api.{region}.amazonaws.com. The stage name is typically appended to this URI to form a complete path to a deployed API stage.
carsAPIEndpoint :: Lens' CreateAPIResponse (Maybe Text)
carsAPIEndpoint = lens _carsAPIEndpoint (\ s a -> s{_carsAPIEndpoint = a})

-- | The warning messages reported when failonwarnings is turned on during API import.
carsWarnings :: Lens' CreateAPIResponse [Text]
carsWarnings = lens _carsWarnings (\ s a -> s{_carsWarnings = a}) . _Default . _Coerce

-- | The timestamp when the API was created.
carsCreatedDate :: Lens' CreateAPIResponse (Maybe UTCTime)
carsCreatedDate = lens _carsCreatedDate (\ s a -> s{_carsCreatedDate = a}) . mapping _Time

-- | The name of the API.
carsName :: Lens' CreateAPIResponse (Maybe Text)
carsName = lens _carsName (\ s a -> s{_carsName = a})

-- | A version identifier for the API.
carsVersion :: Lens' CreateAPIResponse (Maybe Text)
carsVersion = lens _carsVersion (\ s a -> s{_carsVersion = a})

-- | An API key selection expression. See <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions> .
carsAPIKeySelectionExpression :: Lens' CreateAPIResponse (Maybe Text)
carsAPIKeySelectionExpression = lens _carsAPIKeySelectionExpression (\ s a -> s{_carsAPIKeySelectionExpression = a})

-- | The route selection expression for the API.
carsRouteSelectionExpression :: Lens' CreateAPIResponse (Maybe Text)
carsRouteSelectionExpression = lens _carsRouteSelectionExpression (\ s a -> s{_carsRouteSelectionExpression = a})

-- | Avoid validating models when creating a deployment.
carsDisableSchemaValidation :: Lens' CreateAPIResponse (Maybe Bool)
carsDisableSchemaValidation = lens _carsDisableSchemaValidation (\ s a -> s{_carsDisableSchemaValidation = a})

-- | The description of the API.
carsDescription :: Lens' CreateAPIResponse (Maybe Text)
carsDescription = lens _carsDescription (\ s a -> s{_carsDescription = a})

-- | The API protocol: HTTP or WEBSOCKET.
carsProtocolType :: Lens' CreateAPIResponse (Maybe ProtocolType)
carsProtocolType = lens _carsProtocolType (\ s a -> s{_carsProtocolType = a})

-- | -- | The response status code.
carsResponseStatus :: Lens' CreateAPIResponse Int
carsResponseStatus = lens _carsResponseStatus (\ s a -> s{_carsResponseStatus = a})

instance NFData CreateAPIResponse where
