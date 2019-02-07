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
-- Module      : Network.AWS.APIGatewayV2.GetAPI
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an Api resource.
--
--
module Network.AWS.APIGatewayV2.GetAPI
    (
    -- * Creating a Request
      getAPI
    , GetAPI
    -- * Request Lenses
    , gapiAPIId

    -- * Destructuring the Response
    , getAPIResponse
    , GetAPIResponse
    -- * Response Lenses
    , garsAPIId
    , garsAPIEndpoint
    , garsWarnings
    , garsCreatedDate
    , garsName
    , garsVersion
    , garsAPIKeySelectionExpression
    , garsRouteSelectionExpression
    , garsDisableSchemaValidation
    , garsDescription
    , garsProtocolType
    , garsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAPI' smart constructor.
newtype GetAPI = GetAPI'
  { _gapiAPIId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAPI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gapiAPIId' - The API identifier.
getAPI
    :: Text -- ^ 'gapiAPIId'
    -> GetAPI
getAPI pAPIId_ = GetAPI' {_gapiAPIId = pAPIId_}


-- | The API identifier.
gapiAPIId :: Lens' GetAPI Text
gapiAPIId = lens _gapiAPIId (\ s a -> s{_gapiAPIId = a})

instance AWSRequest GetAPI where
        type Rs GetAPI = GetAPIResponse
        request = get apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 GetAPIResponse' <$>
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

instance Hashable GetAPI where

instance NFData GetAPI where

instance ToHeaders GetAPI where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetAPI where
        toPath GetAPI'{..}
          = mconcat ["/v2/apis/", toBS _gapiAPIId]

instance ToQuery GetAPI where
        toQuery = const mempty

-- | /See:/ 'getAPIResponse' smart constructor.
data GetAPIResponse = GetAPIResponse'
  { _garsAPIId :: !(Maybe Text)
  , _garsAPIEndpoint :: !(Maybe Text)
  , _garsWarnings :: !(Maybe [Text])
  , _garsCreatedDate :: !(Maybe POSIX)
  , _garsName :: !(Maybe Text)
  , _garsVersion :: !(Maybe Text)
  , _garsAPIKeySelectionExpression :: !(Maybe Text)
  , _garsRouteSelectionExpression :: !(Maybe Text)
  , _garsDisableSchemaValidation :: !(Maybe Bool)
  , _garsDescription :: !(Maybe Text)
  , _garsProtocolType :: !(Maybe ProtocolType)
  , _garsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAPIResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'garsAPIId' - The API ID.
--
-- * 'garsAPIEndpoint' - The URI of the API, of the form {api-id}.execute-api.{region}.amazonaws.com. The stage name is typically appended to this URI to form a complete path to a deployed API stage.
--
-- * 'garsWarnings' - The warning messages reported when failonwarnings is turned on during API import.
--
-- * 'garsCreatedDate' - The timestamp when the API was created.
--
-- * 'garsName' - The name of the API.
--
-- * 'garsVersion' - A version identifier for the API.
--
-- * 'garsAPIKeySelectionExpression' - An API key selection expression. See <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions> .
--
-- * 'garsRouteSelectionExpression' - The route selection expression for the API.
--
-- * 'garsDisableSchemaValidation' - Avoid validating models when creating a deployment.
--
-- * 'garsDescription' - The description of the API.
--
-- * 'garsProtocolType' - The API protocol: HTTP or WEBSOCKET.
--
-- * 'garsResponseStatus' - -- | The response status code.
getAPIResponse
    :: Int -- ^ 'garsResponseStatus'
    -> GetAPIResponse
getAPIResponse pResponseStatus_ =
  GetAPIResponse'
    { _garsAPIId = Nothing
    , _garsAPIEndpoint = Nothing
    , _garsWarnings = Nothing
    , _garsCreatedDate = Nothing
    , _garsName = Nothing
    , _garsVersion = Nothing
    , _garsAPIKeySelectionExpression = Nothing
    , _garsRouteSelectionExpression = Nothing
    , _garsDisableSchemaValidation = Nothing
    , _garsDescription = Nothing
    , _garsProtocolType = Nothing
    , _garsResponseStatus = pResponseStatus_
    }


-- | The API ID.
garsAPIId :: Lens' GetAPIResponse (Maybe Text)
garsAPIId = lens _garsAPIId (\ s a -> s{_garsAPIId = a})

-- | The URI of the API, of the form {api-id}.execute-api.{region}.amazonaws.com. The stage name is typically appended to this URI to form a complete path to a deployed API stage.
garsAPIEndpoint :: Lens' GetAPIResponse (Maybe Text)
garsAPIEndpoint = lens _garsAPIEndpoint (\ s a -> s{_garsAPIEndpoint = a})

-- | The warning messages reported when failonwarnings is turned on during API import.
garsWarnings :: Lens' GetAPIResponse [Text]
garsWarnings = lens _garsWarnings (\ s a -> s{_garsWarnings = a}) . _Default . _Coerce

-- | The timestamp when the API was created.
garsCreatedDate :: Lens' GetAPIResponse (Maybe UTCTime)
garsCreatedDate = lens _garsCreatedDate (\ s a -> s{_garsCreatedDate = a}) . mapping _Time

-- | The name of the API.
garsName :: Lens' GetAPIResponse (Maybe Text)
garsName = lens _garsName (\ s a -> s{_garsName = a})

-- | A version identifier for the API.
garsVersion :: Lens' GetAPIResponse (Maybe Text)
garsVersion = lens _garsVersion (\ s a -> s{_garsVersion = a})

-- | An API key selection expression. See <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions> .
garsAPIKeySelectionExpression :: Lens' GetAPIResponse (Maybe Text)
garsAPIKeySelectionExpression = lens _garsAPIKeySelectionExpression (\ s a -> s{_garsAPIKeySelectionExpression = a})

-- | The route selection expression for the API.
garsRouteSelectionExpression :: Lens' GetAPIResponse (Maybe Text)
garsRouteSelectionExpression = lens _garsRouteSelectionExpression (\ s a -> s{_garsRouteSelectionExpression = a})

-- | Avoid validating models when creating a deployment.
garsDisableSchemaValidation :: Lens' GetAPIResponse (Maybe Bool)
garsDisableSchemaValidation = lens _garsDisableSchemaValidation (\ s a -> s{_garsDisableSchemaValidation = a})

-- | The description of the API.
garsDescription :: Lens' GetAPIResponse (Maybe Text)
garsDescription = lens _garsDescription (\ s a -> s{_garsDescription = a})

-- | The API protocol: HTTP or WEBSOCKET.
garsProtocolType :: Lens' GetAPIResponse (Maybe ProtocolType)
garsProtocolType = lens _garsProtocolType (\ s a -> s{_garsProtocolType = a})

-- | -- | The response status code.
garsResponseStatus :: Lens' GetAPIResponse Int
garsResponseStatus = lens _garsResponseStatus (\ s a -> s{_garsResponseStatus = a})

instance NFData GetAPIResponse where
