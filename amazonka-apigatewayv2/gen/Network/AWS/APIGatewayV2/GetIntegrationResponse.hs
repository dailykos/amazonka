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
-- Module      : Network.AWS.APIGatewayV2.GetIntegrationResponse
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an IntegrationResponses.
--
--
module Network.AWS.APIGatewayV2.GetIntegrationResponse
    (
    -- * Creating a Request
      getIntegrationResponse
    , GetIntegrationResponse
    -- * Request Lenses
    , getAPIId
    , getIntegrationResponseId
    , getIntegrationId

    -- * Destructuring the Response
    , getIntegrationResponseResponse
    , GetIntegrationResponseResponse
    -- * Response Lenses
    , girrsIntegrationResponseId
    , girrsIntegrationResponseKey
    , girrsTemplateSelectionExpression
    , girrsContentHandlingStrategy
    , girrsResponseTemplates
    , girrsResponseParameters
    , girrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getIntegrationResponse' smart constructor.
data GetIntegrationResponse = GetIntegrationResponse'
  { _getAPIId :: !Text
  , _getIntegrationResponseId :: !Text
  , _getIntegrationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIntegrationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getAPIId' - The API identifier.
--
-- * 'getIntegrationResponseId' - The integration response ID.
--
-- * 'getIntegrationId' - The integration ID.
getIntegrationResponse
    :: Text -- ^ 'getAPIId'
    -> Text -- ^ 'getIntegrationResponseId'
    -> Text -- ^ 'getIntegrationId'
    -> GetIntegrationResponse
getIntegrationResponse pAPIId_ pIntegrationResponseId_ pIntegrationId_ =
  GetIntegrationResponse'
    { _getAPIId = pAPIId_
    , _getIntegrationResponseId = pIntegrationResponseId_
    , _getIntegrationId = pIntegrationId_
    }


-- | The API identifier.
getAPIId :: Lens' GetIntegrationResponse Text
getAPIId = lens _getAPIId (\ s a -> s{_getAPIId = a})

-- | The integration response ID.
getIntegrationResponseId :: Lens' GetIntegrationResponse Text
getIntegrationResponseId = lens _getIntegrationResponseId (\ s a -> s{_getIntegrationResponseId = a})

-- | The integration ID.
getIntegrationId :: Lens' GetIntegrationResponse Text
getIntegrationId = lens _getIntegrationId (\ s a -> s{_getIntegrationId = a})

instance AWSRequest GetIntegrationResponse where
        type Rs GetIntegrationResponse =
             GetIntegrationResponseResponse
        request = get apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 GetIntegrationResponseResponse' <$>
                   (x .?> "integrationResponseId") <*>
                     (x .?> "integrationResponseKey")
                     <*> (x .?> "templateSelectionExpression")
                     <*> (x .?> "contentHandlingStrategy")
                     <*> (x .?> "responseTemplates" .!@ mempty)
                     <*> (x .?> "responseParameters" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetIntegrationResponse where

instance NFData GetIntegrationResponse where

instance ToHeaders GetIntegrationResponse where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetIntegrationResponse where
        toPath GetIntegrationResponse'{..}
          = mconcat
              ["/v2/apis/", toBS _getAPIId, "/integrations/",
               toBS _getIntegrationId, "/integrationresponses/",
               toBS _getIntegrationResponseId]

instance ToQuery GetIntegrationResponse where
        toQuery = const mempty

-- | /See:/ 'getIntegrationResponseResponse' smart constructor.
data GetIntegrationResponseResponse = GetIntegrationResponseResponse'
  { _girrsIntegrationResponseId :: !(Maybe Text)
  , _girrsIntegrationResponseKey :: !(Maybe Text)
  , _girrsTemplateSelectionExpression :: !(Maybe Text)
  , _girrsContentHandlingStrategy :: !(Maybe ContentHandlingStrategy)
  , _girrsResponseTemplates :: !(Maybe (Map Text Text))
  , _girrsResponseParameters :: !(Maybe (Map Text Text))
  , _girrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIntegrationResponseResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'girrsIntegrationResponseId' - The integration response ID.
--
-- * 'girrsIntegrationResponseKey' - The integration response key.
--
-- * 'girrsTemplateSelectionExpression' - The template selection expressions for the integration response.
--
-- * 'girrsContentHandlingStrategy' - Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
--
-- * 'girrsResponseTemplates' - The collection of response templates for the integration response as a string-to-string map of key-value pairs. Response templates are represented as a key/value map, with a content-type as the key and a template as the value.
--
-- * 'girrsResponseParameters' - A key-value map specifying response parameters that are passed to the method response from the backend. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of method.response.header.{name}, where name is a valid and unique header name. The mapped non-static value must match the pattern of integration.response.header.{name} or integration.response.body.{JSON-expression}, where name is a valid and unique response header name and JSON-expression is a valid JSON expression without the $ prefix.
--
-- * 'girrsResponseStatus' - -- | The response status code.
getIntegrationResponseResponse
    :: Int -- ^ 'girrsResponseStatus'
    -> GetIntegrationResponseResponse
getIntegrationResponseResponse pResponseStatus_ =
  GetIntegrationResponseResponse'
    { _girrsIntegrationResponseId = Nothing
    , _girrsIntegrationResponseKey = Nothing
    , _girrsTemplateSelectionExpression = Nothing
    , _girrsContentHandlingStrategy = Nothing
    , _girrsResponseTemplates = Nothing
    , _girrsResponseParameters = Nothing
    , _girrsResponseStatus = pResponseStatus_
    }


-- | The integration response ID.
girrsIntegrationResponseId :: Lens' GetIntegrationResponseResponse (Maybe Text)
girrsIntegrationResponseId = lens _girrsIntegrationResponseId (\ s a -> s{_girrsIntegrationResponseId = a})

-- | The integration response key.
girrsIntegrationResponseKey :: Lens' GetIntegrationResponseResponse (Maybe Text)
girrsIntegrationResponseKey = lens _girrsIntegrationResponseKey (\ s a -> s{_girrsIntegrationResponseKey = a})

-- | The template selection expressions for the integration response.
girrsTemplateSelectionExpression :: Lens' GetIntegrationResponseResponse (Maybe Text)
girrsTemplateSelectionExpression = lens _girrsTemplateSelectionExpression (\ s a -> s{_girrsTemplateSelectionExpression = a})

-- | Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
girrsContentHandlingStrategy :: Lens' GetIntegrationResponseResponse (Maybe ContentHandlingStrategy)
girrsContentHandlingStrategy = lens _girrsContentHandlingStrategy (\ s a -> s{_girrsContentHandlingStrategy = a})

-- | The collection of response templates for the integration response as a string-to-string map of key-value pairs. Response templates are represented as a key/value map, with a content-type as the key and a template as the value.
girrsResponseTemplates :: Lens' GetIntegrationResponseResponse (HashMap Text Text)
girrsResponseTemplates = lens _girrsResponseTemplates (\ s a -> s{_girrsResponseTemplates = a}) . _Default . _Map

-- | A key-value map specifying response parameters that are passed to the method response from the backend. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of method.response.header.{name}, where name is a valid and unique header name. The mapped non-static value must match the pattern of integration.response.header.{name} or integration.response.body.{JSON-expression}, where name is a valid and unique response header name and JSON-expression is a valid JSON expression without the $ prefix.
girrsResponseParameters :: Lens' GetIntegrationResponseResponse (HashMap Text Text)
girrsResponseParameters = lens _girrsResponseParameters (\ s a -> s{_girrsResponseParameters = a}) . _Default . _Map

-- | -- | The response status code.
girrsResponseStatus :: Lens' GetIntegrationResponseResponse Int
girrsResponseStatus = lens _girrsResponseStatus (\ s a -> s{_girrsResponseStatus = a})

instance NFData GetIntegrationResponseResponse where
