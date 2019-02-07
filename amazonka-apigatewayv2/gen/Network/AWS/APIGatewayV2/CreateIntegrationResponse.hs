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
-- Module      : Network.AWS.APIGatewayV2.CreateIntegrationResponse
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an IntegrationResponses.
--
--
module Network.AWS.APIGatewayV2.CreateIntegrationResponse
    (
    -- * Creating a Request
      createIntegrationResponse
    , CreateIntegrationResponse
    -- * Request Lenses
    , cTemplateSelectionExpression
    , cContentHandlingStrategy
    , cResponseTemplates
    , cResponseParameters
    , cAPIId
    , cIntegrationId
    , cIntegrationResponseKey

    -- * Destructuring the Response
    , createIntegrationResponseResponse
    , CreateIntegrationResponseResponse
    -- * Response Lenses
    , cirrsIntegrationResponseId
    , cirrsIntegrationResponseKey
    , cirrsTemplateSelectionExpression
    , cirrsContentHandlingStrategy
    , cirrsResponseTemplates
    , cirrsResponseParameters
    , cirrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createIntegrationResponse' smart constructor.
data CreateIntegrationResponse = CreateIntegrationResponse'
  { _cTemplateSelectionExpression :: !(Maybe Text)
  , _cContentHandlingStrategy :: !(Maybe ContentHandlingStrategy)
  , _cResponseTemplates :: !(Maybe (Map Text Text))
  , _cResponseParameters :: !(Maybe (Map Text Text))
  , _cAPIId :: !Text
  , _cIntegrationId :: !Text
  , _cIntegrationResponseKey :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateIntegrationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cTemplateSelectionExpression' - The template selection expression for the integration response.
--
-- * 'cContentHandlingStrategy' - Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
--
-- * 'cResponseTemplates' - The collection of response templates for the integration response as a string-to-string map of key-value pairs. Response templates are represented as a key/value map, with a content-type as the key and a template as the value.
--
-- * 'cResponseParameters' - A key-value map specifying response parameters that are passed to the method response from the backend. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of method.response.header.{name}, where {name} is a valid and unique header name. The mapped non-static value must match the pattern of integration.response.header.{name} or integration.response.body.{JSON-expression}, where {name} is a valid and unique response header name and {JSON-expression} is a valid JSON expression without the $ prefix.
--
-- * 'cAPIId' - The API identifier.
--
-- * 'cIntegrationId' - The integration ID.
--
-- * 'cIntegrationResponseKey' - The integration response key.
createIntegrationResponse
    :: Text -- ^ 'cAPIId'
    -> Text -- ^ 'cIntegrationId'
    -> Text -- ^ 'cIntegrationResponseKey'
    -> CreateIntegrationResponse
createIntegrationResponse pAPIId_ pIntegrationId_ pIntegrationResponseKey_ =
  CreateIntegrationResponse'
    { _cTemplateSelectionExpression = Nothing
    , _cContentHandlingStrategy = Nothing
    , _cResponseTemplates = Nothing
    , _cResponseParameters = Nothing
    , _cAPIId = pAPIId_
    , _cIntegrationId = pIntegrationId_
    , _cIntegrationResponseKey = pIntegrationResponseKey_
    }


-- | The template selection expression for the integration response.
cTemplateSelectionExpression :: Lens' CreateIntegrationResponse (Maybe Text)
cTemplateSelectionExpression = lens _cTemplateSelectionExpression (\ s a -> s{_cTemplateSelectionExpression = a})

-- | Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
cContentHandlingStrategy :: Lens' CreateIntegrationResponse (Maybe ContentHandlingStrategy)
cContentHandlingStrategy = lens _cContentHandlingStrategy (\ s a -> s{_cContentHandlingStrategy = a})

-- | The collection of response templates for the integration response as a string-to-string map of key-value pairs. Response templates are represented as a key/value map, with a content-type as the key and a template as the value.
cResponseTemplates :: Lens' CreateIntegrationResponse (HashMap Text Text)
cResponseTemplates = lens _cResponseTemplates (\ s a -> s{_cResponseTemplates = a}) . _Default . _Map

-- | A key-value map specifying response parameters that are passed to the method response from the backend. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of method.response.header.{name}, where {name} is a valid and unique header name. The mapped non-static value must match the pattern of integration.response.header.{name} or integration.response.body.{JSON-expression}, where {name} is a valid and unique response header name and {JSON-expression} is a valid JSON expression without the $ prefix.
cResponseParameters :: Lens' CreateIntegrationResponse (HashMap Text Text)
cResponseParameters = lens _cResponseParameters (\ s a -> s{_cResponseParameters = a}) . _Default . _Map

-- | The API identifier.
cAPIId :: Lens' CreateIntegrationResponse Text
cAPIId = lens _cAPIId (\ s a -> s{_cAPIId = a})

-- | The integration ID.
cIntegrationId :: Lens' CreateIntegrationResponse Text
cIntegrationId = lens _cIntegrationId (\ s a -> s{_cIntegrationId = a})

-- | The integration response key.
cIntegrationResponseKey :: Lens' CreateIntegrationResponse Text
cIntegrationResponseKey = lens _cIntegrationResponseKey (\ s a -> s{_cIntegrationResponseKey = a})

instance AWSRequest CreateIntegrationResponse where
        type Rs CreateIntegrationResponse =
             CreateIntegrationResponseResponse
        request = postJSON apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 CreateIntegrationResponseResponse' <$>
                   (x .?> "integrationResponseId") <*>
                     (x .?> "integrationResponseKey")
                     <*> (x .?> "templateSelectionExpression")
                     <*> (x .?> "contentHandlingStrategy")
                     <*> (x .?> "responseTemplates" .!@ mempty)
                     <*> (x .?> "responseParameters" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable CreateIntegrationResponse where

instance NFData CreateIntegrationResponse where

instance ToHeaders CreateIntegrationResponse where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateIntegrationResponse where
        toJSON CreateIntegrationResponse'{..}
          = object
              (catMaybes
                 [("templateSelectionExpression" .=) <$>
                    _cTemplateSelectionExpression,
                  ("contentHandlingStrategy" .=) <$>
                    _cContentHandlingStrategy,
                  ("responseTemplates" .=) <$> _cResponseTemplates,
                  ("responseParameters" .=) <$> _cResponseParameters,
                  Just
                    ("integrationResponseKey" .=
                       _cIntegrationResponseKey)])

instance ToPath CreateIntegrationResponse where
        toPath CreateIntegrationResponse'{..}
          = mconcat
              ["/v2/apis/", toBS _cAPIId, "/integrations/",
               toBS _cIntegrationId, "/integrationresponses"]

instance ToQuery CreateIntegrationResponse where
        toQuery = const mempty

-- | /See:/ 'createIntegrationResponseResponse' smart constructor.
data CreateIntegrationResponseResponse = CreateIntegrationResponseResponse'
  { _cirrsIntegrationResponseId :: !(Maybe Text)
  , _cirrsIntegrationResponseKey :: !(Maybe Text)
  , _cirrsTemplateSelectionExpression :: !(Maybe Text)
  , _cirrsContentHandlingStrategy :: !(Maybe ContentHandlingStrategy)
  , _cirrsResponseTemplates :: !(Maybe (Map Text Text))
  , _cirrsResponseParameters :: !(Maybe (Map Text Text))
  , _cirrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateIntegrationResponseResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cirrsIntegrationResponseId' - The integration response ID.
--
-- * 'cirrsIntegrationResponseKey' - The integration response key.
--
-- * 'cirrsTemplateSelectionExpression' - The template selection expressions for the integration response.
--
-- * 'cirrsContentHandlingStrategy' - Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
--
-- * 'cirrsResponseTemplates' - The collection of response templates for the integration response as a string-to-string map of key-value pairs. Response templates are represented as a key/value map, with a content-type as the key and a template as the value.
--
-- * 'cirrsResponseParameters' - A key-value map specifying response parameters that are passed to the method response from the backend. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of method.response.header.{name}, where name is a valid and unique header name. The mapped non-static value must match the pattern of integration.response.header.{name} or integration.response.body.{JSON-expression}, where name is a valid and unique response header name and JSON-expression is a valid JSON expression without the $ prefix.
--
-- * 'cirrsResponseStatus' - -- | The response status code.
createIntegrationResponseResponse
    :: Int -- ^ 'cirrsResponseStatus'
    -> CreateIntegrationResponseResponse
createIntegrationResponseResponse pResponseStatus_ =
  CreateIntegrationResponseResponse'
    { _cirrsIntegrationResponseId = Nothing
    , _cirrsIntegrationResponseKey = Nothing
    , _cirrsTemplateSelectionExpression = Nothing
    , _cirrsContentHandlingStrategy = Nothing
    , _cirrsResponseTemplates = Nothing
    , _cirrsResponseParameters = Nothing
    , _cirrsResponseStatus = pResponseStatus_
    }


-- | The integration response ID.
cirrsIntegrationResponseId :: Lens' CreateIntegrationResponseResponse (Maybe Text)
cirrsIntegrationResponseId = lens _cirrsIntegrationResponseId (\ s a -> s{_cirrsIntegrationResponseId = a})

-- | The integration response key.
cirrsIntegrationResponseKey :: Lens' CreateIntegrationResponseResponse (Maybe Text)
cirrsIntegrationResponseKey = lens _cirrsIntegrationResponseKey (\ s a -> s{_cirrsIntegrationResponseKey = a})

-- | The template selection expressions for the integration response.
cirrsTemplateSelectionExpression :: Lens' CreateIntegrationResponseResponse (Maybe Text)
cirrsTemplateSelectionExpression = lens _cirrsTemplateSelectionExpression (\ s a -> s{_cirrsTemplateSelectionExpression = a})

-- | Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
cirrsContentHandlingStrategy :: Lens' CreateIntegrationResponseResponse (Maybe ContentHandlingStrategy)
cirrsContentHandlingStrategy = lens _cirrsContentHandlingStrategy (\ s a -> s{_cirrsContentHandlingStrategy = a})

-- | The collection of response templates for the integration response as a string-to-string map of key-value pairs. Response templates are represented as a key/value map, with a content-type as the key and a template as the value.
cirrsResponseTemplates :: Lens' CreateIntegrationResponseResponse (HashMap Text Text)
cirrsResponseTemplates = lens _cirrsResponseTemplates (\ s a -> s{_cirrsResponseTemplates = a}) . _Default . _Map

-- | A key-value map specifying response parameters that are passed to the method response from the backend. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of method.response.header.{name}, where name is a valid and unique header name. The mapped non-static value must match the pattern of integration.response.header.{name} or integration.response.body.{JSON-expression}, where name is a valid and unique response header name and JSON-expression is a valid JSON expression without the $ prefix.
cirrsResponseParameters :: Lens' CreateIntegrationResponseResponse (HashMap Text Text)
cirrsResponseParameters = lens _cirrsResponseParameters (\ s a -> s{_cirrsResponseParameters = a}) . _Default . _Map

-- | -- | The response status code.
cirrsResponseStatus :: Lens' CreateIntegrationResponseResponse Int
cirrsResponseStatus = lens _cirrsResponseStatus (\ s a -> s{_cirrsResponseStatus = a})

instance NFData CreateIntegrationResponseResponse
         where
