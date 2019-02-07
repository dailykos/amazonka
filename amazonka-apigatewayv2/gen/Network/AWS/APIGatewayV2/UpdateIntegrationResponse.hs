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
-- Module      : Network.AWS.APIGatewayV2.UpdateIntegrationResponse
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an IntegrationResponses.
--
--
module Network.AWS.APIGatewayV2.UpdateIntegrationResponse
    (
    -- * Creating a Request
      updateIntegrationResponse
    , UpdateIntegrationResponse
    -- * Request Lenses
    , uiIntegrationResponseKey
    , uiTemplateSelectionExpression
    , uiContentHandlingStrategy
    , uiResponseTemplates
    , uiResponseParameters
    , uiAPIId
    , uiIntegrationResponseId
    , uiIntegrationId

    -- * Destructuring the Response
    , updateIntegrationResponseResponse
    , UpdateIntegrationResponseResponse
    -- * Response Lenses
    , uirrsIntegrationResponseId
    , uirrsIntegrationResponseKey
    , uirrsTemplateSelectionExpression
    , uirrsContentHandlingStrategy
    , uirrsResponseTemplates
    , uirrsResponseParameters
    , uirrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateIntegrationResponse' smart constructor.
data UpdateIntegrationResponse = UpdateIntegrationResponse'
  { _uiIntegrationResponseKey :: !(Maybe Text)
  , _uiTemplateSelectionExpression :: !(Maybe Text)
  , _uiContentHandlingStrategy :: !(Maybe ContentHandlingStrategy)
  , _uiResponseTemplates :: !(Maybe (Map Text Text))
  , _uiResponseParameters :: !(Maybe (Map Text Text))
  , _uiAPIId :: !Text
  , _uiIntegrationResponseId :: !Text
  , _uiIntegrationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateIntegrationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiIntegrationResponseKey' - The integration response key.
--
-- * 'uiTemplateSelectionExpression' - The template selection expression for the integration response.
--
-- * 'uiContentHandlingStrategy' - Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
--
-- * 'uiResponseTemplates' - The collection of response templates for the integration response as a string-to-string map of key-value pairs. Response templates are represented as a key/value map, with a content-type as the key and a template as the value.
--
-- * 'uiResponseParameters' - A key-value map specifying response parameters that are passed to the method response from the backend. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of method.response.header.{name} , where name is a valid and unique header name. The mapped non-static value must match the pattern of integration.response.header.{name}  or integration.response.body.{JSON-expression} , where  {name}  is a valid and unique response header name and  {JSON-expression}  is a valid JSON expression without the $ prefix.
--
-- * 'uiAPIId' - The API identifier.
--
-- * 'uiIntegrationResponseId' - The integration response ID.
--
-- * 'uiIntegrationId' - The integration ID.
updateIntegrationResponse
    :: Text -- ^ 'uiAPIId'
    -> Text -- ^ 'uiIntegrationResponseId'
    -> Text -- ^ 'uiIntegrationId'
    -> UpdateIntegrationResponse
updateIntegrationResponse pAPIId_ pIntegrationResponseId_ pIntegrationId_ =
  UpdateIntegrationResponse'
    { _uiIntegrationResponseKey = Nothing
    , _uiTemplateSelectionExpression = Nothing
    , _uiContentHandlingStrategy = Nothing
    , _uiResponseTemplates = Nothing
    , _uiResponseParameters = Nothing
    , _uiAPIId = pAPIId_
    , _uiIntegrationResponseId = pIntegrationResponseId_
    , _uiIntegrationId = pIntegrationId_
    }


-- | The integration response key.
uiIntegrationResponseKey :: Lens' UpdateIntegrationResponse (Maybe Text)
uiIntegrationResponseKey = lens _uiIntegrationResponseKey (\ s a -> s{_uiIntegrationResponseKey = a})

-- | The template selection expression for the integration response.
uiTemplateSelectionExpression :: Lens' UpdateIntegrationResponse (Maybe Text)
uiTemplateSelectionExpression = lens _uiTemplateSelectionExpression (\ s a -> s{_uiTemplateSelectionExpression = a})

-- | Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
uiContentHandlingStrategy :: Lens' UpdateIntegrationResponse (Maybe ContentHandlingStrategy)
uiContentHandlingStrategy = lens _uiContentHandlingStrategy (\ s a -> s{_uiContentHandlingStrategy = a})

-- | The collection of response templates for the integration response as a string-to-string map of key-value pairs. Response templates are represented as a key/value map, with a content-type as the key and a template as the value.
uiResponseTemplates :: Lens' UpdateIntegrationResponse (HashMap Text Text)
uiResponseTemplates = lens _uiResponseTemplates (\ s a -> s{_uiResponseTemplates = a}) . _Default . _Map

-- | A key-value map specifying response parameters that are passed to the method response from the backend. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of method.response.header.{name} , where name is a valid and unique header name. The mapped non-static value must match the pattern of integration.response.header.{name}  or integration.response.body.{JSON-expression} , where  {name}  is a valid and unique response header name and  {JSON-expression}  is a valid JSON expression without the $ prefix.
uiResponseParameters :: Lens' UpdateIntegrationResponse (HashMap Text Text)
uiResponseParameters = lens _uiResponseParameters (\ s a -> s{_uiResponseParameters = a}) . _Default . _Map

-- | The API identifier.
uiAPIId :: Lens' UpdateIntegrationResponse Text
uiAPIId = lens _uiAPIId (\ s a -> s{_uiAPIId = a})

-- | The integration response ID.
uiIntegrationResponseId :: Lens' UpdateIntegrationResponse Text
uiIntegrationResponseId = lens _uiIntegrationResponseId (\ s a -> s{_uiIntegrationResponseId = a})

-- | The integration ID.
uiIntegrationId :: Lens' UpdateIntegrationResponse Text
uiIntegrationId = lens _uiIntegrationId (\ s a -> s{_uiIntegrationId = a})

instance AWSRequest UpdateIntegrationResponse where
        type Rs UpdateIntegrationResponse =
             UpdateIntegrationResponseResponse
        request = patchJSON apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 UpdateIntegrationResponseResponse' <$>
                   (x .?> "integrationResponseId") <*>
                     (x .?> "integrationResponseKey")
                     <*> (x .?> "templateSelectionExpression")
                     <*> (x .?> "contentHandlingStrategy")
                     <*> (x .?> "responseTemplates" .!@ mempty)
                     <*> (x .?> "responseParameters" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable UpdateIntegrationResponse where

instance NFData UpdateIntegrationResponse where

instance ToHeaders UpdateIntegrationResponse where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateIntegrationResponse where
        toJSON UpdateIntegrationResponse'{..}
          = object
              (catMaybes
                 [("integrationResponseKey" .=) <$>
                    _uiIntegrationResponseKey,
                  ("templateSelectionExpression" .=) <$>
                    _uiTemplateSelectionExpression,
                  ("contentHandlingStrategy" .=) <$>
                    _uiContentHandlingStrategy,
                  ("responseTemplates" .=) <$> _uiResponseTemplates,
                  ("responseParameters" .=) <$> _uiResponseParameters])

instance ToPath UpdateIntegrationResponse where
        toPath UpdateIntegrationResponse'{..}
          = mconcat
              ["/v2/apis/", toBS _uiAPIId, "/integrations/",
               toBS _uiIntegrationId, "/integrationresponses/",
               toBS _uiIntegrationResponseId]

instance ToQuery UpdateIntegrationResponse where
        toQuery = const mempty

-- | /See:/ 'updateIntegrationResponseResponse' smart constructor.
data UpdateIntegrationResponseResponse = UpdateIntegrationResponseResponse'
  { _uirrsIntegrationResponseId :: !(Maybe Text)
  , _uirrsIntegrationResponseKey :: !(Maybe Text)
  , _uirrsTemplateSelectionExpression :: !(Maybe Text)
  , _uirrsContentHandlingStrategy :: !(Maybe ContentHandlingStrategy)
  , _uirrsResponseTemplates :: !(Maybe (Map Text Text))
  , _uirrsResponseParameters :: !(Maybe (Map Text Text))
  , _uirrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateIntegrationResponseResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uirrsIntegrationResponseId' - The integration response ID.
--
-- * 'uirrsIntegrationResponseKey' - The integration response key.
--
-- * 'uirrsTemplateSelectionExpression' - The template selection expressions for the integration response.
--
-- * 'uirrsContentHandlingStrategy' - Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
--
-- * 'uirrsResponseTemplates' - The collection of response templates for the integration response as a string-to-string map of key-value pairs. Response templates are represented as a key/value map, with a content-type as the key and a template as the value.
--
-- * 'uirrsResponseParameters' - A key-value map specifying response parameters that are passed to the method response from the backend. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of method.response.header.{name}, where name is a valid and unique header name. The mapped non-static value must match the pattern of integration.response.header.{name} or integration.response.body.{JSON-expression}, where name is a valid and unique response header name and JSON-expression is a valid JSON expression without the $ prefix.
--
-- * 'uirrsResponseStatus' - -- | The response status code.
updateIntegrationResponseResponse
    :: Int -- ^ 'uirrsResponseStatus'
    -> UpdateIntegrationResponseResponse
updateIntegrationResponseResponse pResponseStatus_ =
  UpdateIntegrationResponseResponse'
    { _uirrsIntegrationResponseId = Nothing
    , _uirrsIntegrationResponseKey = Nothing
    , _uirrsTemplateSelectionExpression = Nothing
    , _uirrsContentHandlingStrategy = Nothing
    , _uirrsResponseTemplates = Nothing
    , _uirrsResponseParameters = Nothing
    , _uirrsResponseStatus = pResponseStatus_
    }


-- | The integration response ID.
uirrsIntegrationResponseId :: Lens' UpdateIntegrationResponseResponse (Maybe Text)
uirrsIntegrationResponseId = lens _uirrsIntegrationResponseId (\ s a -> s{_uirrsIntegrationResponseId = a})

-- | The integration response key.
uirrsIntegrationResponseKey :: Lens' UpdateIntegrationResponseResponse (Maybe Text)
uirrsIntegrationResponseKey = lens _uirrsIntegrationResponseKey (\ s a -> s{_uirrsIntegrationResponseKey = a})

-- | The template selection expressions for the integration response.
uirrsTemplateSelectionExpression :: Lens' UpdateIntegrationResponseResponse (Maybe Text)
uirrsTemplateSelectionExpression = lens _uirrsTemplateSelectionExpression (\ s a -> s{_uirrsTemplateSelectionExpression = a})

-- | Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
uirrsContentHandlingStrategy :: Lens' UpdateIntegrationResponseResponse (Maybe ContentHandlingStrategy)
uirrsContentHandlingStrategy = lens _uirrsContentHandlingStrategy (\ s a -> s{_uirrsContentHandlingStrategy = a})

-- | The collection of response templates for the integration response as a string-to-string map of key-value pairs. Response templates are represented as a key/value map, with a content-type as the key and a template as the value.
uirrsResponseTemplates :: Lens' UpdateIntegrationResponseResponse (HashMap Text Text)
uirrsResponseTemplates = lens _uirrsResponseTemplates (\ s a -> s{_uirrsResponseTemplates = a}) . _Default . _Map

-- | A key-value map specifying response parameters that are passed to the method response from the backend. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of method.response.header.{name}, where name is a valid and unique header name. The mapped non-static value must match the pattern of integration.response.header.{name} or integration.response.body.{JSON-expression}, where name is a valid and unique response header name and JSON-expression is a valid JSON expression without the $ prefix.
uirrsResponseParameters :: Lens' UpdateIntegrationResponseResponse (HashMap Text Text)
uirrsResponseParameters = lens _uirrsResponseParameters (\ s a -> s{_uirrsResponseParameters = a}) . _Default . _Map

-- | -- | The response status code.
uirrsResponseStatus :: Lens' UpdateIntegrationResponseResponse Int
uirrsResponseStatus = lens _uirrsResponseStatus (\ s a -> s{_uirrsResponseStatus = a})

instance NFData UpdateIntegrationResponseResponse
         where
