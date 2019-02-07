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
-- Module      : Network.AWS.APIGatewayV2.UpdateIntegration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Integration.
--
--
module Network.AWS.APIGatewayV2.UpdateIntegration
    (
    -- * Creating a Request
      updateIntegration
    , UpdateIntegration
    -- * Request Lenses
    , updRequestTemplates
    , updCredentialsARN
    , updIntegrationURI
    , updRequestParameters
    , updConnectionId
    , updPassthroughBehavior
    , updIntegrationMethod
    , updTemplateSelectionExpression
    , updTimeoutInMillis
    , updContentHandlingStrategy
    , updIntegrationType
    , updDescription
    , updConnectionType
    , updAPIId
    , updIntegrationId

    -- * Destructuring the Response
    , updateIntegrationResponse'
    , UpdateIntegrationResponse'
    -- * Response Lenses
    , updrsIntegrationResponseSelectionExpression
    , updrsRequestTemplates
    , updrsCredentialsARN
    , updrsIntegrationURI
    , updrsIntegrationId
    , updrsRequestParameters
    , updrsConnectionId
    , updrsPassthroughBehavior
    , updrsIntegrationMethod
    , updrsTemplateSelectionExpression
    , updrsTimeoutInMillis
    , updrsContentHandlingStrategy
    , updrsIntegrationType
    , updrsDescription
    , updrsConnectionType
    , updrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateIntegration' smart constructor.
data UpdateIntegration = UpdateIntegration'
  { _updRequestTemplates :: !(Maybe (Map Text Text))
  , _updCredentialsARN :: !(Maybe Text)
  , _updIntegrationURI :: !(Maybe Text)
  , _updRequestParameters :: !(Maybe (Map Text Text))
  , _updConnectionId :: !(Maybe Text)
  , _updPassthroughBehavior :: !(Maybe PassthroughBehavior)
  , _updIntegrationMethod :: !(Maybe Text)
  , _updTemplateSelectionExpression :: !(Maybe Text)
  , _updTimeoutInMillis :: !(Maybe Nat)
  , _updContentHandlingStrategy :: !(Maybe ContentHandlingStrategy)
  , _updIntegrationType :: !(Maybe IntegrationType)
  , _updDescription :: !(Maybe Text)
  , _updConnectionType :: !(Maybe ConnectionType)
  , _updAPIId :: !Text
  , _updIntegrationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateIntegration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'updRequestTemplates' - Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
--
-- * 'updCredentialsARN' - Specifies the credentials required for the integration, if any. For AWS integrations, three options are available. To specify an IAM Role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To require that the caller's identity be passed through from the request, specify the string arn:aws:iam::*:user/*. To use resource-based permissions on supported AWS services, specify null.
--
-- * 'updIntegrationURI' - Specifies the Uniform Resource Identifier (URI) of the integration endpoint. For HTTP or HTTP_PROXY integrations, the URI must be a fully formed, encoded HTTP(S) URL according to the <https://en.wikipedia.org/wiki/Uniform_Resource_Identifier RFC-3986 specification> , for either standard integration, where connectionType is not VPC_LINK, or private integration, where connectionType is VPC_LINK. For a private HTTP integration, the URI is not used for routing. For AWS or AWS_PROXY integrations, the URI is of the form arn:aws:apigateway:{region}:{subdomain.service|service}:path|action/{service_api}. Here, {Region} is the API Gateway region (e.g., us-east-1); {service} is the name of the integrated AWS service (e.g., s3); and {subdomain} is a designated subdomain supported by certain AWS service for fast host-name lookup. action can be used for an AWS service action-based API, using an Action={name}&{p1}={v1}&p2={v2}... query string. The ensuing {service_api} refers to a supported action {name} plus any required input parameters. Alternatively, path can be used for an AWS service path-based API. The ensuing service_api refers to the path to an AWS service resource, including the region of the integrated AWS service, if applicable. For example, for integration with the S3 API of GetObject, the URI can be either arn:aws:apigateway:us-west-2:s3:action/GetObject&Bucket={bucket}&Key={key} or arn:aws:apigateway:us-west-2:s3:path/{bucket}/{key}
--
-- * 'updRequestParameters' - A key-value map specifying request parameters that are passed from the method request to the backend. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the backend. The method request parameter value must match the pattern of method.request.{location}.{name} , where  {location}  is querystring, path, or header; and  {name}  must be a valid and unique method request parameter name.
--
-- * 'updConnectionId' - The identifier of the VpcLink used for the integration when the connectionType is VPC_LINK; otherwise undefined.
--
-- * 'updPassthroughBehavior' - Specifies the pass-through behavior for incoming requests based on the Content-Type header in the request, and the available mapping templates specified as the requestTemplates property on the Integration resource. There are three valid values: WHEN_NO_MATCH, WHEN_NO_TEMPLATES, and NEVER. WHEN_NO_MATCH passes the request body for unmapped content types through to the integration backend without transformation. NEVER rejects unmapped content types with an HTTP 415 Unsupported Media Type response. WHEN_NO_TEMPLATES allows pass-through when the integration has no content types mapped to templates. However, if there is at least one content type defined, unmapped content types will be rejected with the same HTTP 415 Unsupported Media Type response.
--
-- * 'updIntegrationMethod' - Specifies the integration's HTTP method type.
--
-- * 'updTemplateSelectionExpression' - The template selection expression for the integration.
--
-- * 'updTimeoutInMillis' - Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
--
-- * 'updContentHandlingStrategy' - Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
--
-- * 'updIntegrationType' - The integration type of an integration. One of the following: AWS: for integrating the route or method request with an AWS service action, including the Lambda function-invoking action. With the Lambda function-invoking action, this is referred to as the Lambda custom integration. With any other AWS service action, this is known as AWS integration. AWS_PROXY: for integrating the route or method request with the Lambda function-invoking action with the client request passed through as-is. This integration is also referred to as Lambda proxy integration. HTTP: for integrating the route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC. This integration is also referred to as the HTTP custom integration. HTTP_PROXY: for integrating route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC, with the client request passed through as-is. This is also referred to as HTTP proxy integration. MOCK: for integrating the route or method request with API Gateway as a "loopback" endpoint without invoking any backend.
--
-- * 'updDescription' - The description of the integration
--
-- * 'updConnectionType' - The type of the network connection to the integration endpoint. The valid value is INTERNET for connections through the public routable internet or VPC_LINK for private connections between API Gateway and a network load balancer in a VPC. The default value is INTERNET.
--
-- * 'updAPIId' - The API identifier.
--
-- * 'updIntegrationId' - The integration ID.
updateIntegration
    :: Text -- ^ 'updAPIId'
    -> Text -- ^ 'updIntegrationId'
    -> UpdateIntegration
updateIntegration pAPIId_ pIntegrationId_ =
  UpdateIntegration'
    { _updRequestTemplates = Nothing
    , _updCredentialsARN = Nothing
    , _updIntegrationURI = Nothing
    , _updRequestParameters = Nothing
    , _updConnectionId = Nothing
    , _updPassthroughBehavior = Nothing
    , _updIntegrationMethod = Nothing
    , _updTemplateSelectionExpression = Nothing
    , _updTimeoutInMillis = Nothing
    , _updContentHandlingStrategy = Nothing
    , _updIntegrationType = Nothing
    , _updDescription = Nothing
    , _updConnectionType = Nothing
    , _updAPIId = pAPIId_
    , _updIntegrationId = pIntegrationId_
    }


-- | Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
updRequestTemplates :: Lens' UpdateIntegration (HashMap Text Text)
updRequestTemplates = lens _updRequestTemplates (\ s a -> s{_updRequestTemplates = a}) . _Default . _Map

-- | Specifies the credentials required for the integration, if any. For AWS integrations, three options are available. To specify an IAM Role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To require that the caller's identity be passed through from the request, specify the string arn:aws:iam::*:user/*. To use resource-based permissions on supported AWS services, specify null.
updCredentialsARN :: Lens' UpdateIntegration (Maybe Text)
updCredentialsARN = lens _updCredentialsARN (\ s a -> s{_updCredentialsARN = a})

-- | Specifies the Uniform Resource Identifier (URI) of the integration endpoint. For HTTP or HTTP_PROXY integrations, the URI must be a fully formed, encoded HTTP(S) URL according to the <https://en.wikipedia.org/wiki/Uniform_Resource_Identifier RFC-3986 specification> , for either standard integration, where connectionType is not VPC_LINK, or private integration, where connectionType is VPC_LINK. For a private HTTP integration, the URI is not used for routing. For AWS or AWS_PROXY integrations, the URI is of the form arn:aws:apigateway:{region}:{subdomain.service|service}:path|action/{service_api}. Here, {Region} is the API Gateway region (e.g., us-east-1); {service} is the name of the integrated AWS service (e.g., s3); and {subdomain} is a designated subdomain supported by certain AWS service for fast host-name lookup. action can be used for an AWS service action-based API, using an Action={name}&{p1}={v1}&p2={v2}... query string. The ensuing {service_api} refers to a supported action {name} plus any required input parameters. Alternatively, path can be used for an AWS service path-based API. The ensuing service_api refers to the path to an AWS service resource, including the region of the integrated AWS service, if applicable. For example, for integration with the S3 API of GetObject, the URI can be either arn:aws:apigateway:us-west-2:s3:action/GetObject&Bucket={bucket}&Key={key} or arn:aws:apigateway:us-west-2:s3:path/{bucket}/{key}
updIntegrationURI :: Lens' UpdateIntegration (Maybe Text)
updIntegrationURI = lens _updIntegrationURI (\ s a -> s{_updIntegrationURI = a})

-- | A key-value map specifying request parameters that are passed from the method request to the backend. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the backend. The method request parameter value must match the pattern of method.request.{location}.{name} , where  {location}  is querystring, path, or header; and  {name}  must be a valid and unique method request parameter name.
updRequestParameters :: Lens' UpdateIntegration (HashMap Text Text)
updRequestParameters = lens _updRequestParameters (\ s a -> s{_updRequestParameters = a}) . _Default . _Map

-- | The identifier of the VpcLink used for the integration when the connectionType is VPC_LINK; otherwise undefined.
updConnectionId :: Lens' UpdateIntegration (Maybe Text)
updConnectionId = lens _updConnectionId (\ s a -> s{_updConnectionId = a})

-- | Specifies the pass-through behavior for incoming requests based on the Content-Type header in the request, and the available mapping templates specified as the requestTemplates property on the Integration resource. There are three valid values: WHEN_NO_MATCH, WHEN_NO_TEMPLATES, and NEVER. WHEN_NO_MATCH passes the request body for unmapped content types through to the integration backend without transformation. NEVER rejects unmapped content types with an HTTP 415 Unsupported Media Type response. WHEN_NO_TEMPLATES allows pass-through when the integration has no content types mapped to templates. However, if there is at least one content type defined, unmapped content types will be rejected with the same HTTP 415 Unsupported Media Type response.
updPassthroughBehavior :: Lens' UpdateIntegration (Maybe PassthroughBehavior)
updPassthroughBehavior = lens _updPassthroughBehavior (\ s a -> s{_updPassthroughBehavior = a})

-- | Specifies the integration's HTTP method type.
updIntegrationMethod :: Lens' UpdateIntegration (Maybe Text)
updIntegrationMethod = lens _updIntegrationMethod (\ s a -> s{_updIntegrationMethod = a})

-- | The template selection expression for the integration.
updTemplateSelectionExpression :: Lens' UpdateIntegration (Maybe Text)
updTemplateSelectionExpression = lens _updTemplateSelectionExpression (\ s a -> s{_updTemplateSelectionExpression = a})

-- | Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
updTimeoutInMillis :: Lens' UpdateIntegration (Maybe Natural)
updTimeoutInMillis = lens _updTimeoutInMillis (\ s a -> s{_updTimeoutInMillis = a}) . mapping _Nat

-- | Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
updContentHandlingStrategy :: Lens' UpdateIntegration (Maybe ContentHandlingStrategy)
updContentHandlingStrategy = lens _updContentHandlingStrategy (\ s a -> s{_updContentHandlingStrategy = a})

-- | The integration type of an integration. One of the following: AWS: for integrating the route or method request with an AWS service action, including the Lambda function-invoking action. With the Lambda function-invoking action, this is referred to as the Lambda custom integration. With any other AWS service action, this is known as AWS integration. AWS_PROXY: for integrating the route or method request with the Lambda function-invoking action with the client request passed through as-is. This integration is also referred to as Lambda proxy integration. HTTP: for integrating the route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC. This integration is also referred to as the HTTP custom integration. HTTP_PROXY: for integrating route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC, with the client request passed through as-is. This is also referred to as HTTP proxy integration. MOCK: for integrating the route or method request with API Gateway as a "loopback" endpoint without invoking any backend.
updIntegrationType :: Lens' UpdateIntegration (Maybe IntegrationType)
updIntegrationType = lens _updIntegrationType (\ s a -> s{_updIntegrationType = a})

-- | The description of the integration
updDescription :: Lens' UpdateIntegration (Maybe Text)
updDescription = lens _updDescription (\ s a -> s{_updDescription = a})

-- | The type of the network connection to the integration endpoint. The valid value is INTERNET for connections through the public routable internet or VPC_LINK for private connections between API Gateway and a network load balancer in a VPC. The default value is INTERNET.
updConnectionType :: Lens' UpdateIntegration (Maybe ConnectionType)
updConnectionType = lens _updConnectionType (\ s a -> s{_updConnectionType = a})

-- | The API identifier.
updAPIId :: Lens' UpdateIntegration Text
updAPIId = lens _updAPIId (\ s a -> s{_updAPIId = a})

-- | The integration ID.
updIntegrationId :: Lens' UpdateIntegration Text
updIntegrationId = lens _updIntegrationId (\ s a -> s{_updIntegrationId = a})

instance AWSRequest UpdateIntegration where
        type Rs UpdateIntegration =
             UpdateIntegrationResponse'
        request = patchJSON apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 UpdateIntegrationResponse'' <$>
                   (x .?> "integrationResponseSelectionExpression") <*>
                     (x .?> "requestTemplates" .!@ mempty)
                     <*> (x .?> "credentialsArn")
                     <*> (x .?> "integrationUri")
                     <*> (x .?> "integrationId")
                     <*> (x .?> "requestParameters" .!@ mempty)
                     <*> (x .?> "connectionId")
                     <*> (x .?> "passthroughBehavior")
                     <*> (x .?> "integrationMethod")
                     <*> (x .?> "templateSelectionExpression")
                     <*> (x .?> "timeoutInMillis")
                     <*> (x .?> "contentHandlingStrategy")
                     <*> (x .?> "integrationType")
                     <*> (x .?> "description")
                     <*> (x .?> "connectionType")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateIntegration where

instance NFData UpdateIntegration where

instance ToHeaders UpdateIntegration where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateIntegration where
        toJSON UpdateIntegration'{..}
          = object
              (catMaybes
                 [("requestTemplates" .=) <$> _updRequestTemplates,
                  ("credentialsArn" .=) <$> _updCredentialsARN,
                  ("integrationUri" .=) <$> _updIntegrationURI,
                  ("requestParameters" .=) <$> _updRequestParameters,
                  ("connectionId" .=) <$> _updConnectionId,
                  ("passthroughBehavior" .=) <$>
                    _updPassthroughBehavior,
                  ("integrationMethod" .=) <$> _updIntegrationMethod,
                  ("templateSelectionExpression" .=) <$>
                    _updTemplateSelectionExpression,
                  ("timeoutInMillis" .=) <$> _updTimeoutInMillis,
                  ("contentHandlingStrategy" .=) <$>
                    _updContentHandlingStrategy,
                  ("integrationType" .=) <$> _updIntegrationType,
                  ("description" .=) <$> _updDescription,
                  ("connectionType" .=) <$> _updConnectionType])

instance ToPath UpdateIntegration where
        toPath UpdateIntegration'{..}
          = mconcat
              ["/v2/apis/", toBS _updAPIId, "/integrations/",
               toBS _updIntegrationId]

instance ToQuery UpdateIntegration where
        toQuery = const mempty

-- | /See:/ 'updateIntegrationResponse'' smart constructor.
data UpdateIntegrationResponse' = UpdateIntegrationResponse''
  { _updrsIntegrationResponseSelectionExpression :: !(Maybe Text)
  , _updrsRequestTemplates :: !(Maybe (Map Text Text))
  , _updrsCredentialsARN :: !(Maybe Text)
  , _updrsIntegrationURI :: !(Maybe Text)
  , _updrsIntegrationId :: !(Maybe Text)
  , _updrsRequestParameters :: !(Maybe (Map Text Text))
  , _updrsConnectionId :: !(Maybe Text)
  , _updrsPassthroughBehavior :: !(Maybe PassthroughBehavior)
  , _updrsIntegrationMethod :: !(Maybe Text)
  , _updrsTemplateSelectionExpression :: !(Maybe Text)
  , _updrsTimeoutInMillis :: !(Maybe Nat)
  , _updrsContentHandlingStrategy :: !(Maybe ContentHandlingStrategy)
  , _updrsIntegrationType :: !(Maybe IntegrationType)
  , _updrsDescription :: !(Maybe Text)
  , _updrsConnectionType :: !(Maybe ConnectionType)
  , _updrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateIntegrationResponse'' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'updrsIntegrationResponseSelectionExpression' - 
--
-- * 'updrsRequestTemplates' - Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
--
-- * 'updrsCredentialsARN' - Specifies the credentials required for the integration, if any. For AWS integrations, three options are available. To specify an IAM Role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To require that the caller's identity be passed through from the request, specify the string arn:aws:iam::*:user/*. To use resource-based permissions on supported AWS services, specify null.
--
-- * 'updrsIntegrationURI' - Specifies the Uniform Resource Identifier (URI) of the integration endpoint. For HTTP or HTTP_PROXY integrations, the URI must be a fully formed, encoded HTTP(S) URL according to the <https://en.wikipedia.org/wiki/Uniform_Resource_Identifier RFC-3986 specification> , for either standard integration, where connectionType is not VPC_LINK, or private integration, where connectionType is VPC_LINK. For a private HTTP integration, the URI is not used for routing. For AWS or AWS_PROXY integrations, the URI is of the form arn:aws:apigateway:{region}:{subdomain.service|service}:path|action/{service_api}. Here, {Region} is the API Gateway region (e.g., us-east-1); {service} is the name of the integrated AWS service (e.g., s3); and {subdomain} is a designated subdomain supported by certain AWS service for fast host-name lookup. action can be used for an AWS service action-based API, using an Action={name}&{p1}={v1}&p2={v2}... query string. The ensuing {service_api} refers to a supported action {name} plus any required input parameters. Alternatively, path can be used for an AWS service path-based API. The ensuing service_api refers to the path to an AWS service resource, including the region of the integrated AWS service, if applicable. For example, for integration with the S3 API of GetObject, the URI can be either arn:aws:apigateway:us-west-2:s3:action/GetObject&Bucket={bucket}&Key={key} or arn:aws:apigateway:us-west-2:s3:path/{bucket}/{key}
--
-- * 'updrsIntegrationId' - Represents the identifier of an integration.
--
-- * 'updrsRequestParameters' - A key-value map specifying request parameters that are passed from the method request to the backend. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the backend. The method request parameter value must match the pattern of method.request.{location}.{name} , where  {location}  is querystring, path, or header; and  {name}  must be a valid and unique method request parameter name.
--
-- * 'updrsConnectionId' - The identifier of the VpcLink used for the integration when the connectionType is VPC_LINK; otherwise undefined.
--
-- * 'updrsPassthroughBehavior' - Specifies the pass-through behavior for incoming requests based on the Content-Type header in the request, and the available mapping templates specified as the requestTemplates property on the Integration resource. There are three valid values: WHEN_NO_MATCH, WHEN_NO_TEMPLATES, and NEVER. WHEN_NO_MATCH passes the request body for unmapped content types through to the integration backend without transformation. NEVER rejects unmapped content types with an HTTP 415 Unsupported Media Type response. WHEN_NO_TEMPLATES allows pass-through when the integration has no content types mapped to templates. However, if there is at least one content type defined, unmapped content types will be rejected with the same HTTP 415 Unsupported Media Type response.
--
-- * 'updrsIntegrationMethod' - Specifies the integration's HTTP method type.
--
-- * 'updrsTemplateSelectionExpression' - The template selection expression for the integration.
--
-- * 'updrsTimeoutInMillis' - Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
--
-- * 'updrsContentHandlingStrategy' - Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
--
-- * 'updrsIntegrationType' - The integration type of an integration. One of the following: AWS: for integrating the route or method request with an AWS service action, including the Lambda function-invoking action. With the Lambda function-invoking action, this is referred to as the Lambda custom integration. With any other AWS service action, this is known as AWS integration. AWS_PROXY: for integrating the route or method request with the Lambda function-invoking action with the client request passed through as-is. This integration is also referred to as Lambda proxy integration. HTTP: for integrating the route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC. This integration is also referred to as the HTTP custom integration. HTTP_PROXY: for integrating route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC, with the client request passed through as-is. This is also referred to as HTTP proxy integration. MOCK: for integrating the route or method request with API Gateway as a "loopback" endpoint without invoking any backend.
--
-- * 'updrsDescription' - Represents the description of an integration.
--
-- * 'updrsConnectionType' - The type of the network connection to the integration endpoint. The valid value is INTERNET for connections through the public routable internet or VPC_LINK for private connections between API Gateway and a network load balancer in a VPC. The default value is INTERNET.
--
-- * 'updrsResponseStatus' - -- | The response status code.
updateIntegrationResponse'
    :: Int -- ^ 'updrsResponseStatus'
    -> UpdateIntegrationResponse'
updateIntegrationResponse' pResponseStatus_ =
  UpdateIntegrationResponse''
    { _updrsIntegrationResponseSelectionExpression = Nothing
    , _updrsRequestTemplates = Nothing
    , _updrsCredentialsARN = Nothing
    , _updrsIntegrationURI = Nothing
    , _updrsIntegrationId = Nothing
    , _updrsRequestParameters = Nothing
    , _updrsConnectionId = Nothing
    , _updrsPassthroughBehavior = Nothing
    , _updrsIntegrationMethod = Nothing
    , _updrsTemplateSelectionExpression = Nothing
    , _updrsTimeoutInMillis = Nothing
    , _updrsContentHandlingStrategy = Nothing
    , _updrsIntegrationType = Nothing
    , _updrsDescription = Nothing
    , _updrsConnectionType = Nothing
    , _updrsResponseStatus = pResponseStatus_
    }


-- | 
updrsIntegrationResponseSelectionExpression :: Lens' UpdateIntegrationResponse' (Maybe Text)
updrsIntegrationResponseSelectionExpression = lens _updrsIntegrationResponseSelectionExpression (\ s a -> s{_updrsIntegrationResponseSelectionExpression = a})

-- | Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
updrsRequestTemplates :: Lens' UpdateIntegrationResponse' (HashMap Text Text)
updrsRequestTemplates = lens _updrsRequestTemplates (\ s a -> s{_updrsRequestTemplates = a}) . _Default . _Map

-- | Specifies the credentials required for the integration, if any. For AWS integrations, three options are available. To specify an IAM Role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To require that the caller's identity be passed through from the request, specify the string arn:aws:iam::*:user/*. To use resource-based permissions on supported AWS services, specify null.
updrsCredentialsARN :: Lens' UpdateIntegrationResponse' (Maybe Text)
updrsCredentialsARN = lens _updrsCredentialsARN (\ s a -> s{_updrsCredentialsARN = a})

-- | Specifies the Uniform Resource Identifier (URI) of the integration endpoint. For HTTP or HTTP_PROXY integrations, the URI must be a fully formed, encoded HTTP(S) URL according to the <https://en.wikipedia.org/wiki/Uniform_Resource_Identifier RFC-3986 specification> , for either standard integration, where connectionType is not VPC_LINK, or private integration, where connectionType is VPC_LINK. For a private HTTP integration, the URI is not used for routing. For AWS or AWS_PROXY integrations, the URI is of the form arn:aws:apigateway:{region}:{subdomain.service|service}:path|action/{service_api}. Here, {Region} is the API Gateway region (e.g., us-east-1); {service} is the name of the integrated AWS service (e.g., s3); and {subdomain} is a designated subdomain supported by certain AWS service for fast host-name lookup. action can be used for an AWS service action-based API, using an Action={name}&{p1}={v1}&p2={v2}... query string. The ensuing {service_api} refers to a supported action {name} plus any required input parameters. Alternatively, path can be used for an AWS service path-based API. The ensuing service_api refers to the path to an AWS service resource, including the region of the integrated AWS service, if applicable. For example, for integration with the S3 API of GetObject, the URI can be either arn:aws:apigateway:us-west-2:s3:action/GetObject&Bucket={bucket}&Key={key} or arn:aws:apigateway:us-west-2:s3:path/{bucket}/{key}
updrsIntegrationURI :: Lens' UpdateIntegrationResponse' (Maybe Text)
updrsIntegrationURI = lens _updrsIntegrationURI (\ s a -> s{_updrsIntegrationURI = a})

-- | Represents the identifier of an integration.
updrsIntegrationId :: Lens' UpdateIntegrationResponse' (Maybe Text)
updrsIntegrationId = lens _updrsIntegrationId (\ s a -> s{_updrsIntegrationId = a})

-- | A key-value map specifying request parameters that are passed from the method request to the backend. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the backend. The method request parameter value must match the pattern of method.request.{location}.{name} , where  {location}  is querystring, path, or header; and  {name}  must be a valid and unique method request parameter name.
updrsRequestParameters :: Lens' UpdateIntegrationResponse' (HashMap Text Text)
updrsRequestParameters = lens _updrsRequestParameters (\ s a -> s{_updrsRequestParameters = a}) . _Default . _Map

-- | The identifier of the VpcLink used for the integration when the connectionType is VPC_LINK; otherwise undefined.
updrsConnectionId :: Lens' UpdateIntegrationResponse' (Maybe Text)
updrsConnectionId = lens _updrsConnectionId (\ s a -> s{_updrsConnectionId = a})

-- | Specifies the pass-through behavior for incoming requests based on the Content-Type header in the request, and the available mapping templates specified as the requestTemplates property on the Integration resource. There are three valid values: WHEN_NO_MATCH, WHEN_NO_TEMPLATES, and NEVER. WHEN_NO_MATCH passes the request body for unmapped content types through to the integration backend without transformation. NEVER rejects unmapped content types with an HTTP 415 Unsupported Media Type response. WHEN_NO_TEMPLATES allows pass-through when the integration has no content types mapped to templates. However, if there is at least one content type defined, unmapped content types will be rejected with the same HTTP 415 Unsupported Media Type response.
updrsPassthroughBehavior :: Lens' UpdateIntegrationResponse' (Maybe PassthroughBehavior)
updrsPassthroughBehavior = lens _updrsPassthroughBehavior (\ s a -> s{_updrsPassthroughBehavior = a})

-- | Specifies the integration's HTTP method type.
updrsIntegrationMethod :: Lens' UpdateIntegrationResponse' (Maybe Text)
updrsIntegrationMethod = lens _updrsIntegrationMethod (\ s a -> s{_updrsIntegrationMethod = a})

-- | The template selection expression for the integration.
updrsTemplateSelectionExpression :: Lens' UpdateIntegrationResponse' (Maybe Text)
updrsTemplateSelectionExpression = lens _updrsTemplateSelectionExpression (\ s a -> s{_updrsTemplateSelectionExpression = a})

-- | Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
updrsTimeoutInMillis :: Lens' UpdateIntegrationResponse' (Maybe Natural)
updrsTimeoutInMillis = lens _updrsTimeoutInMillis (\ s a -> s{_updrsTimeoutInMillis = a}) . mapping _Nat

-- | Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
updrsContentHandlingStrategy :: Lens' UpdateIntegrationResponse' (Maybe ContentHandlingStrategy)
updrsContentHandlingStrategy = lens _updrsContentHandlingStrategy (\ s a -> s{_updrsContentHandlingStrategy = a})

-- | The integration type of an integration. One of the following: AWS: for integrating the route or method request with an AWS service action, including the Lambda function-invoking action. With the Lambda function-invoking action, this is referred to as the Lambda custom integration. With any other AWS service action, this is known as AWS integration. AWS_PROXY: for integrating the route or method request with the Lambda function-invoking action with the client request passed through as-is. This integration is also referred to as Lambda proxy integration. HTTP: for integrating the route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC. This integration is also referred to as the HTTP custom integration. HTTP_PROXY: for integrating route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC, with the client request passed through as-is. This is also referred to as HTTP proxy integration. MOCK: for integrating the route or method request with API Gateway as a "loopback" endpoint without invoking any backend.
updrsIntegrationType :: Lens' UpdateIntegrationResponse' (Maybe IntegrationType)
updrsIntegrationType = lens _updrsIntegrationType (\ s a -> s{_updrsIntegrationType = a})

-- | Represents the description of an integration.
updrsDescription :: Lens' UpdateIntegrationResponse' (Maybe Text)
updrsDescription = lens _updrsDescription (\ s a -> s{_updrsDescription = a})

-- | The type of the network connection to the integration endpoint. The valid value is INTERNET for connections through the public routable internet or VPC_LINK for private connections between API Gateway and a network load balancer in a VPC. The default value is INTERNET.
updrsConnectionType :: Lens' UpdateIntegrationResponse' (Maybe ConnectionType)
updrsConnectionType = lens _updrsConnectionType (\ s a -> s{_updrsConnectionType = a})

-- | -- | The response status code.
updrsResponseStatus :: Lens' UpdateIntegrationResponse' Int
updrsResponseStatus = lens _updrsResponseStatus (\ s a -> s{_updrsResponseStatus = a})

instance NFData UpdateIntegrationResponse' where
