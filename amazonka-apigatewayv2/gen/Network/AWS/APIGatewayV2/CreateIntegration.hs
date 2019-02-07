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
-- Module      : Network.AWS.APIGatewayV2.CreateIntegration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Integration.
--
--
module Network.AWS.APIGatewayV2.CreateIntegration
    (
    -- * Creating a Request
      createIntegration
    , CreateIntegration
    -- * Request Lenses
    , ciRequestTemplates
    , ciCredentialsARN
    , ciIntegrationURI
    , ciRequestParameters
    , ciConnectionId
    , ciPassthroughBehavior
    , ciIntegrationMethod
    , ciTemplateSelectionExpression
    , ciTimeoutInMillis
    , ciContentHandlingStrategy
    , ciIntegrationType
    , ciDescription
    , ciConnectionType
    , ciAPIId

    -- * Destructuring the Response
    , createIntegrationResponse'
    , CreateIntegrationResponse'
    -- * Response Lenses
    , crersIntegrationResponseSelectionExpression
    , crersRequestTemplates
    , crersCredentialsARN
    , crersIntegrationURI
    , crersIntegrationId
    , crersRequestParameters
    , crersConnectionId
    , crersPassthroughBehavior
    , crersIntegrationMethod
    , crersTemplateSelectionExpression
    , crersTimeoutInMillis
    , crersContentHandlingStrategy
    , crersIntegrationType
    , crersDescription
    , crersConnectionType
    , crersResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createIntegration' smart constructor.
data CreateIntegration = CreateIntegration'
  { _ciRequestTemplates :: !(Maybe (Map Text Text))
  , _ciCredentialsARN :: !(Maybe Text)
  , _ciIntegrationURI :: !(Maybe Text)
  , _ciRequestParameters :: !(Maybe (Map Text Text))
  , _ciConnectionId :: !(Maybe Text)
  , _ciPassthroughBehavior :: !(Maybe PassthroughBehavior)
  , _ciIntegrationMethod :: !(Maybe Text)
  , _ciTemplateSelectionExpression :: !(Maybe Text)
  , _ciTimeoutInMillis :: !(Maybe Nat)
  , _ciContentHandlingStrategy :: !(Maybe ContentHandlingStrategy)
  , _ciIntegrationType :: !(Maybe IntegrationType)
  , _ciDescription :: !(Maybe Text)
  , _ciConnectionType :: !(Maybe ConnectionType)
  , _ciAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateIntegration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciRequestTemplates' - Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
--
-- * 'ciCredentialsARN' - Specifies the credentials required for the integration, if any. For AWS integrations, three options are available. To specify an IAM Role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To require that the caller's identity be passed through from the request, specify the string arn:aws:iam::*:user/*. To use resource-based permissions on supported AWS services, specify null.
--
-- * 'ciIntegrationURI' - Specifies the Uniform Resource Identifier (URI) of the integration endpoint. For HTTP or HTTP_PROXY integrations, the URI must be a fully formed, encoded HTTP(S) URL according to the <https://en.wikipedia.org/wiki/Uniform_Resource_Identifier RFC-3986 specification> , for either standard integration, where connectionType is not VPC_LINK, or private integration, where connectionType is VPC_LINK. For a private HTTP integration, the URI is not used for routing. For AWS or AWS_PROXY integrations, the URI is of the form arn:aws:apigateway:{region}:{subdomain.service|service}:path|action/{service_api}. Here, {Region} is the API Gateway region (e.g., us-east-1); {service} is the name of the integrated AWS service (e.g., s3); and {subdomain} is a designated subdomain supported by certain AWS service for fast host-name lookup. action can be used for an AWS service action-based API, using an Action={name}&{p1}={v1}&p2={v2}... query string. The ensuing {service_api} refers to a supported action {name} plus any required input parameters. Alternatively, path can be used for an AWS service path-based API. The ensuing service_api refers to the path to an AWS service resource, including the region of the integrated AWS service, if applicable. For example, for integration with the S3 API of GetObject, the URI can be either arn:aws:apigateway:us-west-2:s3:action/GetObject&Bucket={bucket}&Key={key} or arn:aws:apigateway:us-west-2:s3:path/{bucket}/{key}
--
-- * 'ciRequestParameters' - A key-value map specifying request parameters that are passed from the method request to the backend. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the backend. The method request parameter value must match the pattern of method.request.{location}.{name} , where  {location}  is querystring, path, or header; and  {name}  must be a valid and unique method request parameter name.
--
-- * 'ciConnectionId' - The identifier of the VpcLink used for the integration when the connectionType is VPC_LINK; otherwise undefined.
--
-- * 'ciPassthroughBehavior' - Specifies the pass-through behavior for incoming requests based on the Content-Type header in the request, and the available mapping templates specified as the requestTemplates property on the Integration resource. There are three valid values: WHEN_NO_MATCH, WHEN_NO_TEMPLATES, and NEVER. WHEN_NO_MATCH passes the request body for unmapped content types through to the integration backend without transformation. NEVER rejects unmapped content types with an HTTP 415 Unsupported Media Type response. WHEN_NO_TEMPLATES allows pass-through when the integration has no content types mapped to templates. However, if there is at least one content type defined, unmapped content types will be rejected with the same HTTP 415 Unsupported Media Type response.
--
-- * 'ciIntegrationMethod' - Specifies the integration's HTTP method type.
--
-- * 'ciTemplateSelectionExpression' - The template selection expression for the integration.
--
-- * 'ciTimeoutInMillis' - Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
--
-- * 'ciContentHandlingStrategy' - Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
--
-- * 'ciIntegrationType' - The integration type of an integration. One of the following: AWS: for integrating the route or method request with an AWS service action, including the Lambda function-invoking action. With the Lambda function-invoking action, this is referred to as the Lambda custom integration. With any other AWS service action, this is known as AWS integration. AWS_PROXY: for integrating the route or method request with the Lambda function-invoking action with the client request passed through as-is. This integration is also referred to as Lambda proxy integration. HTTP: for integrating the route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC. This integration is also referred to as the HTTP custom integration. HTTP_PROXY: for integrating route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC, with the client request passed through as-is. This is also referred to as HTTP proxy integration. MOCK: for integrating the route or method request with API Gateway as a "loopback" endpoint without invoking any backend.
--
-- * 'ciDescription' - The description of the integration.
--
-- * 'ciConnectionType' - The type of the network connection to the integration endpoint. The valid value is INTERNET for connections through the public routable internet or VPC_LINK for private connections between API Gateway and a network load balancer in a VPC. The default value is INTERNET.
--
-- * 'ciAPIId' - The API identifier.
createIntegration
    :: Text -- ^ 'ciAPIId'
    -> CreateIntegration
createIntegration pAPIId_ =
  CreateIntegration'
    { _ciRequestTemplates = Nothing
    , _ciCredentialsARN = Nothing
    , _ciIntegrationURI = Nothing
    , _ciRequestParameters = Nothing
    , _ciConnectionId = Nothing
    , _ciPassthroughBehavior = Nothing
    , _ciIntegrationMethod = Nothing
    , _ciTemplateSelectionExpression = Nothing
    , _ciTimeoutInMillis = Nothing
    , _ciContentHandlingStrategy = Nothing
    , _ciIntegrationType = Nothing
    , _ciDescription = Nothing
    , _ciConnectionType = Nothing
    , _ciAPIId = pAPIId_
    }


-- | Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
ciRequestTemplates :: Lens' CreateIntegration (HashMap Text Text)
ciRequestTemplates = lens _ciRequestTemplates (\ s a -> s{_ciRequestTemplates = a}) . _Default . _Map

-- | Specifies the credentials required for the integration, if any. For AWS integrations, three options are available. To specify an IAM Role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To require that the caller's identity be passed through from the request, specify the string arn:aws:iam::*:user/*. To use resource-based permissions on supported AWS services, specify null.
ciCredentialsARN :: Lens' CreateIntegration (Maybe Text)
ciCredentialsARN = lens _ciCredentialsARN (\ s a -> s{_ciCredentialsARN = a})

-- | Specifies the Uniform Resource Identifier (URI) of the integration endpoint. For HTTP or HTTP_PROXY integrations, the URI must be a fully formed, encoded HTTP(S) URL according to the <https://en.wikipedia.org/wiki/Uniform_Resource_Identifier RFC-3986 specification> , for either standard integration, where connectionType is not VPC_LINK, or private integration, where connectionType is VPC_LINK. For a private HTTP integration, the URI is not used for routing. For AWS or AWS_PROXY integrations, the URI is of the form arn:aws:apigateway:{region}:{subdomain.service|service}:path|action/{service_api}. Here, {Region} is the API Gateway region (e.g., us-east-1); {service} is the name of the integrated AWS service (e.g., s3); and {subdomain} is a designated subdomain supported by certain AWS service for fast host-name lookup. action can be used for an AWS service action-based API, using an Action={name}&{p1}={v1}&p2={v2}... query string. The ensuing {service_api} refers to a supported action {name} plus any required input parameters. Alternatively, path can be used for an AWS service path-based API. The ensuing service_api refers to the path to an AWS service resource, including the region of the integrated AWS service, if applicable. For example, for integration with the S3 API of GetObject, the URI can be either arn:aws:apigateway:us-west-2:s3:action/GetObject&Bucket={bucket}&Key={key} or arn:aws:apigateway:us-west-2:s3:path/{bucket}/{key}
ciIntegrationURI :: Lens' CreateIntegration (Maybe Text)
ciIntegrationURI = lens _ciIntegrationURI (\ s a -> s{_ciIntegrationURI = a})

-- | A key-value map specifying request parameters that are passed from the method request to the backend. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the backend. The method request parameter value must match the pattern of method.request.{location}.{name} , where  {location}  is querystring, path, or header; and  {name}  must be a valid and unique method request parameter name.
ciRequestParameters :: Lens' CreateIntegration (HashMap Text Text)
ciRequestParameters = lens _ciRequestParameters (\ s a -> s{_ciRequestParameters = a}) . _Default . _Map

-- | The identifier of the VpcLink used for the integration when the connectionType is VPC_LINK; otherwise undefined.
ciConnectionId :: Lens' CreateIntegration (Maybe Text)
ciConnectionId = lens _ciConnectionId (\ s a -> s{_ciConnectionId = a})

-- | Specifies the pass-through behavior for incoming requests based on the Content-Type header in the request, and the available mapping templates specified as the requestTemplates property on the Integration resource. There are three valid values: WHEN_NO_MATCH, WHEN_NO_TEMPLATES, and NEVER. WHEN_NO_MATCH passes the request body for unmapped content types through to the integration backend without transformation. NEVER rejects unmapped content types with an HTTP 415 Unsupported Media Type response. WHEN_NO_TEMPLATES allows pass-through when the integration has no content types mapped to templates. However, if there is at least one content type defined, unmapped content types will be rejected with the same HTTP 415 Unsupported Media Type response.
ciPassthroughBehavior :: Lens' CreateIntegration (Maybe PassthroughBehavior)
ciPassthroughBehavior = lens _ciPassthroughBehavior (\ s a -> s{_ciPassthroughBehavior = a})

-- | Specifies the integration's HTTP method type.
ciIntegrationMethod :: Lens' CreateIntegration (Maybe Text)
ciIntegrationMethod = lens _ciIntegrationMethod (\ s a -> s{_ciIntegrationMethod = a})

-- | The template selection expression for the integration.
ciTemplateSelectionExpression :: Lens' CreateIntegration (Maybe Text)
ciTemplateSelectionExpression = lens _ciTemplateSelectionExpression (\ s a -> s{_ciTemplateSelectionExpression = a})

-- | Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
ciTimeoutInMillis :: Lens' CreateIntegration (Maybe Natural)
ciTimeoutInMillis = lens _ciTimeoutInMillis (\ s a -> s{_ciTimeoutInMillis = a}) . mapping _Nat

-- | Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
ciContentHandlingStrategy :: Lens' CreateIntegration (Maybe ContentHandlingStrategy)
ciContentHandlingStrategy = lens _ciContentHandlingStrategy (\ s a -> s{_ciContentHandlingStrategy = a})

-- | The integration type of an integration. One of the following: AWS: for integrating the route or method request with an AWS service action, including the Lambda function-invoking action. With the Lambda function-invoking action, this is referred to as the Lambda custom integration. With any other AWS service action, this is known as AWS integration. AWS_PROXY: for integrating the route or method request with the Lambda function-invoking action with the client request passed through as-is. This integration is also referred to as Lambda proxy integration. HTTP: for integrating the route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC. This integration is also referred to as the HTTP custom integration. HTTP_PROXY: for integrating route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC, with the client request passed through as-is. This is also referred to as HTTP proxy integration. MOCK: for integrating the route or method request with API Gateway as a "loopback" endpoint without invoking any backend.
ciIntegrationType :: Lens' CreateIntegration (Maybe IntegrationType)
ciIntegrationType = lens _ciIntegrationType (\ s a -> s{_ciIntegrationType = a})

-- | The description of the integration.
ciDescription :: Lens' CreateIntegration (Maybe Text)
ciDescription = lens _ciDescription (\ s a -> s{_ciDescription = a})

-- | The type of the network connection to the integration endpoint. The valid value is INTERNET for connections through the public routable internet or VPC_LINK for private connections between API Gateway and a network load balancer in a VPC. The default value is INTERNET.
ciConnectionType :: Lens' CreateIntegration (Maybe ConnectionType)
ciConnectionType = lens _ciConnectionType (\ s a -> s{_ciConnectionType = a})

-- | The API identifier.
ciAPIId :: Lens' CreateIntegration Text
ciAPIId = lens _ciAPIId (\ s a -> s{_ciAPIId = a})

instance AWSRequest CreateIntegration where
        type Rs CreateIntegration =
             CreateIntegrationResponse'
        request = postJSON apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 CreateIntegrationResponse'' <$>
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

instance Hashable CreateIntegration where

instance NFData CreateIntegration where

instance ToHeaders CreateIntegration where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateIntegration where
        toJSON CreateIntegration'{..}
          = object
              (catMaybes
                 [("requestTemplates" .=) <$> _ciRequestTemplates,
                  ("credentialsArn" .=) <$> _ciCredentialsARN,
                  ("integrationUri" .=) <$> _ciIntegrationURI,
                  ("requestParameters" .=) <$> _ciRequestParameters,
                  ("connectionId" .=) <$> _ciConnectionId,
                  ("passthroughBehavior" .=) <$>
                    _ciPassthroughBehavior,
                  ("integrationMethod" .=) <$> _ciIntegrationMethod,
                  ("templateSelectionExpression" .=) <$>
                    _ciTemplateSelectionExpression,
                  ("timeoutInMillis" .=) <$> _ciTimeoutInMillis,
                  ("contentHandlingStrategy" .=) <$>
                    _ciContentHandlingStrategy,
                  ("integrationType" .=) <$> _ciIntegrationType,
                  ("description" .=) <$> _ciDescription,
                  ("connectionType" .=) <$> _ciConnectionType])

instance ToPath CreateIntegration where
        toPath CreateIntegration'{..}
          = mconcat
              ["/v2/apis/", toBS _ciAPIId, "/integrations"]

instance ToQuery CreateIntegration where
        toQuery = const mempty

-- | /See:/ 'createIntegrationResponse'' smart constructor.
data CreateIntegrationResponse' = CreateIntegrationResponse''
  { _crersIntegrationResponseSelectionExpression :: !(Maybe Text)
  , _crersRequestTemplates :: !(Maybe (Map Text Text))
  , _crersCredentialsARN :: !(Maybe Text)
  , _crersIntegrationURI :: !(Maybe Text)
  , _crersIntegrationId :: !(Maybe Text)
  , _crersRequestParameters :: !(Maybe (Map Text Text))
  , _crersConnectionId :: !(Maybe Text)
  , _crersPassthroughBehavior :: !(Maybe PassthroughBehavior)
  , _crersIntegrationMethod :: !(Maybe Text)
  , _crersTemplateSelectionExpression :: !(Maybe Text)
  , _crersTimeoutInMillis :: !(Maybe Nat)
  , _crersContentHandlingStrategy :: !(Maybe ContentHandlingStrategy)
  , _crersIntegrationType :: !(Maybe IntegrationType)
  , _crersDescription :: !(Maybe Text)
  , _crersConnectionType :: !(Maybe ConnectionType)
  , _crersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateIntegrationResponse'' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crersIntegrationResponseSelectionExpression' - 
--
-- * 'crersRequestTemplates' - Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
--
-- * 'crersCredentialsARN' - Specifies the credentials required for the integration, if any. For AWS integrations, three options are available. To specify an IAM Role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To require that the caller's identity be passed through from the request, specify the string arn:aws:iam::*:user/*. To use resource-based permissions on supported AWS services, specify null.
--
-- * 'crersIntegrationURI' - Specifies the Uniform Resource Identifier (URI) of the integration endpoint. For HTTP or HTTP_PROXY integrations, the URI must be a fully formed, encoded HTTP(S) URL according to the <https://en.wikipedia.org/wiki/Uniform_Resource_Identifier RFC-3986 specification> , for either standard integration, where connectionType is not VPC_LINK, or private integration, where connectionType is VPC_LINK. For a private HTTP integration, the URI is not used for routing. For AWS or AWS_PROXY integrations, the URI is of the form arn:aws:apigateway:{region}:{subdomain.service|service}:path|action/{service_api}. Here, {Region} is the API Gateway region (e.g., us-east-1); {service} is the name of the integrated AWS service (e.g., s3); and {subdomain} is a designated subdomain supported by certain AWS service for fast host-name lookup. action can be used for an AWS service action-based API, using an Action={name}&{p1}={v1}&p2={v2}... query string. The ensuing {service_api} refers to a supported action {name} plus any required input parameters. Alternatively, path can be used for an AWS service path-based API. The ensuing service_api refers to the path to an AWS service resource, including the region of the integrated AWS service, if applicable. For example, for integration with the S3 API of GetObject, the URI can be either arn:aws:apigateway:us-west-2:s3:action/GetObject&Bucket={bucket}&Key={key} or arn:aws:apigateway:us-west-2:s3:path/{bucket}/{key}
--
-- * 'crersIntegrationId' - Represents the identifier of an integration.
--
-- * 'crersRequestParameters' - A key-value map specifying request parameters that are passed from the method request to the backend. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the backend. The method request parameter value must match the pattern of method.request.{location}.{name} , where  {location}  is querystring, path, or header; and  {name}  must be a valid and unique method request parameter name.
--
-- * 'crersConnectionId' - The identifier of the VpcLink used for the integration when the connectionType is VPC_LINK; otherwise undefined.
--
-- * 'crersPassthroughBehavior' - Specifies the pass-through behavior for incoming requests based on the Content-Type header in the request, and the available mapping templates specified as the requestTemplates property on the Integration resource. There are three valid values: WHEN_NO_MATCH, WHEN_NO_TEMPLATES, and NEVER. WHEN_NO_MATCH passes the request body for unmapped content types through to the integration backend without transformation. NEVER rejects unmapped content types with an HTTP 415 Unsupported Media Type response. WHEN_NO_TEMPLATES allows pass-through when the integration has no content types mapped to templates. However, if there is at least one content type defined, unmapped content types will be rejected with the same HTTP 415 Unsupported Media Type response.
--
-- * 'crersIntegrationMethod' - Specifies the integration's HTTP method type.
--
-- * 'crersTemplateSelectionExpression' - The template selection expression for the integration.
--
-- * 'crersTimeoutInMillis' - Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
--
-- * 'crersContentHandlingStrategy' - Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
--
-- * 'crersIntegrationType' - The integration type of an integration. One of the following: AWS: for integrating the route or method request with an AWS service action, including the Lambda function-invoking action. With the Lambda function-invoking action, this is referred to as the Lambda custom integration. With any other AWS service action, this is known as AWS integration. AWS_PROXY: for integrating the route or method request with the Lambda function-invoking action with the client request passed through as-is. This integration is also referred to as Lambda proxy integration. HTTP: for integrating the route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC. This integration is also referred to as the HTTP custom integration. HTTP_PROXY: for integrating route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC, with the client request passed through as-is. This is also referred to as HTTP proxy integration. MOCK: for integrating the route or method request with API Gateway as a "loopback" endpoint without invoking any backend.
--
-- * 'crersDescription' - Represents the description of an integration.
--
-- * 'crersConnectionType' - The type of the network connection to the integration endpoint. The valid value is INTERNET for connections through the public routable internet or VPC_LINK for private connections between API Gateway and a network load balancer in a VPC. The default value is INTERNET.
--
-- * 'crersResponseStatus' - -- | The response status code.
createIntegrationResponse'
    :: Int -- ^ 'crersResponseStatus'
    -> CreateIntegrationResponse'
createIntegrationResponse' pResponseStatus_ =
  CreateIntegrationResponse''
    { _crersIntegrationResponseSelectionExpression = Nothing
    , _crersRequestTemplates = Nothing
    , _crersCredentialsARN = Nothing
    , _crersIntegrationURI = Nothing
    , _crersIntegrationId = Nothing
    , _crersRequestParameters = Nothing
    , _crersConnectionId = Nothing
    , _crersPassthroughBehavior = Nothing
    , _crersIntegrationMethod = Nothing
    , _crersTemplateSelectionExpression = Nothing
    , _crersTimeoutInMillis = Nothing
    , _crersContentHandlingStrategy = Nothing
    , _crersIntegrationType = Nothing
    , _crersDescription = Nothing
    , _crersConnectionType = Nothing
    , _crersResponseStatus = pResponseStatus_
    }


-- | 
crersIntegrationResponseSelectionExpression :: Lens' CreateIntegrationResponse' (Maybe Text)
crersIntegrationResponseSelectionExpression = lens _crersIntegrationResponseSelectionExpression (\ s a -> s{_crersIntegrationResponseSelectionExpression = a})

-- | Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
crersRequestTemplates :: Lens' CreateIntegrationResponse' (HashMap Text Text)
crersRequestTemplates = lens _crersRequestTemplates (\ s a -> s{_crersRequestTemplates = a}) . _Default . _Map

-- | Specifies the credentials required for the integration, if any. For AWS integrations, three options are available. To specify an IAM Role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To require that the caller's identity be passed through from the request, specify the string arn:aws:iam::*:user/*. To use resource-based permissions on supported AWS services, specify null.
crersCredentialsARN :: Lens' CreateIntegrationResponse' (Maybe Text)
crersCredentialsARN = lens _crersCredentialsARN (\ s a -> s{_crersCredentialsARN = a})

-- | Specifies the Uniform Resource Identifier (URI) of the integration endpoint. For HTTP or HTTP_PROXY integrations, the URI must be a fully formed, encoded HTTP(S) URL according to the <https://en.wikipedia.org/wiki/Uniform_Resource_Identifier RFC-3986 specification> , for either standard integration, where connectionType is not VPC_LINK, or private integration, where connectionType is VPC_LINK. For a private HTTP integration, the URI is not used for routing. For AWS or AWS_PROXY integrations, the URI is of the form arn:aws:apigateway:{region}:{subdomain.service|service}:path|action/{service_api}. Here, {Region} is the API Gateway region (e.g., us-east-1); {service} is the name of the integrated AWS service (e.g., s3); and {subdomain} is a designated subdomain supported by certain AWS service for fast host-name lookup. action can be used for an AWS service action-based API, using an Action={name}&{p1}={v1}&p2={v2}... query string. The ensuing {service_api} refers to a supported action {name} plus any required input parameters. Alternatively, path can be used for an AWS service path-based API. The ensuing service_api refers to the path to an AWS service resource, including the region of the integrated AWS service, if applicable. For example, for integration with the S3 API of GetObject, the URI can be either arn:aws:apigateway:us-west-2:s3:action/GetObject&Bucket={bucket}&Key={key} or arn:aws:apigateway:us-west-2:s3:path/{bucket}/{key}
crersIntegrationURI :: Lens' CreateIntegrationResponse' (Maybe Text)
crersIntegrationURI = lens _crersIntegrationURI (\ s a -> s{_crersIntegrationURI = a})

-- | Represents the identifier of an integration.
crersIntegrationId :: Lens' CreateIntegrationResponse' (Maybe Text)
crersIntegrationId = lens _crersIntegrationId (\ s a -> s{_crersIntegrationId = a})

-- | A key-value map specifying request parameters that are passed from the method request to the backend. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the backend. The method request parameter value must match the pattern of method.request.{location}.{name} , where  {location}  is querystring, path, or header; and  {name}  must be a valid and unique method request parameter name.
crersRequestParameters :: Lens' CreateIntegrationResponse' (HashMap Text Text)
crersRequestParameters = lens _crersRequestParameters (\ s a -> s{_crersRequestParameters = a}) . _Default . _Map

-- | The identifier of the VpcLink used for the integration when the connectionType is VPC_LINK; otherwise undefined.
crersConnectionId :: Lens' CreateIntegrationResponse' (Maybe Text)
crersConnectionId = lens _crersConnectionId (\ s a -> s{_crersConnectionId = a})

-- | Specifies the pass-through behavior for incoming requests based on the Content-Type header in the request, and the available mapping templates specified as the requestTemplates property on the Integration resource. There are three valid values: WHEN_NO_MATCH, WHEN_NO_TEMPLATES, and NEVER. WHEN_NO_MATCH passes the request body for unmapped content types through to the integration backend without transformation. NEVER rejects unmapped content types with an HTTP 415 Unsupported Media Type response. WHEN_NO_TEMPLATES allows pass-through when the integration has no content types mapped to templates. However, if there is at least one content type defined, unmapped content types will be rejected with the same HTTP 415 Unsupported Media Type response.
crersPassthroughBehavior :: Lens' CreateIntegrationResponse' (Maybe PassthroughBehavior)
crersPassthroughBehavior = lens _crersPassthroughBehavior (\ s a -> s{_crersPassthroughBehavior = a})

-- | Specifies the integration's HTTP method type.
crersIntegrationMethod :: Lens' CreateIntegrationResponse' (Maybe Text)
crersIntegrationMethod = lens _crersIntegrationMethod (\ s a -> s{_crersIntegrationMethod = a})

-- | The template selection expression for the integration.
crersTemplateSelectionExpression :: Lens' CreateIntegrationResponse' (Maybe Text)
crersTemplateSelectionExpression = lens _crersTemplateSelectionExpression (\ s a -> s{_crersTemplateSelectionExpression = a})

-- | Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
crersTimeoutInMillis :: Lens' CreateIntegrationResponse' (Maybe Natural)
crersTimeoutInMillis = lens _crersTimeoutInMillis (\ s a -> s{_crersTimeoutInMillis = a}) . mapping _Nat

-- | Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
crersContentHandlingStrategy :: Lens' CreateIntegrationResponse' (Maybe ContentHandlingStrategy)
crersContentHandlingStrategy = lens _crersContentHandlingStrategy (\ s a -> s{_crersContentHandlingStrategy = a})

-- | The integration type of an integration. One of the following: AWS: for integrating the route or method request with an AWS service action, including the Lambda function-invoking action. With the Lambda function-invoking action, this is referred to as the Lambda custom integration. With any other AWS service action, this is known as AWS integration. AWS_PROXY: for integrating the route or method request with the Lambda function-invoking action with the client request passed through as-is. This integration is also referred to as Lambda proxy integration. HTTP: for integrating the route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC. This integration is also referred to as the HTTP custom integration. HTTP_PROXY: for integrating route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC, with the client request passed through as-is. This is also referred to as HTTP proxy integration. MOCK: for integrating the route or method request with API Gateway as a "loopback" endpoint without invoking any backend.
crersIntegrationType :: Lens' CreateIntegrationResponse' (Maybe IntegrationType)
crersIntegrationType = lens _crersIntegrationType (\ s a -> s{_crersIntegrationType = a})

-- | Represents the description of an integration.
crersDescription :: Lens' CreateIntegrationResponse' (Maybe Text)
crersDescription = lens _crersDescription (\ s a -> s{_crersDescription = a})

-- | The type of the network connection to the integration endpoint. The valid value is INTERNET for connections through the public routable internet or VPC_LINK for private connections between API Gateway and a network load balancer in a VPC. The default value is INTERNET.
crersConnectionType :: Lens' CreateIntegrationResponse' (Maybe ConnectionType)
crersConnectionType = lens _crersConnectionType (\ s a -> s{_crersConnectionType = a})

-- | -- | The response status code.
crersResponseStatus :: Lens' CreateIntegrationResponse' Int
crersResponseStatus = lens _crersResponseStatus (\ s a -> s{_crersResponseStatus = a})

instance NFData CreateIntegrationResponse' where
