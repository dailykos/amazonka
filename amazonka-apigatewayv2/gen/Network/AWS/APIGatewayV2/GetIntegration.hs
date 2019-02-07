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
-- Module      : Network.AWS.APIGatewayV2.GetIntegration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an Integration.
--
--
module Network.AWS.APIGatewayV2.GetIntegration
    (
    -- * Creating a Request
      getIntegration
    , GetIntegration
    -- * Request Lenses
    , giAPIId
    , giIntegrationId

    -- * Destructuring the Response
    , getIntegrationResponse'
    , GetIntegrationResponse'
    -- * Response Lenses
    , girirsIntegrationResponseSelectionExpression
    , girirsRequestTemplates
    , girirsCredentialsARN
    , girirsIntegrationURI
    , girirsIntegrationId
    , girirsRequestParameters
    , girirsConnectionId
    , girirsPassthroughBehavior
    , girirsIntegrationMethod
    , girirsTemplateSelectionExpression
    , girirsTimeoutInMillis
    , girirsContentHandlingStrategy
    , girirsIntegrationType
    , girirsDescription
    , girirsConnectionType
    , girirsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getIntegration' smart constructor.
data GetIntegration = GetIntegration'
  { _giAPIId :: !Text
  , _giIntegrationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIntegration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giAPIId' - The API identifier.
--
-- * 'giIntegrationId' - The integration ID.
getIntegration
    :: Text -- ^ 'giAPIId'
    -> Text -- ^ 'giIntegrationId'
    -> GetIntegration
getIntegration pAPIId_ pIntegrationId_ =
  GetIntegration' {_giAPIId = pAPIId_, _giIntegrationId = pIntegrationId_}


-- | The API identifier.
giAPIId :: Lens' GetIntegration Text
giAPIId = lens _giAPIId (\ s a -> s{_giAPIId = a})

-- | The integration ID.
giIntegrationId :: Lens' GetIntegration Text
giIntegrationId = lens _giIntegrationId (\ s a -> s{_giIntegrationId = a})

instance AWSRequest GetIntegration where
        type Rs GetIntegration = GetIntegrationResponse'
        request = get apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 GetIntegrationResponse'' <$>
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

instance Hashable GetIntegration where

instance NFData GetIntegration where

instance ToHeaders GetIntegration where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetIntegration where
        toPath GetIntegration'{..}
          = mconcat
              ["/v2/apis/", toBS _giAPIId, "/integrations/",
               toBS _giIntegrationId]

instance ToQuery GetIntegration where
        toQuery = const mempty

-- | /See:/ 'getIntegrationResponse'' smart constructor.
data GetIntegrationResponse' = GetIntegrationResponse''
  { _girirsIntegrationResponseSelectionExpression :: !(Maybe Text)
  , _girirsRequestTemplates :: !(Maybe (Map Text Text))
  , _girirsCredentialsARN :: !(Maybe Text)
  , _girirsIntegrationURI :: !(Maybe Text)
  , _girirsIntegrationId :: !(Maybe Text)
  , _girirsRequestParameters :: !(Maybe (Map Text Text))
  , _girirsConnectionId :: !(Maybe Text)
  , _girirsPassthroughBehavior :: !(Maybe PassthroughBehavior)
  , _girirsIntegrationMethod :: !(Maybe Text)
  , _girirsTemplateSelectionExpression :: !(Maybe Text)
  , _girirsTimeoutInMillis :: !(Maybe Nat)
  , _girirsContentHandlingStrategy :: !(Maybe ContentHandlingStrategy)
  , _girirsIntegrationType :: !(Maybe IntegrationType)
  , _girirsDescription :: !(Maybe Text)
  , _girirsConnectionType :: !(Maybe ConnectionType)
  , _girirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIntegrationResponse'' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'girirsIntegrationResponseSelectionExpression' - 
--
-- * 'girirsRequestTemplates' - Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
--
-- * 'girirsCredentialsARN' - Specifies the credentials required for the integration, if any. For AWS integrations, three options are available. To specify an IAM Role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To require that the caller's identity be passed through from the request, specify the string arn:aws:iam::*:user/*. To use resource-based permissions on supported AWS services, specify null.
--
-- * 'girirsIntegrationURI' - Specifies the Uniform Resource Identifier (URI) of the integration endpoint. For HTTP or HTTP_PROXY integrations, the URI must be a fully formed, encoded HTTP(S) URL according to the <https://en.wikipedia.org/wiki/Uniform_Resource_Identifier RFC-3986 specification> , for either standard integration, where connectionType is not VPC_LINK, or private integration, where connectionType is VPC_LINK. For a private HTTP integration, the URI is not used for routing. For AWS or AWS_PROXY integrations, the URI is of the form arn:aws:apigateway:{region}:{subdomain.service|service}:path|action/{service_api}. Here, {Region} is the API Gateway region (e.g., us-east-1); {service} is the name of the integrated AWS service (e.g., s3); and {subdomain} is a designated subdomain supported by certain AWS service for fast host-name lookup. action can be used for an AWS service action-based API, using an Action={name}&{p1}={v1}&p2={v2}... query string. The ensuing {service_api} refers to a supported action {name} plus any required input parameters. Alternatively, path can be used for an AWS service path-based API. The ensuing service_api refers to the path to an AWS service resource, including the region of the integrated AWS service, if applicable. For example, for integration with the S3 API of GetObject, the URI can be either arn:aws:apigateway:us-west-2:s3:action/GetObject&Bucket={bucket}&Key={key} or arn:aws:apigateway:us-west-2:s3:path/{bucket}/{key}
--
-- * 'girirsIntegrationId' - Represents the identifier of an integration.
--
-- * 'girirsRequestParameters' - A key-value map specifying request parameters that are passed from the method request to the backend. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the backend. The method request parameter value must match the pattern of method.request.{location}.{name} , where  {location}  is querystring, path, or header; and  {name}  must be a valid and unique method request parameter name.
--
-- * 'girirsConnectionId' - The identifier of the VpcLink used for the integration when the connectionType is VPC_LINK; otherwise undefined.
--
-- * 'girirsPassthroughBehavior' - Specifies the pass-through behavior for incoming requests based on the Content-Type header in the request, and the available mapping templates specified as the requestTemplates property on the Integration resource. There are three valid values: WHEN_NO_MATCH, WHEN_NO_TEMPLATES, and NEVER. WHEN_NO_MATCH passes the request body for unmapped content types through to the integration backend without transformation. NEVER rejects unmapped content types with an HTTP 415 Unsupported Media Type response. WHEN_NO_TEMPLATES allows pass-through when the integration has no content types mapped to templates. However, if there is at least one content type defined, unmapped content types will be rejected with the same HTTP 415 Unsupported Media Type response.
--
-- * 'girirsIntegrationMethod' - Specifies the integration's HTTP method type.
--
-- * 'girirsTemplateSelectionExpression' - The template selection expression for the integration.
--
-- * 'girirsTimeoutInMillis' - Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
--
-- * 'girirsContentHandlingStrategy' - Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
--
-- * 'girirsIntegrationType' - The integration type of an integration. One of the following: AWS: for integrating the route or method request with an AWS service action, including the Lambda function-invoking action. With the Lambda function-invoking action, this is referred to as the Lambda custom integration. With any other AWS service action, this is known as AWS integration. AWS_PROXY: for integrating the route or method request with the Lambda function-invoking action with the client request passed through as-is. This integration is also referred to as Lambda proxy integration. HTTP: for integrating the route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC. This integration is also referred to as the HTTP custom integration. HTTP_PROXY: for integrating route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC, with the client request passed through as-is. This is also referred to as HTTP proxy integration. MOCK: for integrating the route or method request with API Gateway as a "loopback" endpoint without invoking any backend.
--
-- * 'girirsDescription' - Represents the description of an integration.
--
-- * 'girirsConnectionType' - The type of the network connection to the integration endpoint. The valid value is INTERNET for connections through the public routable internet or VPC_LINK for private connections between API Gateway and a network load balancer in a VPC. The default value is INTERNET.
--
-- * 'girirsResponseStatus' - -- | The response status code.
getIntegrationResponse'
    :: Int -- ^ 'girirsResponseStatus'
    -> GetIntegrationResponse'
getIntegrationResponse' pResponseStatus_ =
  GetIntegrationResponse''
    { _girirsIntegrationResponseSelectionExpression = Nothing
    , _girirsRequestTemplates = Nothing
    , _girirsCredentialsARN = Nothing
    , _girirsIntegrationURI = Nothing
    , _girirsIntegrationId = Nothing
    , _girirsRequestParameters = Nothing
    , _girirsConnectionId = Nothing
    , _girirsPassthroughBehavior = Nothing
    , _girirsIntegrationMethod = Nothing
    , _girirsTemplateSelectionExpression = Nothing
    , _girirsTimeoutInMillis = Nothing
    , _girirsContentHandlingStrategy = Nothing
    , _girirsIntegrationType = Nothing
    , _girirsDescription = Nothing
    , _girirsConnectionType = Nothing
    , _girirsResponseStatus = pResponseStatus_
    }


-- | 
girirsIntegrationResponseSelectionExpression :: Lens' GetIntegrationResponse' (Maybe Text)
girirsIntegrationResponseSelectionExpression = lens _girirsIntegrationResponseSelectionExpression (\ s a -> s{_girirsIntegrationResponseSelectionExpression = a})

-- | Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
girirsRequestTemplates :: Lens' GetIntegrationResponse' (HashMap Text Text)
girirsRequestTemplates = lens _girirsRequestTemplates (\ s a -> s{_girirsRequestTemplates = a}) . _Default . _Map

-- | Specifies the credentials required for the integration, if any. For AWS integrations, three options are available. To specify an IAM Role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To require that the caller's identity be passed through from the request, specify the string arn:aws:iam::*:user/*. To use resource-based permissions on supported AWS services, specify null.
girirsCredentialsARN :: Lens' GetIntegrationResponse' (Maybe Text)
girirsCredentialsARN = lens _girirsCredentialsARN (\ s a -> s{_girirsCredentialsARN = a})

-- | Specifies the Uniform Resource Identifier (URI) of the integration endpoint. For HTTP or HTTP_PROXY integrations, the URI must be a fully formed, encoded HTTP(S) URL according to the <https://en.wikipedia.org/wiki/Uniform_Resource_Identifier RFC-3986 specification> , for either standard integration, where connectionType is not VPC_LINK, or private integration, where connectionType is VPC_LINK. For a private HTTP integration, the URI is not used for routing. For AWS or AWS_PROXY integrations, the URI is of the form arn:aws:apigateway:{region}:{subdomain.service|service}:path|action/{service_api}. Here, {Region} is the API Gateway region (e.g., us-east-1); {service} is the name of the integrated AWS service (e.g., s3); and {subdomain} is a designated subdomain supported by certain AWS service for fast host-name lookup. action can be used for an AWS service action-based API, using an Action={name}&{p1}={v1}&p2={v2}... query string. The ensuing {service_api} refers to a supported action {name} plus any required input parameters. Alternatively, path can be used for an AWS service path-based API. The ensuing service_api refers to the path to an AWS service resource, including the region of the integrated AWS service, if applicable. For example, for integration with the S3 API of GetObject, the URI can be either arn:aws:apigateway:us-west-2:s3:action/GetObject&Bucket={bucket}&Key={key} or arn:aws:apigateway:us-west-2:s3:path/{bucket}/{key}
girirsIntegrationURI :: Lens' GetIntegrationResponse' (Maybe Text)
girirsIntegrationURI = lens _girirsIntegrationURI (\ s a -> s{_girirsIntegrationURI = a})

-- | Represents the identifier of an integration.
girirsIntegrationId :: Lens' GetIntegrationResponse' (Maybe Text)
girirsIntegrationId = lens _girirsIntegrationId (\ s a -> s{_girirsIntegrationId = a})

-- | A key-value map specifying request parameters that are passed from the method request to the backend. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the backend. The method request parameter value must match the pattern of method.request.{location}.{name} , where  {location}  is querystring, path, or header; and  {name}  must be a valid and unique method request parameter name.
girirsRequestParameters :: Lens' GetIntegrationResponse' (HashMap Text Text)
girirsRequestParameters = lens _girirsRequestParameters (\ s a -> s{_girirsRequestParameters = a}) . _Default . _Map

-- | The identifier of the VpcLink used for the integration when the connectionType is VPC_LINK; otherwise undefined.
girirsConnectionId :: Lens' GetIntegrationResponse' (Maybe Text)
girirsConnectionId = lens _girirsConnectionId (\ s a -> s{_girirsConnectionId = a})

-- | Specifies the pass-through behavior for incoming requests based on the Content-Type header in the request, and the available mapping templates specified as the requestTemplates property on the Integration resource. There are three valid values: WHEN_NO_MATCH, WHEN_NO_TEMPLATES, and NEVER. WHEN_NO_MATCH passes the request body for unmapped content types through to the integration backend without transformation. NEVER rejects unmapped content types with an HTTP 415 Unsupported Media Type response. WHEN_NO_TEMPLATES allows pass-through when the integration has no content types mapped to templates. However, if there is at least one content type defined, unmapped content types will be rejected with the same HTTP 415 Unsupported Media Type response.
girirsPassthroughBehavior :: Lens' GetIntegrationResponse' (Maybe PassthroughBehavior)
girirsPassthroughBehavior = lens _girirsPassthroughBehavior (\ s a -> s{_girirsPassthroughBehavior = a})

-- | Specifies the integration's HTTP method type.
girirsIntegrationMethod :: Lens' GetIntegrationResponse' (Maybe Text)
girirsIntegrationMethod = lens _girirsIntegrationMethod (\ s a -> s{_girirsIntegrationMethod = a})

-- | The template selection expression for the integration.
girirsTemplateSelectionExpression :: Lens' GetIntegrationResponse' (Maybe Text)
girirsTemplateSelectionExpression = lens _girirsTemplateSelectionExpression (\ s a -> s{_girirsTemplateSelectionExpression = a})

-- | Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
girirsTimeoutInMillis :: Lens' GetIntegrationResponse' (Maybe Natural)
girirsTimeoutInMillis = lens _girirsTimeoutInMillis (\ s a -> s{_girirsTimeoutInMillis = a}) . mapping _Nat

-- | Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
girirsContentHandlingStrategy :: Lens' GetIntegrationResponse' (Maybe ContentHandlingStrategy)
girirsContentHandlingStrategy = lens _girirsContentHandlingStrategy (\ s a -> s{_girirsContentHandlingStrategy = a})

-- | The integration type of an integration. One of the following: AWS: for integrating the route or method request with an AWS service action, including the Lambda function-invoking action. With the Lambda function-invoking action, this is referred to as the Lambda custom integration. With any other AWS service action, this is known as AWS integration. AWS_PROXY: for integrating the route or method request with the Lambda function-invoking action with the client request passed through as-is. This integration is also referred to as Lambda proxy integration. HTTP: for integrating the route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC. This integration is also referred to as the HTTP custom integration. HTTP_PROXY: for integrating route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC, with the client request passed through as-is. This is also referred to as HTTP proxy integration. MOCK: for integrating the route or method request with API Gateway as a "loopback" endpoint without invoking any backend.
girirsIntegrationType :: Lens' GetIntegrationResponse' (Maybe IntegrationType)
girirsIntegrationType = lens _girirsIntegrationType (\ s a -> s{_girirsIntegrationType = a})

-- | Represents the description of an integration.
girirsDescription :: Lens' GetIntegrationResponse' (Maybe Text)
girirsDescription = lens _girirsDescription (\ s a -> s{_girirsDescription = a})

-- | The type of the network connection to the integration endpoint. The valid value is INTERNET for connections through the public routable internet or VPC_LINK for private connections between API Gateway and a network load balancer in a VPC. The default value is INTERNET.
girirsConnectionType :: Lens' GetIntegrationResponse' (Maybe ConnectionType)
girirsConnectionType = lens _girirsConnectionType (\ s a -> s{_girirsConnectionType = a})

-- | -- | The response status code.
girirsResponseStatus :: Lens' GetIntegrationResponse' Int
girirsResponseStatus = lens _girirsResponseStatus (\ s a -> s{_girirsResponseStatus = a})

instance NFData GetIntegrationResponse' where
