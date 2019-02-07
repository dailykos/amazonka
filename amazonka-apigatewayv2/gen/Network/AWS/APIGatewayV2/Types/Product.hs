{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGatewayV2.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.APIGatewayV2.Types.Product where

import Network.AWS.APIGatewayV2.Internal
import Network.AWS.APIGatewayV2.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an API.
--
--
--
-- /See:/ 'api' smart constructor.
data API = API'
  { _apiAPIId :: !(Maybe Text)
  , _apiAPIEndpoint :: !(Maybe Text)
  , _apiWarnings :: !(Maybe [Text])
  , _apiCreatedDate :: !(Maybe POSIX)
  , _apiVersion :: !(Maybe Text)
  , _apiAPIKeySelectionExpression :: !(Maybe Text)
  , _apiDisableSchemaValidation :: !(Maybe Bool)
  , _apiDescription :: !(Maybe Text)
  , _apiRouteSelectionExpression :: !Text
  , _apiProtocolType :: !ProtocolType
  , _apiName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'API' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apiAPIId' - The API ID.
--
-- * 'apiAPIEndpoint' - The URI of the API, of the form {api-id}.execute-api.{region}.amazonaws.com. The stage name is typically appended to this URI to form a complete path to a deployed API stage.
--
-- * 'apiWarnings' - The warning messages reported when failonwarnings is turned on during API import.
--
-- * 'apiCreatedDate' - The timestamp when the API was created.
--
-- * 'apiVersion' - A version identifier for the API.
--
-- * 'apiAPIKeySelectionExpression' - An API key selection expression. See <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions> .
--
-- * 'apiDisableSchemaValidation' - Avoid validating models when creating a deployment.
--
-- * 'apiDescription' - The description of the API.
--
-- * 'apiRouteSelectionExpression' - The route selection expression for the API.
--
-- * 'apiProtocolType' - The API protocol: HTTP or WEBSOCKET.
--
-- * 'apiName' - The name of the API.
api
    :: Text -- ^ 'apiRouteSelectionExpression'
    -> ProtocolType -- ^ 'apiProtocolType'
    -> Text -- ^ 'apiName'
    -> API
api pRouteSelectionExpression_ pProtocolType_ pName_ =
  API'
    { _apiAPIId = Nothing
    , _apiAPIEndpoint = Nothing
    , _apiWarnings = Nothing
    , _apiCreatedDate = Nothing
    , _apiVersion = Nothing
    , _apiAPIKeySelectionExpression = Nothing
    , _apiDisableSchemaValidation = Nothing
    , _apiDescription = Nothing
    , _apiRouteSelectionExpression = pRouteSelectionExpression_
    , _apiProtocolType = pProtocolType_
    , _apiName = pName_
    }


-- | The API ID.
apiAPIId :: Lens' API (Maybe Text)
apiAPIId = lens _apiAPIId (\ s a -> s{_apiAPIId = a})

-- | The URI of the API, of the form {api-id}.execute-api.{region}.amazonaws.com. The stage name is typically appended to this URI to form a complete path to a deployed API stage.
apiAPIEndpoint :: Lens' API (Maybe Text)
apiAPIEndpoint = lens _apiAPIEndpoint (\ s a -> s{_apiAPIEndpoint = a})

-- | The warning messages reported when failonwarnings is turned on during API import.
apiWarnings :: Lens' API [Text]
apiWarnings = lens _apiWarnings (\ s a -> s{_apiWarnings = a}) . _Default . _Coerce

-- | The timestamp when the API was created.
apiCreatedDate :: Lens' API (Maybe UTCTime)
apiCreatedDate = lens _apiCreatedDate (\ s a -> s{_apiCreatedDate = a}) . mapping _Time

-- | A version identifier for the API.
apiVersion :: Lens' API (Maybe Text)
apiVersion = lens _apiVersion (\ s a -> s{_apiVersion = a})

-- | An API key selection expression. See <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions> .
apiAPIKeySelectionExpression :: Lens' API (Maybe Text)
apiAPIKeySelectionExpression = lens _apiAPIKeySelectionExpression (\ s a -> s{_apiAPIKeySelectionExpression = a})

-- | Avoid validating models when creating a deployment.
apiDisableSchemaValidation :: Lens' API (Maybe Bool)
apiDisableSchemaValidation = lens _apiDisableSchemaValidation (\ s a -> s{_apiDisableSchemaValidation = a})

-- | The description of the API.
apiDescription :: Lens' API (Maybe Text)
apiDescription = lens _apiDescription (\ s a -> s{_apiDescription = a})

-- | The route selection expression for the API.
apiRouteSelectionExpression :: Lens' API Text
apiRouteSelectionExpression = lens _apiRouteSelectionExpression (\ s a -> s{_apiRouteSelectionExpression = a})

-- | The API protocol: HTTP or WEBSOCKET.
apiProtocolType :: Lens' API ProtocolType
apiProtocolType = lens _apiProtocolType (\ s a -> s{_apiProtocolType = a})

-- | The name of the API.
apiName :: Lens' API Text
apiName = lens _apiName (\ s a -> s{_apiName = a})

instance FromJSON API where
        parseJSON
          = withObject "API"
              (\ x ->
                 API' <$>
                   (x .:? "apiId") <*> (x .:? "apiEndpoint") <*>
                     (x .:? "warnings" .!= mempty)
                     <*> (x .:? "createdDate")
                     <*> (x .:? "version")
                     <*> (x .:? "apiKeySelectionExpression")
                     <*> (x .:? "disableSchemaValidation")
                     <*> (x .:? "description")
                     <*> (x .: "routeSelectionExpression")
                     <*> (x .: "protocolType")
                     <*> (x .: "name"))

instance Hashable API where

instance NFData API where

-- | Settings for logging access in a stage.
--
--
--
-- /See:/ 'accessLogSettings' smart constructor.
data AccessLogSettings = AccessLogSettings'
  { _alsFormat :: !(Maybe Text)
  , _alsDestinationARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccessLogSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alsFormat' - A single line format of the access logs of data, as specified by selected $context variables. The format must include at least $context.requestId.
--
-- * 'alsDestinationARN' - The ARN of the CloudWatch Logs log group to receive access logs.
accessLogSettings
    :: AccessLogSettings
accessLogSettings =
  AccessLogSettings' {_alsFormat = Nothing, _alsDestinationARN = Nothing}


-- | A single line format of the access logs of data, as specified by selected $context variables. The format must include at least $context.requestId.
alsFormat :: Lens' AccessLogSettings (Maybe Text)
alsFormat = lens _alsFormat (\ s a -> s{_alsFormat = a})

-- | The ARN of the CloudWatch Logs log group to receive access logs.
alsDestinationARN :: Lens' AccessLogSettings (Maybe Text)
alsDestinationARN = lens _alsDestinationARN (\ s a -> s{_alsDestinationARN = a})

instance FromJSON AccessLogSettings where
        parseJSON
          = withObject "AccessLogSettings"
              (\ x ->
                 AccessLogSettings' <$>
                   (x .:? "format") <*> (x .:? "destinationArn"))

instance Hashable AccessLogSettings where

instance NFData AccessLogSettings where

instance ToJSON AccessLogSettings where
        toJSON AccessLogSettings'{..}
          = object
              (catMaybes
                 [("format" .=) <$> _alsFormat,
                  ("destinationArn" .=) <$> _alsDestinationARN])

-- | Represents an authorizer.
--
--
--
-- /See:/ 'authorizer' smart constructor.
data Authorizer = Authorizer'
  { _aAuthorizerCredentialsARN :: !(Maybe Text)
  , _aIdentityValidationExpression :: !(Maybe Text)
  , _aAuthorizerURI :: !(Maybe Text)
  , _aAuthorizerId :: !(Maybe Text)
  , _aProviderARNs :: !(Maybe [Text])
  , _aAuthorizerResultTtlInSeconds :: !(Maybe Nat)
  , _aIdentitySource :: !(Maybe [Text])
  , _aAuthorizerType :: !(Maybe AuthorizerType)
  , _aName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Authorizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aAuthorizerCredentialsARN' - Specifies the required credentials as an IAM role for API Gateway to invoke the authorizer. To specify an IAM role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
--
-- * 'aIdentityValidationExpression' - The validation expression does not apply to the REQUEST authorizer.
--
-- * 'aAuthorizerURI' - The authorizer's Uniform Resource Identifier (URI). ForREQUEST authorizers, this must be a well-formed Lambda function URI, for example, arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations. In general, the URI has this form: arn:aws:apigateway:{region}:lambda:path/{service_api} , where {region} is the same as the region hosting the Lambda function, path indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial /. For Lambda functions, this is usually of the form /2015-03-31/functions/[FunctionARN]/invocations.
--
-- * 'aAuthorizerId' - The authorizer identifier.
--
-- * 'aProviderARNs' - For REQUEST authorizer, this is not defined.
--
-- * 'aAuthorizerResultTtlInSeconds' - The time to live (TTL), in seconds, of cached authorizer results. If it equals 0, authorization caching is disabled. If it is greater than 0, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
--
-- * 'aIdentitySource' - The identity source for which authorization is requested. For the REQUEST authorizer, this is required when authorization caching is enabled. The value is a comma-separated string of one or more mapping expressions of the specified request parameters. For example, if an Auth header and a Name query string parameters are defined as identity sources, this value is method.request.header.Auth, method.request.querystring.Name. These parameters will be used to derive the authorization caching key and to perform runtime validation of the REQUEST authorizer by verifying all of the identity-related request parameters are present, not null, and non-empty. Only when this is true does the authorizer invoke the authorizer Lambda function, otherwise, it returns a 401 Unauthorized response without calling the Lambda function. The valid value is a string of comma-separated mapping expressions of the specified request parameters. When the authorization caching is not enabled, this property is optional.
--
-- * 'aAuthorizerType' - The authorizer type. Currently the only valid value is REQUEST, for a Lambda function using incoming request parameters.
--
-- * 'aName' - The name of the authorizer.
authorizer
    :: Text -- ^ 'aName'
    -> Authorizer
authorizer pName_ =
  Authorizer'
    { _aAuthorizerCredentialsARN = Nothing
    , _aIdentityValidationExpression = Nothing
    , _aAuthorizerURI = Nothing
    , _aAuthorizerId = Nothing
    , _aProviderARNs = Nothing
    , _aAuthorizerResultTtlInSeconds = Nothing
    , _aIdentitySource = Nothing
    , _aAuthorizerType = Nothing
    , _aName = pName_
    }


-- | Specifies the required credentials as an IAM role for API Gateway to invoke the authorizer. To specify an IAM role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
aAuthorizerCredentialsARN :: Lens' Authorizer (Maybe Text)
aAuthorizerCredentialsARN = lens _aAuthorizerCredentialsARN (\ s a -> s{_aAuthorizerCredentialsARN = a})

-- | The validation expression does not apply to the REQUEST authorizer.
aIdentityValidationExpression :: Lens' Authorizer (Maybe Text)
aIdentityValidationExpression = lens _aIdentityValidationExpression (\ s a -> s{_aIdentityValidationExpression = a})

-- | The authorizer's Uniform Resource Identifier (URI). ForREQUEST authorizers, this must be a well-formed Lambda function URI, for example, arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations. In general, the URI has this form: arn:aws:apigateway:{region}:lambda:path/{service_api} , where {region} is the same as the region hosting the Lambda function, path indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial /. For Lambda functions, this is usually of the form /2015-03-31/functions/[FunctionARN]/invocations.
aAuthorizerURI :: Lens' Authorizer (Maybe Text)
aAuthorizerURI = lens _aAuthorizerURI (\ s a -> s{_aAuthorizerURI = a})

-- | The authorizer identifier.
aAuthorizerId :: Lens' Authorizer (Maybe Text)
aAuthorizerId = lens _aAuthorizerId (\ s a -> s{_aAuthorizerId = a})

-- | For REQUEST authorizer, this is not defined.
aProviderARNs :: Lens' Authorizer [Text]
aProviderARNs = lens _aProviderARNs (\ s a -> s{_aProviderARNs = a}) . _Default . _Coerce

-- | The time to live (TTL), in seconds, of cached authorizer results. If it equals 0, authorization caching is disabled. If it is greater than 0, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
aAuthorizerResultTtlInSeconds :: Lens' Authorizer (Maybe Natural)
aAuthorizerResultTtlInSeconds = lens _aAuthorizerResultTtlInSeconds (\ s a -> s{_aAuthorizerResultTtlInSeconds = a}) . mapping _Nat

-- | The identity source for which authorization is requested. For the REQUEST authorizer, this is required when authorization caching is enabled. The value is a comma-separated string of one or more mapping expressions of the specified request parameters. For example, if an Auth header and a Name query string parameters are defined as identity sources, this value is method.request.header.Auth, method.request.querystring.Name. These parameters will be used to derive the authorization caching key and to perform runtime validation of the REQUEST authorizer by verifying all of the identity-related request parameters are present, not null, and non-empty. Only when this is true does the authorizer invoke the authorizer Lambda function, otherwise, it returns a 401 Unauthorized response without calling the Lambda function. The valid value is a string of comma-separated mapping expressions of the specified request parameters. When the authorization caching is not enabled, this property is optional.
aIdentitySource :: Lens' Authorizer [Text]
aIdentitySource = lens _aIdentitySource (\ s a -> s{_aIdentitySource = a}) . _Default . _Coerce

-- | The authorizer type. Currently the only valid value is REQUEST, for a Lambda function using incoming request parameters.
aAuthorizerType :: Lens' Authorizer (Maybe AuthorizerType)
aAuthorizerType = lens _aAuthorizerType (\ s a -> s{_aAuthorizerType = a})

-- | The name of the authorizer.
aName :: Lens' Authorizer Text
aName = lens _aName (\ s a -> s{_aName = a})

instance FromJSON Authorizer where
        parseJSON
          = withObject "Authorizer"
              (\ x ->
                 Authorizer' <$>
                   (x .:? "authorizerCredentialsArn") <*>
                     (x .:? "identityValidationExpression")
                     <*> (x .:? "authorizerUri")
                     <*> (x .:? "authorizerId")
                     <*> (x .:? "providerArns" .!= mempty)
                     <*> (x .:? "authorizerResultTtlInSeconds")
                     <*> (x .:? "identitySource" .!= mempty)
                     <*> (x .:? "authorizerType")
                     <*> (x .: "name"))

instance Hashable Authorizer where

instance NFData Authorizer where

-- | An immutable representation of an API that can be called by users. A Deployment must be associated with a Stage for it to be callable over the internet.
--
--
--
-- /See:/ 'deployment' smart constructor.
data Deployment = Deployment'
  { _dDeploymentId :: !(Maybe Text)
  , _dDeploymentStatusMessage :: !(Maybe Text)
  , _dCreatedDate :: !(Maybe POSIX)
  , _dDeploymentStatus :: !(Maybe DeploymentStatus)
  , _dDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Deployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDeploymentId' - The identifier for the deployment.
--
-- * 'dDeploymentStatusMessage' - May contain additional feedback on the status of an API deployment.
--
-- * 'dCreatedDate' - The date and time when the Deployment resource was created.
--
-- * 'dDeploymentStatus' - The status of the deployment: PENDING, FAILED, or SUCCEEDED.
--
-- * 'dDescription' - The description for the deployment.
deployment
    :: Deployment
deployment =
  Deployment'
    { _dDeploymentId = Nothing
    , _dDeploymentStatusMessage = Nothing
    , _dCreatedDate = Nothing
    , _dDeploymentStatus = Nothing
    , _dDescription = Nothing
    }


-- | The identifier for the deployment.
dDeploymentId :: Lens' Deployment (Maybe Text)
dDeploymentId = lens _dDeploymentId (\ s a -> s{_dDeploymentId = a})

-- | May contain additional feedback on the status of an API deployment.
dDeploymentStatusMessage :: Lens' Deployment (Maybe Text)
dDeploymentStatusMessage = lens _dDeploymentStatusMessage (\ s a -> s{_dDeploymentStatusMessage = a})

-- | The date and time when the Deployment resource was created.
dCreatedDate :: Lens' Deployment (Maybe UTCTime)
dCreatedDate = lens _dCreatedDate (\ s a -> s{_dCreatedDate = a}) . mapping _Time

-- | The status of the deployment: PENDING, FAILED, or SUCCEEDED.
dDeploymentStatus :: Lens' Deployment (Maybe DeploymentStatus)
dDeploymentStatus = lens _dDeploymentStatus (\ s a -> s{_dDeploymentStatus = a})

-- | The description for the deployment.
dDescription :: Lens' Deployment (Maybe Text)
dDescription = lens _dDescription (\ s a -> s{_dDescription = a})

instance FromJSON Deployment where
        parseJSON
          = withObject "Deployment"
              (\ x ->
                 Deployment' <$>
                   (x .:? "deploymentId") <*>
                     (x .:? "deploymentStatusMessage")
                     <*> (x .:? "createdDate")
                     <*> (x .:? "deploymentStatus")
                     <*> (x .:? "description"))

instance Hashable Deployment where

instance NFData Deployment where

-- | Represents a domain name.
--
--
--
-- /See:/ 'domainName' smart constructor.
data DomainName = DomainName'
  { _dnDomainNameConfigurations :: !(Maybe [DomainNameConfiguration])
  , _dnAPIMappingSelectionExpression :: !(Maybe Text)
  , _dnDomainName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DomainName' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnDomainNameConfigurations' - The domain name configurations.
--
-- * 'dnAPIMappingSelectionExpression' - The API mapping selection expression.
--
-- * 'dnDomainName' - The name of the DomainName resource.
domainName
    :: Text -- ^ 'dnDomainName'
    -> DomainName
domainName pDomainName_ =
  DomainName'
    { _dnDomainNameConfigurations = Nothing
    , _dnAPIMappingSelectionExpression = Nothing
    , _dnDomainName = pDomainName_
    }


-- | The domain name configurations.
dnDomainNameConfigurations :: Lens' DomainName [DomainNameConfiguration]
dnDomainNameConfigurations = lens _dnDomainNameConfigurations (\ s a -> s{_dnDomainNameConfigurations = a}) . _Default . _Coerce

-- | The API mapping selection expression.
dnAPIMappingSelectionExpression :: Lens' DomainName (Maybe Text)
dnAPIMappingSelectionExpression = lens _dnAPIMappingSelectionExpression (\ s a -> s{_dnAPIMappingSelectionExpression = a})

-- | The name of the DomainName resource.
dnDomainName :: Lens' DomainName Text
dnDomainName = lens _dnDomainName (\ s a -> s{_dnDomainName = a})

instance FromJSON DomainName where
        parseJSON
          = withObject "DomainName"
              (\ x ->
                 DomainName' <$>
                   (x .:? "domainNameConfigurations" .!= mempty) <*>
                     (x .:? "apiMappingSelectionExpression")
                     <*> (x .: "domainName"))

instance Hashable DomainName where

instance NFData DomainName where

-- | The domain name configuration.
--
--
--
-- /See:/ 'domainNameConfiguration' smart constructor.
data DomainNameConfiguration = DomainNameConfiguration'
  { _dncAPIGatewayDomainName :: !(Maybe Text)
  , _dncCertificateName :: !(Maybe Text)
  , _dncHostedZoneId :: !(Maybe Text)
  , _dncCertificateARN :: !(Maybe Text)
  , _dncEndpointType :: !(Maybe EndpointType)
  , _dncCertificateUploadDate :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DomainNameConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dncAPIGatewayDomainName' - A domain name for the WebSocket API.
--
-- * 'dncCertificateName' - The user-friendly name of the certificate that will be used by the edge-optimized endpoint for this domain name.
--
-- * 'dncHostedZoneId' - The Amazon Route 53 Hosted Zone ID of the endpoint. See <docs.aws.amazon.com/general/latest/gr/rande.html#apigateway_region AWS Regions and Endpoints for API Gateway> .
--
-- * 'dncCertificateARN' - An AWS-managed certificate that will be used by the edge-optimized endpoint for this domain name. AWS Certificate Manager is the only supported source.
--
-- * 'dncEndpointType' - The endpoint type.
--
-- * 'dncCertificateUploadDate' - The timestamp when the certificate that was used by edge-optimized endpoint for this domain name was uploaded.
domainNameConfiguration
    :: DomainNameConfiguration
domainNameConfiguration =
  DomainNameConfiguration'
    { _dncAPIGatewayDomainName = Nothing
    , _dncCertificateName = Nothing
    , _dncHostedZoneId = Nothing
    , _dncCertificateARN = Nothing
    , _dncEndpointType = Nothing
    , _dncCertificateUploadDate = Nothing
    }


-- | A domain name for the WebSocket API.
dncAPIGatewayDomainName :: Lens' DomainNameConfiguration (Maybe Text)
dncAPIGatewayDomainName = lens _dncAPIGatewayDomainName (\ s a -> s{_dncAPIGatewayDomainName = a})

-- | The user-friendly name of the certificate that will be used by the edge-optimized endpoint for this domain name.
dncCertificateName :: Lens' DomainNameConfiguration (Maybe Text)
dncCertificateName = lens _dncCertificateName (\ s a -> s{_dncCertificateName = a})

-- | The Amazon Route 53 Hosted Zone ID of the endpoint. See <docs.aws.amazon.com/general/latest/gr/rande.html#apigateway_region AWS Regions and Endpoints for API Gateway> .
dncHostedZoneId :: Lens' DomainNameConfiguration (Maybe Text)
dncHostedZoneId = lens _dncHostedZoneId (\ s a -> s{_dncHostedZoneId = a})

-- | An AWS-managed certificate that will be used by the edge-optimized endpoint for this domain name. AWS Certificate Manager is the only supported source.
dncCertificateARN :: Lens' DomainNameConfiguration (Maybe Text)
dncCertificateARN = lens _dncCertificateARN (\ s a -> s{_dncCertificateARN = a})

-- | The endpoint type.
dncEndpointType :: Lens' DomainNameConfiguration (Maybe EndpointType)
dncEndpointType = lens _dncEndpointType (\ s a -> s{_dncEndpointType = a})

-- | The timestamp when the certificate that was used by edge-optimized endpoint for this domain name was uploaded.
dncCertificateUploadDate :: Lens' DomainNameConfiguration (Maybe UTCTime)
dncCertificateUploadDate = lens _dncCertificateUploadDate (\ s a -> s{_dncCertificateUploadDate = a}) . mapping _Time

instance FromJSON DomainNameConfiguration where
        parseJSON
          = withObject "DomainNameConfiguration"
              (\ x ->
                 DomainNameConfiguration' <$>
                   (x .:? "apiGatewayDomainName") <*>
                     (x .:? "certificateName")
                     <*> (x .:? "hostedZoneId")
                     <*> (x .:? "certificateArn")
                     <*> (x .:? "endpointType")
                     <*> (x .:? "certificateUploadDate"))

instance Hashable DomainNameConfiguration where

instance NFData DomainNameConfiguration where

instance ToJSON DomainNameConfiguration where
        toJSON DomainNameConfiguration'{..}
          = object
              (catMaybes
                 [("apiGatewayDomainName" .=) <$>
                    _dncAPIGatewayDomainName,
                  ("certificateName" .=) <$> _dncCertificateName,
                  ("hostedZoneId" .=) <$> _dncHostedZoneId,
                  ("certificateArn" .=) <$> _dncCertificateARN,
                  ("endpointType" .=) <$> _dncEndpointType,
                  ("certificateUploadDate" .=) <$>
                    _dncCertificateUploadDate])

-- | Represents an integration.
--
--
--
-- /See:/ 'integration' smart constructor.
data Integration = Integration'
  { _iIntegrationResponseSelectionExpression :: !(Maybe Text)
  , _iRequestTemplates :: !(Maybe (Map Text Text))
  , _iCredentialsARN :: !(Maybe Text)
  , _iIntegrationURI :: !(Maybe Text)
  , _iIntegrationId :: !(Maybe Text)
  , _iRequestParameters :: !(Maybe (Map Text Text))
  , _iConnectionId :: !(Maybe Text)
  , _iPassthroughBehavior :: !(Maybe PassthroughBehavior)
  , _iIntegrationMethod :: !(Maybe Text)
  , _iTemplateSelectionExpression :: !(Maybe Text)
  , _iTimeoutInMillis :: !(Maybe Nat)
  , _iContentHandlingStrategy :: !(Maybe ContentHandlingStrategy)
  , _iIntegrationType :: !(Maybe IntegrationType)
  , _iDescription :: !(Maybe Text)
  , _iConnectionType :: !(Maybe ConnectionType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Integration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iIntegrationResponseSelectionExpression' - 
--
-- * 'iRequestTemplates' - Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
--
-- * 'iCredentialsARN' - Specifies the credentials required for the integration, if any. For AWS integrations, three options are available. To specify an IAM Role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To require that the caller's identity be passed through from the request, specify the string arn:aws:iam::*:user/*. To use resource-based permissions on supported AWS services, specify null.
--
-- * 'iIntegrationURI' - Specifies the Uniform Resource Identifier (URI) of the integration endpoint. For HTTP or HTTP_PROXY integrations, the URI must be a fully formed, encoded HTTP(S) URL according to the <https://en.wikipedia.org/wiki/Uniform_Resource_Identifier RFC-3986 specification> , for either standard integration, where connectionType is not VPC_LINK, or private integration, where connectionType is VPC_LINK. For a private HTTP integration, the URI is not used for routing. For AWS or AWS_PROXY integrations, the URI is of the form arn:aws:apigateway:{region}:{subdomain.service|service}:path|action/{service_api}. Here, {Region} is the API Gateway region (e.g., us-east-1); {service} is the name of the integrated AWS service (e.g., s3); and {subdomain} is a designated subdomain supported by certain AWS service for fast host-name lookup. action can be used for an AWS service action-based API, using an Action={name}&{p1}={v1}&p2={v2}... query string. The ensuing {service_api} refers to a supported action {name} plus any required input parameters. Alternatively, path can be used for an AWS service path-based API. The ensuing service_api refers to the path to an AWS service resource, including the region of the integrated AWS service, if applicable. For example, for integration with the S3 API of GetObject, the URI can be either arn:aws:apigateway:us-west-2:s3:action/GetObject&Bucket={bucket}&Key={key} or arn:aws:apigateway:us-west-2:s3:path/{bucket}/{key}
--
-- * 'iIntegrationId' - Represents the identifier of an integration.
--
-- * 'iRequestParameters' - A key-value map specifying request parameters that are passed from the method request to the backend. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the backend. The method request parameter value must match the pattern of method.request.{location}.{name} , where  {location}  is querystring, path, or header; and  {name}  must be a valid and unique method request parameter name.
--
-- * 'iConnectionId' - The identifier of the VpcLink used for the integration when the connectionType is VPC_LINK; otherwise undefined.
--
-- * 'iPassthroughBehavior' - Specifies the pass-through behavior for incoming requests based on the Content-Type header in the request, and the available mapping templates specified as the requestTemplates property on the Integration resource. There are three valid values: WHEN_NO_MATCH, WHEN_NO_TEMPLATES, and NEVER. WHEN_NO_MATCH passes the request body for unmapped content types through to the integration backend without transformation. NEVER rejects unmapped content types with an HTTP 415 Unsupported Media Type response. WHEN_NO_TEMPLATES allows pass-through when the integration has no content types mapped to templates. However, if there is at least one content type defined, unmapped content types will be rejected with the same HTTP 415 Unsupported Media Type response.
--
-- * 'iIntegrationMethod' - Specifies the integration's HTTP method type.
--
-- * 'iTemplateSelectionExpression' - The template selection expression for the integration.
--
-- * 'iTimeoutInMillis' - Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
--
-- * 'iContentHandlingStrategy' - Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
--
-- * 'iIntegrationType' - The integration type of an integration. One of the following: AWS: for integrating the route or method request with an AWS service action, including the Lambda function-invoking action. With the Lambda function-invoking action, this is referred to as the Lambda custom integration. With any other AWS service action, this is known as AWS integration. AWS_PROXY: for integrating the route or method request with the Lambda function-invoking action with the client request passed through as-is. This integration is also referred to as Lambda proxy integration. HTTP: for integrating the route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC. This integration is also referred to as the HTTP custom integration. HTTP_PROXY: for integrating route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC, with the client request passed through as-is. This is also referred to as HTTP proxy integration. MOCK: for integrating the route or method request with API Gateway as a "loopback" endpoint without invoking any backend.
--
-- * 'iDescription' - Represents the description of an integration.
--
-- * 'iConnectionType' - The type of the network connection to the integration endpoint. The valid value is INTERNET for connections through the public routable internet or VPC_LINK for private connections between API Gateway and a network load balancer in a VPC. The default value is INTERNET.
integration
    :: Integration
integration =
  Integration'
    { _iIntegrationResponseSelectionExpression = Nothing
    , _iRequestTemplates = Nothing
    , _iCredentialsARN = Nothing
    , _iIntegrationURI = Nothing
    , _iIntegrationId = Nothing
    , _iRequestParameters = Nothing
    , _iConnectionId = Nothing
    , _iPassthroughBehavior = Nothing
    , _iIntegrationMethod = Nothing
    , _iTemplateSelectionExpression = Nothing
    , _iTimeoutInMillis = Nothing
    , _iContentHandlingStrategy = Nothing
    , _iIntegrationType = Nothing
    , _iDescription = Nothing
    , _iConnectionType = Nothing
    }


-- | 
iIntegrationResponseSelectionExpression :: Lens' Integration (Maybe Text)
iIntegrationResponseSelectionExpression = lens _iIntegrationResponseSelectionExpression (\ s a -> s{_iIntegrationResponseSelectionExpression = a})

-- | Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
iRequestTemplates :: Lens' Integration (HashMap Text Text)
iRequestTemplates = lens _iRequestTemplates (\ s a -> s{_iRequestTemplates = a}) . _Default . _Map

-- | Specifies the credentials required for the integration, if any. For AWS integrations, three options are available. To specify an IAM Role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To require that the caller's identity be passed through from the request, specify the string arn:aws:iam::*:user/*. To use resource-based permissions on supported AWS services, specify null.
iCredentialsARN :: Lens' Integration (Maybe Text)
iCredentialsARN = lens _iCredentialsARN (\ s a -> s{_iCredentialsARN = a})

-- | Specifies the Uniform Resource Identifier (URI) of the integration endpoint. For HTTP or HTTP_PROXY integrations, the URI must be a fully formed, encoded HTTP(S) URL according to the <https://en.wikipedia.org/wiki/Uniform_Resource_Identifier RFC-3986 specification> , for either standard integration, where connectionType is not VPC_LINK, or private integration, where connectionType is VPC_LINK. For a private HTTP integration, the URI is not used for routing. For AWS or AWS_PROXY integrations, the URI is of the form arn:aws:apigateway:{region}:{subdomain.service|service}:path|action/{service_api}. Here, {Region} is the API Gateway region (e.g., us-east-1); {service} is the name of the integrated AWS service (e.g., s3); and {subdomain} is a designated subdomain supported by certain AWS service for fast host-name lookup. action can be used for an AWS service action-based API, using an Action={name}&{p1}={v1}&p2={v2}... query string. The ensuing {service_api} refers to a supported action {name} plus any required input parameters. Alternatively, path can be used for an AWS service path-based API. The ensuing service_api refers to the path to an AWS service resource, including the region of the integrated AWS service, if applicable. For example, for integration with the S3 API of GetObject, the URI can be either arn:aws:apigateway:us-west-2:s3:action/GetObject&Bucket={bucket}&Key={key} or arn:aws:apigateway:us-west-2:s3:path/{bucket}/{key}
iIntegrationURI :: Lens' Integration (Maybe Text)
iIntegrationURI = lens _iIntegrationURI (\ s a -> s{_iIntegrationURI = a})

-- | Represents the identifier of an integration.
iIntegrationId :: Lens' Integration (Maybe Text)
iIntegrationId = lens _iIntegrationId (\ s a -> s{_iIntegrationId = a})

-- | A key-value map specifying request parameters that are passed from the method request to the backend. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the backend. The method request parameter value must match the pattern of method.request.{location}.{name} , where  {location}  is querystring, path, or header; and  {name}  must be a valid and unique method request parameter name.
iRequestParameters :: Lens' Integration (HashMap Text Text)
iRequestParameters = lens _iRequestParameters (\ s a -> s{_iRequestParameters = a}) . _Default . _Map

-- | The identifier of the VpcLink used for the integration when the connectionType is VPC_LINK; otherwise undefined.
iConnectionId :: Lens' Integration (Maybe Text)
iConnectionId = lens _iConnectionId (\ s a -> s{_iConnectionId = a})

-- | Specifies the pass-through behavior for incoming requests based on the Content-Type header in the request, and the available mapping templates specified as the requestTemplates property on the Integration resource. There are three valid values: WHEN_NO_MATCH, WHEN_NO_TEMPLATES, and NEVER. WHEN_NO_MATCH passes the request body for unmapped content types through to the integration backend without transformation. NEVER rejects unmapped content types with an HTTP 415 Unsupported Media Type response. WHEN_NO_TEMPLATES allows pass-through when the integration has no content types mapped to templates. However, if there is at least one content type defined, unmapped content types will be rejected with the same HTTP 415 Unsupported Media Type response.
iPassthroughBehavior :: Lens' Integration (Maybe PassthroughBehavior)
iPassthroughBehavior = lens _iPassthroughBehavior (\ s a -> s{_iPassthroughBehavior = a})

-- | Specifies the integration's HTTP method type.
iIntegrationMethod :: Lens' Integration (Maybe Text)
iIntegrationMethod = lens _iIntegrationMethod (\ s a -> s{_iIntegrationMethod = a})

-- | The template selection expression for the integration.
iTemplateSelectionExpression :: Lens' Integration (Maybe Text)
iTemplateSelectionExpression = lens _iTemplateSelectionExpression (\ s a -> s{_iTemplateSelectionExpression = a})

-- | Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
iTimeoutInMillis :: Lens' Integration (Maybe Natural)
iTimeoutInMillis = lens _iTimeoutInMillis (\ s a -> s{_iTimeoutInMillis = a}) . mapping _Nat

-- | Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
iContentHandlingStrategy :: Lens' Integration (Maybe ContentHandlingStrategy)
iContentHandlingStrategy = lens _iContentHandlingStrategy (\ s a -> s{_iContentHandlingStrategy = a})

-- | The integration type of an integration. One of the following: AWS: for integrating the route or method request with an AWS service action, including the Lambda function-invoking action. With the Lambda function-invoking action, this is referred to as the Lambda custom integration. With any other AWS service action, this is known as AWS integration. AWS_PROXY: for integrating the route or method request with the Lambda function-invoking action with the client request passed through as-is. This integration is also referred to as Lambda proxy integration. HTTP: for integrating the route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC. This integration is also referred to as the HTTP custom integration. HTTP_PROXY: for integrating route or method request with an HTTP endpoint, including a private HTTP endpoint within a VPC, with the client request passed through as-is. This is also referred to as HTTP proxy integration. MOCK: for integrating the route or method request with API Gateway as a "loopback" endpoint without invoking any backend.
iIntegrationType :: Lens' Integration (Maybe IntegrationType)
iIntegrationType = lens _iIntegrationType (\ s a -> s{_iIntegrationType = a})

-- | Represents the description of an integration.
iDescription :: Lens' Integration (Maybe Text)
iDescription = lens _iDescription (\ s a -> s{_iDescription = a})

-- | The type of the network connection to the integration endpoint. The valid value is INTERNET for connections through the public routable internet or VPC_LINK for private connections between API Gateway and a network load balancer in a VPC. The default value is INTERNET.
iConnectionType :: Lens' Integration (Maybe ConnectionType)
iConnectionType = lens _iConnectionType (\ s a -> s{_iConnectionType = a})

instance FromJSON Integration where
        parseJSON
          = withObject "Integration"
              (\ x ->
                 Integration' <$>
                   (x .:? "integrationResponseSelectionExpression") <*>
                     (x .:? "requestTemplates" .!= mempty)
                     <*> (x .:? "credentialsArn")
                     <*> (x .:? "integrationUri")
                     <*> (x .:? "integrationId")
                     <*> (x .:? "requestParameters" .!= mempty)
                     <*> (x .:? "connectionId")
                     <*> (x .:? "passthroughBehavior")
                     <*> (x .:? "integrationMethod")
                     <*> (x .:? "templateSelectionExpression")
                     <*> (x .:? "timeoutInMillis")
                     <*> (x .:? "contentHandlingStrategy")
                     <*> (x .:? "integrationType")
                     <*> (x .:? "description")
                     <*> (x .:? "connectionType"))

instance Hashable Integration where

instance NFData Integration where

-- | Represents an integration response.
--
--
--
-- /See:/ 'integrationResponse' smart constructor.
data IntegrationResponse = IntegrationResponse'
  { _intIntegrationResponseId :: !(Maybe Text)
  , _intTemplateSelectionExpression :: !(Maybe Text)
  , _intContentHandlingStrategy :: !(Maybe ContentHandlingStrategy)
  , _intResponseTemplates :: !(Maybe (Map Text Text))
  , _intResponseParameters :: !(Maybe (Map Text Text))
  , _intIntegrationResponseKey :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IntegrationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'intIntegrationResponseId' - The integration response ID.
--
-- * 'intTemplateSelectionExpression' - The template selection expressions for the integration response.
--
-- * 'intContentHandlingStrategy' - Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
--
-- * 'intResponseTemplates' - The collection of response templates for the integration response as a string-to-string map of key-value pairs. Response templates are represented as a key/value map, with a content-type as the key and a template as the value.
--
-- * 'intResponseParameters' - A key-value map specifying response parameters that are passed to the method response from the backend. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of method.response.header.{name}, where name is a valid and unique header name. The mapped non-static value must match the pattern of integration.response.header.{name} or integration.response.body.{JSON-expression}, where name is a valid and unique response header name and JSON-expression is a valid JSON expression without the $ prefix.
--
-- * 'intIntegrationResponseKey' - The integration response key.
integrationResponse
    :: Text -- ^ 'intIntegrationResponseKey'
    -> IntegrationResponse
integrationResponse pIntegrationResponseKey_ =
  IntegrationResponse'
    { _intIntegrationResponseId = Nothing
    , _intTemplateSelectionExpression = Nothing
    , _intContentHandlingStrategy = Nothing
    , _intResponseTemplates = Nothing
    , _intResponseParameters = Nothing
    , _intIntegrationResponseKey = pIntegrationResponseKey_
    }


-- | The integration response ID.
intIntegrationResponseId :: Lens' IntegrationResponse (Maybe Text)
intIntegrationResponseId = lens _intIntegrationResponseId (\ s a -> s{_intIntegrationResponseId = a})

-- | The template selection expressions for the integration response.
intTemplateSelectionExpression :: Lens' IntegrationResponse (Maybe Text)
intTemplateSelectionExpression = lens _intTemplateSelectionExpression (\ s a -> s{_intTemplateSelectionExpression = a})

-- | Specifies how to handle response payload content type conversions. Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the following behaviors: CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded string to the corresponding binary blob. CONVERT_TO_TEXT: Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the route response or method response without modification.
intContentHandlingStrategy :: Lens' IntegrationResponse (Maybe ContentHandlingStrategy)
intContentHandlingStrategy = lens _intContentHandlingStrategy (\ s a -> s{_intContentHandlingStrategy = a})

-- | The collection of response templates for the integration response as a string-to-string map of key-value pairs. Response templates are represented as a key/value map, with a content-type as the key and a template as the value.
intResponseTemplates :: Lens' IntegrationResponse (HashMap Text Text)
intResponseTemplates = lens _intResponseTemplates (\ s a -> s{_intResponseTemplates = a}) . _Default . _Map

-- | A key-value map specifying response parameters that are passed to the method response from the backend. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of method.response.header.{name}, where name is a valid and unique header name. The mapped non-static value must match the pattern of integration.response.header.{name} or integration.response.body.{JSON-expression}, where name is a valid and unique response header name and JSON-expression is a valid JSON expression without the $ prefix.
intResponseParameters :: Lens' IntegrationResponse (HashMap Text Text)
intResponseParameters = lens _intResponseParameters (\ s a -> s{_intResponseParameters = a}) . _Default . _Map

-- | The integration response key.
intIntegrationResponseKey :: Lens' IntegrationResponse Text
intIntegrationResponseKey = lens _intIntegrationResponseKey (\ s a -> s{_intIntegrationResponseKey = a})

instance FromJSON IntegrationResponse where
        parseJSON
          = withObject "IntegrationResponse"
              (\ x ->
                 IntegrationResponse' <$>
                   (x .:? "integrationResponseId") <*>
                     (x .:? "templateSelectionExpression")
                     <*> (x .:? "contentHandlingStrategy")
                     <*> (x .:? "responseTemplates" .!= mempty)
                     <*> (x .:? "responseParameters" .!= mempty)
                     <*> (x .: "integrationResponseKey"))

instance Hashable IntegrationResponse where

instance NFData IntegrationResponse where

-- | Represents a data model for an API. See <https://docs.aws.amazon.com/apigateway/latest/developerguide/models-mappings.html Create Models and Mapping Templates for Request and Response Mappings> .
--
--
--
-- /See:/ 'model' smart constructor.
data Model = Model'
  { _mModelId :: !(Maybe Text)
  , _mSchema :: !(Maybe Text)
  , _mDescription :: !(Maybe Text)
  , _mContentType :: !(Maybe Text)
  , _mName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Model' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mModelId' - The model identifier.
--
-- * 'mSchema' - The schema for the model. For application/json models, this should be JSON schema draft 4 model.
--
-- * 'mDescription' - The description of the model.
--
-- * 'mContentType' - The content-type for the model, for example, "application/json".
--
-- * 'mName' - The name of the model. Must be alphanumeric.
model
    :: Text -- ^ 'mName'
    -> Model
model pName_ =
  Model'
    { _mModelId = Nothing
    , _mSchema = Nothing
    , _mDescription = Nothing
    , _mContentType = Nothing
    , _mName = pName_
    }


-- | The model identifier.
mModelId :: Lens' Model (Maybe Text)
mModelId = lens _mModelId (\ s a -> s{_mModelId = a})

-- | The schema for the model. For application/json models, this should be JSON schema draft 4 model.
mSchema :: Lens' Model (Maybe Text)
mSchema = lens _mSchema (\ s a -> s{_mSchema = a})

-- | The description of the model.
mDescription :: Lens' Model (Maybe Text)
mDescription = lens _mDescription (\ s a -> s{_mDescription = a})

-- | The content-type for the model, for example, "application/json".
mContentType :: Lens' Model (Maybe Text)
mContentType = lens _mContentType (\ s a -> s{_mContentType = a})

-- | The name of the model. Must be alphanumeric.
mName :: Lens' Model Text
mName = lens _mName (\ s a -> s{_mName = a})

instance FromJSON Model where
        parseJSON
          = withObject "Model"
              (\ x ->
                 Model' <$>
                   (x .:? "modelId") <*> (x .:? "schema") <*>
                     (x .:? "description")
                     <*> (x .:? "contentType")
                     <*> (x .: "name"))

instance Hashable Model where

instance NFData Model where

-- | Validation constraints imposed on parameters of a request (path, query string, headers).
--
--
--
-- /See:/ 'parameterConstraints' smart constructor.
newtype ParameterConstraints = ParameterConstraints'
  { _pcRequired :: Maybe Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ParameterConstraints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcRequired' - Whether or not the parameter is required.
parameterConstraints
    :: ParameterConstraints
parameterConstraints = ParameterConstraints' {_pcRequired = Nothing}


-- | Whether or not the parameter is required.
pcRequired :: Lens' ParameterConstraints (Maybe Bool)
pcRequired = lens _pcRequired (\ s a -> s{_pcRequired = a})

instance FromJSON ParameterConstraints where
        parseJSON
          = withObject "ParameterConstraints"
              (\ x -> ParameterConstraints' <$> (x .:? "required"))

instance Hashable ParameterConstraints where

instance NFData ParameterConstraints where

instance ToJSON ParameterConstraints where
        toJSON ParameterConstraints'{..}
          = object
              (catMaybes [("required" .=) <$> _pcRequired])

-- | Represents a route.
--
--
--
-- /See:/ 'route' smart constructor.
data Route = Route'
  { _rouAuthorizationScopes :: !(Maybe [Text])
  , _rouModelSelectionExpression :: !(Maybe Text)
  , _rouRequestModels :: !(Maybe (Map Text Text))
  , _rouRouteResponseSelectionExpression :: !(Maybe Text)
  , _rouRequestParameters :: !(Maybe (Map Text ParameterConstraints))
  , _rouRouteId :: !(Maybe Text)
  , _rouAuthorizerId :: !(Maybe Text)
  , _rouOperationName :: !(Maybe Text)
  , _rouAuthorizationType :: !(Maybe AuthorizationType)
  , _rouAPIKeyRequired :: !(Maybe Bool)
  , _rouTarget :: !(Maybe Text)
  , _rouRouteKey :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Route' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rouAuthorizationScopes' - The authorization scopes supported by this route. 
--
-- * 'rouModelSelectionExpression' - The model selection expression for the route.
--
-- * 'rouRequestModels' - The request models for the route.
--
-- * 'rouRouteResponseSelectionExpression' - The route response selection expression for the route.
--
-- * 'rouRequestParameters' - The request parameters for the route.
--
-- * 'rouRouteId' - The route ID.
--
-- * 'rouAuthorizerId' - The identifier of the Authorizer resource to be associated with this route.
--
-- * 'rouOperationName' - The operation name for the route.
--
-- * 'rouAuthorizationType' - The authorization type for the route. Valid values are NONE for open access, AWS_IAM for using AWS IAM permissions.
--
-- * 'rouAPIKeyRequired' - Specifies whether an API key is required for this route.
--
-- * 'rouTarget' - The target for the route.
--
-- * 'rouRouteKey' - The route key for the route.
route
    :: Text -- ^ 'rouRouteKey'
    -> Route
route pRouteKey_ =
  Route'
    { _rouAuthorizationScopes = Nothing
    , _rouModelSelectionExpression = Nothing
    , _rouRequestModels = Nothing
    , _rouRouteResponseSelectionExpression = Nothing
    , _rouRequestParameters = Nothing
    , _rouRouteId = Nothing
    , _rouAuthorizerId = Nothing
    , _rouOperationName = Nothing
    , _rouAuthorizationType = Nothing
    , _rouAPIKeyRequired = Nothing
    , _rouTarget = Nothing
    , _rouRouteKey = pRouteKey_
    }


-- | The authorization scopes supported by this route. 
rouAuthorizationScopes :: Lens' Route [Text]
rouAuthorizationScopes = lens _rouAuthorizationScopes (\ s a -> s{_rouAuthorizationScopes = a}) . _Default . _Coerce

-- | The model selection expression for the route.
rouModelSelectionExpression :: Lens' Route (Maybe Text)
rouModelSelectionExpression = lens _rouModelSelectionExpression (\ s a -> s{_rouModelSelectionExpression = a})

-- | The request models for the route.
rouRequestModels :: Lens' Route (HashMap Text Text)
rouRequestModels = lens _rouRequestModels (\ s a -> s{_rouRequestModels = a}) . _Default . _Map

-- | The route response selection expression for the route.
rouRouteResponseSelectionExpression :: Lens' Route (Maybe Text)
rouRouteResponseSelectionExpression = lens _rouRouteResponseSelectionExpression (\ s a -> s{_rouRouteResponseSelectionExpression = a})

-- | The request parameters for the route.
rouRequestParameters :: Lens' Route (HashMap Text ParameterConstraints)
rouRequestParameters = lens _rouRequestParameters (\ s a -> s{_rouRequestParameters = a}) . _Default . _Map

-- | The route ID.
rouRouteId :: Lens' Route (Maybe Text)
rouRouteId = lens _rouRouteId (\ s a -> s{_rouRouteId = a})

-- | The identifier of the Authorizer resource to be associated with this route.
rouAuthorizerId :: Lens' Route (Maybe Text)
rouAuthorizerId = lens _rouAuthorizerId (\ s a -> s{_rouAuthorizerId = a})

-- | The operation name for the route.
rouOperationName :: Lens' Route (Maybe Text)
rouOperationName = lens _rouOperationName (\ s a -> s{_rouOperationName = a})

-- | The authorization type for the route. Valid values are NONE for open access, AWS_IAM for using AWS IAM permissions.
rouAuthorizationType :: Lens' Route (Maybe AuthorizationType)
rouAuthorizationType = lens _rouAuthorizationType (\ s a -> s{_rouAuthorizationType = a})

-- | Specifies whether an API key is required for this route.
rouAPIKeyRequired :: Lens' Route (Maybe Bool)
rouAPIKeyRequired = lens _rouAPIKeyRequired (\ s a -> s{_rouAPIKeyRequired = a})

-- | The target for the route.
rouTarget :: Lens' Route (Maybe Text)
rouTarget = lens _rouTarget (\ s a -> s{_rouTarget = a})

-- | The route key for the route.
rouRouteKey :: Lens' Route Text
rouRouteKey = lens _rouRouteKey (\ s a -> s{_rouRouteKey = a})

instance FromJSON Route where
        parseJSON
          = withObject "Route"
              (\ x ->
                 Route' <$>
                   (x .:? "authorizationScopes" .!= mempty) <*>
                     (x .:? "modelSelectionExpression")
                     <*> (x .:? "requestModels" .!= mempty)
                     <*> (x .:? "routeResponseSelectionExpression")
                     <*> (x .:? "requestParameters" .!= mempty)
                     <*> (x .:? "routeId")
                     <*> (x .:? "authorizerId")
                     <*> (x .:? "operationName")
                     <*> (x .:? "authorizationType")
                     <*> (x .:? "apiKeyRequired")
                     <*> (x .:? "target")
                     <*> (x .: "routeKey"))

instance Hashable Route where

instance NFData Route where

-- | Represents a route response.
--
--
--
-- /See:/ 'routeResponse' smart constructor.
data RouteResponse = RouteResponse'
  { _rModelSelectionExpression :: !(Maybe Text)
  , _rResponseModels :: !(Maybe (Map Text Text))
  , _rRouteResponseId :: !(Maybe Text)
  , _rResponseParameters :: !(Maybe (Map Text ParameterConstraints))
  , _rRouteResponseKey :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RouteResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rModelSelectionExpression' - Represents the model selection expression of a route response.
--
-- * 'rResponseModels' - Represents the response models of a route response.
--
-- * 'rRouteResponseId' - Represents the identifier of a route response.
--
-- * 'rResponseParameters' - Represents the response parameters of a route response.
--
-- * 'rRouteResponseKey' - Represents the route response key of a route response.
routeResponse
    :: Text -- ^ 'rRouteResponseKey'
    -> RouteResponse
routeResponse pRouteResponseKey_ =
  RouteResponse'
    { _rModelSelectionExpression = Nothing
    , _rResponseModels = Nothing
    , _rRouteResponseId = Nothing
    , _rResponseParameters = Nothing
    , _rRouteResponseKey = pRouteResponseKey_
    }


-- | Represents the model selection expression of a route response.
rModelSelectionExpression :: Lens' RouteResponse (Maybe Text)
rModelSelectionExpression = lens _rModelSelectionExpression (\ s a -> s{_rModelSelectionExpression = a})

-- | Represents the response models of a route response.
rResponseModels :: Lens' RouteResponse (HashMap Text Text)
rResponseModels = lens _rResponseModels (\ s a -> s{_rResponseModels = a}) . _Default . _Map

-- | Represents the identifier of a route response.
rRouteResponseId :: Lens' RouteResponse (Maybe Text)
rRouteResponseId = lens _rRouteResponseId (\ s a -> s{_rRouteResponseId = a})

-- | Represents the response parameters of a route response.
rResponseParameters :: Lens' RouteResponse (HashMap Text ParameterConstraints)
rResponseParameters = lens _rResponseParameters (\ s a -> s{_rResponseParameters = a}) . _Default . _Map

-- | Represents the route response key of a route response.
rRouteResponseKey :: Lens' RouteResponse Text
rRouteResponseKey = lens _rRouteResponseKey (\ s a -> s{_rRouteResponseKey = a})

instance FromJSON RouteResponse where
        parseJSON
          = withObject "RouteResponse"
              (\ x ->
                 RouteResponse' <$>
                   (x .:? "modelSelectionExpression") <*>
                     (x .:? "responseModels" .!= mempty)
                     <*> (x .:? "routeResponseId")
                     <*> (x .:? "responseParameters" .!= mempty)
                     <*> (x .: "routeResponseKey"))

instance Hashable RouteResponse where

instance NFData RouteResponse where

-- | Represents a collection of route settings.
--
--
--
-- /See:/ 'routeSettings' smart constructor.
data RouteSettings = RouteSettings'
  { _rsDataTraceEnabled :: !(Maybe Bool)
  , _rsThrottlingBurstLimit :: !(Maybe Int)
  , _rsLoggingLevel :: !(Maybe LoggingLevel)
  , _rsThrottlingRateLimit :: !(Maybe Double)
  , _rsDetailedMetricsEnabled :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RouteSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsDataTraceEnabled' - Specifies whether (true) or not (false) data trace logging is enabled for this route. This property affects the log entries pushed to Amazon CloudWatch Logs.
--
-- * 'rsThrottlingBurstLimit' - Specifies the throttling burst limit.
--
-- * 'rsLoggingLevel' - Specifies the logging level for this route: DEBUG, INFO, or WARN. This property affects the log entries pushed to Amazon CloudWatch Logs.
--
-- * 'rsThrottlingRateLimit' - Specifies the throttling rate limit.
--
-- * 'rsDetailedMetricsEnabled' - Specifies whether detailed metrics are enabled.
routeSettings
    :: RouteSettings
routeSettings =
  RouteSettings'
    { _rsDataTraceEnabled = Nothing
    , _rsThrottlingBurstLimit = Nothing
    , _rsLoggingLevel = Nothing
    , _rsThrottlingRateLimit = Nothing
    , _rsDetailedMetricsEnabled = Nothing
    }


-- | Specifies whether (true) or not (false) data trace logging is enabled for this route. This property affects the log entries pushed to Amazon CloudWatch Logs.
rsDataTraceEnabled :: Lens' RouteSettings (Maybe Bool)
rsDataTraceEnabled = lens _rsDataTraceEnabled (\ s a -> s{_rsDataTraceEnabled = a})

-- | Specifies the throttling burst limit.
rsThrottlingBurstLimit :: Lens' RouteSettings (Maybe Int)
rsThrottlingBurstLimit = lens _rsThrottlingBurstLimit (\ s a -> s{_rsThrottlingBurstLimit = a})

-- | Specifies the logging level for this route: DEBUG, INFO, or WARN. This property affects the log entries pushed to Amazon CloudWatch Logs.
rsLoggingLevel :: Lens' RouteSettings (Maybe LoggingLevel)
rsLoggingLevel = lens _rsLoggingLevel (\ s a -> s{_rsLoggingLevel = a})

-- | Specifies the throttling rate limit.
rsThrottlingRateLimit :: Lens' RouteSettings (Maybe Double)
rsThrottlingRateLimit = lens _rsThrottlingRateLimit (\ s a -> s{_rsThrottlingRateLimit = a})

-- | Specifies whether detailed metrics are enabled.
rsDetailedMetricsEnabled :: Lens' RouteSettings (Maybe Bool)
rsDetailedMetricsEnabled = lens _rsDetailedMetricsEnabled (\ s a -> s{_rsDetailedMetricsEnabled = a})

instance FromJSON RouteSettings where
        parseJSON
          = withObject "RouteSettings"
              (\ x ->
                 RouteSettings' <$>
                   (x .:? "dataTraceEnabled") <*>
                     (x .:? "throttlingBurstLimit")
                     <*> (x .:? "loggingLevel")
                     <*> (x .:? "throttlingRateLimit")
                     <*> (x .:? "detailedMetricsEnabled"))

instance Hashable RouteSettings where

instance NFData RouteSettings where

instance ToJSON RouteSettings where
        toJSON RouteSettings'{..}
          = object
              (catMaybes
                 [("dataTraceEnabled" .=) <$> _rsDataTraceEnabled,
                  ("throttlingBurstLimit" .=) <$>
                    _rsThrottlingBurstLimit,
                  ("loggingLevel" .=) <$> _rsLoggingLevel,
                  ("throttlingRateLimit" .=) <$>
                    _rsThrottlingRateLimit,
                  ("detailedMetricsEnabled" .=) <$>
                    _rsDetailedMetricsEnabled])

-- | Represents an API stage.
--
--
--
-- /See:/ 'stage' smart constructor.
data Stage = Stage'
  { _sDeploymentId :: !(Maybe Text)
  , _sRouteSettings :: !(Maybe (Map Text RouteSettings))
  , _sAccessLogSettings :: !(Maybe AccessLogSettings)
  , _sClientCertificateId :: !(Maybe Text)
  , _sStageVariables :: !(Maybe (Map Text Text))
  , _sCreatedDate :: !(Maybe POSIX)
  , _sDefaultRouteSettings :: !(Maybe RouteSettings)
  , _sLastUpdatedDate :: !(Maybe POSIX)
  , _sDescription :: !(Maybe Text)
  , _sStageName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Stage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sDeploymentId' - The identifier of the Deployment that the Stage is associated with.
--
-- * 'sRouteSettings' - Route settings for the stage.
--
-- * 'sAccessLogSettings' - Settings for logging access in this stage.
--
-- * 'sClientCertificateId' - The identifier of a client certificate for a Stage.
--
-- * 'sStageVariables' - A map that defines the stage variables for a stage resource. Variable names can have alphanumeric and underscore characters, and the values must match [A-Za-z0-9-._~:/?#&=,]+.
--
-- * 'sCreatedDate' - The timestamp when the stage was created.
--
-- * 'sDefaultRouteSettings' - Default route settings for the stage.
--
-- * 'sLastUpdatedDate' - The timestamp when the stage was last updated.
--
-- * 'sDescription' - The description of the stage.
--
-- * 'sStageName' - The name of the stage.
stage
    :: Text -- ^ 'sStageName'
    -> Stage
stage pStageName_ =
  Stage'
    { _sDeploymentId = Nothing
    , _sRouteSettings = Nothing
    , _sAccessLogSettings = Nothing
    , _sClientCertificateId = Nothing
    , _sStageVariables = Nothing
    , _sCreatedDate = Nothing
    , _sDefaultRouteSettings = Nothing
    , _sLastUpdatedDate = Nothing
    , _sDescription = Nothing
    , _sStageName = pStageName_
    }


-- | The identifier of the Deployment that the Stage is associated with.
sDeploymentId :: Lens' Stage (Maybe Text)
sDeploymentId = lens _sDeploymentId (\ s a -> s{_sDeploymentId = a})

-- | Route settings for the stage.
sRouteSettings :: Lens' Stage (HashMap Text RouteSettings)
sRouteSettings = lens _sRouteSettings (\ s a -> s{_sRouteSettings = a}) . _Default . _Map

-- | Settings for logging access in this stage.
sAccessLogSettings :: Lens' Stage (Maybe AccessLogSettings)
sAccessLogSettings = lens _sAccessLogSettings (\ s a -> s{_sAccessLogSettings = a})

-- | The identifier of a client certificate for a Stage.
sClientCertificateId :: Lens' Stage (Maybe Text)
sClientCertificateId = lens _sClientCertificateId (\ s a -> s{_sClientCertificateId = a})

-- | A map that defines the stage variables for a stage resource. Variable names can have alphanumeric and underscore characters, and the values must match [A-Za-z0-9-._~:/?#&=,]+.
sStageVariables :: Lens' Stage (HashMap Text Text)
sStageVariables = lens _sStageVariables (\ s a -> s{_sStageVariables = a}) . _Default . _Map

-- | The timestamp when the stage was created.
sCreatedDate :: Lens' Stage (Maybe UTCTime)
sCreatedDate = lens _sCreatedDate (\ s a -> s{_sCreatedDate = a}) . mapping _Time

-- | Default route settings for the stage.
sDefaultRouteSettings :: Lens' Stage (Maybe RouteSettings)
sDefaultRouteSettings = lens _sDefaultRouteSettings (\ s a -> s{_sDefaultRouteSettings = a})

-- | The timestamp when the stage was last updated.
sLastUpdatedDate :: Lens' Stage (Maybe UTCTime)
sLastUpdatedDate = lens _sLastUpdatedDate (\ s a -> s{_sLastUpdatedDate = a}) . mapping _Time

-- | The description of the stage.
sDescription :: Lens' Stage (Maybe Text)
sDescription = lens _sDescription (\ s a -> s{_sDescription = a})

-- | The name of the stage.
sStageName :: Lens' Stage Text
sStageName = lens _sStageName (\ s a -> s{_sStageName = a})

instance FromJSON Stage where
        parseJSON
          = withObject "Stage"
              (\ x ->
                 Stage' <$>
                   (x .:? "deploymentId") <*>
                     (x .:? "routeSettings" .!= mempty)
                     <*> (x .:? "accessLogSettings")
                     <*> (x .:? "clientCertificateId")
                     <*> (x .:? "stageVariables" .!= mempty)
                     <*> (x .:? "createdDate")
                     <*> (x .:? "defaultRouteSettings")
                     <*> (x .:? "lastUpdatedDate")
                     <*> (x .:? "description")
                     <*> (x .: "stageName"))

instance Hashable Stage where

instance NFData Stage where
