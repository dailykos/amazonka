{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGatewayV2.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.APIGatewayV2.Types
    (
    -- * Service Configuration
      apiGatewayV2

    -- * Errors
    , _ConflictException
    , _NotFoundException
    , _TooManyRequestsException
    , _BadRequestException

    -- * Re-exported Types
    , module Network.AWS.APIGatewayV2.Internal

    -- * AuthorizationType
    , AuthorizationType (..)

    -- * AuthorizerType
    , AuthorizerType (..)

    -- * ConnectionType
    , ConnectionType (..)

    -- * ContentHandlingStrategy
    , ContentHandlingStrategy (..)

    -- * DeploymentStatus
    , DeploymentStatus (..)

    -- * EndpointType
    , EndpointType (..)

    -- * IntegrationType
    , IntegrationType (..)

    -- * LoggingLevel
    , LoggingLevel (..)

    -- * PassthroughBehavior
    , PassthroughBehavior (..)

    -- * ProtocolType
    , ProtocolType (..)

    -- * API
    , API
    , api
    , apiAPIId
    , apiAPIEndpoint
    , apiWarnings
    , apiCreatedDate
    , apiVersion
    , apiAPIKeySelectionExpression
    , apiDisableSchemaValidation
    , apiDescription
    , apiRouteSelectionExpression
    , apiProtocolType
    , apiName

    -- * AccessLogSettings
    , AccessLogSettings
    , accessLogSettings
    , alsFormat
    , alsDestinationARN

    -- * Authorizer
    , Authorizer
    , authorizer
    , aAuthorizerCredentialsARN
    , aIdentityValidationExpression
    , aAuthorizerURI
    , aAuthorizerId
    , aProviderARNs
    , aAuthorizerResultTtlInSeconds
    , aIdentitySource
    , aAuthorizerType
    , aName

    -- * Deployment
    , Deployment
    , deployment
    , dDeploymentId
    , dDeploymentStatusMessage
    , dCreatedDate
    , dDeploymentStatus
    , dDescription

    -- * DomainName
    , DomainName
    , domainName
    , dnDomainNameConfigurations
    , dnAPIMappingSelectionExpression
    , dnDomainName

    -- * DomainNameConfiguration
    , DomainNameConfiguration
    , domainNameConfiguration
    , dncAPIGatewayDomainName
    , dncCertificateName
    , dncHostedZoneId
    , dncCertificateARN
    , dncEndpointType
    , dncCertificateUploadDate

    -- * Integration
    , Integration
    , integration
    , iIntegrationResponseSelectionExpression
    , iRequestTemplates
    , iCredentialsARN
    , iIntegrationURI
    , iIntegrationId
    , iRequestParameters
    , iConnectionId
    , iPassthroughBehavior
    , iIntegrationMethod
    , iTemplateSelectionExpression
    , iTimeoutInMillis
    , iContentHandlingStrategy
    , iIntegrationType
    , iDescription
    , iConnectionType

    -- * IntegrationResponse
    , IntegrationResponse
    , integrationResponse
    , intIntegrationResponseId
    , intTemplateSelectionExpression
    , intContentHandlingStrategy
    , intResponseTemplates
    , intResponseParameters
    , intIntegrationResponseKey

    -- * Model
    , Model
    , model
    , mModelId
    , mSchema
    , mDescription
    , mContentType
    , mName

    -- * ParameterConstraints
    , ParameterConstraints
    , parameterConstraints
    , pcRequired

    -- * Route
    , Route
    , route
    , rouAuthorizationScopes
    , rouModelSelectionExpression
    , rouRequestModels
    , rouRouteResponseSelectionExpression
    , rouRequestParameters
    , rouRouteId
    , rouAuthorizerId
    , rouOperationName
    , rouAuthorizationType
    , rouAPIKeyRequired
    , rouTarget
    , rouRouteKey

    -- * RouteResponse
    , RouteResponse
    , routeResponse
    , rModelSelectionExpression
    , rResponseModels
    , rRouteResponseId
    , rResponseParameters
    , rRouteResponseKey

    -- * RouteSettings
    , RouteSettings
    , routeSettings
    , rsDataTraceEnabled
    , rsThrottlingBurstLimit
    , rsLoggingLevel
    , rsThrottlingRateLimit
    , rsDetailedMetricsEnabled

    -- * Stage
    , Stage
    , stage
    , sDeploymentId
    , sRouteSettings
    , sAccessLogSettings
    , sClientCertificateId
    , sStageVariables
    , sCreatedDate
    , sDefaultRouteSettings
    , sLastUpdatedDate
    , sDescription
    , sStageName
    ) where

import Network.AWS.APIGatewayV2.Internal
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.APIGatewayV2.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2018-11-29@ of the Amazon ApiGatewayV2 SDK configuration.
apiGatewayV2 :: Service
apiGatewayV2 =
  Service
    { _svcAbbrev = "APIGatewayV2"
    , _svcSigner = v4
    , _svcPrefix = "apigateway"
    , _svcVersion = "2018-11-29"
    , _svcEndpoint = defaultEndpoint apiGatewayV2
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "APIGatewayV2"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The requested operation would cause a conflict with the current state of a service resource associated with the request. Resolve the conflict before retrying this request. See the accompanying error message for details.
--
--
_ConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictException =
  _MatchServiceError apiGatewayV2 "ConflictException" . hasStatus 409


-- | The resource specified in the request was not found. See the message field for more information.
--
--
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException =
  _MatchServiceError apiGatewayV2 "NotFoundException" . hasStatus 404


-- | A limit has been exceeded. See the accompanying error message for details.
--
--
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException =
  _MatchServiceError apiGatewayV2 "TooManyRequestsException" . hasStatus 429


-- | The request is not valid, for example, the input is incomplete or incorrect. See the accompanying error message for details.
--
--
_BadRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_BadRequestException =
  _MatchServiceError apiGatewayV2 "BadRequestException" . hasStatus 400

