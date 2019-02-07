{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGatewayV2
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon API Gateway V2
--
--
module Network.AWS.APIGatewayV2
    (
    -- * Service Configuration
      apiGatewayV2

    -- * Errors
    -- $errors

    -- ** ConflictException
    , _ConflictException

    -- ** NotFoundException
    , _NotFoundException

    -- ** TooManyRequestsException
    , _TooManyRequestsException

    -- ** BadRequestException
    , _BadRequestException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateAPI 
    , module Network.AWS.APIGatewayV2.CreateAPI

    -- ** GetDeployments (Paginated)
    , module Network.AWS.APIGatewayV2.GetDeployments

    -- ** GetRouteResponses (Paginated)
    , module Network.AWS.APIGatewayV2.GetRouteResponses

    -- ** GetDeployment 
    , module Network.AWS.APIGatewayV2.GetDeployment

    -- ** GetDomainNames (Paginated)
    , module Network.AWS.APIGatewayV2.GetDomainNames

    -- ** GetModels (Paginated)
    , module Network.AWS.APIGatewayV2.GetModels

    -- ** CreateIntegration 
    , module Network.AWS.APIGatewayV2.CreateIntegration

    -- ** DeleteStage 
    , module Network.AWS.APIGatewayV2.DeleteStage

    -- ** UpdateStage 
    , module Network.AWS.APIGatewayV2.UpdateStage

    -- ** CreateDeployment 
    , module Network.AWS.APIGatewayV2.CreateDeployment

    -- ** DeleteRoute 
    , module Network.AWS.APIGatewayV2.DeleteRoute

    -- ** UpdateRoute 
    , module Network.AWS.APIGatewayV2.UpdateRoute

    -- ** GetIntegrationResponses (Paginated)
    , module Network.AWS.APIGatewayV2.GetIntegrationResponses

    -- ** GetIntegration 
    , module Network.AWS.APIGatewayV2.GetIntegration

    -- ** DeleteDeployment 
    , module Network.AWS.APIGatewayV2.DeleteDeployment

    -- ** UpdateDeployment 
    , module Network.AWS.APIGatewayV2.UpdateDeployment

    -- ** DeleteRouteResponse 
    , module Network.AWS.APIGatewayV2.DeleteRouteResponse

    -- ** UpdateRouteResponse 
    , module Network.AWS.APIGatewayV2.UpdateRouteResponse

    -- ** CreateModel 
    , module Network.AWS.APIGatewayV2.CreateModel

    -- ** GetIntegrationResponse 
    , module Network.AWS.APIGatewayV2.GetIntegrationResponse

    -- ** CreateDomainName 
    , module Network.AWS.APIGatewayV2.CreateDomainName

    -- ** DeleteModel 
    , module Network.AWS.APIGatewayV2.DeleteModel

    -- ** UpdateModel 
    , module Network.AWS.APIGatewayV2.UpdateModel

    -- ** CreateRouteResponse 
    , module Network.AWS.APIGatewayV2.CreateRouteResponse

    -- ** GetStages (Paginated)
    , module Network.AWS.APIGatewayV2.GetStages

    -- ** GetModel 
    , module Network.AWS.APIGatewayV2.GetModel

    -- ** GetAPIMappings 
    , module Network.AWS.APIGatewayV2.GetAPIMappings

    -- ** CreateIntegrationResponse 
    , module Network.AWS.APIGatewayV2.CreateIntegrationResponse

    -- ** GetDomainName 
    , module Network.AWS.APIGatewayV2.GetDomainName

    -- ** GetAuthorizers (Paginated)
    , module Network.AWS.APIGatewayV2.GetAuthorizers

    -- ** GetRouteResponse 
    , module Network.AWS.APIGatewayV2.GetRouteResponse

    -- ** GetRoutes (Paginated)
    , module Network.AWS.APIGatewayV2.GetRoutes

    -- ** DeleteIntegrationResponse 
    , module Network.AWS.APIGatewayV2.DeleteIntegrationResponse

    -- ** UpdateIntegrationResponse 
    , module Network.AWS.APIGatewayV2.UpdateIntegrationResponse

    -- ** DeleteIntegration 
    , module Network.AWS.APIGatewayV2.DeleteIntegration

    -- ** UpdateIntegration 
    , module Network.AWS.APIGatewayV2.UpdateIntegration

    -- ** GetRoute 
    , module Network.AWS.APIGatewayV2.GetRoute

    -- ** GetAuthorizer 
    , module Network.AWS.APIGatewayV2.GetAuthorizer

    -- ** GetStage 
    , module Network.AWS.APIGatewayV2.GetStage

    -- ** GetAPIMapping 
    , module Network.AWS.APIGatewayV2.GetAPIMapping

    -- ** GetAPIs (Paginated)
    , module Network.AWS.APIGatewayV2.GetAPIs

    -- ** UpdateAPIMapping 
    , module Network.AWS.APIGatewayV2.UpdateAPIMapping

    -- ** DeleteAPIMapping 
    , module Network.AWS.APIGatewayV2.DeleteAPIMapping

    -- ** CreateRoute 
    , module Network.AWS.APIGatewayV2.CreateRoute

    -- ** CreateAuthorizer 
    , module Network.AWS.APIGatewayV2.CreateAuthorizer

    -- ** UpdateAuthorizer 
    , module Network.AWS.APIGatewayV2.UpdateAuthorizer

    -- ** DeleteAuthorizer 
    , module Network.AWS.APIGatewayV2.DeleteAuthorizer

    -- ** CreateAPIMapping 
    , module Network.AWS.APIGatewayV2.CreateAPIMapping

    -- ** CreateStage 
    , module Network.AWS.APIGatewayV2.CreateStage

    -- ** GetIntegrations (Paginated)
    , module Network.AWS.APIGatewayV2.GetIntegrations

    -- ** UpdateDomainName 
    , module Network.AWS.APIGatewayV2.UpdateDomainName

    -- ** DeleteDomainName 
    , module Network.AWS.APIGatewayV2.DeleteDomainName

    -- ** GetAPI 
    , module Network.AWS.APIGatewayV2.GetAPI

    -- ** DeleteAPI 
    , module Network.AWS.APIGatewayV2.DeleteAPI

    -- ** UpdateAPI 
    , module Network.AWS.APIGatewayV2.UpdateAPI

    -- ** GetModelTemplate 
    , module Network.AWS.APIGatewayV2.GetModelTemplate

    -- * Types

    -- ** Common
    , module Network.AWS.APIGatewayV2.Internal

    -- ** AuthorizationType
    , AuthorizationType (..)

    -- ** AuthorizerType
    , AuthorizerType (..)

    -- ** ConnectionType
    , ConnectionType (..)

    -- ** ContentHandlingStrategy
    , ContentHandlingStrategy (..)

    -- ** DeploymentStatus
    , DeploymentStatus (..)

    -- ** EndpointType
    , EndpointType (..)

    -- ** IntegrationType
    , IntegrationType (..)

    -- ** LoggingLevel
    , LoggingLevel (..)

    -- ** PassthroughBehavior
    , PassthroughBehavior (..)

    -- ** ProtocolType
    , ProtocolType (..)

    -- ** API
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

    -- ** AccessLogSettings
    , AccessLogSettings
    , accessLogSettings
    , alsFormat
    , alsDestinationARN

    -- ** Authorizer
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

    -- ** Deployment
    , Deployment
    , deployment
    , dDeploymentId
    , dDeploymentStatusMessage
    , dCreatedDate
    , dDeploymentStatus
    , dDescription

    -- ** DomainName
    , DomainName
    , domainName
    , dnDomainNameConfigurations
    , dnAPIMappingSelectionExpression
    , dnDomainName

    -- ** DomainNameConfiguration
    , DomainNameConfiguration
    , domainNameConfiguration
    , dncAPIGatewayDomainName
    , dncCertificateName
    , dncHostedZoneId
    , dncCertificateARN
    , dncEndpointType
    , dncCertificateUploadDate

    -- ** Integration
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

    -- ** IntegrationResponse
    , IntegrationResponse
    , integrationResponse
    , intIntegrationResponseId
    , intTemplateSelectionExpression
    , intContentHandlingStrategy
    , intResponseTemplates
    , intResponseParameters
    , intIntegrationResponseKey

    -- ** Model
    , Model
    , model
    , mModelId
    , mSchema
    , mDescription
    , mContentType
    , mName

    -- ** ParameterConstraints
    , ParameterConstraints
    , parameterConstraints
    , pcRequired

    -- ** Route
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

    -- ** RouteResponse
    , RouteResponse
    , routeResponse
    , rModelSelectionExpression
    , rResponseModels
    , rRouteResponseId
    , rResponseParameters
    , rRouteResponseKey

    -- ** RouteSettings
    , RouteSettings
    , routeSettings
    , rsDataTraceEnabled
    , rsThrottlingBurstLimit
    , rsLoggingLevel
    , rsThrottlingRateLimit
    , rsDetailedMetricsEnabled

    -- ** Stage
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

import Network.AWS.APIGatewayV2.CreateAPI
import Network.AWS.APIGatewayV2.CreateAPIMapping
import Network.AWS.APIGatewayV2.CreateAuthorizer
import Network.AWS.APIGatewayV2.CreateDeployment
import Network.AWS.APIGatewayV2.CreateDomainName
import Network.AWS.APIGatewayV2.CreateIntegration
import Network.AWS.APIGatewayV2.CreateIntegrationResponse
import Network.AWS.APIGatewayV2.CreateModel
import Network.AWS.APIGatewayV2.CreateRoute
import Network.AWS.APIGatewayV2.CreateRouteResponse
import Network.AWS.APIGatewayV2.CreateStage
import Network.AWS.APIGatewayV2.DeleteAPI
import Network.AWS.APIGatewayV2.DeleteAPIMapping
import Network.AWS.APIGatewayV2.DeleteAuthorizer
import Network.AWS.APIGatewayV2.DeleteDeployment
import Network.AWS.APIGatewayV2.DeleteDomainName
import Network.AWS.APIGatewayV2.DeleteIntegration
import Network.AWS.APIGatewayV2.DeleteIntegrationResponse
import Network.AWS.APIGatewayV2.DeleteModel
import Network.AWS.APIGatewayV2.DeleteRoute
import Network.AWS.APIGatewayV2.DeleteRouteResponse
import Network.AWS.APIGatewayV2.DeleteStage
import Network.AWS.APIGatewayV2.GetAPI
import Network.AWS.APIGatewayV2.GetAPIMapping
import Network.AWS.APIGatewayV2.GetAPIMappings
import Network.AWS.APIGatewayV2.GetAPIs
import Network.AWS.APIGatewayV2.GetAuthorizer
import Network.AWS.APIGatewayV2.GetAuthorizers
import Network.AWS.APIGatewayV2.GetDeployment
import Network.AWS.APIGatewayV2.GetDeployments
import Network.AWS.APIGatewayV2.GetDomainName
import Network.AWS.APIGatewayV2.GetDomainNames
import Network.AWS.APIGatewayV2.GetIntegration
import Network.AWS.APIGatewayV2.GetIntegrationResponse
import Network.AWS.APIGatewayV2.GetIntegrationResponses
import Network.AWS.APIGatewayV2.GetIntegrations
import Network.AWS.APIGatewayV2.GetModel
import Network.AWS.APIGatewayV2.GetModelTemplate
import Network.AWS.APIGatewayV2.GetModels
import Network.AWS.APIGatewayV2.GetRoute
import Network.AWS.APIGatewayV2.GetRouteResponse
import Network.AWS.APIGatewayV2.GetRouteResponses
import Network.AWS.APIGatewayV2.GetRoutes
import Network.AWS.APIGatewayV2.GetStage
import Network.AWS.APIGatewayV2.GetStages
import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.UpdateAPI
import Network.AWS.APIGatewayV2.UpdateAPIMapping
import Network.AWS.APIGatewayV2.UpdateAuthorizer
import Network.AWS.APIGatewayV2.UpdateDeployment
import Network.AWS.APIGatewayV2.UpdateDomainName
import Network.AWS.APIGatewayV2.UpdateIntegration
import Network.AWS.APIGatewayV2.UpdateIntegrationResponse
import Network.AWS.APIGatewayV2.UpdateModel
import Network.AWS.APIGatewayV2.UpdateRoute
import Network.AWS.APIGatewayV2.UpdateRouteResponse
import Network.AWS.APIGatewayV2.UpdateStage
import Network.AWS.APIGatewayV2.Waiters
import Network.AWS.APIGatewayV2.Internal

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'APIGatewayV2'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
