{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppMesh.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppMesh.Types
    (
    -- * Service Configuration
      appMesh

    -- * Errors
    , _ConflictException
    , _ForbiddenException
    , _NotFoundException
    , _TooManyRequestsException
    , _InternalServerErrorException
    , _ServiceUnavailableException
    , _BadRequestException
    , _LimitExceededException
    , _ResourceInUseException

    -- * Re-exported Types
    , module Network.AWS.AppMesh.Internal

    -- * MeshStatusCode
    , MeshStatusCode (..)

    -- * PortProtocol
    , PortProtocol (..)

    -- * RouteStatusCode
    , RouteStatusCode (..)

    -- * VirtualNodeStatusCode
    , VirtualNodeStatusCode (..)

    -- * VirtualRouterStatusCode
    , VirtualRouterStatusCode (..)

    -- * DNSServiceDiscovery
    , DNSServiceDiscovery
    , dnsServiceDiscovery
    , dsdServiceName

    -- * HTTPRoute
    , HTTPRoute
    , hTTPRoute
    , httprAction
    , httprMatch

    -- * HTTPRouteAction
    , HTTPRouteAction
    , hTTPRouteAction
    , httpraWeightedTargets

    -- * HTTPRouteMatch
    , HTTPRouteMatch
    , hTTPRouteMatch
    , httprmPrefix

    -- * HealthCheckPolicy
    , HealthCheckPolicy
    , healthCheckPolicy
    , hcpPath
    , hcpPort
    , hcpHealthyThreshold
    , hcpIntervalMillis
    , hcpProtocol
    , hcpTimeoutMillis
    , hcpUnhealthyThreshold

    -- * Listener
    , Listener
    , listener
    , lHealthCheck
    , lPortMapping

    -- * MeshData
    , MeshData
    , meshData
    , mdStatus
    , mdMeshName
    , mdMetadata

    -- * MeshRef
    , MeshRef
    , meshRef
    , mrMeshName
    , mrArn

    -- * MeshStatus
    , MeshStatus
    , meshStatus
    , msStatus

    -- * PortMapping
    , PortMapping
    , portMapping
    , pmProtocol
    , pmPort

    -- * ResourceMetadata
    , ResourceMetadata
    , resourceMetadata
    , rmLastUpdatedAt
    , rmArn
    , rmCreatedAt
    , rmUid
    , rmVersion

    -- * RouteData
    , RouteData
    , routeData
    , rdStatus
    , rdSpec
    , rdMetadata
    , rdMeshName
    , rdRouteName
    , rdVirtualRouterName

    -- * RouteRef
    , RouteRef
    , routeRef
    , rrMeshName
    , rrArn
    , rrRouteName
    , rrVirtualRouterName

    -- * RouteSpec
    , RouteSpec
    , routeSpec
    , rsHttpRoute

    -- * RouteStatus
    , RouteStatus
    , routeStatus
    , rsStatus

    -- * ServiceDiscovery
    , ServiceDiscovery
    , serviceDiscovery
    , sdDns

    -- * VirtualNodeData
    , VirtualNodeData
    , virtualNodeData
    , vndStatus
    , vndSpec
    , vndMetadata
    , vndMeshName
    , vndVirtualNodeName

    -- * VirtualNodeRef
    , VirtualNodeRef
    , virtualNodeRef
    , vnrMeshName
    , vnrArn
    , vnrVirtualNodeName

    -- * VirtualNodeSpec
    , VirtualNodeSpec
    , virtualNodeSpec
    , vnsBackends
    , vnsServiceDiscovery
    , vnsListeners

    -- * VirtualNodeStatus
    , VirtualNodeStatus
    , virtualNodeStatus
    , vnsStatus

    -- * VirtualRouterData
    , VirtualRouterData
    , virtualRouterData
    , vrdStatus
    , vrdSpec
    , vrdMetadata
    , vrdMeshName
    , vrdVirtualRouterName

    -- * VirtualRouterRef
    , VirtualRouterRef
    , virtualRouterRef
    , vrrMeshName
    , vrrArn
    , vrrVirtualRouterName

    -- * VirtualRouterSpec
    , VirtualRouterSpec
    , virtualRouterSpec
    , vrsServiceNames

    -- * VirtualRouterStatus
    , VirtualRouterStatus
    , virtualRouterStatus
    , vrsStatus

    -- * WeightedTarget
    , WeightedTarget
    , weightedTarget
    , wtWeight
    , wtVirtualNode
    ) where

import Network.AWS.AppMesh.Internal
import Network.AWS.AppMesh.Types.Product
import Network.AWS.AppMesh.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2018-10-01@ of the Amazon App Mesh SDK configuration.
appMesh :: Service
appMesh =
  Service
    { _svcAbbrev = "AppMesh"
    , _svcSigner = v4
    , _svcPrefix = "appmesh"
    , _svcVersion = "2018-10-01"
    , _svcEndpoint = defaultEndpoint appMesh
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "AppMesh"
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


-- | The request contains a client token that was used for a previous update resource call
--
--          with different specifications. Try the request again with a new client token.
--
_ConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictException =
  _MatchServiceError appMesh "ConflictException" . hasStatus 409


-- | You do not have permissions to perform this action.
--
--
_ForbiddenException :: AsError a => Getting (First ServiceError) a ServiceError
_ForbiddenException =
  _MatchServiceError appMesh "ForbiddenException" . hasStatus 403


-- | The specified resource does not exist. Check your request syntax and try again.
--
--
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException =
  _MatchServiceError appMesh "NotFoundException" . hasStatus 404


-- | The maximum request rate permitted by the App Mesh APIs has been exceeded for your
--
--          account. For best results, use an increasing or variable sleep interval between requests.
--
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException =
  _MatchServiceError appMesh "TooManyRequestsException" . hasStatus 429


-- | The request processing has failed because of an unknown error, exception, or failure.
--
--
_InternalServerErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerErrorException =
  _MatchServiceError appMesh "InternalServerErrorException" . hasStatus 500


-- | The request has failed due to a temporary failure of the service.
--
--
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException =
  _MatchServiceError appMesh "ServiceUnavailableException" . hasStatus 503


-- | The request syntax was malformed. Check your request syntax and try again.
--
--
_BadRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_BadRequestException =
  _MatchServiceError appMesh "BadRequestException" . hasStatus 400


-- | You have exceeded a service limit for your account. For more information, see <https://docs.aws.amazon.com/app-mesh/latest/userguide/service_limits.html Service Limits> in the /AWS App Mesh User Guide/ .
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError appMesh "LimitExceededException" . hasStatus 400


-- | You cannot delete the specified resource because it is in use or required by another resource.
--
--
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException =
  _MatchServiceError appMesh "ResourceInUseException" . hasStatus 409

