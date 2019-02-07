{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppMesh
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS App Mesh is a service mesh based on the Envoy proxy that makes it easy to monitor and
--
--          control containerized microservices. App Mesh standardizes how your microservices
--          communicate, giving you end-to-end visibility and helping to ensure high-availability for
--          your applications.
--
-- App Mesh gives you consistent visibility and network traffic controls for every
--          microservice in an application. You can use App Mesh with Amazon ECS
--          (using the Amazon EC2 launch type), Amazon EKS, and Kubernetes on AWS.
--
module Network.AWS.AppMesh
    (
    -- * Service Configuration
      appMesh

    -- * Errors
    -- $errors

    -- ** ConflictException
    , _ConflictException

    -- ** ForbiddenException
    , _ForbiddenException

    -- ** NotFoundException
    , _NotFoundException

    -- ** TooManyRequestsException
    , _TooManyRequestsException

    -- ** InternalServerErrorException
    , _InternalServerErrorException

    -- ** ServiceUnavailableException
    , _ServiceUnavailableException

    -- ** BadRequestException
    , _BadRequestException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** ResourceInUseException
    , _ResourceInUseException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeVirtualNode 
    , module Network.AWS.AppMesh.DescribeVirtualNode

    -- ** DescribeRoute 
    , module Network.AWS.AppMesh.DescribeRoute

    -- ** DescribeVirtualRouter 
    , module Network.AWS.AppMesh.DescribeVirtualRouter

    -- ** ListMeshes (Paginated)
    , module Network.AWS.AppMesh.ListMeshes

    -- ** CreateMesh 
    , module Network.AWS.AppMesh.CreateMesh

    -- ** DeleteMesh 
    , module Network.AWS.AppMesh.DeleteMesh

    -- ** DeleteRoute 
    , module Network.AWS.AppMesh.DeleteRoute

    -- ** UpdateRoute 
    , module Network.AWS.AppMesh.UpdateRoute

    -- ** DeleteVirtualNode 
    , module Network.AWS.AppMesh.DeleteVirtualNode

    -- ** UpdateVirtualNode 
    , module Network.AWS.AppMesh.UpdateVirtualNode

    -- ** ListRoutes (Paginated)
    , module Network.AWS.AppMesh.ListRoutes

    -- ** ListVirtualNodes (Paginated)
    , module Network.AWS.AppMesh.ListVirtualNodes

    -- ** DeleteVirtualRouter 
    , module Network.AWS.AppMesh.DeleteVirtualRouter

    -- ** UpdateVirtualRouter 
    , module Network.AWS.AppMesh.UpdateVirtualRouter

    -- ** CreateVirtualRouter 
    , module Network.AWS.AppMesh.CreateVirtualRouter

    -- ** CreateRoute 
    , module Network.AWS.AppMesh.CreateRoute

    -- ** CreateVirtualNode 
    , module Network.AWS.AppMesh.CreateVirtualNode

    -- ** ListVirtualRouters (Paginated)
    , module Network.AWS.AppMesh.ListVirtualRouters

    -- ** DescribeMesh 
    , module Network.AWS.AppMesh.DescribeMesh

    -- * Types

    -- ** Common
    , module Network.AWS.AppMesh.Internal

    -- ** MeshStatusCode
    , MeshStatusCode (..)

    -- ** PortProtocol
    , PortProtocol (..)

    -- ** RouteStatusCode
    , RouteStatusCode (..)

    -- ** VirtualNodeStatusCode
    , VirtualNodeStatusCode (..)

    -- ** VirtualRouterStatusCode
    , VirtualRouterStatusCode (..)

    -- ** DNSServiceDiscovery
    , DNSServiceDiscovery
    , dnsServiceDiscovery
    , dsdServiceName

    -- ** HTTPRoute
    , HTTPRoute
    , hTTPRoute
    , httprAction
    , httprMatch

    -- ** HTTPRouteAction
    , HTTPRouteAction
    , hTTPRouteAction
    , httpraWeightedTargets

    -- ** HTTPRouteMatch
    , HTTPRouteMatch
    , hTTPRouteMatch
    , httprmPrefix

    -- ** HealthCheckPolicy
    , HealthCheckPolicy
    , healthCheckPolicy
    , hcpPath
    , hcpPort
    , hcpHealthyThreshold
    , hcpIntervalMillis
    , hcpProtocol
    , hcpTimeoutMillis
    , hcpUnhealthyThreshold

    -- ** Listener
    , Listener
    , listener
    , lHealthCheck
    , lPortMapping

    -- ** MeshData
    , MeshData
    , meshData
    , mdStatus
    , mdMeshName
    , mdMetadata

    -- ** MeshRef
    , MeshRef
    , meshRef
    , mrMeshName
    , mrArn

    -- ** MeshStatus
    , MeshStatus
    , meshStatus
    , msStatus

    -- ** PortMapping
    , PortMapping
    , portMapping
    , pmProtocol
    , pmPort

    -- ** ResourceMetadata
    , ResourceMetadata
    , resourceMetadata
    , rmLastUpdatedAt
    , rmArn
    , rmCreatedAt
    , rmUid
    , rmVersion

    -- ** RouteData
    , RouteData
    , routeData
    , rdStatus
    , rdSpec
    , rdMetadata
    , rdMeshName
    , rdRouteName
    , rdVirtualRouterName

    -- ** RouteRef
    , RouteRef
    , routeRef
    , rrMeshName
    , rrArn
    , rrRouteName
    , rrVirtualRouterName

    -- ** RouteSpec
    , RouteSpec
    , routeSpec
    , rsHttpRoute

    -- ** RouteStatus
    , RouteStatus
    , routeStatus
    , rsStatus

    -- ** ServiceDiscovery
    , ServiceDiscovery
    , serviceDiscovery
    , sdDns

    -- ** VirtualNodeData
    , VirtualNodeData
    , virtualNodeData
    , vndStatus
    , vndSpec
    , vndMetadata
    , vndMeshName
    , vndVirtualNodeName

    -- ** VirtualNodeRef
    , VirtualNodeRef
    , virtualNodeRef
    , vnrMeshName
    , vnrArn
    , vnrVirtualNodeName

    -- ** VirtualNodeSpec
    , VirtualNodeSpec
    , virtualNodeSpec
    , vnsBackends
    , vnsServiceDiscovery
    , vnsListeners

    -- ** VirtualNodeStatus
    , VirtualNodeStatus
    , virtualNodeStatus
    , vnsStatus

    -- ** VirtualRouterData
    , VirtualRouterData
    , virtualRouterData
    , vrdStatus
    , vrdSpec
    , vrdMetadata
    , vrdMeshName
    , vrdVirtualRouterName

    -- ** VirtualRouterRef
    , VirtualRouterRef
    , virtualRouterRef
    , vrrMeshName
    , vrrArn
    , vrrVirtualRouterName

    -- ** VirtualRouterSpec
    , VirtualRouterSpec
    , virtualRouterSpec
    , vrsServiceNames

    -- ** VirtualRouterStatus
    , VirtualRouterStatus
    , virtualRouterStatus
    , vrsStatus

    -- ** WeightedTarget
    , WeightedTarget
    , weightedTarget
    , wtWeight
    , wtVirtualNode
    ) where

import Network.AWS.AppMesh.CreateMesh
import Network.AWS.AppMesh.CreateRoute
import Network.AWS.AppMesh.CreateVirtualNode
import Network.AWS.AppMesh.CreateVirtualRouter
import Network.AWS.AppMesh.DeleteMesh
import Network.AWS.AppMesh.DeleteRoute
import Network.AWS.AppMesh.DeleteVirtualNode
import Network.AWS.AppMesh.DeleteVirtualRouter
import Network.AWS.AppMesh.DescribeMesh
import Network.AWS.AppMesh.DescribeRoute
import Network.AWS.AppMesh.DescribeVirtualNode
import Network.AWS.AppMesh.DescribeVirtualRouter
import Network.AWS.AppMesh.ListMeshes
import Network.AWS.AppMesh.ListRoutes
import Network.AWS.AppMesh.ListVirtualNodes
import Network.AWS.AppMesh.ListVirtualRouters
import Network.AWS.AppMesh.Types
import Network.AWS.AppMesh.UpdateRoute
import Network.AWS.AppMesh.UpdateVirtualNode
import Network.AWS.AppMesh.UpdateVirtualRouter
import Network.AWS.AppMesh.Waiters
import Network.AWS.AppMesh.Internal

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'AppMesh'.
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
