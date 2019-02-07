{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AppMesh
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.AppMesh where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.AppMesh
import Test.AWS.AppMesh.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeVirtualNode $
--             describeVirtualNode
--
--         , requestDescribeRoute $
--             describeRoute
--
--         , requestDescribeVirtualRouter $
--             describeVirtualRouter
--
--         , requestListMeshes $
--             listMeshes
--
--         , requestCreateMesh $
--             createMesh
--
--         , requestDeleteMesh $
--             deleteMesh
--
--         , requestDeleteRoute $
--             deleteRoute
--
--         , requestUpdateRoute $
--             updateRoute
--
--         , requestDeleteVirtualNode $
--             deleteVirtualNode
--
--         , requestUpdateVirtualNode $
--             updateVirtualNode
--
--         , requestListRoutes $
--             listRoutes
--
--         , requestListVirtualNodes $
--             listVirtualNodes
--
--         , requestDeleteVirtualRouter $
--             deleteVirtualRouter
--
--         , requestUpdateVirtualRouter $
--             updateVirtualRouter
--
--         , requestCreateVirtualRouter $
--             createVirtualRouter
--
--         , requestCreateRoute $
--             createRoute
--
--         , requestCreateVirtualNode $
--             createVirtualNode
--
--         , requestListVirtualRouters $
--             listVirtualRouters
--
--         , requestDescribeMesh $
--             describeMesh
--
--           ]

--     , testGroup "response"
--         [ responseDescribeVirtualNode $
--             describeVirtualNodeResponse
--
--         , responseDescribeRoute $
--             describeRouteResponse
--
--         , responseDescribeVirtualRouter $
--             describeVirtualRouterResponse
--
--         , responseListMeshes $
--             listMeshesResponse
--
--         , responseCreateMesh $
--             createMeshResponse
--
--         , responseDeleteMesh $
--             deleteMeshResponse
--
--         , responseDeleteRoute $
--             deleteRouteResponse
--
--         , responseUpdateRoute $
--             updateRouteResponse
--
--         , responseDeleteVirtualNode $
--             deleteVirtualNodeResponse
--
--         , responseUpdateVirtualNode $
--             updateVirtualNodeResponse
--
--         , responseListRoutes $
--             listRoutesResponse
--
--         , responseListVirtualNodes $
--             listVirtualNodesResponse
--
--         , responseDeleteVirtualRouter $
--             deleteVirtualRouterResponse
--
--         , responseUpdateVirtualRouter $
--             updateVirtualRouterResponse
--
--         , responseCreateVirtualRouter $
--             createVirtualRouterResponse
--
--         , responseCreateRoute $
--             createRouteResponse
--
--         , responseCreateVirtualNode $
--             createVirtualNodeResponse
--
--         , responseListVirtualRouters $
--             listVirtualRoutersResponse
--
--         , responseDescribeMesh $
--             describeMeshResponse
--
--           ]
--     ]

-- Requests

requestDescribeVirtualNode :: DescribeVirtualNode -> TestTree
requestDescribeVirtualNode = req
    "DescribeVirtualNode"
    "fixture/DescribeVirtualNode.yaml"

requestDescribeRoute :: DescribeRoute -> TestTree
requestDescribeRoute = req
    "DescribeRoute"
    "fixture/DescribeRoute.yaml"

requestDescribeVirtualRouter :: DescribeVirtualRouter -> TestTree
requestDescribeVirtualRouter = req
    "DescribeVirtualRouter"
    "fixture/DescribeVirtualRouter.yaml"

requestListMeshes :: ListMeshes -> TestTree
requestListMeshes = req
    "ListMeshes"
    "fixture/ListMeshes.yaml"

requestCreateMesh :: CreateMesh -> TestTree
requestCreateMesh = req
    "CreateMesh"
    "fixture/CreateMesh.yaml"

requestDeleteMesh :: DeleteMesh -> TestTree
requestDeleteMesh = req
    "DeleteMesh"
    "fixture/DeleteMesh.yaml"

requestDeleteRoute :: DeleteRoute -> TestTree
requestDeleteRoute = req
    "DeleteRoute"
    "fixture/DeleteRoute.yaml"

requestUpdateRoute :: UpdateRoute -> TestTree
requestUpdateRoute = req
    "UpdateRoute"
    "fixture/UpdateRoute.yaml"

requestDeleteVirtualNode :: DeleteVirtualNode -> TestTree
requestDeleteVirtualNode = req
    "DeleteVirtualNode"
    "fixture/DeleteVirtualNode.yaml"

requestUpdateVirtualNode :: UpdateVirtualNode -> TestTree
requestUpdateVirtualNode = req
    "UpdateVirtualNode"
    "fixture/UpdateVirtualNode.yaml"

requestListRoutes :: ListRoutes -> TestTree
requestListRoutes = req
    "ListRoutes"
    "fixture/ListRoutes.yaml"

requestListVirtualNodes :: ListVirtualNodes -> TestTree
requestListVirtualNodes = req
    "ListVirtualNodes"
    "fixture/ListVirtualNodes.yaml"

requestDeleteVirtualRouter :: DeleteVirtualRouter -> TestTree
requestDeleteVirtualRouter = req
    "DeleteVirtualRouter"
    "fixture/DeleteVirtualRouter.yaml"

requestUpdateVirtualRouter :: UpdateVirtualRouter -> TestTree
requestUpdateVirtualRouter = req
    "UpdateVirtualRouter"
    "fixture/UpdateVirtualRouter.yaml"

requestCreateVirtualRouter :: CreateVirtualRouter -> TestTree
requestCreateVirtualRouter = req
    "CreateVirtualRouter"
    "fixture/CreateVirtualRouter.yaml"

requestCreateRoute :: CreateRoute -> TestTree
requestCreateRoute = req
    "CreateRoute"
    "fixture/CreateRoute.yaml"

requestCreateVirtualNode :: CreateVirtualNode -> TestTree
requestCreateVirtualNode = req
    "CreateVirtualNode"
    "fixture/CreateVirtualNode.yaml"

requestListVirtualRouters :: ListVirtualRouters -> TestTree
requestListVirtualRouters = req
    "ListVirtualRouters"
    "fixture/ListVirtualRouters.yaml"

requestDescribeMesh :: DescribeMesh -> TestTree
requestDescribeMesh = req
    "DescribeMesh"
    "fixture/DescribeMesh.yaml"

-- Responses

responseDescribeVirtualNode :: DescribeVirtualNodeResponse -> TestTree
responseDescribeVirtualNode = res
    "DescribeVirtualNodeResponse"
    "fixture/DescribeVirtualNodeResponse.proto"
    appMesh
    (Proxy :: Proxy DescribeVirtualNode)

responseDescribeRoute :: DescribeRouteResponse -> TestTree
responseDescribeRoute = res
    "DescribeRouteResponse"
    "fixture/DescribeRouteResponse.proto"
    appMesh
    (Proxy :: Proxy DescribeRoute)

responseDescribeVirtualRouter :: DescribeVirtualRouterResponse -> TestTree
responseDescribeVirtualRouter = res
    "DescribeVirtualRouterResponse"
    "fixture/DescribeVirtualRouterResponse.proto"
    appMesh
    (Proxy :: Proxy DescribeVirtualRouter)

responseListMeshes :: ListMeshesResponse -> TestTree
responseListMeshes = res
    "ListMeshesResponse"
    "fixture/ListMeshesResponse.proto"
    appMesh
    (Proxy :: Proxy ListMeshes)

responseCreateMesh :: CreateMeshResponse -> TestTree
responseCreateMesh = res
    "CreateMeshResponse"
    "fixture/CreateMeshResponse.proto"
    appMesh
    (Proxy :: Proxy CreateMesh)

responseDeleteMesh :: DeleteMeshResponse -> TestTree
responseDeleteMesh = res
    "DeleteMeshResponse"
    "fixture/DeleteMeshResponse.proto"
    appMesh
    (Proxy :: Proxy DeleteMesh)

responseDeleteRoute :: DeleteRouteResponse -> TestTree
responseDeleteRoute = res
    "DeleteRouteResponse"
    "fixture/DeleteRouteResponse.proto"
    appMesh
    (Proxy :: Proxy DeleteRoute)

responseUpdateRoute :: UpdateRouteResponse -> TestTree
responseUpdateRoute = res
    "UpdateRouteResponse"
    "fixture/UpdateRouteResponse.proto"
    appMesh
    (Proxy :: Proxy UpdateRoute)

responseDeleteVirtualNode :: DeleteVirtualNodeResponse -> TestTree
responseDeleteVirtualNode = res
    "DeleteVirtualNodeResponse"
    "fixture/DeleteVirtualNodeResponse.proto"
    appMesh
    (Proxy :: Proxy DeleteVirtualNode)

responseUpdateVirtualNode :: UpdateVirtualNodeResponse -> TestTree
responseUpdateVirtualNode = res
    "UpdateVirtualNodeResponse"
    "fixture/UpdateVirtualNodeResponse.proto"
    appMesh
    (Proxy :: Proxy UpdateVirtualNode)

responseListRoutes :: ListRoutesResponse -> TestTree
responseListRoutes = res
    "ListRoutesResponse"
    "fixture/ListRoutesResponse.proto"
    appMesh
    (Proxy :: Proxy ListRoutes)

responseListVirtualNodes :: ListVirtualNodesResponse -> TestTree
responseListVirtualNodes = res
    "ListVirtualNodesResponse"
    "fixture/ListVirtualNodesResponse.proto"
    appMesh
    (Proxy :: Proxy ListVirtualNodes)

responseDeleteVirtualRouter :: DeleteVirtualRouterResponse -> TestTree
responseDeleteVirtualRouter = res
    "DeleteVirtualRouterResponse"
    "fixture/DeleteVirtualRouterResponse.proto"
    appMesh
    (Proxy :: Proxy DeleteVirtualRouter)

responseUpdateVirtualRouter :: UpdateVirtualRouterResponse -> TestTree
responseUpdateVirtualRouter = res
    "UpdateVirtualRouterResponse"
    "fixture/UpdateVirtualRouterResponse.proto"
    appMesh
    (Proxy :: Proxy UpdateVirtualRouter)

responseCreateVirtualRouter :: CreateVirtualRouterResponse -> TestTree
responseCreateVirtualRouter = res
    "CreateVirtualRouterResponse"
    "fixture/CreateVirtualRouterResponse.proto"
    appMesh
    (Proxy :: Proxy CreateVirtualRouter)

responseCreateRoute :: CreateRouteResponse -> TestTree
responseCreateRoute = res
    "CreateRouteResponse"
    "fixture/CreateRouteResponse.proto"
    appMesh
    (Proxy :: Proxy CreateRoute)

responseCreateVirtualNode :: CreateVirtualNodeResponse -> TestTree
responseCreateVirtualNode = res
    "CreateVirtualNodeResponse"
    "fixture/CreateVirtualNodeResponse.proto"
    appMesh
    (Proxy :: Proxy CreateVirtualNode)

responseListVirtualRouters :: ListVirtualRoutersResponse -> TestTree
responseListVirtualRouters = res
    "ListVirtualRoutersResponse"
    "fixture/ListVirtualRoutersResponse.proto"
    appMesh
    (Proxy :: Proxy ListVirtualRouters)

responseDescribeMesh :: DescribeMeshResponse -> TestTree
responseDescribeMesh = res
    "DescribeMeshResponse"
    "fixture/DescribeMeshResponse.proto"
    appMesh
    (Proxy :: Proxy DescribeMesh)
