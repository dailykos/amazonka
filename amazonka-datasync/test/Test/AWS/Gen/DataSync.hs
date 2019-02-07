{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DataSync
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.DataSync where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.DataSync
import Test.AWS.DataSync.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestUpdateTask $
--             updateTask
--
--         , requestDescribeAgent $
--             describeAgent
--
--         , requestDeleteTask $
--             deleteTask
--
--         , requestListLocations $
--             listLocations
--
--         , requestCreateLocationNfs $
--             createLocationNfs
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestDescribeTask $
--             describeTask
--
--         , requestDescribeLocationS3 $
--             describeLocationS3
--
--         , requestListAgents $
--             listAgents
--
--         , requestDeleteAgent $
--             deleteAgent
--
--         , requestUpdateAgent $
--             updateAgent
--
--         , requestListTaskExecutions $
--             listTaskExecutions
--
--         , requestCreateLocationS3 $
--             createLocationS3
--
--         , requestCreateTask $
--             createTask
--
--         , requestCreateLocationEfs $
--             createLocationEfs
--
--         , requestDeleteLocation $
--             deleteLocation
--
--         , requestListTasks $
--             listTasks
--
--         , requestStartTaskExecution $
--             startTaskExecution
--
--         , requestDescribeTaskExecution $
--             describeTaskExecution
--
--         , requestCreateAgent $
--             createAgent
--
--         , requestDescribeLocationEfs $
--             describeLocationEfs
--
--         , requestTagResource $
--             tagResource
--
--         , requestUntagResource $
--             untagResource
--
--         , requestDescribeLocationNfs $
--             describeLocationNfs
--
--         , requestCancelTaskExecution $
--             cancelTaskExecution
--
--           ]

--     , testGroup "response"
--         [ responseUpdateTask $
--             updateTaskResponse
--
--         , responseDescribeAgent $
--             describeAgentResponse
--
--         , responseDeleteTask $
--             deleteTaskResponse
--
--         , responseListLocations $
--             listLocationsResponse
--
--         , responseCreateLocationNfs $
--             createLocationNfsResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseDescribeTask $
--             describeTaskResponse
--
--         , responseDescribeLocationS3 $
--             describeLocationS3Response
--
--         , responseListAgents $
--             listAgentsResponse
--
--         , responseDeleteAgent $
--             deleteAgentResponse
--
--         , responseUpdateAgent $
--             updateAgentResponse
--
--         , responseListTaskExecutions $
--             listTaskExecutionsResponse
--
--         , responseCreateLocationS3 $
--             createLocationS3Response
--
--         , responseCreateTask $
--             createTaskResponse
--
--         , responseCreateLocationEfs $
--             createLocationEfsResponse
--
--         , responseDeleteLocation $
--             deleteLocationResponse
--
--         , responseListTasks $
--             listTasksResponse
--
--         , responseStartTaskExecution $
--             startTaskExecutionResponse
--
--         , responseDescribeTaskExecution $
--             describeTaskExecutionResponse
--
--         , responseCreateAgent $
--             createAgentResponse
--
--         , responseDescribeLocationEfs $
--             describeLocationEfsResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseDescribeLocationNfs $
--             describeLocationNfsResponse
--
--         , responseCancelTaskExecution $
--             cancelTaskExecutionResponse
--
--           ]
--     ]

-- Requests

requestUpdateTask :: UpdateTask -> TestTree
requestUpdateTask = req
    "UpdateTask"
    "fixture/UpdateTask.yaml"

requestDescribeAgent :: DescribeAgent -> TestTree
requestDescribeAgent = req
    "DescribeAgent"
    "fixture/DescribeAgent.yaml"

requestDeleteTask :: DeleteTask -> TestTree
requestDeleteTask = req
    "DeleteTask"
    "fixture/DeleteTask.yaml"

requestListLocations :: ListLocations -> TestTree
requestListLocations = req
    "ListLocations"
    "fixture/ListLocations.yaml"

requestCreateLocationNfs :: CreateLocationNfs -> TestTree
requestCreateLocationNfs = req
    "CreateLocationNfs"
    "fixture/CreateLocationNfs.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribeTask :: DescribeTask -> TestTree
requestDescribeTask = req
    "DescribeTask"
    "fixture/DescribeTask.yaml"

requestDescribeLocationS3 :: DescribeLocationS3 -> TestTree
requestDescribeLocationS3 = req
    "DescribeLocationS3"
    "fixture/DescribeLocationS3.yaml"

requestListAgents :: ListAgents -> TestTree
requestListAgents = req
    "ListAgents"
    "fixture/ListAgents.yaml"

requestDeleteAgent :: DeleteAgent -> TestTree
requestDeleteAgent = req
    "DeleteAgent"
    "fixture/DeleteAgent.yaml"

requestUpdateAgent :: UpdateAgent -> TestTree
requestUpdateAgent = req
    "UpdateAgent"
    "fixture/UpdateAgent.yaml"

requestListTaskExecutions :: ListTaskExecutions -> TestTree
requestListTaskExecutions = req
    "ListTaskExecutions"
    "fixture/ListTaskExecutions.yaml"

requestCreateLocationS3 :: CreateLocationS3 -> TestTree
requestCreateLocationS3 = req
    "CreateLocationS3"
    "fixture/CreateLocationS3.yaml"

requestCreateTask :: CreateTask -> TestTree
requestCreateTask = req
    "CreateTask"
    "fixture/CreateTask.yaml"

requestCreateLocationEfs :: CreateLocationEfs -> TestTree
requestCreateLocationEfs = req
    "CreateLocationEfs"
    "fixture/CreateLocationEfs.yaml"

requestDeleteLocation :: DeleteLocation -> TestTree
requestDeleteLocation = req
    "DeleteLocation"
    "fixture/DeleteLocation.yaml"

requestListTasks :: ListTasks -> TestTree
requestListTasks = req
    "ListTasks"
    "fixture/ListTasks.yaml"

requestStartTaskExecution :: StartTaskExecution -> TestTree
requestStartTaskExecution = req
    "StartTaskExecution"
    "fixture/StartTaskExecution.yaml"

requestDescribeTaskExecution :: DescribeTaskExecution -> TestTree
requestDescribeTaskExecution = req
    "DescribeTaskExecution"
    "fixture/DescribeTaskExecution.yaml"

requestCreateAgent :: CreateAgent -> TestTree
requestCreateAgent = req
    "CreateAgent"
    "fixture/CreateAgent.yaml"

requestDescribeLocationEfs :: DescribeLocationEfs -> TestTree
requestDescribeLocationEfs = req
    "DescribeLocationEfs"
    "fixture/DescribeLocationEfs.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeLocationNfs :: DescribeLocationNfs -> TestTree
requestDescribeLocationNfs = req
    "DescribeLocationNfs"
    "fixture/DescribeLocationNfs.yaml"

requestCancelTaskExecution :: CancelTaskExecution -> TestTree
requestCancelTaskExecution = req
    "CancelTaskExecution"
    "fixture/CancelTaskExecution.yaml"

-- Responses

responseUpdateTask :: UpdateTaskResponse -> TestTree
responseUpdateTask = res
    "UpdateTaskResponse"
    "fixture/UpdateTaskResponse.proto"
    dataSync
    (Proxy :: Proxy UpdateTask)

responseDescribeAgent :: DescribeAgentResponse -> TestTree
responseDescribeAgent = res
    "DescribeAgentResponse"
    "fixture/DescribeAgentResponse.proto"
    dataSync
    (Proxy :: Proxy DescribeAgent)

responseDeleteTask :: DeleteTaskResponse -> TestTree
responseDeleteTask = res
    "DeleteTaskResponse"
    "fixture/DeleteTaskResponse.proto"
    dataSync
    (Proxy :: Proxy DeleteTask)

responseListLocations :: ListLocationsResponse -> TestTree
responseListLocations = res
    "ListLocationsResponse"
    "fixture/ListLocationsResponse.proto"
    dataSync
    (Proxy :: Proxy ListLocations)

responseCreateLocationNfs :: CreateLocationNfsResponse -> TestTree
responseCreateLocationNfs = res
    "CreateLocationNfsResponse"
    "fixture/CreateLocationNfsResponse.proto"
    dataSync
    (Proxy :: Proxy CreateLocationNfs)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    dataSync
    (Proxy :: Proxy ListTagsForResource)

responseDescribeTask :: DescribeTaskResponse -> TestTree
responseDescribeTask = res
    "DescribeTaskResponse"
    "fixture/DescribeTaskResponse.proto"
    dataSync
    (Proxy :: Proxy DescribeTask)

responseDescribeLocationS3 :: DescribeLocationS3Response -> TestTree
responseDescribeLocationS3 = res
    "DescribeLocationS3Response"
    "fixture/DescribeLocationS3Response.proto"
    dataSync
    (Proxy :: Proxy DescribeLocationS3)

responseListAgents :: ListAgentsResponse -> TestTree
responseListAgents = res
    "ListAgentsResponse"
    "fixture/ListAgentsResponse.proto"
    dataSync
    (Proxy :: Proxy ListAgents)

responseDeleteAgent :: DeleteAgentResponse -> TestTree
responseDeleteAgent = res
    "DeleteAgentResponse"
    "fixture/DeleteAgentResponse.proto"
    dataSync
    (Proxy :: Proxy DeleteAgent)

responseUpdateAgent :: UpdateAgentResponse -> TestTree
responseUpdateAgent = res
    "UpdateAgentResponse"
    "fixture/UpdateAgentResponse.proto"
    dataSync
    (Proxy :: Proxy UpdateAgent)

responseListTaskExecutions :: ListTaskExecutionsResponse -> TestTree
responseListTaskExecutions = res
    "ListTaskExecutionsResponse"
    "fixture/ListTaskExecutionsResponse.proto"
    dataSync
    (Proxy :: Proxy ListTaskExecutions)

responseCreateLocationS3 :: CreateLocationS3Response -> TestTree
responseCreateLocationS3 = res
    "CreateLocationS3Response"
    "fixture/CreateLocationS3Response.proto"
    dataSync
    (Proxy :: Proxy CreateLocationS3)

responseCreateTask :: CreateTaskResponse -> TestTree
responseCreateTask = res
    "CreateTaskResponse"
    "fixture/CreateTaskResponse.proto"
    dataSync
    (Proxy :: Proxy CreateTask)

responseCreateLocationEfs :: CreateLocationEfsResponse -> TestTree
responseCreateLocationEfs = res
    "CreateLocationEfsResponse"
    "fixture/CreateLocationEfsResponse.proto"
    dataSync
    (Proxy :: Proxy CreateLocationEfs)

responseDeleteLocation :: DeleteLocationResponse -> TestTree
responseDeleteLocation = res
    "DeleteLocationResponse"
    "fixture/DeleteLocationResponse.proto"
    dataSync
    (Proxy :: Proxy DeleteLocation)

responseListTasks :: ListTasksResponse -> TestTree
responseListTasks = res
    "ListTasksResponse"
    "fixture/ListTasksResponse.proto"
    dataSync
    (Proxy :: Proxy ListTasks)

responseStartTaskExecution :: StartTaskExecutionResponse -> TestTree
responseStartTaskExecution = res
    "StartTaskExecutionResponse"
    "fixture/StartTaskExecutionResponse.proto"
    dataSync
    (Proxy :: Proxy StartTaskExecution)

responseDescribeTaskExecution :: DescribeTaskExecutionResponse -> TestTree
responseDescribeTaskExecution = res
    "DescribeTaskExecutionResponse"
    "fixture/DescribeTaskExecutionResponse.proto"
    dataSync
    (Proxy :: Proxy DescribeTaskExecution)

responseCreateAgent :: CreateAgentResponse -> TestTree
responseCreateAgent = res
    "CreateAgentResponse"
    "fixture/CreateAgentResponse.proto"
    dataSync
    (Proxy :: Proxy CreateAgent)

responseDescribeLocationEfs :: DescribeLocationEfsResponse -> TestTree
responseDescribeLocationEfs = res
    "DescribeLocationEfsResponse"
    "fixture/DescribeLocationEfsResponse.proto"
    dataSync
    (Proxy :: Proxy DescribeLocationEfs)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    dataSync
    (Proxy :: Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    dataSync
    (Proxy :: Proxy UntagResource)

responseDescribeLocationNfs :: DescribeLocationNfsResponse -> TestTree
responseDescribeLocationNfs = res
    "DescribeLocationNfsResponse"
    "fixture/DescribeLocationNfsResponse.proto"
    dataSync
    (Proxy :: Proxy DescribeLocationNfs)

responseCancelTaskExecution :: CancelTaskExecutionResponse -> TestTree
responseCancelTaskExecution = res
    "CancelTaskExecutionResponse"
    "fixture/CancelTaskExecutionResponse.proto"
    dataSync
    (Proxy :: Proxy CancelTaskExecution)
