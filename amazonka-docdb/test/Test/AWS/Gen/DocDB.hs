{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DocDB
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.DocDB where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.DocDB
import Test.AWS.DocDB.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeDBClusterParameterGroups $
--             describeDBClusterParameterGroups
--
--         , requestDescribeDBEngineVersions $
--             describeDBEngineVersions
--
--         , requestModifyDBInstance $
--             modifyDBInstance
--
--         , requestResetDBClusterParameterGroup $
--             resetDBClusterParameterGroup
--
--         , requestDescribeEvents $
--             describeEvents
--
--         , requestDescribeDBClusters $
--             describeDBClusters
--
--         , requestModifyDBSubnetGroup $
--             modifyDBSubnetGroup
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestDeleteDBCluster $
--             deleteDBCluster
--
--         , requestDescribeEngineDefaultClusterParameters $
--             describeEngineDefaultClusterParameters
--
--         , requestRemoveTagsFromResource $
--             removeTagsFromResource
--
--         , requestCreateDBInstance $
--             createDBInstance
--
--         , requestDeleteDBClusterParameterGroup $
--             deleteDBClusterParameterGroup
--
--         , requestRestoreDBClusterFromSnapshot $
--             restoreDBClusterFromSnapshot
--
--         , requestDescribeOrderableDBInstanceOptions $
--             describeOrderableDBInstanceOptions
--
--         , requestCreateDBClusterParameterGroup $
--             createDBClusterParameterGroup
--
--         , requestDeleteDBClusterSnapshot $
--             deleteDBClusterSnapshot
--
--         , requestAddTagsToResource $
--             addTagsToResource
--
--         , requestCreateDBClusterSnapshot $
--             createDBClusterSnapshot
--
--         , requestDescribeDBSubnetGroups $
--             describeDBSubnetGroups
--
--         , requestModifyDBClusterSnapshotAttribute $
--             modifyDBClusterSnapshotAttribute
--
--         , requestModifyDBCluster $
--             modifyDBCluster
--
--         , requestCopyDBClusterParameterGroup $
--             copyDBClusterParameterGroup
--
--         , requestDescribeEventCategories $
--             describeEventCategories
--
--         , requestModifyDBClusterParameterGroup $
--             modifyDBClusterParameterGroup
--
--         , requestDescribeDBClusterSnapshotAttributes $
--             describeDBClusterSnapshotAttributes
--
--         , requestDescribePendingMaintenanceActions $
--             describePendingMaintenanceActions
--
--         , requestCopyDBClusterSnapshot $
--             copyDBClusterSnapshot
--
--         , requestCreateDBCluster $
--             createDBCluster
--
--         , requestFailoverDBCluster $
--             failoverDBCluster
--
--         , requestApplyPendingMaintenanceAction $
--             applyPendingMaintenanceAction
--
--         , requestDescribeDBClusterParameters $
--             describeDBClusterParameters
--
--         , requestDeleteDBSubnetGroup $
--             deleteDBSubnetGroup
--
--         , requestDescribeDBClusterSnapshots $
--             describeDBClusterSnapshots
--
--         , requestRebootDBInstance $
--             rebootDBInstance
--
--         , requestCreateDBSubnetGroup $
--             createDBSubnetGroup
--
--         , requestDeleteDBInstance $
--             deleteDBInstance
--
--         , requestRestoreDBClusterToPointInTime $
--             restoreDBClusterToPointInTime
--
--         , requestDescribeDBInstances $
--             describeDBInstances
--
--           ]

--     , testGroup "response"
--         [ responseDescribeDBClusterParameterGroups $
--             describeDBClusterParameterGroupsResponse
--
--         , responseDescribeDBEngineVersions $
--             describeDBEngineVersionsResponse
--
--         , responseModifyDBInstance $
--             modifyDBInstanceResponse
--
--         , responseResetDBClusterParameterGroup $
--             dbClusterParameterGroupNameMessage
--
--         , responseDescribeEvents $
--             describeEventsResponse
--
--         , responseDescribeDBClusters $
--             describeDBClustersResponse
--
--         , responseModifyDBSubnetGroup $
--             modifyDBSubnetGroupResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseDeleteDBCluster $
--             deleteDBClusterResponse
--
--         , responseDescribeEngineDefaultClusterParameters $
--             describeEngineDefaultClusterParametersResponse
--
--         , responseRemoveTagsFromResource $
--             removeTagsFromResourceResponse
--
--         , responseCreateDBInstance $
--             createDBInstanceResponse
--
--         , responseDeleteDBClusterParameterGroup $
--             deleteDBClusterParameterGroupResponse
--
--         , responseRestoreDBClusterFromSnapshot $
--             restoreDBClusterFromSnapshotResponse
--
--         , responseDescribeOrderableDBInstanceOptions $
--             describeOrderableDBInstanceOptionsResponse
--
--         , responseCreateDBClusterParameterGroup $
--             createDBClusterParameterGroupResponse
--
--         , responseDeleteDBClusterSnapshot $
--             deleteDBClusterSnapshotResponse
--
--         , responseAddTagsToResource $
--             addTagsToResourceResponse
--
--         , responseCreateDBClusterSnapshot $
--             createDBClusterSnapshotResponse
--
--         , responseDescribeDBSubnetGroups $
--             describeDBSubnetGroupsResponse
--
--         , responseModifyDBClusterSnapshotAttribute $
--             modifyDBClusterSnapshotAttributeResponse
--
--         , responseModifyDBCluster $
--             modifyDBClusterResponse
--
--         , responseCopyDBClusterParameterGroup $
--             copyDBClusterParameterGroupResponse
--
--         , responseDescribeEventCategories $
--             describeEventCategoriesResponse
--
--         , responseModifyDBClusterParameterGroup $
--             dbClusterParameterGroupNameMessage
--
--         , responseDescribeDBClusterSnapshotAttributes $
--             describeDBClusterSnapshotAttributesResponse
--
--         , responseDescribePendingMaintenanceActions $
--             describePendingMaintenanceActionsResponse
--
--         , responseCopyDBClusterSnapshot $
--             copyDBClusterSnapshotResponse
--
--         , responseCreateDBCluster $
--             createDBClusterResponse
--
--         , responseFailoverDBCluster $
--             failoverDBClusterResponse
--
--         , responseApplyPendingMaintenanceAction $
--             applyPendingMaintenanceActionResponse
--
--         , responseDescribeDBClusterParameters $
--             describeDBClusterParametersResponse
--
--         , responseDeleteDBSubnetGroup $
--             deleteDBSubnetGroupResponse
--
--         , responseDescribeDBClusterSnapshots $
--             describeDBClusterSnapshotsResponse
--
--         , responseRebootDBInstance $
--             rebootDBInstanceResponse
--
--         , responseCreateDBSubnetGroup $
--             createDBSubnetGroupResponse
--
--         , responseDeleteDBInstance $
--             deleteDBInstanceResponse
--
--         , responseRestoreDBClusterToPointInTime $
--             restoreDBClusterToPointInTimeResponse
--
--         , responseDescribeDBInstances $
--             describeDBInstancesResponse
--
--           ]
--     ]

-- Requests

requestDescribeDBClusterParameterGroups :: DescribeDBClusterParameterGroups -> TestTree
requestDescribeDBClusterParameterGroups = req
    "DescribeDBClusterParameterGroups"
    "fixture/DescribeDBClusterParameterGroups.yaml"

requestDescribeDBEngineVersions :: DescribeDBEngineVersions -> TestTree
requestDescribeDBEngineVersions = req
    "DescribeDBEngineVersions"
    "fixture/DescribeDBEngineVersions.yaml"

requestModifyDBInstance :: ModifyDBInstance -> TestTree
requestModifyDBInstance = req
    "ModifyDBInstance"
    "fixture/ModifyDBInstance.yaml"

requestResetDBClusterParameterGroup :: ResetDBClusterParameterGroup -> TestTree
requestResetDBClusterParameterGroup = req
    "ResetDBClusterParameterGroup"
    "fixture/ResetDBClusterParameterGroup.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents = req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestDescribeDBClusters :: DescribeDBClusters -> TestTree
requestDescribeDBClusters = req
    "DescribeDBClusters"
    "fixture/DescribeDBClusters.yaml"

requestModifyDBSubnetGroup :: ModifyDBSubnetGroup -> TestTree
requestModifyDBSubnetGroup = req
    "ModifyDBSubnetGroup"
    "fixture/ModifyDBSubnetGroup.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDeleteDBCluster :: DeleteDBCluster -> TestTree
requestDeleteDBCluster = req
    "DeleteDBCluster"
    "fixture/DeleteDBCluster.yaml"

requestDescribeEngineDefaultClusterParameters :: DescribeEngineDefaultClusterParameters -> TestTree
requestDescribeEngineDefaultClusterParameters = req
    "DescribeEngineDefaultClusterParameters"
    "fixture/DescribeEngineDefaultClusterParameters.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource = req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestCreateDBInstance :: CreateDBInstance -> TestTree
requestCreateDBInstance = req
    "CreateDBInstance"
    "fixture/CreateDBInstance.yaml"

requestDeleteDBClusterParameterGroup :: DeleteDBClusterParameterGroup -> TestTree
requestDeleteDBClusterParameterGroup = req
    "DeleteDBClusterParameterGroup"
    "fixture/DeleteDBClusterParameterGroup.yaml"

requestRestoreDBClusterFromSnapshot :: RestoreDBClusterFromSnapshot -> TestTree
requestRestoreDBClusterFromSnapshot = req
    "RestoreDBClusterFromSnapshot"
    "fixture/RestoreDBClusterFromSnapshot.yaml"

requestDescribeOrderableDBInstanceOptions :: DescribeOrderableDBInstanceOptions -> TestTree
requestDescribeOrderableDBInstanceOptions = req
    "DescribeOrderableDBInstanceOptions"
    "fixture/DescribeOrderableDBInstanceOptions.yaml"

requestCreateDBClusterParameterGroup :: CreateDBClusterParameterGroup -> TestTree
requestCreateDBClusterParameterGroup = req
    "CreateDBClusterParameterGroup"
    "fixture/CreateDBClusterParameterGroup.yaml"

requestDeleteDBClusterSnapshot :: DeleteDBClusterSnapshot -> TestTree
requestDeleteDBClusterSnapshot = req
    "DeleteDBClusterSnapshot"
    "fixture/DeleteDBClusterSnapshot.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource = req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestCreateDBClusterSnapshot :: CreateDBClusterSnapshot -> TestTree
requestCreateDBClusterSnapshot = req
    "CreateDBClusterSnapshot"
    "fixture/CreateDBClusterSnapshot.yaml"

requestDescribeDBSubnetGroups :: DescribeDBSubnetGroups -> TestTree
requestDescribeDBSubnetGroups = req
    "DescribeDBSubnetGroups"
    "fixture/DescribeDBSubnetGroups.yaml"

requestModifyDBClusterSnapshotAttribute :: ModifyDBClusterSnapshotAttribute -> TestTree
requestModifyDBClusterSnapshotAttribute = req
    "ModifyDBClusterSnapshotAttribute"
    "fixture/ModifyDBClusterSnapshotAttribute.yaml"

requestModifyDBCluster :: ModifyDBCluster -> TestTree
requestModifyDBCluster = req
    "ModifyDBCluster"
    "fixture/ModifyDBCluster.yaml"

requestCopyDBClusterParameterGroup :: CopyDBClusterParameterGroup -> TestTree
requestCopyDBClusterParameterGroup = req
    "CopyDBClusterParameterGroup"
    "fixture/CopyDBClusterParameterGroup.yaml"

requestDescribeEventCategories :: DescribeEventCategories -> TestTree
requestDescribeEventCategories = req
    "DescribeEventCategories"
    "fixture/DescribeEventCategories.yaml"

requestModifyDBClusterParameterGroup :: ModifyDBClusterParameterGroup -> TestTree
requestModifyDBClusterParameterGroup = req
    "ModifyDBClusterParameterGroup"
    "fixture/ModifyDBClusterParameterGroup.yaml"

requestDescribeDBClusterSnapshotAttributes :: DescribeDBClusterSnapshotAttributes -> TestTree
requestDescribeDBClusterSnapshotAttributes = req
    "DescribeDBClusterSnapshotAttributes"
    "fixture/DescribeDBClusterSnapshotAttributes.yaml"

requestDescribePendingMaintenanceActions :: DescribePendingMaintenanceActions -> TestTree
requestDescribePendingMaintenanceActions = req
    "DescribePendingMaintenanceActions"
    "fixture/DescribePendingMaintenanceActions.yaml"

requestCopyDBClusterSnapshot :: CopyDBClusterSnapshot -> TestTree
requestCopyDBClusterSnapshot = req
    "CopyDBClusterSnapshot"
    "fixture/CopyDBClusterSnapshot.yaml"

requestCreateDBCluster :: CreateDBCluster -> TestTree
requestCreateDBCluster = req
    "CreateDBCluster"
    "fixture/CreateDBCluster.yaml"

requestFailoverDBCluster :: FailoverDBCluster -> TestTree
requestFailoverDBCluster = req
    "FailoverDBCluster"
    "fixture/FailoverDBCluster.yaml"

requestApplyPendingMaintenanceAction :: ApplyPendingMaintenanceAction -> TestTree
requestApplyPendingMaintenanceAction = req
    "ApplyPendingMaintenanceAction"
    "fixture/ApplyPendingMaintenanceAction.yaml"

requestDescribeDBClusterParameters :: DescribeDBClusterParameters -> TestTree
requestDescribeDBClusterParameters = req
    "DescribeDBClusterParameters"
    "fixture/DescribeDBClusterParameters.yaml"

requestDeleteDBSubnetGroup :: DeleteDBSubnetGroup -> TestTree
requestDeleteDBSubnetGroup = req
    "DeleteDBSubnetGroup"
    "fixture/DeleteDBSubnetGroup.yaml"

requestDescribeDBClusterSnapshots :: DescribeDBClusterSnapshots -> TestTree
requestDescribeDBClusterSnapshots = req
    "DescribeDBClusterSnapshots"
    "fixture/DescribeDBClusterSnapshots.yaml"

requestRebootDBInstance :: RebootDBInstance -> TestTree
requestRebootDBInstance = req
    "RebootDBInstance"
    "fixture/RebootDBInstance.yaml"

requestCreateDBSubnetGroup :: CreateDBSubnetGroup -> TestTree
requestCreateDBSubnetGroup = req
    "CreateDBSubnetGroup"
    "fixture/CreateDBSubnetGroup.yaml"

requestDeleteDBInstance :: DeleteDBInstance -> TestTree
requestDeleteDBInstance = req
    "DeleteDBInstance"
    "fixture/DeleteDBInstance.yaml"

requestRestoreDBClusterToPointInTime :: RestoreDBClusterToPointInTime -> TestTree
requestRestoreDBClusterToPointInTime = req
    "RestoreDBClusterToPointInTime"
    "fixture/RestoreDBClusterToPointInTime.yaml"

requestDescribeDBInstances :: DescribeDBInstances -> TestTree
requestDescribeDBInstances = req
    "DescribeDBInstances"
    "fixture/DescribeDBInstances.yaml"

-- Responses

responseDescribeDBClusterParameterGroups :: DescribeDBClusterParameterGroupsResponse -> TestTree
responseDescribeDBClusterParameterGroups = res
    "DescribeDBClusterParameterGroupsResponse"
    "fixture/DescribeDBClusterParameterGroupsResponse.proto"
    docDB
    (Proxy :: Proxy DescribeDBClusterParameterGroups)

responseDescribeDBEngineVersions :: DescribeDBEngineVersionsResponse -> TestTree
responseDescribeDBEngineVersions = res
    "DescribeDBEngineVersionsResponse"
    "fixture/DescribeDBEngineVersionsResponse.proto"
    docDB
    (Proxy :: Proxy DescribeDBEngineVersions)

responseModifyDBInstance :: ModifyDBInstanceResponse -> TestTree
responseModifyDBInstance = res
    "ModifyDBInstanceResponse"
    "fixture/ModifyDBInstanceResponse.proto"
    docDB
    (Proxy :: Proxy ModifyDBInstance)

responseResetDBClusterParameterGroup :: DBClusterParameterGroupNameMessage -> TestTree
responseResetDBClusterParameterGroup = res
    "ResetDBClusterParameterGroupResponse"
    "fixture/ResetDBClusterParameterGroupResponse.proto"
    docDB
    (Proxy :: Proxy ResetDBClusterParameterGroup)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents = res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    docDB
    (Proxy :: Proxy DescribeEvents)

responseDescribeDBClusters :: DescribeDBClustersResponse -> TestTree
responseDescribeDBClusters = res
    "DescribeDBClustersResponse"
    "fixture/DescribeDBClustersResponse.proto"
    docDB
    (Proxy :: Proxy DescribeDBClusters)

responseModifyDBSubnetGroup :: ModifyDBSubnetGroupResponse -> TestTree
responseModifyDBSubnetGroup = res
    "ModifyDBSubnetGroupResponse"
    "fixture/ModifyDBSubnetGroupResponse.proto"
    docDB
    (Proxy :: Proxy ModifyDBSubnetGroup)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    docDB
    (Proxy :: Proxy ListTagsForResource)

responseDeleteDBCluster :: DeleteDBClusterResponse -> TestTree
responseDeleteDBCluster = res
    "DeleteDBClusterResponse"
    "fixture/DeleteDBClusterResponse.proto"
    docDB
    (Proxy :: Proxy DeleteDBCluster)

responseDescribeEngineDefaultClusterParameters :: DescribeEngineDefaultClusterParametersResponse -> TestTree
responseDescribeEngineDefaultClusterParameters = res
    "DescribeEngineDefaultClusterParametersResponse"
    "fixture/DescribeEngineDefaultClusterParametersResponse.proto"
    docDB
    (Proxy :: Proxy DescribeEngineDefaultClusterParameters)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource = res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    docDB
    (Proxy :: Proxy RemoveTagsFromResource)

responseCreateDBInstance :: CreateDBInstanceResponse -> TestTree
responseCreateDBInstance = res
    "CreateDBInstanceResponse"
    "fixture/CreateDBInstanceResponse.proto"
    docDB
    (Proxy :: Proxy CreateDBInstance)

responseDeleteDBClusterParameterGroup :: DeleteDBClusterParameterGroupResponse -> TestTree
responseDeleteDBClusterParameterGroup = res
    "DeleteDBClusterParameterGroupResponse"
    "fixture/DeleteDBClusterParameterGroupResponse.proto"
    docDB
    (Proxy :: Proxy DeleteDBClusterParameterGroup)

responseRestoreDBClusterFromSnapshot :: RestoreDBClusterFromSnapshotResponse -> TestTree
responseRestoreDBClusterFromSnapshot = res
    "RestoreDBClusterFromSnapshotResponse"
    "fixture/RestoreDBClusterFromSnapshotResponse.proto"
    docDB
    (Proxy :: Proxy RestoreDBClusterFromSnapshot)

responseDescribeOrderableDBInstanceOptions :: DescribeOrderableDBInstanceOptionsResponse -> TestTree
responseDescribeOrderableDBInstanceOptions = res
    "DescribeOrderableDBInstanceOptionsResponse"
    "fixture/DescribeOrderableDBInstanceOptionsResponse.proto"
    docDB
    (Proxy :: Proxy DescribeOrderableDBInstanceOptions)

responseCreateDBClusterParameterGroup :: CreateDBClusterParameterGroupResponse -> TestTree
responseCreateDBClusterParameterGroup = res
    "CreateDBClusterParameterGroupResponse"
    "fixture/CreateDBClusterParameterGroupResponse.proto"
    docDB
    (Proxy :: Proxy CreateDBClusterParameterGroup)

responseDeleteDBClusterSnapshot :: DeleteDBClusterSnapshotResponse -> TestTree
responseDeleteDBClusterSnapshot = res
    "DeleteDBClusterSnapshotResponse"
    "fixture/DeleteDBClusterSnapshotResponse.proto"
    docDB
    (Proxy :: Proxy DeleteDBClusterSnapshot)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource = res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    docDB
    (Proxy :: Proxy AddTagsToResource)

responseCreateDBClusterSnapshot :: CreateDBClusterSnapshotResponse -> TestTree
responseCreateDBClusterSnapshot = res
    "CreateDBClusterSnapshotResponse"
    "fixture/CreateDBClusterSnapshotResponse.proto"
    docDB
    (Proxy :: Proxy CreateDBClusterSnapshot)

responseDescribeDBSubnetGroups :: DescribeDBSubnetGroupsResponse -> TestTree
responseDescribeDBSubnetGroups = res
    "DescribeDBSubnetGroupsResponse"
    "fixture/DescribeDBSubnetGroupsResponse.proto"
    docDB
    (Proxy :: Proxy DescribeDBSubnetGroups)

responseModifyDBClusterSnapshotAttribute :: ModifyDBClusterSnapshotAttributeResponse -> TestTree
responseModifyDBClusterSnapshotAttribute = res
    "ModifyDBClusterSnapshotAttributeResponse"
    "fixture/ModifyDBClusterSnapshotAttributeResponse.proto"
    docDB
    (Proxy :: Proxy ModifyDBClusterSnapshotAttribute)

responseModifyDBCluster :: ModifyDBClusterResponse -> TestTree
responseModifyDBCluster = res
    "ModifyDBClusterResponse"
    "fixture/ModifyDBClusterResponse.proto"
    docDB
    (Proxy :: Proxy ModifyDBCluster)

responseCopyDBClusterParameterGroup :: CopyDBClusterParameterGroupResponse -> TestTree
responseCopyDBClusterParameterGroup = res
    "CopyDBClusterParameterGroupResponse"
    "fixture/CopyDBClusterParameterGroupResponse.proto"
    docDB
    (Proxy :: Proxy CopyDBClusterParameterGroup)

responseDescribeEventCategories :: DescribeEventCategoriesResponse -> TestTree
responseDescribeEventCategories = res
    "DescribeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse.proto"
    docDB
    (Proxy :: Proxy DescribeEventCategories)

responseModifyDBClusterParameterGroup :: DBClusterParameterGroupNameMessage -> TestTree
responseModifyDBClusterParameterGroup = res
    "ModifyDBClusterParameterGroupResponse"
    "fixture/ModifyDBClusterParameterGroupResponse.proto"
    docDB
    (Proxy :: Proxy ModifyDBClusterParameterGroup)

responseDescribeDBClusterSnapshotAttributes :: DescribeDBClusterSnapshotAttributesResponse -> TestTree
responseDescribeDBClusterSnapshotAttributes = res
    "DescribeDBClusterSnapshotAttributesResponse"
    "fixture/DescribeDBClusterSnapshotAttributesResponse.proto"
    docDB
    (Proxy :: Proxy DescribeDBClusterSnapshotAttributes)

responseDescribePendingMaintenanceActions :: DescribePendingMaintenanceActionsResponse -> TestTree
responseDescribePendingMaintenanceActions = res
    "DescribePendingMaintenanceActionsResponse"
    "fixture/DescribePendingMaintenanceActionsResponse.proto"
    docDB
    (Proxy :: Proxy DescribePendingMaintenanceActions)

responseCopyDBClusterSnapshot :: CopyDBClusterSnapshotResponse -> TestTree
responseCopyDBClusterSnapshot = res
    "CopyDBClusterSnapshotResponse"
    "fixture/CopyDBClusterSnapshotResponse.proto"
    docDB
    (Proxy :: Proxy CopyDBClusterSnapshot)

responseCreateDBCluster :: CreateDBClusterResponse -> TestTree
responseCreateDBCluster = res
    "CreateDBClusterResponse"
    "fixture/CreateDBClusterResponse.proto"
    docDB
    (Proxy :: Proxy CreateDBCluster)

responseFailoverDBCluster :: FailoverDBClusterResponse -> TestTree
responseFailoverDBCluster = res
    "FailoverDBClusterResponse"
    "fixture/FailoverDBClusterResponse.proto"
    docDB
    (Proxy :: Proxy FailoverDBCluster)

responseApplyPendingMaintenanceAction :: ApplyPendingMaintenanceActionResponse -> TestTree
responseApplyPendingMaintenanceAction = res
    "ApplyPendingMaintenanceActionResponse"
    "fixture/ApplyPendingMaintenanceActionResponse.proto"
    docDB
    (Proxy :: Proxy ApplyPendingMaintenanceAction)

responseDescribeDBClusterParameters :: DescribeDBClusterParametersResponse -> TestTree
responseDescribeDBClusterParameters = res
    "DescribeDBClusterParametersResponse"
    "fixture/DescribeDBClusterParametersResponse.proto"
    docDB
    (Proxy :: Proxy DescribeDBClusterParameters)

responseDeleteDBSubnetGroup :: DeleteDBSubnetGroupResponse -> TestTree
responseDeleteDBSubnetGroup = res
    "DeleteDBSubnetGroupResponse"
    "fixture/DeleteDBSubnetGroupResponse.proto"
    docDB
    (Proxy :: Proxy DeleteDBSubnetGroup)

responseDescribeDBClusterSnapshots :: DescribeDBClusterSnapshotsResponse -> TestTree
responseDescribeDBClusterSnapshots = res
    "DescribeDBClusterSnapshotsResponse"
    "fixture/DescribeDBClusterSnapshotsResponse.proto"
    docDB
    (Proxy :: Proxy DescribeDBClusterSnapshots)

responseRebootDBInstance :: RebootDBInstanceResponse -> TestTree
responseRebootDBInstance = res
    "RebootDBInstanceResponse"
    "fixture/RebootDBInstanceResponse.proto"
    docDB
    (Proxy :: Proxy RebootDBInstance)

responseCreateDBSubnetGroup :: CreateDBSubnetGroupResponse -> TestTree
responseCreateDBSubnetGroup = res
    "CreateDBSubnetGroupResponse"
    "fixture/CreateDBSubnetGroupResponse.proto"
    docDB
    (Proxy :: Proxy CreateDBSubnetGroup)

responseDeleteDBInstance :: DeleteDBInstanceResponse -> TestTree
responseDeleteDBInstance = res
    "DeleteDBInstanceResponse"
    "fixture/DeleteDBInstanceResponse.proto"
    docDB
    (Proxy :: Proxy DeleteDBInstance)

responseRestoreDBClusterToPointInTime :: RestoreDBClusterToPointInTimeResponse -> TestTree
responseRestoreDBClusterToPointInTime = res
    "RestoreDBClusterToPointInTimeResponse"
    "fixture/RestoreDBClusterToPointInTimeResponse.proto"
    docDB
    (Proxy :: Proxy RestoreDBClusterToPointInTime)

responseDescribeDBInstances :: DescribeDBInstancesResponse -> TestTree
responseDescribeDBInstances = res
    "DescribeDBInstancesResponse"
    "fixture/DescribeDBInstancesResponse.proto"
    docDB
    (Proxy :: Proxy DescribeDBInstances)
