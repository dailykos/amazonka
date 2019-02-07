{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Backup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Backup where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Backup
import Test.AWS.Backup.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestUpdateBackupPlan $
--             updateBackupPlan
--
--         , requestDeleteBackupPlan $
--             deleteBackupPlan
--
--         , requestDescribeBackupJob $
--             describeBackupJob
--
--         , requestListBackupPlanTemplates $
--             listBackupPlanTemplates
--
--         , requestDeleteBackupSelection $
--             deleteBackupSelection
--
--         , requestDescribeRecoveryPoint $
--             describeRecoveryPoint
--
--         , requestDescribeRestoreJob $
--             describeRestoreJob
--
--         , requestGetBackupPlanFromTemplate $
--             getBackupPlanFromTemplate
--
--         , requestDeleteBackupVault $
--             deleteBackupVault
--
--         , requestListBackupJobs $
--             listBackupJobs
--
--         , requestGetBackupPlan $
--             getBackupPlan
--
--         , requestListBackupPlanVersions $
--             listBackupPlanVersions
--
--         , requestListRestoreJobs $
--             listRestoreJobs
--
--         , requestExportBackupPlanTemplate $
--             exportBackupPlanTemplate
--
--         , requestStartBackupJob $
--             startBackupJob
--
--         , requestCreateBackupPlan $
--             createBackupPlan
--
--         , requestListProtectedResources $
--             listProtectedResources
--
--         , requestDescribeBackupVault $
--             describeBackupVault
--
--         , requestGetBackupVaultNotifications $
--             getBackupVaultNotifications
--
--         , requestGetRecoveryPointRestoreMetadata $
--             getRecoveryPointRestoreMetadata
--
--         , requestListBackupPlans $
--             listBackupPlans
--
--         , requestStartRestoreJob $
--             startRestoreJob
--
--         , requestListBackupSelections $
--             listBackupSelections
--
--         , requestListRecoveryPointsByResource $
--             listRecoveryPointsByResource
--
--         , requestCreateBackupSelection $
--             createBackupSelection
--
--         , requestDescribeProtectedResource $
--             describeProtectedResource
--
--         , requestGetBackupPlanFromJSON $
--             getBackupPlanFromJSON
--
--         , requestListBackupVaults $
--             listBackupVaults
--
--         , requestGetBackupSelection $
--             getBackupSelection
--
--         , requestCreateBackupVault $
--             createBackupVault
--
--         , requestUpdateRecoveryPointLifecycle $
--             updateRecoveryPointLifecycle
--
--         , requestTagResource $
--             tagResource
--
--         , requestPutBackupVaultNotifications $
--             putBackupVaultNotifications
--
--         , requestDeleteBackupVaultNotifications $
--             deleteBackupVaultNotifications
--
--         , requestListTags $
--             listTags
--
--         , requestUntagResource $
--             untagResource
--
--         , requestGetBackupVaultAccessPolicy $
--             getBackupVaultAccessPolicy
--
--         , requestDeleteRecoveryPoint $
--             deleteRecoveryPoint
--
--         , requestGetSupportedResourceTypes $
--             getSupportedResourceTypes
--
--         , requestStopBackupJob $
--             stopBackupJob
--
--         , requestListRecoveryPointsByBackupVault $
--             listRecoveryPointsByBackupVault
--
--         , requestPutBackupVaultAccessPolicy $
--             putBackupVaultAccessPolicy
--
--         , requestDeleteBackupVaultAccessPolicy $
--             deleteBackupVaultAccessPolicy
--
--           ]

--     , testGroup "response"
--         [ responseUpdateBackupPlan $
--             updateBackupPlanResponse
--
--         , responseDeleteBackupPlan $
--             deleteBackupPlanResponse
--
--         , responseDescribeBackupJob $
--             describeBackupJobResponse
--
--         , responseListBackupPlanTemplates $
--             listBackupPlanTemplatesResponse
--
--         , responseDeleteBackupSelection $
--             deleteBackupSelectionResponse
--
--         , responseDescribeRecoveryPoint $
--             describeRecoveryPointResponse
--
--         , responseDescribeRestoreJob $
--             describeRestoreJobResponse
--
--         , responseGetBackupPlanFromTemplate $
--             getBackupPlanFromTemplateResponse
--
--         , responseDeleteBackupVault $
--             deleteBackupVaultResponse
--
--         , responseListBackupJobs $
--             listBackupJobsResponse
--
--         , responseGetBackupPlan $
--             getBackupPlanResponse
--
--         , responseListBackupPlanVersions $
--             listBackupPlanVersionsResponse
--
--         , responseListRestoreJobs $
--             listRestoreJobsResponse
--
--         , responseExportBackupPlanTemplate $
--             exportBackupPlanTemplateResponse
--
--         , responseStartBackupJob $
--             startBackupJobResponse
--
--         , responseCreateBackupPlan $
--             createBackupPlanResponse
--
--         , responseListProtectedResources $
--             listProtectedResourcesResponse
--
--         , responseDescribeBackupVault $
--             describeBackupVaultResponse
--
--         , responseGetBackupVaultNotifications $
--             getBackupVaultNotificationsResponse
--
--         , responseGetRecoveryPointRestoreMetadata $
--             getRecoveryPointRestoreMetadataResponse
--
--         , responseListBackupPlans $
--             listBackupPlansResponse
--
--         , responseStartRestoreJob $
--             startRestoreJobResponse
--
--         , responseListBackupSelections $
--             listBackupSelectionsResponse
--
--         , responseListRecoveryPointsByResource $
--             listRecoveryPointsByResourceResponse
--
--         , responseCreateBackupSelection $
--             createBackupSelectionResponse
--
--         , responseDescribeProtectedResource $
--             describeProtectedResourceResponse
--
--         , responseGetBackupPlanFromJSON $
--             getBackupPlanFromJSONResponse
--
--         , responseListBackupVaults $
--             listBackupVaultsResponse
--
--         , responseGetBackupSelection $
--             getBackupSelectionResponse
--
--         , responseCreateBackupVault $
--             createBackupVaultResponse
--
--         , responseUpdateRecoveryPointLifecycle $
--             updateRecoveryPointLifecycleResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responsePutBackupVaultNotifications $
--             putBackupVaultNotificationsResponse
--
--         , responseDeleteBackupVaultNotifications $
--             deleteBackupVaultNotificationsResponse
--
--         , responseListTags $
--             listTagsResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseGetBackupVaultAccessPolicy $
--             getBackupVaultAccessPolicyResponse
--
--         , responseDeleteRecoveryPoint $
--             deleteRecoveryPointResponse
--
--         , responseGetSupportedResourceTypes $
--             getSupportedResourceTypesResponse
--
--         , responseStopBackupJob $
--             stopBackupJobResponse
--
--         , responseListRecoveryPointsByBackupVault $
--             listRecoveryPointsByBackupVaultResponse
--
--         , responsePutBackupVaultAccessPolicy $
--             putBackupVaultAccessPolicyResponse
--
--         , responseDeleteBackupVaultAccessPolicy $
--             deleteBackupVaultAccessPolicyResponse
--
--           ]
--     ]

-- Requests

requestUpdateBackupPlan :: UpdateBackupPlan -> TestTree
requestUpdateBackupPlan = req
    "UpdateBackupPlan"
    "fixture/UpdateBackupPlan.yaml"

requestDeleteBackupPlan :: DeleteBackupPlan -> TestTree
requestDeleteBackupPlan = req
    "DeleteBackupPlan"
    "fixture/DeleteBackupPlan.yaml"

requestDescribeBackupJob :: DescribeBackupJob -> TestTree
requestDescribeBackupJob = req
    "DescribeBackupJob"
    "fixture/DescribeBackupJob.yaml"

requestListBackupPlanTemplates :: ListBackupPlanTemplates -> TestTree
requestListBackupPlanTemplates = req
    "ListBackupPlanTemplates"
    "fixture/ListBackupPlanTemplates.yaml"

requestDeleteBackupSelection :: DeleteBackupSelection -> TestTree
requestDeleteBackupSelection = req
    "DeleteBackupSelection"
    "fixture/DeleteBackupSelection.yaml"

requestDescribeRecoveryPoint :: DescribeRecoveryPoint -> TestTree
requestDescribeRecoveryPoint = req
    "DescribeRecoveryPoint"
    "fixture/DescribeRecoveryPoint.yaml"

requestDescribeRestoreJob :: DescribeRestoreJob -> TestTree
requestDescribeRestoreJob = req
    "DescribeRestoreJob"
    "fixture/DescribeRestoreJob.yaml"

requestGetBackupPlanFromTemplate :: GetBackupPlanFromTemplate -> TestTree
requestGetBackupPlanFromTemplate = req
    "GetBackupPlanFromTemplate"
    "fixture/GetBackupPlanFromTemplate.yaml"

requestDeleteBackupVault :: DeleteBackupVault -> TestTree
requestDeleteBackupVault = req
    "DeleteBackupVault"
    "fixture/DeleteBackupVault.yaml"

requestListBackupJobs :: ListBackupJobs -> TestTree
requestListBackupJobs = req
    "ListBackupJobs"
    "fixture/ListBackupJobs.yaml"

requestGetBackupPlan :: GetBackupPlan -> TestTree
requestGetBackupPlan = req
    "GetBackupPlan"
    "fixture/GetBackupPlan.yaml"

requestListBackupPlanVersions :: ListBackupPlanVersions -> TestTree
requestListBackupPlanVersions = req
    "ListBackupPlanVersions"
    "fixture/ListBackupPlanVersions.yaml"

requestListRestoreJobs :: ListRestoreJobs -> TestTree
requestListRestoreJobs = req
    "ListRestoreJobs"
    "fixture/ListRestoreJobs.yaml"

requestExportBackupPlanTemplate :: ExportBackupPlanTemplate -> TestTree
requestExportBackupPlanTemplate = req
    "ExportBackupPlanTemplate"
    "fixture/ExportBackupPlanTemplate.yaml"

requestStartBackupJob :: StartBackupJob -> TestTree
requestStartBackupJob = req
    "StartBackupJob"
    "fixture/StartBackupJob.yaml"

requestCreateBackupPlan :: CreateBackupPlan -> TestTree
requestCreateBackupPlan = req
    "CreateBackupPlan"
    "fixture/CreateBackupPlan.yaml"

requestListProtectedResources :: ListProtectedResources -> TestTree
requestListProtectedResources = req
    "ListProtectedResources"
    "fixture/ListProtectedResources.yaml"

requestDescribeBackupVault :: DescribeBackupVault -> TestTree
requestDescribeBackupVault = req
    "DescribeBackupVault"
    "fixture/DescribeBackupVault.yaml"

requestGetBackupVaultNotifications :: GetBackupVaultNotifications -> TestTree
requestGetBackupVaultNotifications = req
    "GetBackupVaultNotifications"
    "fixture/GetBackupVaultNotifications.yaml"

requestGetRecoveryPointRestoreMetadata :: GetRecoveryPointRestoreMetadata -> TestTree
requestGetRecoveryPointRestoreMetadata = req
    "GetRecoveryPointRestoreMetadata"
    "fixture/GetRecoveryPointRestoreMetadata.yaml"

requestListBackupPlans :: ListBackupPlans -> TestTree
requestListBackupPlans = req
    "ListBackupPlans"
    "fixture/ListBackupPlans.yaml"

requestStartRestoreJob :: StartRestoreJob -> TestTree
requestStartRestoreJob = req
    "StartRestoreJob"
    "fixture/StartRestoreJob.yaml"

requestListBackupSelections :: ListBackupSelections -> TestTree
requestListBackupSelections = req
    "ListBackupSelections"
    "fixture/ListBackupSelections.yaml"

requestListRecoveryPointsByResource :: ListRecoveryPointsByResource -> TestTree
requestListRecoveryPointsByResource = req
    "ListRecoveryPointsByResource"
    "fixture/ListRecoveryPointsByResource.yaml"

requestCreateBackupSelection :: CreateBackupSelection -> TestTree
requestCreateBackupSelection = req
    "CreateBackupSelection"
    "fixture/CreateBackupSelection.yaml"

requestDescribeProtectedResource :: DescribeProtectedResource -> TestTree
requestDescribeProtectedResource = req
    "DescribeProtectedResource"
    "fixture/DescribeProtectedResource.yaml"

requestGetBackupPlanFromJSON :: GetBackupPlanFromJSON -> TestTree
requestGetBackupPlanFromJSON = req
    "GetBackupPlanFromJSON"
    "fixture/GetBackupPlanFromJSON.yaml"

requestListBackupVaults :: ListBackupVaults -> TestTree
requestListBackupVaults = req
    "ListBackupVaults"
    "fixture/ListBackupVaults.yaml"

requestGetBackupSelection :: GetBackupSelection -> TestTree
requestGetBackupSelection = req
    "GetBackupSelection"
    "fixture/GetBackupSelection.yaml"

requestCreateBackupVault :: CreateBackupVault -> TestTree
requestCreateBackupVault = req
    "CreateBackupVault"
    "fixture/CreateBackupVault.yaml"

requestUpdateRecoveryPointLifecycle :: UpdateRecoveryPointLifecycle -> TestTree
requestUpdateRecoveryPointLifecycle = req
    "UpdateRecoveryPointLifecycle"
    "fixture/UpdateRecoveryPointLifecycle.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestPutBackupVaultNotifications :: PutBackupVaultNotifications -> TestTree
requestPutBackupVaultNotifications = req
    "PutBackupVaultNotifications"
    "fixture/PutBackupVaultNotifications.yaml"

requestDeleteBackupVaultNotifications :: DeleteBackupVaultNotifications -> TestTree
requestDeleteBackupVaultNotifications = req
    "DeleteBackupVaultNotifications"
    "fixture/DeleteBackupVaultNotifications.yaml"

requestListTags :: ListTags -> TestTree
requestListTags = req
    "ListTags"
    "fixture/ListTags.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestGetBackupVaultAccessPolicy :: GetBackupVaultAccessPolicy -> TestTree
requestGetBackupVaultAccessPolicy = req
    "GetBackupVaultAccessPolicy"
    "fixture/GetBackupVaultAccessPolicy.yaml"

requestDeleteRecoveryPoint :: DeleteRecoveryPoint -> TestTree
requestDeleteRecoveryPoint = req
    "DeleteRecoveryPoint"
    "fixture/DeleteRecoveryPoint.yaml"

requestGetSupportedResourceTypes :: GetSupportedResourceTypes -> TestTree
requestGetSupportedResourceTypes = req
    "GetSupportedResourceTypes"
    "fixture/GetSupportedResourceTypes.yaml"

requestStopBackupJob :: StopBackupJob -> TestTree
requestStopBackupJob = req
    "StopBackupJob"
    "fixture/StopBackupJob.yaml"

requestListRecoveryPointsByBackupVault :: ListRecoveryPointsByBackupVault -> TestTree
requestListRecoveryPointsByBackupVault = req
    "ListRecoveryPointsByBackupVault"
    "fixture/ListRecoveryPointsByBackupVault.yaml"

requestPutBackupVaultAccessPolicy :: PutBackupVaultAccessPolicy -> TestTree
requestPutBackupVaultAccessPolicy = req
    "PutBackupVaultAccessPolicy"
    "fixture/PutBackupVaultAccessPolicy.yaml"

requestDeleteBackupVaultAccessPolicy :: DeleteBackupVaultAccessPolicy -> TestTree
requestDeleteBackupVaultAccessPolicy = req
    "DeleteBackupVaultAccessPolicy"
    "fixture/DeleteBackupVaultAccessPolicy.yaml"

-- Responses

responseUpdateBackupPlan :: UpdateBackupPlanResponse -> TestTree
responseUpdateBackupPlan = res
    "UpdateBackupPlanResponse"
    "fixture/UpdateBackupPlanResponse.proto"
    backup
    (Proxy :: Proxy UpdateBackupPlan)

responseDeleteBackupPlan :: DeleteBackupPlanResponse -> TestTree
responseDeleteBackupPlan = res
    "DeleteBackupPlanResponse"
    "fixture/DeleteBackupPlanResponse.proto"
    backup
    (Proxy :: Proxy DeleteBackupPlan)

responseDescribeBackupJob :: DescribeBackupJobResponse -> TestTree
responseDescribeBackupJob = res
    "DescribeBackupJobResponse"
    "fixture/DescribeBackupJobResponse.proto"
    backup
    (Proxy :: Proxy DescribeBackupJob)

responseListBackupPlanTemplates :: ListBackupPlanTemplatesResponse -> TestTree
responseListBackupPlanTemplates = res
    "ListBackupPlanTemplatesResponse"
    "fixture/ListBackupPlanTemplatesResponse.proto"
    backup
    (Proxy :: Proxy ListBackupPlanTemplates)

responseDeleteBackupSelection :: DeleteBackupSelectionResponse -> TestTree
responseDeleteBackupSelection = res
    "DeleteBackupSelectionResponse"
    "fixture/DeleteBackupSelectionResponse.proto"
    backup
    (Proxy :: Proxy DeleteBackupSelection)

responseDescribeRecoveryPoint :: DescribeRecoveryPointResponse -> TestTree
responseDescribeRecoveryPoint = res
    "DescribeRecoveryPointResponse"
    "fixture/DescribeRecoveryPointResponse.proto"
    backup
    (Proxy :: Proxy DescribeRecoveryPoint)

responseDescribeRestoreJob :: DescribeRestoreJobResponse -> TestTree
responseDescribeRestoreJob = res
    "DescribeRestoreJobResponse"
    "fixture/DescribeRestoreJobResponse.proto"
    backup
    (Proxy :: Proxy DescribeRestoreJob)

responseGetBackupPlanFromTemplate :: GetBackupPlanFromTemplateResponse -> TestTree
responseGetBackupPlanFromTemplate = res
    "GetBackupPlanFromTemplateResponse"
    "fixture/GetBackupPlanFromTemplateResponse.proto"
    backup
    (Proxy :: Proxy GetBackupPlanFromTemplate)

responseDeleteBackupVault :: DeleteBackupVaultResponse -> TestTree
responseDeleteBackupVault = res
    "DeleteBackupVaultResponse"
    "fixture/DeleteBackupVaultResponse.proto"
    backup
    (Proxy :: Proxy DeleteBackupVault)

responseListBackupJobs :: ListBackupJobsResponse -> TestTree
responseListBackupJobs = res
    "ListBackupJobsResponse"
    "fixture/ListBackupJobsResponse.proto"
    backup
    (Proxy :: Proxy ListBackupJobs)

responseGetBackupPlan :: GetBackupPlanResponse -> TestTree
responseGetBackupPlan = res
    "GetBackupPlanResponse"
    "fixture/GetBackupPlanResponse.proto"
    backup
    (Proxy :: Proxy GetBackupPlan)

responseListBackupPlanVersions :: ListBackupPlanVersionsResponse -> TestTree
responseListBackupPlanVersions = res
    "ListBackupPlanVersionsResponse"
    "fixture/ListBackupPlanVersionsResponse.proto"
    backup
    (Proxy :: Proxy ListBackupPlanVersions)

responseListRestoreJobs :: ListRestoreJobsResponse -> TestTree
responseListRestoreJobs = res
    "ListRestoreJobsResponse"
    "fixture/ListRestoreJobsResponse.proto"
    backup
    (Proxy :: Proxy ListRestoreJobs)

responseExportBackupPlanTemplate :: ExportBackupPlanTemplateResponse -> TestTree
responseExportBackupPlanTemplate = res
    "ExportBackupPlanTemplateResponse"
    "fixture/ExportBackupPlanTemplateResponse.proto"
    backup
    (Proxy :: Proxy ExportBackupPlanTemplate)

responseStartBackupJob :: StartBackupJobResponse -> TestTree
responseStartBackupJob = res
    "StartBackupJobResponse"
    "fixture/StartBackupJobResponse.proto"
    backup
    (Proxy :: Proxy StartBackupJob)

responseCreateBackupPlan :: CreateBackupPlanResponse -> TestTree
responseCreateBackupPlan = res
    "CreateBackupPlanResponse"
    "fixture/CreateBackupPlanResponse.proto"
    backup
    (Proxy :: Proxy CreateBackupPlan)

responseListProtectedResources :: ListProtectedResourcesResponse -> TestTree
responseListProtectedResources = res
    "ListProtectedResourcesResponse"
    "fixture/ListProtectedResourcesResponse.proto"
    backup
    (Proxy :: Proxy ListProtectedResources)

responseDescribeBackupVault :: DescribeBackupVaultResponse -> TestTree
responseDescribeBackupVault = res
    "DescribeBackupVaultResponse"
    "fixture/DescribeBackupVaultResponse.proto"
    backup
    (Proxy :: Proxy DescribeBackupVault)

responseGetBackupVaultNotifications :: GetBackupVaultNotificationsResponse -> TestTree
responseGetBackupVaultNotifications = res
    "GetBackupVaultNotificationsResponse"
    "fixture/GetBackupVaultNotificationsResponse.proto"
    backup
    (Proxy :: Proxy GetBackupVaultNotifications)

responseGetRecoveryPointRestoreMetadata :: GetRecoveryPointRestoreMetadataResponse -> TestTree
responseGetRecoveryPointRestoreMetadata = res
    "GetRecoveryPointRestoreMetadataResponse"
    "fixture/GetRecoveryPointRestoreMetadataResponse.proto"
    backup
    (Proxy :: Proxy GetRecoveryPointRestoreMetadata)

responseListBackupPlans :: ListBackupPlansResponse -> TestTree
responseListBackupPlans = res
    "ListBackupPlansResponse"
    "fixture/ListBackupPlansResponse.proto"
    backup
    (Proxy :: Proxy ListBackupPlans)

responseStartRestoreJob :: StartRestoreJobResponse -> TestTree
responseStartRestoreJob = res
    "StartRestoreJobResponse"
    "fixture/StartRestoreJobResponse.proto"
    backup
    (Proxy :: Proxy StartRestoreJob)

responseListBackupSelections :: ListBackupSelectionsResponse -> TestTree
responseListBackupSelections = res
    "ListBackupSelectionsResponse"
    "fixture/ListBackupSelectionsResponse.proto"
    backup
    (Proxy :: Proxy ListBackupSelections)

responseListRecoveryPointsByResource :: ListRecoveryPointsByResourceResponse -> TestTree
responseListRecoveryPointsByResource = res
    "ListRecoveryPointsByResourceResponse"
    "fixture/ListRecoveryPointsByResourceResponse.proto"
    backup
    (Proxy :: Proxy ListRecoveryPointsByResource)

responseCreateBackupSelection :: CreateBackupSelectionResponse -> TestTree
responseCreateBackupSelection = res
    "CreateBackupSelectionResponse"
    "fixture/CreateBackupSelectionResponse.proto"
    backup
    (Proxy :: Proxy CreateBackupSelection)

responseDescribeProtectedResource :: DescribeProtectedResourceResponse -> TestTree
responseDescribeProtectedResource = res
    "DescribeProtectedResourceResponse"
    "fixture/DescribeProtectedResourceResponse.proto"
    backup
    (Proxy :: Proxy DescribeProtectedResource)

responseGetBackupPlanFromJSON :: GetBackupPlanFromJSONResponse -> TestTree
responseGetBackupPlanFromJSON = res
    "GetBackupPlanFromJSONResponse"
    "fixture/GetBackupPlanFromJSONResponse.proto"
    backup
    (Proxy :: Proxy GetBackupPlanFromJSON)

responseListBackupVaults :: ListBackupVaultsResponse -> TestTree
responseListBackupVaults = res
    "ListBackupVaultsResponse"
    "fixture/ListBackupVaultsResponse.proto"
    backup
    (Proxy :: Proxy ListBackupVaults)

responseGetBackupSelection :: GetBackupSelectionResponse -> TestTree
responseGetBackupSelection = res
    "GetBackupSelectionResponse"
    "fixture/GetBackupSelectionResponse.proto"
    backup
    (Proxy :: Proxy GetBackupSelection)

responseCreateBackupVault :: CreateBackupVaultResponse -> TestTree
responseCreateBackupVault = res
    "CreateBackupVaultResponse"
    "fixture/CreateBackupVaultResponse.proto"
    backup
    (Proxy :: Proxy CreateBackupVault)

responseUpdateRecoveryPointLifecycle :: UpdateRecoveryPointLifecycleResponse -> TestTree
responseUpdateRecoveryPointLifecycle = res
    "UpdateRecoveryPointLifecycleResponse"
    "fixture/UpdateRecoveryPointLifecycleResponse.proto"
    backup
    (Proxy :: Proxy UpdateRecoveryPointLifecycle)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    backup
    (Proxy :: Proxy TagResource)

responsePutBackupVaultNotifications :: PutBackupVaultNotificationsResponse -> TestTree
responsePutBackupVaultNotifications = res
    "PutBackupVaultNotificationsResponse"
    "fixture/PutBackupVaultNotificationsResponse.proto"
    backup
    (Proxy :: Proxy PutBackupVaultNotifications)

responseDeleteBackupVaultNotifications :: DeleteBackupVaultNotificationsResponse -> TestTree
responseDeleteBackupVaultNotifications = res
    "DeleteBackupVaultNotificationsResponse"
    "fixture/DeleteBackupVaultNotificationsResponse.proto"
    backup
    (Proxy :: Proxy DeleteBackupVaultNotifications)

responseListTags :: ListTagsResponse -> TestTree
responseListTags = res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    backup
    (Proxy :: Proxy ListTags)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    backup
    (Proxy :: Proxy UntagResource)

responseGetBackupVaultAccessPolicy :: GetBackupVaultAccessPolicyResponse -> TestTree
responseGetBackupVaultAccessPolicy = res
    "GetBackupVaultAccessPolicyResponse"
    "fixture/GetBackupVaultAccessPolicyResponse.proto"
    backup
    (Proxy :: Proxy GetBackupVaultAccessPolicy)

responseDeleteRecoveryPoint :: DeleteRecoveryPointResponse -> TestTree
responseDeleteRecoveryPoint = res
    "DeleteRecoveryPointResponse"
    "fixture/DeleteRecoveryPointResponse.proto"
    backup
    (Proxy :: Proxy DeleteRecoveryPoint)

responseGetSupportedResourceTypes :: GetSupportedResourceTypesResponse -> TestTree
responseGetSupportedResourceTypes = res
    "GetSupportedResourceTypesResponse"
    "fixture/GetSupportedResourceTypesResponse.proto"
    backup
    (Proxy :: Proxy GetSupportedResourceTypes)

responseStopBackupJob :: StopBackupJobResponse -> TestTree
responseStopBackupJob = res
    "StopBackupJobResponse"
    "fixture/StopBackupJobResponse.proto"
    backup
    (Proxy :: Proxy StopBackupJob)

responseListRecoveryPointsByBackupVault :: ListRecoveryPointsByBackupVaultResponse -> TestTree
responseListRecoveryPointsByBackupVault = res
    "ListRecoveryPointsByBackupVaultResponse"
    "fixture/ListRecoveryPointsByBackupVaultResponse.proto"
    backup
    (Proxy :: Proxy ListRecoveryPointsByBackupVault)

responsePutBackupVaultAccessPolicy :: PutBackupVaultAccessPolicyResponse -> TestTree
responsePutBackupVaultAccessPolicy = res
    "PutBackupVaultAccessPolicyResponse"
    "fixture/PutBackupVaultAccessPolicyResponse.proto"
    backup
    (Proxy :: Proxy PutBackupVaultAccessPolicy)

responseDeleteBackupVaultAccessPolicy :: DeleteBackupVaultAccessPolicyResponse -> TestTree
responseDeleteBackupVaultAccessPolicy = res
    "DeleteBackupVaultAccessPolicyResponse"
    "fixture/DeleteBackupVaultAccessPolicyResponse.proto"
    backup
    (Proxy :: Proxy DeleteBackupVaultAccessPolicy)
