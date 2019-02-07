{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Amplify
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Amplify where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Amplify
import Test.AWS.Amplify.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetDomainAssociation $
--             getDomainAssociation
--
--         , requestStopJob $
--             stopJob
--
--         , requestGetBranch $
--             getBranch
--
--         , requestCreateDomainAssociation $
--             createDomainAssociation
--
--         , requestDeleteBranch $
--             deleteBranch
--
--         , requestUpdateBranch $
--             updateBranch
--
--         , requestCreateBranch $
--             createBranch
--
--         , requestListApps $
--             listApps
--
--         , requestListBranches $
--             listBranches
--
--         , requestDeleteApp $
--             deleteApp
--
--         , requestUpdateApp $
--             updateApp
--
--         , requestListJobs $
--             listJobs
--
--         , requestDeleteJob $
--             deleteJob
--
--         , requestGetJob $
--             getJob
--
--         , requestStartJob $
--             startJob
--
--         , requestGetApp $
--             getApp
--
--         , requestCreateApp $
--             createApp
--
--         , requestDeleteDomainAssociation $
--             deleteDomainAssociation
--
--         , requestUpdateDomainAssociation $
--             updateDomainAssociation
--
--         , requestListDomainAssociations $
--             listDomainAssociations
--
--           ]

--     , testGroup "response"
--         [ responseGetDomainAssociation $
--             getDomainAssociationResponse
--
--         , responseStopJob $
--             stopJobResponse
--
--         , responseGetBranch $
--             getBranchResponse
--
--         , responseCreateDomainAssociation $
--             createDomainAssociationResponse
--
--         , responseDeleteBranch $
--             deleteBranchResponse
--
--         , responseUpdateBranch $
--             updateBranchResponse
--
--         , responseCreateBranch $
--             createBranchResponse
--
--         , responseListApps $
--             listAppsResponse
--
--         , responseListBranches $
--             listBranchesResponse
--
--         , responseDeleteApp $
--             deleteAppResponse
--
--         , responseUpdateApp $
--             updateAppResponse
--
--         , responseListJobs $
--             listJobsResponse
--
--         , responseDeleteJob $
--             deleteJobResponse
--
--         , responseGetJob $
--             getJobResponse
--
--         , responseStartJob $
--             startJobResponse
--
--         , responseGetApp $
--             getAppResponse
--
--         , responseCreateApp $
--             createAppResponse
--
--         , responseDeleteDomainAssociation $
--             deleteDomainAssociationResponse
--
--         , responseUpdateDomainAssociation $
--             updateDomainAssociationResponse
--
--         , responseListDomainAssociations $
--             listDomainAssociationsResponse
--
--           ]
--     ]

-- Requests

requestGetDomainAssociation :: GetDomainAssociation -> TestTree
requestGetDomainAssociation = req
    "GetDomainAssociation"
    "fixture/GetDomainAssociation.yaml"

requestStopJob :: StopJob -> TestTree
requestStopJob = req
    "StopJob"
    "fixture/StopJob.yaml"

requestGetBranch :: GetBranch -> TestTree
requestGetBranch = req
    "GetBranch"
    "fixture/GetBranch.yaml"

requestCreateDomainAssociation :: CreateDomainAssociation -> TestTree
requestCreateDomainAssociation = req
    "CreateDomainAssociation"
    "fixture/CreateDomainAssociation.yaml"

requestDeleteBranch :: DeleteBranch -> TestTree
requestDeleteBranch = req
    "DeleteBranch"
    "fixture/DeleteBranch.yaml"

requestUpdateBranch :: UpdateBranch -> TestTree
requestUpdateBranch = req
    "UpdateBranch"
    "fixture/UpdateBranch.yaml"

requestCreateBranch :: CreateBranch -> TestTree
requestCreateBranch = req
    "CreateBranch"
    "fixture/CreateBranch.yaml"

requestListApps :: ListApps -> TestTree
requestListApps = req
    "ListApps"
    "fixture/ListApps.yaml"

requestListBranches :: ListBranches -> TestTree
requestListBranches = req
    "ListBranches"
    "fixture/ListBranches.yaml"

requestDeleteApp :: DeleteApp -> TestTree
requestDeleteApp = req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

requestUpdateApp :: UpdateApp -> TestTree
requestUpdateApp = req
    "UpdateApp"
    "fixture/UpdateApp.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs = req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestDeleteJob :: DeleteJob -> TestTree
requestDeleteJob = req
    "DeleteJob"
    "fixture/DeleteJob.yaml"

requestGetJob :: GetJob -> TestTree
requestGetJob = req
    "GetJob"
    "fixture/GetJob.yaml"

requestStartJob :: StartJob -> TestTree
requestStartJob = req
    "StartJob"
    "fixture/StartJob.yaml"

requestGetApp :: GetApp -> TestTree
requestGetApp = req
    "GetApp"
    "fixture/GetApp.yaml"

requestCreateApp :: CreateApp -> TestTree
requestCreateApp = req
    "CreateApp"
    "fixture/CreateApp.yaml"

requestDeleteDomainAssociation :: DeleteDomainAssociation -> TestTree
requestDeleteDomainAssociation = req
    "DeleteDomainAssociation"
    "fixture/DeleteDomainAssociation.yaml"

requestUpdateDomainAssociation :: UpdateDomainAssociation -> TestTree
requestUpdateDomainAssociation = req
    "UpdateDomainAssociation"
    "fixture/UpdateDomainAssociation.yaml"

requestListDomainAssociations :: ListDomainAssociations -> TestTree
requestListDomainAssociations = req
    "ListDomainAssociations"
    "fixture/ListDomainAssociations.yaml"

-- Responses

responseGetDomainAssociation :: GetDomainAssociationResponse -> TestTree
responseGetDomainAssociation = res
    "GetDomainAssociationResponse"
    "fixture/GetDomainAssociationResponse.proto"
    amplify
    (Proxy :: Proxy GetDomainAssociation)

responseStopJob :: StopJobResponse -> TestTree
responseStopJob = res
    "StopJobResponse"
    "fixture/StopJobResponse.proto"
    amplify
    (Proxy :: Proxy StopJob)

responseGetBranch :: GetBranchResponse -> TestTree
responseGetBranch = res
    "GetBranchResponse"
    "fixture/GetBranchResponse.proto"
    amplify
    (Proxy :: Proxy GetBranch)

responseCreateDomainAssociation :: CreateDomainAssociationResponse -> TestTree
responseCreateDomainAssociation = res
    "CreateDomainAssociationResponse"
    "fixture/CreateDomainAssociationResponse.proto"
    amplify
    (Proxy :: Proxy CreateDomainAssociation)

responseDeleteBranch :: DeleteBranchResponse -> TestTree
responseDeleteBranch = res
    "DeleteBranchResponse"
    "fixture/DeleteBranchResponse.proto"
    amplify
    (Proxy :: Proxy DeleteBranch)

responseUpdateBranch :: UpdateBranchResponse -> TestTree
responseUpdateBranch = res
    "UpdateBranchResponse"
    "fixture/UpdateBranchResponse.proto"
    amplify
    (Proxy :: Proxy UpdateBranch)

responseCreateBranch :: CreateBranchResponse -> TestTree
responseCreateBranch = res
    "CreateBranchResponse"
    "fixture/CreateBranchResponse.proto"
    amplify
    (Proxy :: Proxy CreateBranch)

responseListApps :: ListAppsResponse -> TestTree
responseListApps = res
    "ListAppsResponse"
    "fixture/ListAppsResponse.proto"
    amplify
    (Proxy :: Proxy ListApps)

responseListBranches :: ListBranchesResponse -> TestTree
responseListBranches = res
    "ListBranchesResponse"
    "fixture/ListBranchesResponse.proto"
    amplify
    (Proxy :: Proxy ListBranches)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp = res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    amplify
    (Proxy :: Proxy DeleteApp)

responseUpdateApp :: UpdateAppResponse -> TestTree
responseUpdateApp = res
    "UpdateAppResponse"
    "fixture/UpdateAppResponse.proto"
    amplify
    (Proxy :: Proxy UpdateApp)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs = res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    amplify
    (Proxy :: Proxy ListJobs)

responseDeleteJob :: DeleteJobResponse -> TestTree
responseDeleteJob = res
    "DeleteJobResponse"
    "fixture/DeleteJobResponse.proto"
    amplify
    (Proxy :: Proxy DeleteJob)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob = res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    amplify
    (Proxy :: Proxy GetJob)

responseStartJob :: StartJobResponse -> TestTree
responseStartJob = res
    "StartJobResponse"
    "fixture/StartJobResponse.proto"
    amplify
    (Proxy :: Proxy StartJob)

responseGetApp :: GetAppResponse -> TestTree
responseGetApp = res
    "GetAppResponse"
    "fixture/GetAppResponse.proto"
    amplify
    (Proxy :: Proxy GetApp)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp = res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    amplify
    (Proxy :: Proxy CreateApp)

responseDeleteDomainAssociation :: DeleteDomainAssociationResponse -> TestTree
responseDeleteDomainAssociation = res
    "DeleteDomainAssociationResponse"
    "fixture/DeleteDomainAssociationResponse.proto"
    amplify
    (Proxy :: Proxy DeleteDomainAssociation)

responseUpdateDomainAssociation :: UpdateDomainAssociationResponse -> TestTree
responseUpdateDomainAssociation = res
    "UpdateDomainAssociationResponse"
    "fixture/UpdateDomainAssociationResponse.proto"
    amplify
    (Proxy :: Proxy UpdateDomainAssociation)

responseListDomainAssociations :: ListDomainAssociationsResponse -> TestTree
responseListDomainAssociations = res
    "ListDomainAssociationsResponse"
    "fixture/ListDomainAssociationsResponse.proto"
    amplify
    (Proxy :: Proxy ListDomainAssociations)
