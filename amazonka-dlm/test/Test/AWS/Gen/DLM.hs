{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DLM
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.DLM where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.DLM
import Test.AWS.DLM.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteLifecyclePolicy $
--             deleteLifecyclePolicy
--
--         , requestUpdateLifecyclePolicy $
--             updateLifecyclePolicy
--
--         , requestCreateLifecyclePolicy $
--             createLifecyclePolicy
--
--         , requestGetLifecyclePolicy $
--             getLifecyclePolicy
--
--         , requestGetLifecyclePolicies $
--             getLifecyclePolicies
--
--           ]

--     , testGroup "response"
--         [ responseDeleteLifecyclePolicy $
--             deleteLifecyclePolicyResponse
--
--         , responseUpdateLifecyclePolicy $
--             updateLifecyclePolicyResponse
--
--         , responseCreateLifecyclePolicy $
--             createLifecyclePolicyResponse
--
--         , responseGetLifecyclePolicy $
--             getLifecyclePolicyResponse
--
--         , responseGetLifecyclePolicies $
--             getLifecyclePoliciesResponse
--
--           ]
--     ]

-- Requests

requestDeleteLifecyclePolicy :: DeleteLifecyclePolicy -> TestTree
requestDeleteLifecyclePolicy = req
    "DeleteLifecyclePolicy"
    "fixture/DeleteLifecyclePolicy.yaml"

requestUpdateLifecyclePolicy :: UpdateLifecyclePolicy -> TestTree
requestUpdateLifecyclePolicy = req
    "UpdateLifecyclePolicy"
    "fixture/UpdateLifecyclePolicy.yaml"

requestCreateLifecyclePolicy :: CreateLifecyclePolicy -> TestTree
requestCreateLifecyclePolicy = req
    "CreateLifecyclePolicy"
    "fixture/CreateLifecyclePolicy.yaml"

requestGetLifecyclePolicy :: GetLifecyclePolicy -> TestTree
requestGetLifecyclePolicy = req
    "GetLifecyclePolicy"
    "fixture/GetLifecyclePolicy.yaml"

requestGetLifecyclePolicies :: GetLifecyclePolicies -> TestTree
requestGetLifecyclePolicies = req
    "GetLifecyclePolicies"
    "fixture/GetLifecyclePolicies.yaml"

-- Responses

responseDeleteLifecyclePolicy :: DeleteLifecyclePolicyResponse -> TestTree
responseDeleteLifecyclePolicy = res
    "DeleteLifecyclePolicyResponse"
    "fixture/DeleteLifecyclePolicyResponse.proto"
    dlm
    (Proxy :: Proxy DeleteLifecyclePolicy)

responseUpdateLifecyclePolicy :: UpdateLifecyclePolicyResponse -> TestTree
responseUpdateLifecyclePolicy = res
    "UpdateLifecyclePolicyResponse"
    "fixture/UpdateLifecyclePolicyResponse.proto"
    dlm
    (Proxy :: Proxy UpdateLifecyclePolicy)

responseCreateLifecyclePolicy :: CreateLifecyclePolicyResponse -> TestTree
responseCreateLifecyclePolicy = res
    "CreateLifecyclePolicyResponse"
    "fixture/CreateLifecyclePolicyResponse.proto"
    dlm
    (Proxy :: Proxy CreateLifecyclePolicy)

responseGetLifecyclePolicy :: GetLifecyclePolicyResponse -> TestTree
responseGetLifecyclePolicy = res
    "GetLifecyclePolicyResponse"
    "fixture/GetLifecyclePolicyResponse.proto"
    dlm
    (Proxy :: Proxy GetLifecyclePolicy)

responseGetLifecyclePolicies :: GetLifecyclePoliciesResponse -> TestTree
responseGetLifecyclePolicies = res
    "GetLifecyclePoliciesResponse"
    "fixture/GetLifecyclePoliciesResponse.proto"
    dlm
    (Proxy :: Proxy GetLifecyclePolicies)
