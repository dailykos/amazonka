{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ComprehendMedical
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.ComprehendMedical where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.ComprehendMedical
import Test.AWS.ComprehendMedical.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDetectEntities $
--             detectEntities
--
--         , requestDetectPHI $
--             detectPHI
--
--           ]

--     , testGroup "response"
--         [ responseDetectEntities $
--             detectEntitiesResponse
--
--         , responseDetectPHI $
--             detectPHIResponse
--
--           ]
--     ]

-- Requests

requestDetectEntities :: DetectEntities -> TestTree
requestDetectEntities = req
    "DetectEntities"
    "fixture/DetectEntities.yaml"

requestDetectPHI :: DetectPHI -> TestTree
requestDetectPHI = req
    "DetectPHI"
    "fixture/DetectPHI.yaml"

-- Responses

responseDetectEntities :: DetectEntitiesResponse -> TestTree
responseDetectEntities = res
    "DetectEntitiesResponse"
    "fixture/DetectEntitiesResponse.proto"
    comprehendMedical
    (Proxy :: Proxy DetectEntities)

responseDetectPHI :: DetectPHIResponse -> TestTree
responseDetectPHI = res
    "DetectPHIResponse"
    "fixture/DetectPHIResponse.proto"
    comprehendMedical
    (Proxy :: Proxy DetectPHI)
