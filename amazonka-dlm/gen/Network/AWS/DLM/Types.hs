{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DLM.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DLM.Types
    (
    -- * Service Configuration
      dlm

    -- * Errors
    , _InvalidRequestException
    , _InternalServerException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * Re-exported Types
    , module Network.AWS.DLM.Internal

    -- * GettablePolicyStateValues
    , GettablePolicyStateValues (..)

    -- * IntervalUnitValues
    , IntervalUnitValues (..)

    -- * ResourceTypeValues
    , ResourceTypeValues (..)

    -- * SettablePolicyStateValues
    , SettablePolicyStateValues (..)

    -- * CreateRule
    , CreateRule
    , createRule
    , crTimes
    , crInterval
    , crIntervalUnit

    -- * LifecyclePolicy
    , LifecyclePolicy
    , lifecyclePolicy
    , lpState
    , lpPolicyDetails
    , lpPolicyId
    , lpExecutionRoleARN
    , lpDateCreated
    , lpDateModified
    , lpDescription

    -- * LifecyclePolicySummary
    , LifecyclePolicySummary
    , lifecyclePolicySummary
    , lpsState
    , lpsPolicyId
    , lpsDescription

    -- * PolicyDetails
    , PolicyDetails
    , policyDetails
    , pdTargetTags
    , pdSchedules
    , pdResourceTypes

    -- * RetainRule
    , RetainRule
    , retainRule
    , rrCount

    -- * Schedule
    , Schedule
    , schedule
    , sCreateRule
    , sCopyTags
    , sName
    , sTagsToAdd
    , sRetainRule

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue
    ) where

import Network.AWS.DLM.Internal
import Network.AWS.DLM.Types.Product
import Network.AWS.DLM.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2018-01-12@ of the Amazon Data Lifecycle Manager SDK configuration.
dlm :: Service
dlm =
  Service
    { _svcAbbrev = "DLM"
    , _svcSigner = v4
    , _svcPrefix = "dlm"
    , _svcVersion = "2018-01-12"
    , _svcEndpoint = defaultEndpoint dlm
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "DLM"
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


-- | Bad request. The request is missing required parameters or has invalid parameters.
--
--
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException =
  _MatchServiceError dlm "InvalidRequestException" . hasStatus 400


-- | The service failed in an unexpected way.
--
--
_InternalServerException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerException =
  _MatchServiceError dlm "InternalServerException" . hasStatus 500


-- | A requested resource was not found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError dlm "ResourceNotFoundException" . hasStatus 404


-- | The request failed because a limit was exceeded.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError dlm "LimitExceededException" . hasStatus 429

