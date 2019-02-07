{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataSync.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DataSync.Types
    (
    -- * Service Configuration
      dataSync

    -- * Errors
    , _InvalidRequestException

    -- * Re-exported Types
    , module Network.AWS.DataSync.Internal

    -- * AgentStatus
    , AgentStatus (..)

    -- * Atime
    , Atime (..)

    -- * Gid
    , Gid (..)

    -- * Mtime
    , Mtime (..)

    -- * PhaseStatus
    , PhaseStatus (..)

    -- * PosixPermissions
    , PosixPermissions (..)

    -- * PreserveDeletedFiles
    , PreserveDeletedFiles (..)

    -- * PreserveDevices
    , PreserveDevices (..)

    -- * TaskExecutionStatus
    , TaskExecutionStatus (..)

    -- * TaskStatus
    , TaskStatus (..)

    -- * Uid
    , Uid (..)

    -- * VerifyMode
    , VerifyMode (..)

    -- * AgentListEntry
    , AgentListEntry
    , agentListEntry
    , aleStatus
    , aleAgentARN
    , aleName

    -- * EC2Config
    , EC2Config
    , ec2Config
    , ecSubnetARN
    , ecSecurityGroupARNs

    -- * LocationListEntry
    , LocationListEntry
    , locationListEntry
    , lleLocationURI
    , lleLocationARN

    -- * OnPremConfig
    , OnPremConfig
    , onPremConfig
    , opcAgentARNs

    -- * Options
    , Options
    , options
    , oAtime
    , oVerifyMode
    , oPosixPermissions
    , oMtime
    , oUid
    , oBytesPerSecond
    , oGid
    , oPreserveDeletedFiles
    , oPreserveDevices

    -- * S3Config
    , S3Config
    , s3Config
    , scBucketAccessRoleARN

    -- * TagListEntry
    , TagListEntry
    , tagListEntry
    , tleValue
    , tleKey

    -- * TaskExecutionListEntry
    , TaskExecutionListEntry
    , taskExecutionListEntry
    , teleStatus
    , teleTaskExecutionARN

    -- * TaskExecutionResultDetail
    , TaskExecutionResultDetail
    , taskExecutionResultDetail
    , terdPrepareDuration
    , terdPrepareStatus
    , terdVerifyStatus
    , terdVerifyDuration
    , terdTransferStatus
    , terdErrorCode
    , terdTransferDuration
    , terdErrorDetail

    -- * TaskListEntry
    , TaskListEntry
    , taskListEntry
    , tleStatus
    , tleTaskARN
    , tleName
    ) where

import Network.AWS.DataSync.Internal
import Network.AWS.DataSync.Types.Product
import Network.AWS.DataSync.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2018-11-09@ of the Amazon DataSync SDK configuration.
dataSync :: Service
dataSync =
  Service
    { _svcAbbrev = "DataSync"
    , _svcSigner = v4
    , _svcPrefix = "datasync"
    , _svcVersion = "2018-11-09"
    , _svcEndpoint = defaultEndpoint dataSync
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "DataSync"
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


-- | This exception is thrown when the client submits a malformed request.
--
--
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException = _MatchServiceError dataSync "InvalidRequestException"

