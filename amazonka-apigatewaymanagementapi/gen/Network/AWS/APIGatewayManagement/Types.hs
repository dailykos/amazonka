{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGatewayManagement.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.APIGatewayManagement.Types
    (
    -- * Service Configuration
      apiGatewayManagement

    -- * Errors
    , _PayloadTooLargeException
    , _ForbiddenException
    , _GoneException
    , _LimitExceededException

    -- * Re-exported Types
    , module Network.AWS.APIGatewayManagement.Internal
    ) where

import Network.AWS.APIGatewayManagement.Internal
import Network.AWS.APIGatewayManagement.Types.Product
import Network.AWS.APIGatewayManagement.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2018-11-29@ of the Amazon ApiGatewayManagementApi SDK configuration.
apiGatewayManagement :: Service
apiGatewayManagement =
  Service
    { _svcAbbrev = "APIGatewayManagement"
    , _svcSigner = v4
    , _svcPrefix = "execute-api"
    , _svcVersion = "2018-11-29"
    , _svcEndpoint = defaultEndpoint apiGatewayManagement
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "APIGatewayManagement"
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


-- | The data has exceeded the maximum size allowed.
--
--
_PayloadTooLargeException :: AsError a => Getting (First ServiceError) a ServiceError
_PayloadTooLargeException =
  _MatchServiceError apiGatewayManagement "PayloadTooLargeException" .
  hasStatus 413


-- | The caller is not authorized to invoke this operation.
--
--
_ForbiddenException :: AsError a => Getting (First ServiceError) a ServiceError
_ForbiddenException =
  _MatchServiceError apiGatewayManagement "ForbiddenException" . hasStatus 403


-- | The connection with the provided id no longer exists.
--
--
_GoneException :: AsError a => Getting (First ServiceError) a ServiceError
_GoneException =
  _MatchServiceError apiGatewayManagement "GoneException" . hasStatus 410


-- | The client is sending more than the allowed number of requests per unit of time.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError apiGatewayManagement "LimitExceededException" .
  hasStatus 429

