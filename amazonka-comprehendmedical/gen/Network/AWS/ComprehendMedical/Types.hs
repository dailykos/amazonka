{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ComprehendMedical.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ComprehendMedical.Types
    (
    -- * Service Configuration
      comprehendMedical

    -- * Errors
    , _InvalidRequestException
    , _TooManyRequestsException
    , _InvalidEncodingException
    , _InternalServerException
    , _ServiceUnavailableException
    , _TextSizeLimitExceededException

    -- * Re-exported Types
    , module Network.AWS.ComprehendMedical.Internal

    -- * AttributeName
    , AttributeName (..)

    -- * EntitySubType
    , EntitySubType (..)

    -- * EntityType
    , EntityType (..)

    -- * Attribute
    , Attribute
    , attribute
    , aRelationshipScore
    , aBeginOffset
    , aText
    , aScore
    , aTraits
    , aEndOffset
    , aId
    , aType

    -- * Entity
    , Entity
    , entity
    , eBeginOffset
    , eText
    , eCategory
    , eScore
    , eTraits
    , eAttributes
    , eEndOffset
    , eId
    , eType

    -- * Trait
    , Trait
    , trait
    , tScore
    , tName

    -- * UnmappedAttribute
    , UnmappedAttribute
    , unmappedAttribute
    , uaAttribute
    , uaType
    ) where

import Network.AWS.ComprehendMedical.Internal
import Network.AWS.ComprehendMedical.Types.Product
import Network.AWS.ComprehendMedical.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2018-10-30@ of the Amazon Comprehend Medical SDK configuration.
comprehendMedical :: Service
comprehendMedical =
  Service
    { _svcAbbrev = "ComprehendMedical"
    , _svcSigner = v4
    , _svcPrefix = "comprehendmedical"
    , _svcVersion = "2018-10-30"
    , _svcEndpoint = defaultEndpoint comprehendMedical
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "ComprehendMedical"
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


-- | The request that you made is invalid. Check your request to determine why it's invalid and then retry the request.
--
--
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException =
  _MatchServiceError comprehendMedical "InvalidRequestException"


-- | You have made too many requests within a short period of time. Wait for a short time and then try your request again. Contact customer support for more information about a service limit increase. 
--
--
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException =
  _MatchServiceError comprehendMedical "TooManyRequestsException"


-- | The input text was not in valid UTF-8 character encoding. Check your text then retry your request.
--
--
_InvalidEncodingException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidEncodingException =
  _MatchServiceError comprehendMedical "InvalidEncodingException"


-- | An internal server error occurred. Retry your request. 
--
--
_InternalServerException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerException =
  _MatchServiceError comprehendMedical "InternalServerException"


-- | The Comprehend Medical service is temporarily unavailable. Please wait and then retry your request. 
--
--
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException =
  _MatchServiceError comprehendMedical "ServiceUnavailableException"


-- | The size of the text you submitted exceeds the size limit. Reduce the size of the text or use a smaller document and then retry your request. 
--
--
_TextSizeLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_TextSizeLimitExceededException =
  _MatchServiceError comprehendMedical "TextSizeLimitExceededException"

