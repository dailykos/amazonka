{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Chime.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Chime.Types
    (
    -- * Service Configuration
      chime

    -- * Errors
    , _ThrottledClientException
    , _UnprocessableEntityException
    , _ConflictException
    , _ForbiddenException
    , _NotFoundException
    , _ServiceFailureException
    , _UnauthorizedClientException
    , _ServiceUnavailableException
    , _BadRequestException

    -- * Re-exported Types
    , module Network.AWS.Chime.Internal

    -- * AccountType
    , AccountType (..)

    -- * EmailStatus
    , EmailStatus (..)

    -- * ErrorCode
    , ErrorCode (..)

    -- * InviteStatus
    , InviteStatus (..)

    -- * License
    , License (..)

    -- * RegistrationStatus
    , RegistrationStatus (..)

    -- * Account
    , Account
    , account
    , aDefaultLicense
    , aSupportedLicenses
    , aCreatedTimestamp
    , aAccountType
    , aAWSAccountId
    , aAccountId
    , aName

    -- * AccountSettings
    , AccountSettings
    , accountSettings
    , asEnableDialOut
    , asDisableRemoteControl

    -- * Invite
    , Invite
    , invite
    , iStatus
    , iEmailStatus
    , iInviteId
    , iEmailAddress

    -- * UpdateUserRequestItem
    , UpdateUserRequestItem
    , updateUserRequestItem
    , uuriLicenseType
    , uuriUserId

    -- * User
    , User
    , user
    , uUserInvitationStatus
    , uPersonalPIN
    , uLicenseType
    , uRegisteredOn
    , uAccountId
    , uUserRegistrationStatus
    , uInvitedOn
    , uDisplayName
    , uPrimaryEmail
    , uUserId

    -- * UserError
    , UserError
    , userError
    , ueUserId
    , ueErrorCode
    , ueErrorMessage
    ) where

import Network.AWS.Chime.Internal
import Network.AWS.Chime.Types.Product
import Network.AWS.Chime.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2018-05-01@ of the Amazon Chime SDK configuration.
chime :: Service
chime =
  Service
    { _svcAbbrev = "Chime"
    , _svcSigner = v4
    , _svcPrefix = "chime"
    , _svcVersion = "2018-05-01"
    , _svcEndpoint = defaultEndpoint chime
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Chime"
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


-- | The client exceeded its request rate limit.
--
--
_ThrottledClientException :: AsError a => Getting (First ServiceError) a ServiceError
_ThrottledClientException =
  _MatchServiceError chime "ThrottledClientException" . hasStatus 429


-- | The request was well-formed but was unable to be followed due to semantic errors.
--
--
_UnprocessableEntityException :: AsError a => Getting (First ServiceError) a ServiceError
_UnprocessableEntityException =
  _MatchServiceError chime "UnprocessableEntityException" . hasStatus 422


-- | The request could not be processed because of conflict in the current state of the resource.
--
--
_ConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictException =
  _MatchServiceError chime "ConflictException" . hasStatus 409


-- | The client is permanently forbidden from making the request. For example, when a user tries to create an account from an unsupported region.
--
--
_ForbiddenException :: AsError a => Getting (First ServiceError) a ServiceError
_ForbiddenException =
  _MatchServiceError chime "ForbiddenException" . hasStatus 403


-- | One or more of the resources in the request does not exist in the system.
--
--
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException =
  _MatchServiceError chime "NotFoundException" . hasStatus 404


-- | The service encountered an unexpected error.
--
--
_ServiceFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceFailureException =
  _MatchServiceError chime "ServiceFailureException" . hasStatus 500


-- | The client is not currently authorized to make the request.
--
--
_UnauthorizedClientException :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthorizedClientException =
  _MatchServiceError chime "UnauthorizedClientException" . hasStatus 401


-- | The service is currently unavailable.
--
--
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException =
  _MatchServiceError chime "ServiceUnavailableException" . hasStatus 503


-- | The input parameters don't match the service's restrictions.
--
--
_BadRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_BadRequestException =
  _MatchServiceError chime "BadRequestException" . hasStatus 400

