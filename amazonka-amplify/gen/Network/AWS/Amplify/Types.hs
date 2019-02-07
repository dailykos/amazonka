{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Amplify.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Amplify.Types
    (
    -- * Service Configuration
      amplify

    -- * Errors
    , _DependentServiceFailureException
    , _NotFoundException
    , _InternalFailureException
    , _UnauthorizedException
    , _BadRequestException
    , _LimitExceededException

    -- * Re-exported Types
    , module Network.AWS.Amplify.Internal

    -- * DomainStatus
    , DomainStatus (..)

    -- * JobStatus
    , JobStatus (..)

    -- * JobType
    , JobType (..)

    -- * Platform
    , Platform (..)

    -- * Stage
    , Stage (..)

    -- * App
    , App
    , app
    , appBasicAuthCredentials
    , appBuildSpec
    , appCustomRules
    , appIamServiceRoleARN
    , appProductionBranch
    , appTags
    , appAppId
    , appAppARN
    , appName
    , appDescription
    , appRepository
    , appPlatform
    , appCreateTime
    , appUpdateTime
    , appEnvironmentVariables
    , appDefaultDomain
    , appEnableBranchAutoBuild
    , appEnableBasicAuth

    -- * Branch
    , Branch
    , branch
    , bThumbnailURL
    , bBasicAuthCredentials
    , bBuildSpec
    , bDisplayName
    , bTags
    , bBranchARN
    , bBranchName
    , bDescription
    , bStage
    , bEnableNotification
    , bCreateTime
    , bUpdateTime
    , bEnvironmentVariables
    , bEnableAutoBuild
    , bCustomDomains
    , bFramework
    , bActiveJobId
    , bTotalNumberOfJobs
    , bEnableBasicAuth
    , bTtl

    -- * CustomRule
    , CustomRule
    , customRule
    , crStatus
    , crCondition
    , crSource
    , crTarget

    -- * DomainAssociation
    , DomainAssociation
    , domainAssociation
    , daDomainAssociationARN
    , daDomainName
    , daEnableAutoSubDomain
    , daDomainStatus
    , daStatusReason
    , daCertificateVerificationDNSRecord
    , daSubDomains

    -- * Job
    , Job
    , job
    , jSummary
    , jSteps

    -- * JobSummary
    , JobSummary
    , jobSummary
    , jsEndTime
    , jsJobARN
    , jsJobId
    , jsCommitId
    , jsCommitMessage
    , jsCommitTime
    , jsStartTime
    , jsStatus
    , jsJobType

    -- * ProductionBranch
    , ProductionBranch
    , productionBranch
    , pbLastDeployTime
    , pbStatus
    , pbThumbnailURL
    , pbBranchName

    -- * Step
    , Step
    , step
    , sLogURL
    , sArtifactsURL
    , sScreenshots
    , sStepName
    , sStartTime
    , sStatus
    , sEndTime

    -- * SubDomain
    , SubDomain
    , subDomain
    , sdSubDomainSetting
    , sdVerified
    , sdDnsRecord

    -- * SubDomainSetting
    , SubDomainSetting
    , subDomainSetting
    , sdsPrefix
    , sdsBranchName
    ) where

import Network.AWS.Amplify.Internal
import Network.AWS.Amplify.Types.Product
import Network.AWS.Amplify.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-07-25@ of the Amazon Amplify SDK configuration.
amplify :: Service
amplify =
  Service
    { _svcAbbrev = "Amplify"
    , _svcSigner = v4
    , _svcPrefix = "amplify"
    , _svcVersion = "2017-07-25"
    , _svcEndpoint = defaultEndpoint amplify
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Amplify"
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


-- | Exception thrown when an operation fails due to a dependent service throwing an exception. 
--
--
_DependentServiceFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_DependentServiceFailureException =
  _MatchServiceError amplify "DependentServiceFailureException" . hasStatus 503


-- | Exception thrown when an entity has not been found during an operation. 
--
--
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException =
  _MatchServiceError amplify "NotFoundException" . hasStatus 404


-- | Exception thrown when the service fails to perform an operation due to an internal issue. 
--
--
_InternalFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalFailureException =
  _MatchServiceError amplify "InternalFailureException" . hasStatus 500


-- | Exception thrown when an operation fails due to a lack of access. 
--
--
_UnauthorizedException :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthorizedException =
  _MatchServiceError amplify "UnauthorizedException" . hasStatus 401


-- | Exception thrown when a request contains unexpected data. 
--
--
_BadRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_BadRequestException =
  _MatchServiceError amplify "BadRequestException" . hasStatus 400


-- | Exception thrown when a resource could not be created because of service limits. 
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError amplify "LimitExceededException" . hasStatus 429

