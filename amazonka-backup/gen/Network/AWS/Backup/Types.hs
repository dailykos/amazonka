{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Backup.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Backup.Types
    (
    -- * Service Configuration
      backup

    -- * Errors
    , _InvalidRequestException
    , _DependencyFailureException
    , _InvalidParameterValueException
    , _ServiceUnavailableException
    , _ResourceNotFoundException
    , _AlreadyExistsException
    , _LimitExceededException
    , _MissingParameterValueException

    -- * Re-exported Types
    , module Network.AWS.Backup.Internal

    -- * BackupJobState
    , BackupJobState (..)

    -- * BackupVaultEvent
    , BackupVaultEvent (..)

    -- * ConditionType
    , ConditionType (..)

    -- * RecoveryPointStatus
    , RecoveryPointStatus (..)

    -- * RestoreJobStatus
    , RestoreJobStatus (..)

    -- * StorageClass
    , StorageClass (..)

    -- * BackupJob
    , BackupJob
    , backupJob
    , bjIAMRoleARN
    , bjState
    , bjResourceType
    , bjPercentDone
    , bjStartBy
    , bjCreatedBy
    , bjExpectedCompletionDate
    , bjBytesTransferred
    , bjBackupVaultARN
    , bjBackupJobId
    , bjResourceARN
    , bjStatusMessage
    , bjRecoveryPointARN
    , bjBackupSizeInBytes
    , bjCreationDate
    , bjCompletionDate
    , bjBackupVaultName

    -- * BackupPlan
    , BackupPlan
    , backupPlan
    , bpBackupPlanName
    , bpRules

    -- * BackupPlanInput
    , BackupPlanInput
    , backupPlanInput
    , bpiBackupPlanName
    , bpiRules

    -- * BackupPlanTemplatesListMember
    , BackupPlanTemplatesListMember
    , backupPlanTemplatesListMember
    , bptlmBackupPlanTemplateName
    , bptlmBackupPlanTemplateId

    -- * BackupPlansListMember
    , BackupPlansListMember
    , backupPlansListMember
    , bplmVersionId
    , bplmBackupPlanName
    , bplmBackupPlanId
    , bplmCreatorRequestId
    , bplmBackupPlanARN
    , bplmLastExecutionDate
    , bplmCreationDate
    , bplmDeletionDate

    -- * BackupRule
    , BackupRule
    , backupRule
    , brRuleId
    , brLifecycle
    , brRecoveryPointTags
    , brScheduleExpression
    , brCompletionWindowMinutes
    , brStartWindowMinutes
    , brRuleName
    , brTargetBackupVaultName

    -- * BackupRuleInput
    , BackupRuleInput
    , backupRuleInput
    , briLifecycle
    , briRecoveryPointTags
    , briScheduleExpression
    , briCompletionWindowMinutes
    , briStartWindowMinutes
    , briRuleName
    , briTargetBackupVaultName

    -- * BackupSelection
    , BackupSelection
    , backupSelection
    , bsResources
    , bsListOfTags
    , bsSelectionName
    , bsIAMRoleARN

    -- * BackupSelectionsListMember
    , BackupSelectionsListMember
    , backupSelectionsListMember
    , bslmIAMRoleARN
    , bslmSelectionName
    , bslmSelectionId
    , bslmBackupPlanId
    , bslmCreatorRequestId
    , bslmCreationDate

    -- * BackupVaultListMember
    , BackupVaultListMember
    , backupVaultListMember
    , bvlmCreatorRequestId
    , bvlmNumberOfRecoveryPoints
    , bvlmBackupVaultARN
    , bvlmEncryptionKeyARN
    , bvlmCreationDate
    , bvlmBackupVaultName

    -- * CalculatedLifecycle
    , CalculatedLifecycle
    , calculatedLifecycle
    , clDeleteAt
    , clMoveToColdStorageAt

    -- * Condition
    , Condition
    , condition
    , cConditionType
    , cConditionKey
    , cConditionValue

    -- * Lifecycle
    , Lifecycle
    , lifecycle
    , lMoveToColdStorageAfterDays
    , lDeleteAfterDays

    -- * ProtectedResource
    , ProtectedResource
    , protectedResource
    , prResourceType
    , prLastBackupTime
    , prResourceARN

    -- * RecoveryPointByBackupVault
    , RecoveryPointByBackupVault
    , recoveryPointByBackupVault
    , rpbbvIsEncrypted
    , rpbbvStatus
    , rpbbvIAMRoleARN
    , rpbbvResourceType
    , rpbbvCreatedBy
    , rpbbvCalculatedLifecycle
    , rpbbvLifecycle
    , rpbbvBackupVaultARN
    , rpbbvLastRestoreTime
    , rpbbvResourceARN
    , rpbbvRecoveryPointARN
    , rpbbvEncryptionKeyARN
    , rpbbvBackupSizeInBytes
    , rpbbvCreationDate
    , rpbbvCompletionDate
    , rpbbvBackupVaultName

    -- * RecoveryPointByResource
    , RecoveryPointByResource
    , recoveryPointByResource
    , rpbrStatus
    , rpbrRecoveryPointARN
    , rpbrBackupSizeBytes
    , rpbrEncryptionKeyARN
    , rpbrCreationDate
    , rpbrBackupVaultName

    -- * RecoveryPointCreator
    , RecoveryPointCreator
    , recoveryPointCreator
    , rpcBackupPlanId
    , rpcBackupPlanARN
    , rpcBackupPlanVersion
    , rpcBackupRuleId

    -- * RestoreJobsListMember
    , RestoreJobsListMember
    , restoreJobsListMember
    , rjlmStatus
    , rjlmIAMRoleARN
    , rjlmExpectedCompletionTimeMinutes
    , rjlmRestoreJobId
    , rjlmPercentDone
    , rjlmCreatedResourceARN
    , rjlmStatusMessage
    , rjlmRecoveryPointARN
    , rjlmBackupSizeInBytes
    , rjlmCreationDate
    , rjlmCompletionDate
    ) where

import Network.AWS.Backup.Internal
import Network.AWS.Backup.Types.Product
import Network.AWS.Backup.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2018-11-15@ of the Amazon Backup SDK configuration.
backup :: Service
backup =
  Service
    { _svcAbbrev = "Backup"
    , _svcSigner = v4
    , _svcPrefix = "backup"
    , _svcVersion = "2018-11-15"
    , _svcEndpoint = defaultEndpoint backup
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Backup"
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


-- | Indicates that something is wrong with the input to the request. For example, a parameter is of the wrong type.
--
--
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException = _MatchServiceError backup "InvalidRequestException"


-- | A dependent AWS service or resource returned an error to the AWS Backup service, and the action cannot be completed.
--
--
_DependencyFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_DependencyFailureException =
  _MatchServiceError backup "DependencyFailureException"


-- | Indicates that something is wrong with a parameter's value. For example, the value is out of range.
--
--
_InvalidParameterValueException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValueException =
  _MatchServiceError backup "InvalidParameterValueException"


-- | The request failed due to a temporary failure of the server.
--
--
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException =
  _MatchServiceError backup "ServiceUnavailableException"


-- | A resource that is required for the action doesn't exist.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError backup "ResourceNotFoundException"


-- | The required resource already exists.
--
--
_AlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_AlreadyExistsException = _MatchServiceError backup "AlreadyExistsException"


-- | A limit in the request has been exceeded; for example, a maximum number of items allowed in a request.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _MatchServiceError backup "LimitExceededException"


-- | Indicates that a required parameter is missing.
--
--
_MissingParameterValueException :: AsError a => Getting (First ServiceError) a ServiceError
_MissingParameterValueException =
  _MatchServiceError backup "MissingParameterValueException"

