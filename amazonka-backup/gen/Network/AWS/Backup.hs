{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Backup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Backup__ 
--
-- AWS Backup is a unified backup service designed to protect AWS services and their associated data. AWS Backup simplifies the creation, migration, restoration, and deletion of backups, while also providing reporting and auditing.
--
module Network.AWS.Backup
    (
    -- * Service Configuration
      backup

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** DependencyFailureException
    , _DependencyFailureException

    -- ** InvalidParameterValueException
    , _InvalidParameterValueException

    -- ** ServiceUnavailableException
    , _ServiceUnavailableException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** AlreadyExistsException
    , _AlreadyExistsException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** MissingParameterValueException
    , _MissingParameterValueException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** UpdateBackupPlan 
    , module Network.AWS.Backup.UpdateBackupPlan

    -- ** DeleteBackupPlan 
    , module Network.AWS.Backup.DeleteBackupPlan

    -- ** DescribeBackupJob 
    , module Network.AWS.Backup.DescribeBackupJob

    -- ** ListBackupPlanTemplates 
    , module Network.AWS.Backup.ListBackupPlanTemplates

    -- ** DeleteBackupSelection 
    , module Network.AWS.Backup.DeleteBackupSelection

    -- ** DescribeRecoveryPoint 
    , module Network.AWS.Backup.DescribeRecoveryPoint

    -- ** DescribeRestoreJob 
    , module Network.AWS.Backup.DescribeRestoreJob

    -- ** GetBackupPlanFromTemplate 
    , module Network.AWS.Backup.GetBackupPlanFromTemplate

    -- ** DeleteBackupVault 
    , module Network.AWS.Backup.DeleteBackupVault

    -- ** ListBackupJobs 
    , module Network.AWS.Backup.ListBackupJobs

    -- ** GetBackupPlan 
    , module Network.AWS.Backup.GetBackupPlan

    -- ** ListBackupPlanVersions 
    , module Network.AWS.Backup.ListBackupPlanVersions

    -- ** ListRestoreJobs 
    , module Network.AWS.Backup.ListRestoreJobs

    -- ** ExportBackupPlanTemplate 
    , module Network.AWS.Backup.ExportBackupPlanTemplate

    -- ** StartBackupJob 
    , module Network.AWS.Backup.StartBackupJob

    -- ** CreateBackupPlan 
    , module Network.AWS.Backup.CreateBackupPlan

    -- ** ListProtectedResources 
    , module Network.AWS.Backup.ListProtectedResources

    -- ** DescribeBackupVault 
    , module Network.AWS.Backup.DescribeBackupVault

    -- ** GetBackupVaultNotifications 
    , module Network.AWS.Backup.GetBackupVaultNotifications

    -- ** GetRecoveryPointRestoreMetadata 
    , module Network.AWS.Backup.GetRecoveryPointRestoreMetadata

    -- ** ListBackupPlans 
    , module Network.AWS.Backup.ListBackupPlans

    -- ** StartRestoreJob 
    , module Network.AWS.Backup.StartRestoreJob

    -- ** ListBackupSelections 
    , module Network.AWS.Backup.ListBackupSelections

    -- ** ListRecoveryPointsByResource 
    , module Network.AWS.Backup.ListRecoveryPointsByResource

    -- ** CreateBackupSelection 
    , module Network.AWS.Backup.CreateBackupSelection

    -- ** DescribeProtectedResource 
    , module Network.AWS.Backup.DescribeProtectedResource

    -- ** GetBackupPlanFromJSON 
    , module Network.AWS.Backup.GetBackupPlanFromJSON

    -- ** ListBackupVaults 
    , module Network.AWS.Backup.ListBackupVaults

    -- ** GetBackupSelection 
    , module Network.AWS.Backup.GetBackupSelection

    -- ** CreateBackupVault 
    , module Network.AWS.Backup.CreateBackupVault

    -- ** UpdateRecoveryPointLifecycle 
    , module Network.AWS.Backup.UpdateRecoveryPointLifecycle

    -- ** TagResource 
    , module Network.AWS.Backup.TagResource

    -- ** PutBackupVaultNotifications 
    , module Network.AWS.Backup.PutBackupVaultNotifications

    -- ** DeleteBackupVaultNotifications 
    , module Network.AWS.Backup.DeleteBackupVaultNotifications

    -- ** ListTags 
    , module Network.AWS.Backup.ListTags

    -- ** UntagResource 
    , module Network.AWS.Backup.UntagResource

    -- ** GetBackupVaultAccessPolicy 
    , module Network.AWS.Backup.GetBackupVaultAccessPolicy

    -- ** DeleteRecoveryPoint 
    , module Network.AWS.Backup.DeleteRecoveryPoint

    -- ** GetSupportedResourceTypes 
    , module Network.AWS.Backup.GetSupportedResourceTypes

    -- ** StopBackupJob 
    , module Network.AWS.Backup.StopBackupJob

    -- ** ListRecoveryPointsByBackupVault 
    , module Network.AWS.Backup.ListRecoveryPointsByBackupVault

    -- ** PutBackupVaultAccessPolicy 
    , module Network.AWS.Backup.PutBackupVaultAccessPolicy

    -- ** DeleteBackupVaultAccessPolicy 
    , module Network.AWS.Backup.DeleteBackupVaultAccessPolicy

    -- * Types

    -- ** Common
    , module Network.AWS.Backup.Internal

    -- ** BackupJobState
    , BackupJobState (..)

    -- ** BackupVaultEvent
    , BackupVaultEvent (..)

    -- ** ConditionType
    , ConditionType (..)

    -- ** RecoveryPointStatus
    , RecoveryPointStatus (..)

    -- ** RestoreJobStatus
    , RestoreJobStatus (..)

    -- ** StorageClass
    , StorageClass (..)

    -- ** BackupJob
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

    -- ** BackupPlan
    , BackupPlan
    , backupPlan
    , bpBackupPlanName
    , bpRules

    -- ** BackupPlanInput
    , BackupPlanInput
    , backupPlanInput
    , bpiBackupPlanName
    , bpiRules

    -- ** BackupPlanTemplatesListMember
    , BackupPlanTemplatesListMember
    , backupPlanTemplatesListMember
    , bptlmBackupPlanTemplateName
    , bptlmBackupPlanTemplateId

    -- ** BackupPlansListMember
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

    -- ** BackupRule
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

    -- ** BackupRuleInput
    , BackupRuleInput
    , backupRuleInput
    , briLifecycle
    , briRecoveryPointTags
    , briScheduleExpression
    , briCompletionWindowMinutes
    , briStartWindowMinutes
    , briRuleName
    , briTargetBackupVaultName

    -- ** BackupSelection
    , BackupSelection
    , backupSelection
    , bsResources
    , bsListOfTags
    , bsSelectionName
    , bsIAMRoleARN

    -- ** BackupSelectionsListMember
    , BackupSelectionsListMember
    , backupSelectionsListMember
    , bslmIAMRoleARN
    , bslmSelectionName
    , bslmSelectionId
    , bslmBackupPlanId
    , bslmCreatorRequestId
    , bslmCreationDate

    -- ** BackupVaultListMember
    , BackupVaultListMember
    , backupVaultListMember
    , bvlmCreatorRequestId
    , bvlmNumberOfRecoveryPoints
    , bvlmBackupVaultARN
    , bvlmEncryptionKeyARN
    , bvlmCreationDate
    , bvlmBackupVaultName

    -- ** CalculatedLifecycle
    , CalculatedLifecycle
    , calculatedLifecycle
    , clDeleteAt
    , clMoveToColdStorageAt

    -- ** Condition
    , Condition
    , condition
    , cConditionType
    , cConditionKey
    , cConditionValue

    -- ** Lifecycle
    , Lifecycle
    , lifecycle
    , lMoveToColdStorageAfterDays
    , lDeleteAfterDays

    -- ** ProtectedResource
    , ProtectedResource
    , protectedResource
    , prResourceType
    , prLastBackupTime
    , prResourceARN

    -- ** RecoveryPointByBackupVault
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

    -- ** RecoveryPointByResource
    , RecoveryPointByResource
    , recoveryPointByResource
    , rpbrStatus
    , rpbrRecoveryPointARN
    , rpbrBackupSizeBytes
    , rpbrEncryptionKeyARN
    , rpbrCreationDate
    , rpbrBackupVaultName

    -- ** RecoveryPointCreator
    , RecoveryPointCreator
    , recoveryPointCreator
    , rpcBackupPlanId
    , rpcBackupPlanARN
    , rpcBackupPlanVersion
    , rpcBackupRuleId

    -- ** RestoreJobsListMember
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

import Network.AWS.Backup.CreateBackupPlan
import Network.AWS.Backup.CreateBackupSelection
import Network.AWS.Backup.CreateBackupVault
import Network.AWS.Backup.DeleteBackupPlan
import Network.AWS.Backup.DeleteBackupSelection
import Network.AWS.Backup.DeleteBackupVault
import Network.AWS.Backup.DeleteBackupVaultAccessPolicy
import Network.AWS.Backup.DeleteBackupVaultNotifications
import Network.AWS.Backup.DeleteRecoveryPoint
import Network.AWS.Backup.DescribeBackupJob
import Network.AWS.Backup.DescribeBackupVault
import Network.AWS.Backup.DescribeProtectedResource
import Network.AWS.Backup.DescribeRecoveryPoint
import Network.AWS.Backup.DescribeRestoreJob
import Network.AWS.Backup.ExportBackupPlanTemplate
import Network.AWS.Backup.GetBackupPlan
import Network.AWS.Backup.GetBackupPlanFromJSON
import Network.AWS.Backup.GetBackupPlanFromTemplate
import Network.AWS.Backup.GetBackupSelection
import Network.AWS.Backup.GetBackupVaultAccessPolicy
import Network.AWS.Backup.GetBackupVaultNotifications
import Network.AWS.Backup.GetRecoveryPointRestoreMetadata
import Network.AWS.Backup.GetSupportedResourceTypes
import Network.AWS.Backup.ListBackupJobs
import Network.AWS.Backup.ListBackupPlanTemplates
import Network.AWS.Backup.ListBackupPlanVersions
import Network.AWS.Backup.ListBackupPlans
import Network.AWS.Backup.ListBackupSelections
import Network.AWS.Backup.ListBackupVaults
import Network.AWS.Backup.ListProtectedResources
import Network.AWS.Backup.ListRecoveryPointsByBackupVault
import Network.AWS.Backup.ListRecoveryPointsByResource
import Network.AWS.Backup.ListRestoreJobs
import Network.AWS.Backup.ListTags
import Network.AWS.Backup.PutBackupVaultAccessPolicy
import Network.AWS.Backup.PutBackupVaultNotifications
import Network.AWS.Backup.StartBackupJob
import Network.AWS.Backup.StartRestoreJob
import Network.AWS.Backup.StopBackupJob
import Network.AWS.Backup.TagResource
import Network.AWS.Backup.Types
import Network.AWS.Backup.UntagResource
import Network.AWS.Backup.UpdateBackupPlan
import Network.AWS.Backup.UpdateRecoveryPointLifecycle
import Network.AWS.Backup.Waiters
import Network.AWS.Backup.Internal

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Backup'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
