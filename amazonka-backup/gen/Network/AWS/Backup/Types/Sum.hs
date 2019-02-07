{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Backup.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Backup.Types.Sum where

import Network.AWS.Backup.Internal
import Network.AWS.Prelude

data BackupJobState
  = Aborted
  | Aborting
  | Completed
  | Created
  | Expired
  | Failed
  | Pending
  | Running
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BackupJobState where
    parser = takeLowerText >>= \case
        "aborted" -> pure Aborted
        "aborting" -> pure Aborting
        "completed" -> pure Completed
        "created" -> pure Created
        "expired" -> pure Expired
        "failed" -> pure Failed
        "pending" -> pure Pending
        "running" -> pure Running
        e -> fromTextError $ "Failure parsing BackupJobState from value: '" <> e
           <> "'. Accepted values: aborted, aborting, completed, created, expired, failed, pending, running"

instance ToText BackupJobState where
    toText = \case
        Aborted -> "ABORTED"
        Aborting -> "ABORTING"
        Completed -> "COMPLETED"
        Created -> "CREATED"
        Expired -> "EXPIRED"
        Failed -> "FAILED"
        Pending -> "PENDING"
        Running -> "RUNNING"

instance Hashable     BackupJobState
instance NFData       BackupJobState
instance ToByteString BackupJobState
instance ToQuery      BackupJobState
instance ToHeader     BackupJobState

instance ToJSON BackupJobState where
    toJSON = toJSONText

instance FromJSON BackupJobState where
    parseJSON = parseJSONText "BackupJobState"

data BackupVaultEvent
  = BackupJobCompleted
  | BackupJobStarted
  | BackupPlanCreated
  | BackupPlanModified
  | RecoveryPointModified
  | RestoreJobCompleted
  | RestoreJobStarted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BackupVaultEvent where
    parser = takeLowerText >>= \case
        "backup_job_completed" -> pure BackupJobCompleted
        "backup_job_started" -> pure BackupJobStarted
        "backup_plan_created" -> pure BackupPlanCreated
        "backup_plan_modified" -> pure BackupPlanModified
        "recovery_point_modified" -> pure RecoveryPointModified
        "restore_job_completed" -> pure RestoreJobCompleted
        "restore_job_started" -> pure RestoreJobStarted
        e -> fromTextError $ "Failure parsing BackupVaultEvent from value: '" <> e
           <> "'. Accepted values: backup_job_completed, backup_job_started, backup_plan_created, backup_plan_modified, recovery_point_modified, restore_job_completed, restore_job_started"

instance ToText BackupVaultEvent where
    toText = \case
        BackupJobCompleted -> "BACKUP_JOB_COMPLETED"
        BackupJobStarted -> "BACKUP_JOB_STARTED"
        BackupPlanCreated -> "BACKUP_PLAN_CREATED"
        BackupPlanModified -> "BACKUP_PLAN_MODIFIED"
        RecoveryPointModified -> "RECOVERY_POINT_MODIFIED"
        RestoreJobCompleted -> "RESTORE_JOB_COMPLETED"
        RestoreJobStarted -> "RESTORE_JOB_STARTED"

instance Hashable     BackupVaultEvent
instance NFData       BackupVaultEvent
instance ToByteString BackupVaultEvent
instance ToQuery      BackupVaultEvent
instance ToHeader     BackupVaultEvent

instance ToJSON BackupVaultEvent where
    toJSON = toJSONText

instance FromJSON BackupVaultEvent where
    parseJSON = parseJSONText "BackupVaultEvent"

data ConditionType =
  Stringequals
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConditionType where
    parser = takeLowerText >>= \case
        "stringequals" -> pure Stringequals
        e -> fromTextError $ "Failure parsing ConditionType from value: '" <> e
           <> "'. Accepted values: stringequals"

instance ToText ConditionType where
    toText = \case
        Stringequals -> "STRINGEQUALS"

instance Hashable     ConditionType
instance NFData       ConditionType
instance ToByteString ConditionType
instance ToQuery      ConditionType
instance ToHeader     ConditionType

instance ToJSON ConditionType where
    toJSON = toJSONText

instance FromJSON ConditionType where
    parseJSON = parseJSONText "ConditionType"

data RecoveryPointStatus
  = RPSCompleted
  | RPSDeleting
  | RPSExpired
  | RPSPartial
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RecoveryPointStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure RPSCompleted
        "deleting" -> pure RPSDeleting
        "expired" -> pure RPSExpired
        "partial" -> pure RPSPartial
        e -> fromTextError $ "Failure parsing RecoveryPointStatus from value: '" <> e
           <> "'. Accepted values: completed, deleting, expired, partial"

instance ToText RecoveryPointStatus where
    toText = \case
        RPSCompleted -> "COMPLETED"
        RPSDeleting -> "DELETING"
        RPSExpired -> "EXPIRED"
        RPSPartial -> "PARTIAL"

instance Hashable     RecoveryPointStatus
instance NFData       RecoveryPointStatus
instance ToByteString RecoveryPointStatus
instance ToQuery      RecoveryPointStatus
instance ToHeader     RecoveryPointStatus

instance FromJSON RecoveryPointStatus where
    parseJSON = parseJSONText "RecoveryPointStatus"

data RestoreJobStatus
  = RJSAborted
  | RJSCompleted
  | RJSFailed
  | RJSPending
  | RJSRunning
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RestoreJobStatus where
    parser = takeLowerText >>= \case
        "aborted" -> pure RJSAborted
        "completed" -> pure RJSCompleted
        "failed" -> pure RJSFailed
        "pending" -> pure RJSPending
        "running" -> pure RJSRunning
        e -> fromTextError $ "Failure parsing RestoreJobStatus from value: '" <> e
           <> "'. Accepted values: aborted, completed, failed, pending, running"

instance ToText RestoreJobStatus where
    toText = \case
        RJSAborted -> "ABORTED"
        RJSCompleted -> "COMPLETED"
        RJSFailed -> "FAILED"
        RJSPending -> "PENDING"
        RJSRunning -> "RUNNING"

instance Hashable     RestoreJobStatus
instance NFData       RestoreJobStatus
instance ToByteString RestoreJobStatus
instance ToQuery      RestoreJobStatus
instance ToHeader     RestoreJobStatus

instance FromJSON RestoreJobStatus where
    parseJSON = parseJSONText "RestoreJobStatus"

data StorageClass
  = Cold
  | Deleted
  | Warm
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StorageClass where
    parser = takeLowerText >>= \case
        "cold" -> pure Cold
        "deleted" -> pure Deleted
        "warm" -> pure Warm
        e -> fromTextError $ "Failure parsing StorageClass from value: '" <> e
           <> "'. Accepted values: cold, deleted, warm"

instance ToText StorageClass where
    toText = \case
        Cold -> "COLD"
        Deleted -> "DELETED"
        Warm -> "WARM"

instance Hashable     StorageClass
instance NFData       StorageClass
instance ToByteString StorageClass
instance ToQuery      StorageClass
instance ToHeader     StorageClass

instance FromJSON StorageClass where
    parseJSON = parseJSONText "StorageClass"
