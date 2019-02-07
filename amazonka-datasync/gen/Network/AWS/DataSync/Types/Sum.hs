{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataSync.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DataSync.Types.Sum where

import Network.AWS.DataSync.Internal
import Network.AWS.Prelude

data AgentStatus
  = Offline
  | Online
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AgentStatus where
    parser = takeLowerText >>= \case
        "offline" -> pure Offline
        "online" -> pure Online
        e -> fromTextError $ "Failure parsing AgentStatus from value: '" <> e
           <> "'. Accepted values: offline, online"

instance ToText AgentStatus where
    toText = \case
        Offline -> "OFFLINE"
        Online -> "ONLINE"

instance Hashable     AgentStatus
instance NFData       AgentStatus
instance ToByteString AgentStatus
instance ToQuery      AgentStatus
instance ToHeader     AgentStatus

instance FromJSON AgentStatus where
    parseJSON = parseJSONText "AgentStatus"

data Atime
  = BestEffort
  | None
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Atime where
    parser = takeLowerText >>= \case
        "best_effort" -> pure BestEffort
        "none" -> pure None
        e -> fromTextError $ "Failure parsing Atime from value: '" <> e
           <> "'. Accepted values: best_effort, none"

instance ToText Atime where
    toText = \case
        BestEffort -> "BEST_EFFORT"
        None -> "NONE"

instance Hashable     Atime
instance NFData       Atime
instance ToByteString Atime
instance ToQuery      Atime
instance ToHeader     Atime

instance ToJSON Atime where
    toJSON = toJSONText

instance FromJSON Atime where
    parseJSON = parseJSONText "Atime"

data Gid
  = GidBoth
  | GidIntValue
  | GidName
  | GidNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Gid where
    parser = takeLowerText >>= \case
        "both" -> pure GidBoth
        "int_value" -> pure GidIntValue
        "name" -> pure GidName
        "none" -> pure GidNone
        e -> fromTextError $ "Failure parsing Gid from value: '" <> e
           <> "'. Accepted values: both, int_value, name, none"

instance ToText Gid where
    toText = \case
        GidBoth -> "BOTH"
        GidIntValue -> "INT_VALUE"
        GidName -> "NAME"
        GidNone -> "NONE"

instance Hashable     Gid
instance NFData       Gid
instance ToByteString Gid
instance ToQuery      Gid
instance ToHeader     Gid

instance ToJSON Gid where
    toJSON = toJSONText

instance FromJSON Gid where
    parseJSON = parseJSONText "Gid"

data Mtime
  = MNone
  | MPreserve
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Mtime where
    parser = takeLowerText >>= \case
        "none" -> pure MNone
        "preserve" -> pure MPreserve
        e -> fromTextError $ "Failure parsing Mtime from value: '" <> e
           <> "'. Accepted values: none, preserve"

instance ToText Mtime where
    toText = \case
        MNone -> "NONE"
        MPreserve -> "PRESERVE"

instance Hashable     Mtime
instance NFData       Mtime
instance ToByteString Mtime
instance ToQuery      Mtime
instance ToHeader     Mtime

instance ToJSON Mtime where
    toJSON = toJSONText

instance FromJSON Mtime where
    parseJSON = parseJSONText "Mtime"

data PhaseStatus
  = Error'
  | Pending
  | Success
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PhaseStatus where
    parser = takeLowerText >>= \case
        "error" -> pure Error'
        "pending" -> pure Pending
        "success" -> pure Success
        e -> fromTextError $ "Failure parsing PhaseStatus from value: '" <> e
           <> "'. Accepted values: error, pending, success"

instance ToText PhaseStatus where
    toText = \case
        Error' -> "ERROR"
        Pending -> "PENDING"
        Success -> "SUCCESS"

instance Hashable     PhaseStatus
instance NFData       PhaseStatus
instance ToByteString PhaseStatus
instance ToQuery      PhaseStatus
instance ToHeader     PhaseStatus

instance FromJSON PhaseStatus where
    parseJSON = parseJSONText "PhaseStatus"

data PosixPermissions
  = PPBestEffort
  | PPNone
  | PPPreserve
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PosixPermissions where
    parser = takeLowerText >>= \case
        "best_effort" -> pure PPBestEffort
        "none" -> pure PPNone
        "preserve" -> pure PPPreserve
        e -> fromTextError $ "Failure parsing PosixPermissions from value: '" <> e
           <> "'. Accepted values: best_effort, none, preserve"

instance ToText PosixPermissions where
    toText = \case
        PPBestEffort -> "BEST_EFFORT"
        PPNone -> "NONE"
        PPPreserve -> "PRESERVE"

instance Hashable     PosixPermissions
instance NFData       PosixPermissions
instance ToByteString PosixPermissions
instance ToQuery      PosixPermissions
instance ToHeader     PosixPermissions

instance ToJSON PosixPermissions where
    toJSON = toJSONText

instance FromJSON PosixPermissions where
    parseJSON = parseJSONText "PosixPermissions"

data PreserveDeletedFiles
  = Preserve
  | Remove
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PreserveDeletedFiles where
    parser = takeLowerText >>= \case
        "preserve" -> pure Preserve
        "remove" -> pure Remove
        e -> fromTextError $ "Failure parsing PreserveDeletedFiles from value: '" <> e
           <> "'. Accepted values: preserve, remove"

instance ToText PreserveDeletedFiles where
    toText = \case
        Preserve -> "PRESERVE"
        Remove -> "REMOVE"

instance Hashable     PreserveDeletedFiles
instance NFData       PreserveDeletedFiles
instance ToByteString PreserveDeletedFiles
instance ToQuery      PreserveDeletedFiles
instance ToHeader     PreserveDeletedFiles

instance ToJSON PreserveDeletedFiles where
    toJSON = toJSONText

instance FromJSON PreserveDeletedFiles where
    parseJSON = parseJSONText "PreserveDeletedFiles"

data PreserveDevices
  = PDNone
  | PDPreserve
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PreserveDevices where
    parser = takeLowerText >>= \case
        "none" -> pure PDNone
        "preserve" -> pure PDPreserve
        e -> fromTextError $ "Failure parsing PreserveDevices from value: '" <> e
           <> "'. Accepted values: none, preserve"

instance ToText PreserveDevices where
    toText = \case
        PDNone -> "NONE"
        PDPreserve -> "PRESERVE"

instance Hashable     PreserveDevices
instance NFData       PreserveDevices
instance ToByteString PreserveDevices
instance ToQuery      PreserveDevices
instance ToHeader     PreserveDevices

instance ToJSON PreserveDevices where
    toJSON = toJSONText

instance FromJSON PreserveDevices where
    parseJSON = parseJSONText "PreserveDevices"

data TaskExecutionStatus
  = TESError'
  | TESLaunching
  | TESPreparing
  | TESSuccess
  | TESTransferring
  | TESVerifying
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TaskExecutionStatus where
    parser = takeLowerText >>= \case
        "error" -> pure TESError'
        "launching" -> pure TESLaunching
        "preparing" -> pure TESPreparing
        "success" -> pure TESSuccess
        "transferring" -> pure TESTransferring
        "verifying" -> pure TESVerifying
        e -> fromTextError $ "Failure parsing TaskExecutionStatus from value: '" <> e
           <> "'. Accepted values: error, launching, preparing, success, transferring, verifying"

instance ToText TaskExecutionStatus where
    toText = \case
        TESError' -> "ERROR"
        TESLaunching -> "LAUNCHING"
        TESPreparing -> "PREPARING"
        TESSuccess -> "SUCCESS"
        TESTransferring -> "TRANSFERRING"
        TESVerifying -> "VERIFYING"

instance Hashable     TaskExecutionStatus
instance NFData       TaskExecutionStatus
instance ToByteString TaskExecutionStatus
instance ToQuery      TaskExecutionStatus
instance ToHeader     TaskExecutionStatus

instance FromJSON TaskExecutionStatus where
    parseJSON = parseJSONText "TaskExecutionStatus"

data TaskStatus
  = Available
  | Creating
  | Running
  | Unavailable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TaskStatus where
    parser = takeLowerText >>= \case
        "available" -> pure Available
        "creating" -> pure Creating
        "running" -> pure Running
        "unavailable" -> pure Unavailable
        e -> fromTextError $ "Failure parsing TaskStatus from value: '" <> e
           <> "'. Accepted values: available, creating, running, unavailable"

instance ToText TaskStatus where
    toText = \case
        Available -> "AVAILABLE"
        Creating -> "CREATING"
        Running -> "RUNNING"
        Unavailable -> "UNAVAILABLE"

instance Hashable     TaskStatus
instance NFData       TaskStatus
instance ToByteString TaskStatus
instance ToQuery      TaskStatus
instance ToHeader     TaskStatus

instance FromJSON TaskStatus where
    parseJSON = parseJSONText "TaskStatus"

data Uid
  = UidBoth
  | UidIntValue
  | UidName
  | UidNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Uid where
    parser = takeLowerText >>= \case
        "both" -> pure UidBoth
        "int_value" -> pure UidIntValue
        "name" -> pure UidName
        "none" -> pure UidNone
        e -> fromTextError $ "Failure parsing Uid from value: '" <> e
           <> "'. Accepted values: both, int_value, name, none"

instance ToText Uid where
    toText = \case
        UidBoth -> "BOTH"
        UidIntValue -> "INT_VALUE"
        UidName -> "NAME"
        UidNone -> "NONE"

instance Hashable     Uid
instance NFData       Uid
instance ToByteString Uid
instance ToQuery      Uid
instance ToHeader     Uid

instance ToJSON Uid where
    toJSON = toJSONText

instance FromJSON Uid where
    parseJSON = parseJSONText "Uid"

data VerifyMode
  = VMNone
  | VMPointInTimeConsistent
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VerifyMode where
    parser = takeLowerText >>= \case
        "none" -> pure VMNone
        "point_in_time_consistent" -> pure VMPointInTimeConsistent
        e -> fromTextError $ "Failure parsing VerifyMode from value: '" <> e
           <> "'. Accepted values: none, point_in_time_consistent"

instance ToText VerifyMode where
    toText = \case
        VMNone -> "NONE"
        VMPointInTimeConsistent -> "POINT_IN_TIME_CONSISTENT"

instance Hashable     VerifyMode
instance NFData       VerifyMode
instance ToByteString VerifyMode
instance ToQuery      VerifyMode
instance ToHeader     VerifyMode

instance ToJSON VerifyMode where
    toJSON = toJSONText

instance FromJSON VerifyMode where
    parseJSON = parseJSONText "VerifyMode"
