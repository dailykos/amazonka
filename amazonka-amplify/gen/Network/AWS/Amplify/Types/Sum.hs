{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Amplify.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Amplify.Types.Sum where

import Network.AWS.Amplify.Internal
import Network.AWS.Prelude

data DomainStatus
  = Available
  | Failed
  | InProgress
  | PendingDeployment
  | PendingVerification
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DomainStatus where
    parser = takeLowerText >>= \case
        "available" -> pure Available
        "failed" -> pure Failed
        "in_progress" -> pure InProgress
        "pending_deployment" -> pure PendingDeployment
        "pending_verification" -> pure PendingVerification
        e -> fromTextError $ "Failure parsing DomainStatus from value: '" <> e
           <> "'. Accepted values: available, failed, in_progress, pending_deployment, pending_verification"

instance ToText DomainStatus where
    toText = \case
        Available -> "AVAILABLE"
        Failed -> "FAILED"
        InProgress -> "IN_PROGRESS"
        PendingDeployment -> "PENDING_DEPLOYMENT"
        PendingVerification -> "PENDING_VERIFICATION"

instance Hashable     DomainStatus
instance NFData       DomainStatus
instance ToByteString DomainStatus
instance ToQuery      DomainStatus
instance ToHeader     DomainStatus

instance FromJSON DomainStatus where
    parseJSON = parseJSONText "DomainStatus"

data JobStatus
  = JSCancelled
  | JSCancelling
  | JSFailed
  | JSPending
  | JSProvisioning
  | JSRunning
  | JSSucceed
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText JobStatus where
    parser = takeLowerText >>= \case
        "cancelled" -> pure JSCancelled
        "cancelling" -> pure JSCancelling
        "failed" -> pure JSFailed
        "pending" -> pure JSPending
        "provisioning" -> pure JSProvisioning
        "running" -> pure JSRunning
        "succeed" -> pure JSSucceed
        e -> fromTextError $ "Failure parsing JobStatus from value: '" <> e
           <> "'. Accepted values: cancelled, cancelling, failed, pending, provisioning, running, succeed"

instance ToText JobStatus where
    toText = \case
        JSCancelled -> "CANCELLED"
        JSCancelling -> "CANCELLING"
        JSFailed -> "FAILED"
        JSPending -> "PENDING"
        JSProvisioning -> "PROVISIONING"
        JSRunning -> "RUNNING"
        JSSucceed -> "SUCCEED"

instance Hashable     JobStatus
instance NFData       JobStatus
instance ToByteString JobStatus
instance ToQuery      JobStatus
instance ToHeader     JobStatus

instance FromJSON JobStatus where
    parseJSON = parseJSONText "JobStatus"

data JobType
  = Release
  | Retry
  | WebHook
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText JobType where
    parser = takeLowerText >>= \case
        "release" -> pure Release
        "retry" -> pure Retry
        "web_hook" -> pure WebHook
        e -> fromTextError $ "Failure parsing JobType from value: '" <> e
           <> "'. Accepted values: release, retry, web_hook"

instance ToText JobType where
    toText = \case
        Release -> "RELEASE"
        Retry -> "RETRY"
        WebHook -> "WEB_HOOK"

instance Hashable     JobType
instance NFData       JobType
instance ToByteString JobType
instance ToQuery      JobType
instance ToHeader     JobType

instance ToJSON JobType where
    toJSON = toJSONText

instance FromJSON JobType where
    parseJSON = parseJSONText "JobType"

data Platform
  = Android
  | Ios
  | ReactNative
  | Web
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Platform where
    parser = takeLowerText >>= \case
        "android" -> pure Android
        "ios" -> pure Ios
        "react_native" -> pure ReactNative
        "web" -> pure Web
        e -> fromTextError $ "Failure parsing Platform from value: '" <> e
           <> "'. Accepted values: android, ios, react_native, web"

instance ToText Platform where
    toText = \case
        Android -> "ANDROID"
        Ios -> "IOS"
        ReactNative -> "REACT_NATIVE"
        Web -> "WEB"

instance Hashable     Platform
instance NFData       Platform
instance ToByteString Platform
instance ToQuery      Platform
instance ToHeader     Platform

instance ToJSON Platform where
    toJSON = toJSONText

instance FromJSON Platform where
    parseJSON = parseJSONText "Platform"

data Stage
  = Beta
  | Development
  | Experimental
  | Production
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Stage where
    parser = takeLowerText >>= \case
        "beta" -> pure Beta
        "development" -> pure Development
        "experimental" -> pure Experimental
        "production" -> pure Production
        e -> fromTextError $ "Failure parsing Stage from value: '" <> e
           <> "'. Accepted values: beta, development, experimental, production"

instance ToText Stage where
    toText = \case
        Beta -> "BETA"
        Development -> "DEVELOPMENT"
        Experimental -> "EXPERIMENTAL"
        Production -> "PRODUCTION"

instance Hashable     Stage
instance NFData       Stage
instance ToByteString Stage
instance ToQuery      Stage
instance ToHeader     Stage

instance ToJSON Stage where
    toJSON = toJSONText

instance FromJSON Stage where
    parseJSON = parseJSONText "Stage"
