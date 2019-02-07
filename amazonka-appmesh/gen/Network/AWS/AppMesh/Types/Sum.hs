{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppMesh.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppMesh.Types.Sum where

import Network.AWS.AppMesh.Internal
import Network.AWS.Prelude

data MeshStatusCode
  = MSCActive
  | MSCDeleted
  | MSCInactive
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MeshStatusCode where
    parser = takeLowerText >>= \case
        "active" -> pure MSCActive
        "deleted" -> pure MSCDeleted
        "inactive" -> pure MSCInactive
        e -> fromTextError $ "Failure parsing MeshStatusCode from value: '" <> e
           <> "'. Accepted values: active, deleted, inactive"

instance ToText MeshStatusCode where
    toText = \case
        MSCActive -> "ACTIVE"
        MSCDeleted -> "DELETED"
        MSCInactive -> "INACTIVE"

instance Hashable     MeshStatusCode
instance NFData       MeshStatusCode
instance ToByteString MeshStatusCode
instance ToQuery      MeshStatusCode
instance ToHeader     MeshStatusCode

instance FromJSON MeshStatusCode where
    parseJSON = parseJSONText "MeshStatusCode"

data PortProtocol
  = HTTP
  | TCP
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PortProtocol where
    parser = takeLowerText >>= \case
        "http" -> pure HTTP
        "tcp" -> pure TCP
        e -> fromTextError $ "Failure parsing PortProtocol from value: '" <> e
           <> "'. Accepted values: http, tcp"

instance ToText PortProtocol where
    toText = \case
        HTTP -> "http"
        TCP -> "tcp"

instance Hashable     PortProtocol
instance NFData       PortProtocol
instance ToByteString PortProtocol
instance ToQuery      PortProtocol
instance ToHeader     PortProtocol

instance ToJSON PortProtocol where
    toJSON = toJSONText

instance FromJSON PortProtocol where
    parseJSON = parseJSONText "PortProtocol"

data RouteStatusCode
  = RSCActive
  | RSCDeleted
  | RSCInactive
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RouteStatusCode where
    parser = takeLowerText >>= \case
        "active" -> pure RSCActive
        "deleted" -> pure RSCDeleted
        "inactive" -> pure RSCInactive
        e -> fromTextError $ "Failure parsing RouteStatusCode from value: '" <> e
           <> "'. Accepted values: active, deleted, inactive"

instance ToText RouteStatusCode where
    toText = \case
        RSCActive -> "ACTIVE"
        RSCDeleted -> "DELETED"
        RSCInactive -> "INACTIVE"

instance Hashable     RouteStatusCode
instance NFData       RouteStatusCode
instance ToByteString RouteStatusCode
instance ToQuery      RouteStatusCode
instance ToHeader     RouteStatusCode

instance FromJSON RouteStatusCode where
    parseJSON = parseJSONText "RouteStatusCode"

data VirtualNodeStatusCode
  = VNSCActive
  | VNSCDeleted
  | VNSCInactive
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VirtualNodeStatusCode where
    parser = takeLowerText >>= \case
        "active" -> pure VNSCActive
        "deleted" -> pure VNSCDeleted
        "inactive" -> pure VNSCInactive
        e -> fromTextError $ "Failure parsing VirtualNodeStatusCode from value: '" <> e
           <> "'. Accepted values: active, deleted, inactive"

instance ToText VirtualNodeStatusCode where
    toText = \case
        VNSCActive -> "ACTIVE"
        VNSCDeleted -> "DELETED"
        VNSCInactive -> "INACTIVE"

instance Hashable     VirtualNodeStatusCode
instance NFData       VirtualNodeStatusCode
instance ToByteString VirtualNodeStatusCode
instance ToQuery      VirtualNodeStatusCode
instance ToHeader     VirtualNodeStatusCode

instance FromJSON VirtualNodeStatusCode where
    parseJSON = parseJSONText "VirtualNodeStatusCode"

data VirtualRouterStatusCode
  = Active
  | Deleted
  | Inactive
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VirtualRouterStatusCode where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "deleted" -> pure Deleted
        "inactive" -> pure Inactive
        e -> fromTextError $ "Failure parsing VirtualRouterStatusCode from value: '" <> e
           <> "'. Accepted values: active, deleted, inactive"

instance ToText VirtualRouterStatusCode where
    toText = \case
        Active -> "ACTIVE"
        Deleted -> "DELETED"
        Inactive -> "INACTIVE"

instance Hashable     VirtualRouterStatusCode
instance NFData       VirtualRouterStatusCode
instance ToByteString VirtualRouterStatusCode
instance ToQuery      VirtualRouterStatusCode
instance ToHeader     VirtualRouterStatusCode

instance FromJSON VirtualRouterStatusCode where
    parseJSON = parseJSONText "VirtualRouterStatusCode"
