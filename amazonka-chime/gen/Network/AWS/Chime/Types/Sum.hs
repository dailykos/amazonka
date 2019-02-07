{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Chime.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Chime.Types.Sum where

import Network.AWS.Chime.Internal
import Network.AWS.Prelude

data AccountType
  = EnterpriseDirectory
  | EnterpriseLWA
  | EnterpriseOIdC
  | Team
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AccountType where
    parser = takeLowerText >>= \case
        "enterprisedirectory" -> pure EnterpriseDirectory
        "enterpriselwa" -> pure EnterpriseLWA
        "enterpriseoidc" -> pure EnterpriseOIdC
        "team" -> pure Team
        e -> fromTextError $ "Failure parsing AccountType from value: '" <> e
           <> "'. Accepted values: enterprisedirectory, enterpriselwa, enterpriseoidc, team"

instance ToText AccountType where
    toText = \case
        EnterpriseDirectory -> "EnterpriseDirectory"
        EnterpriseLWA -> "EnterpriseLWA"
        EnterpriseOIdC -> "EnterpriseOIDC"
        Team -> "Team"

instance Hashable     AccountType
instance NFData       AccountType
instance ToByteString AccountType
instance ToQuery      AccountType
instance ToHeader     AccountType

instance FromJSON AccountType where
    parseJSON = parseJSONText "AccountType"

data EmailStatus
  = ESFailed
  | ESNotSent
  | ESSent
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EmailStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure ESFailed
        "notsent" -> pure ESNotSent
        "sent" -> pure ESSent
        e -> fromTextError $ "Failure parsing EmailStatus from value: '" <> e
           <> "'. Accepted values: failed, notsent, sent"

instance ToText EmailStatus where
    toText = \case
        ESFailed -> "Failed"
        ESNotSent -> "NotSent"
        ESSent -> "Sent"

instance Hashable     EmailStatus
instance NFData       EmailStatus
instance ToByteString EmailStatus
instance ToQuery      EmailStatus
instance ToHeader     EmailStatus

instance FromJSON EmailStatus where
    parseJSON = parseJSONText "EmailStatus"

data ErrorCode
  = BadRequest
  | Conflict
  | Forbidden
  | NotFound
  | PreconditionFailed
  | ServiceFailure
  | ServiceUnavailable
  | Throttled
  | Unauthorized
  | Unprocessable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ErrorCode where
    parser = takeLowerText >>= \case
        "badrequest" -> pure BadRequest
        "conflict" -> pure Conflict
        "forbidden" -> pure Forbidden
        "notfound" -> pure NotFound
        "preconditionfailed" -> pure PreconditionFailed
        "servicefailure" -> pure ServiceFailure
        "serviceunavailable" -> pure ServiceUnavailable
        "throttled" -> pure Throttled
        "unauthorized" -> pure Unauthorized
        "unprocessable" -> pure Unprocessable
        e -> fromTextError $ "Failure parsing ErrorCode from value: '" <> e
           <> "'. Accepted values: badrequest, conflict, forbidden, notfound, preconditionfailed, servicefailure, serviceunavailable, throttled, unauthorized, unprocessable"

instance ToText ErrorCode where
    toText = \case
        BadRequest -> "BadRequest"
        Conflict -> "Conflict"
        Forbidden -> "Forbidden"
        NotFound -> "NotFound"
        PreconditionFailed -> "PreconditionFailed"
        ServiceFailure -> "ServiceFailure"
        ServiceUnavailable -> "ServiceUnavailable"
        Throttled -> "Throttled"
        Unauthorized -> "Unauthorized"
        Unprocessable -> "Unprocessable"

instance Hashable     ErrorCode
instance NFData       ErrorCode
instance ToByteString ErrorCode
instance ToQuery      ErrorCode
instance ToHeader     ErrorCode

instance FromJSON ErrorCode where
    parseJSON = parseJSONText "ErrorCode"

data InviteStatus
  = Accepted
  | Failed
  | Pending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InviteStatus where
    parser = takeLowerText >>= \case
        "accepted" -> pure Accepted
        "failed" -> pure Failed
        "pending" -> pure Pending
        e -> fromTextError $ "Failure parsing InviteStatus from value: '" <> e
           <> "'. Accepted values: accepted, failed, pending"

instance ToText InviteStatus where
    toText = \case
        Accepted -> "Accepted"
        Failed -> "Failed"
        Pending -> "Pending"

instance Hashable     InviteStatus
instance NFData       InviteStatus
instance ToByteString InviteStatus
instance ToQuery      InviteStatus
instance ToHeader     InviteStatus

instance FromJSON InviteStatus where
    parseJSON = parseJSONText "InviteStatus"

data License
  = Basic
  | Plus
  | Pro
  | ProTrial
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText License where
    parser = takeLowerText >>= \case
        "basic" -> pure Basic
        "plus" -> pure Plus
        "pro" -> pure Pro
        "protrial" -> pure ProTrial
        e -> fromTextError $ "Failure parsing License from value: '" <> e
           <> "'. Accepted values: basic, plus, pro, protrial"

instance ToText License where
    toText = \case
        Basic -> "Basic"
        Plus -> "Plus"
        Pro -> "Pro"
        ProTrial -> "ProTrial"

instance Hashable     License
instance NFData       License
instance ToByteString License
instance ToQuery      License
instance ToHeader     License

instance ToJSON License where
    toJSON = toJSONText

instance FromJSON License where
    parseJSON = parseJSONText "License"

data RegistrationStatus
  = Registered
  | Suspended
  | Unregistered
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RegistrationStatus where
    parser = takeLowerText >>= \case
        "registered" -> pure Registered
        "suspended" -> pure Suspended
        "unregistered" -> pure Unregistered
        e -> fromTextError $ "Failure parsing RegistrationStatus from value: '" <> e
           <> "'. Accepted values: registered, suspended, unregistered"

instance ToText RegistrationStatus where
    toText = \case
        Registered -> "Registered"
        Suspended -> "Suspended"
        Unregistered -> "Unregistered"

instance Hashable     RegistrationStatus
instance NFData       RegistrationStatus
instance ToByteString RegistrationStatus
instance ToQuery      RegistrationStatus
instance ToHeader     RegistrationStatus

instance FromJSON RegistrationStatus where
    parseJSON = parseJSONText "RegistrationStatus"
