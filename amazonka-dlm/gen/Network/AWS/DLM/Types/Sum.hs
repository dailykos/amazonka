{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DLM.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DLM.Types.Sum where

import Network.AWS.DLM.Internal
import Network.AWS.Prelude

data GettablePolicyStateValues
  = Disabled
  | Enabled
  | Error'
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText GettablePolicyStateValues where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        "error" -> pure Error'
        e -> fromTextError $ "Failure parsing GettablePolicyStateValues from value: '" <> e
           <> "'. Accepted values: disabled, enabled, error"

instance ToText GettablePolicyStateValues where
    toText = \case
        Disabled -> "DISABLED"
        Enabled -> "ENABLED"
        Error' -> "ERROR"

instance Hashable     GettablePolicyStateValues
instance NFData       GettablePolicyStateValues
instance ToByteString GettablePolicyStateValues
instance ToQuery      GettablePolicyStateValues
instance ToHeader     GettablePolicyStateValues

instance ToJSON GettablePolicyStateValues where
    toJSON = toJSONText

instance FromJSON GettablePolicyStateValues where
    parseJSON = parseJSONText "GettablePolicyStateValues"

data IntervalUnitValues =
  Hours
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText IntervalUnitValues where
    parser = takeLowerText >>= \case
        "hours" -> pure Hours
        e -> fromTextError $ "Failure parsing IntervalUnitValues from value: '" <> e
           <> "'. Accepted values: hours"

instance ToText IntervalUnitValues where
    toText = \case
        Hours -> "HOURS"

instance Hashable     IntervalUnitValues
instance NFData       IntervalUnitValues
instance ToByteString IntervalUnitValues
instance ToQuery      IntervalUnitValues
instance ToHeader     IntervalUnitValues

instance ToJSON IntervalUnitValues where
    toJSON = toJSONText

instance FromJSON IntervalUnitValues where
    parseJSON = parseJSONText "IntervalUnitValues"

data ResourceTypeValues =
  Volume
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResourceTypeValues where
    parser = takeLowerText >>= \case
        "volume" -> pure Volume
        e -> fromTextError $ "Failure parsing ResourceTypeValues from value: '" <> e
           <> "'. Accepted values: volume"

instance ToText ResourceTypeValues where
    toText = \case
        Volume -> "VOLUME"

instance Hashable     ResourceTypeValues
instance NFData       ResourceTypeValues
instance ToByteString ResourceTypeValues
instance ToQuery      ResourceTypeValues
instance ToHeader     ResourceTypeValues

instance ToJSON ResourceTypeValues where
    toJSON = toJSONText

instance FromJSON ResourceTypeValues where
    parseJSON = parseJSONText "ResourceTypeValues"

data SettablePolicyStateValues
  = SPSVDisabled
  | SPSVEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SettablePolicyStateValues where
    parser = takeLowerText >>= \case
        "disabled" -> pure SPSVDisabled
        "enabled" -> pure SPSVEnabled
        e -> fromTextError $ "Failure parsing SettablePolicyStateValues from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText SettablePolicyStateValues where
    toText = \case
        SPSVDisabled -> "DISABLED"
        SPSVEnabled -> "ENABLED"

instance Hashable     SettablePolicyStateValues
instance NFData       SettablePolicyStateValues
instance ToByteString SettablePolicyStateValues
instance ToQuery      SettablePolicyStateValues
instance ToHeader     SettablePolicyStateValues

instance ToJSON SettablePolicyStateValues where
    toJSON = toJSONText
