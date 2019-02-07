{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ComprehendMedical.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ComprehendMedical.Types.Sum where

import Network.AWS.ComprehendMedical.Internal
import Network.AWS.Prelude

data AttributeName
  = Diagnosis
  | Negation
  | Sign
  | Symptom
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AttributeName where
    parser = takeLowerText >>= \case
        "diagnosis" -> pure Diagnosis
        "negation" -> pure Negation
        "sign" -> pure Sign
        "symptom" -> pure Symptom
        e -> fromTextError $ "Failure parsing AttributeName from value: '" <> e
           <> "'. Accepted values: diagnosis, negation, sign, symptom"

instance ToText AttributeName where
    toText = \case
        Diagnosis -> "DIAGNOSIS"
        Negation -> "NEGATION"
        Sign -> "SIGN"
        Symptom -> "SYMPTOM"

instance Hashable     AttributeName
instance NFData       AttributeName
instance ToByteString AttributeName
instance ToQuery      AttributeName
instance ToHeader     AttributeName

instance FromJSON AttributeName where
    parseJSON = parseJSONText "AttributeName"

data EntitySubType
  = Acuity
  | Address
  | Age
  | BrandName
  | ContactPoint
  | Date
  | Direction
  | Dosage
  | Duration
  | Email
  | Form
  | Frequency
  | GenericName
  | Identifier
  | Name
  | ProcedureName
  | Profession
  | Quality
  | Quantity
  | Rate
  | RouteOrMode
  | Strength
  | SystemOrganSite
  | TestName
  | TestUnits
  | TestValue
  | TreatmentName
  | URL
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EntitySubType where
    parser = takeLowerText >>= \case
        "acuity" -> pure Acuity
        "address" -> pure Address
        "age" -> pure Age
        "brand_name" -> pure BrandName
        "contact_point" -> pure ContactPoint
        "date" -> pure Date
        "direction" -> pure Direction
        "dosage" -> pure Dosage
        "duration" -> pure Duration
        "email" -> pure Email
        "form" -> pure Form
        "frequency" -> pure Frequency
        "generic_name" -> pure GenericName
        "identifier" -> pure Identifier
        "name" -> pure Name
        "procedure_name" -> pure ProcedureName
        "profession" -> pure Profession
        "quality" -> pure Quality
        "quantity" -> pure Quantity
        "rate" -> pure Rate
        "route_or_mode" -> pure RouteOrMode
        "strength" -> pure Strength
        "system_organ_site" -> pure SystemOrganSite
        "test_name" -> pure TestName
        "test_units" -> pure TestUnits
        "test_value" -> pure TestValue
        "treatment_name" -> pure TreatmentName
        "url" -> pure URL
        e -> fromTextError $ "Failure parsing EntitySubType from value: '" <> e
           <> "'. Accepted values: acuity, address, age, brand_name, contact_point, date, direction, dosage, duration, email, form, frequency, generic_name, identifier, name, procedure_name, profession, quality, quantity, rate, route_or_mode, strength, system_organ_site, test_name, test_units, test_value, treatment_name, url"

instance ToText EntitySubType where
    toText = \case
        Acuity -> "ACUITY"
        Address -> "ADDRESS"
        Age -> "AGE"
        BrandName -> "BRAND_NAME"
        ContactPoint -> "CONTACT_POINT"
        Date -> "DATE"
        Direction -> "DIRECTION"
        Dosage -> "DOSAGE"
        Duration -> "DURATION"
        Email -> "EMAIL"
        Form -> "FORM"
        Frequency -> "FREQUENCY"
        GenericName -> "GENERIC_NAME"
        Identifier -> "IDENTIFIER"
        Name -> "NAME"
        ProcedureName -> "PROCEDURE_NAME"
        Profession -> "PROFESSION"
        Quality -> "QUALITY"
        Quantity -> "QUANTITY"
        Rate -> "RATE"
        RouteOrMode -> "ROUTE_OR_MODE"
        Strength -> "STRENGTH"
        SystemOrganSite -> "SYSTEM_ORGAN_SITE"
        TestName -> "TEST_NAME"
        TestUnits -> "TEST_UNITS"
        TestValue -> "TEST_VALUE"
        TreatmentName -> "TREATMENT_NAME"
        URL -> "URL"

instance Hashable     EntitySubType
instance NFData       EntitySubType
instance ToByteString EntitySubType
instance ToQuery      EntitySubType
instance ToHeader     EntitySubType

instance FromJSON EntitySubType where
    parseJSON = parseJSONText "EntitySubType"

data EntityType
  = Anatomy
  | MedicalCondition
  | Medication
  | ProtectedHealthInformation
  | TestTreatmentProcedure
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EntityType where
    parser = takeLowerText >>= \case
        "anatomy" -> pure Anatomy
        "medical_condition" -> pure MedicalCondition
        "medication" -> pure Medication
        "protected_health_information" -> pure ProtectedHealthInformation
        "test_treatment_procedure" -> pure TestTreatmentProcedure
        e -> fromTextError $ "Failure parsing EntityType from value: '" <> e
           <> "'. Accepted values: anatomy, medical_condition, medication, protected_health_information, test_treatment_procedure"

instance ToText EntityType where
    toText = \case
        Anatomy -> "ANATOMY"
        MedicalCondition -> "MEDICAL_CONDITION"
        Medication -> "MEDICATION"
        ProtectedHealthInformation -> "PROTECTED_HEALTH_INFORMATION"
        TestTreatmentProcedure -> "TEST_TREATMENT_PROCEDURE"

instance Hashable     EntityType
instance NFData       EntityType
instance ToByteString EntityType
instance ToQuery      EntityType
instance ToHeader     EntityType

instance FromJSON EntityType where
    parseJSON = parseJSONText "EntityType"
