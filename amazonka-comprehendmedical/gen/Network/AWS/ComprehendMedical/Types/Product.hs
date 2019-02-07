{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ComprehendMedical.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ComprehendMedical.Types.Product where

import Network.AWS.ComprehendMedical.Internal
import Network.AWS.ComprehendMedical.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An extracted segment of the text that is an attribute of an entity, or otherwise related to an entity, such as the dosage of a medication taken. It contains information about the attribute such as id, begin and end offset within the input text, and the segment of the input text. 
--
--
--
-- /See:/ 'attribute' smart constructor.
data Attribute = Attribute'
  { _aRelationshipScore :: !(Maybe Double)
  , _aBeginOffset :: !(Maybe Int)
  , _aText :: !(Maybe Text)
  , _aScore :: !(Maybe Double)
  , _aTraits :: !(Maybe [Trait])
  , _aEndOffset :: !(Maybe Int)
  , _aId :: !(Maybe Int)
  , _aType :: !(Maybe EntitySubType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Attribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aRelationshipScore' - The level of confidence that Comprehend Medical has that this attribute is correctly related to this entity. 
--
-- * 'aBeginOffset' - The 0-based character offset in the input text that shows where the attribute begins. The offset returns the UTF-8 code point in the string. 
--
-- * 'aText' - The segment of input text extracted as this attribute.
--
-- * 'aScore' - The level of confidence that Comprehend Medical has that the segment of text is correctly recognized as an attribute. 
--
-- * 'aTraits' - Contextual information for this attribute. 
--
-- * 'aEndOffset' - The 0-based character offset in the input text that shows where the attribute ends. The offset returns the UTF-8 code point in the string. 
--
-- * 'aId' - The numeric identifier for this attribute. This is a monotonically increasing id unique within this response rather than a global unique identifier. 
--
-- * 'aType' - The type of attribute. 
attribute
    :: Attribute
attribute =
  Attribute'
    { _aRelationshipScore = Nothing
    , _aBeginOffset = Nothing
    , _aText = Nothing
    , _aScore = Nothing
    , _aTraits = Nothing
    , _aEndOffset = Nothing
    , _aId = Nothing
    , _aType = Nothing
    }


-- | The level of confidence that Comprehend Medical has that this attribute is correctly related to this entity. 
aRelationshipScore :: Lens' Attribute (Maybe Double)
aRelationshipScore = lens _aRelationshipScore (\ s a -> s{_aRelationshipScore = a})

-- | The 0-based character offset in the input text that shows where the attribute begins. The offset returns the UTF-8 code point in the string. 
aBeginOffset :: Lens' Attribute (Maybe Int)
aBeginOffset = lens _aBeginOffset (\ s a -> s{_aBeginOffset = a})

-- | The segment of input text extracted as this attribute.
aText :: Lens' Attribute (Maybe Text)
aText = lens _aText (\ s a -> s{_aText = a})

-- | The level of confidence that Comprehend Medical has that the segment of text is correctly recognized as an attribute. 
aScore :: Lens' Attribute (Maybe Double)
aScore = lens _aScore (\ s a -> s{_aScore = a})

-- | Contextual information for this attribute. 
aTraits :: Lens' Attribute [Trait]
aTraits = lens _aTraits (\ s a -> s{_aTraits = a}) . _Default . _Coerce

-- | The 0-based character offset in the input text that shows where the attribute ends. The offset returns the UTF-8 code point in the string. 
aEndOffset :: Lens' Attribute (Maybe Int)
aEndOffset = lens _aEndOffset (\ s a -> s{_aEndOffset = a})

-- | The numeric identifier for this attribute. This is a monotonically increasing id unique within this response rather than a global unique identifier. 
aId :: Lens' Attribute (Maybe Int)
aId = lens _aId (\ s a -> s{_aId = a})

-- | The type of attribute. 
aType :: Lens' Attribute (Maybe EntitySubType)
aType = lens _aType (\ s a -> s{_aType = a})

instance FromJSON Attribute where
        parseJSON
          = withObject "Attribute"
              (\ x ->
                 Attribute' <$>
                   (x .:? "RelationshipScore") <*> (x .:? "BeginOffset")
                     <*> (x .:? "Text")
                     <*> (x .:? "Score")
                     <*> (x .:? "Traits" .!= mempty)
                     <*> (x .:? "EndOffset")
                     <*> (x .:? "Id")
                     <*> (x .:? "Type"))

instance Hashable Attribute where

instance NFData Attribute where

-- | Provides information about an extracted medical entity.
--
--
--
-- /See:/ 'entity' smart constructor.
data Entity = Entity'
  { _eBeginOffset :: !(Maybe Int)
  , _eText :: !(Maybe Text)
  , _eCategory :: !(Maybe EntityType)
  , _eScore :: !(Maybe Double)
  , _eTraits :: !(Maybe [Trait])
  , _eAttributes :: !(Maybe [Attribute])
  , _eEndOffset :: !(Maybe Int)
  , _eId :: !(Maybe Int)
  , _eType :: !(Maybe EntitySubType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Entity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eBeginOffset' - The 0-based character offset in the input text that shows where the entity begins. The offset returns the UTF-8 code point in the string. 
--
-- * 'eText' - The segment of input text extracted as this entity.
--
-- * 'eCategory' - The category of the entity.
--
-- * 'eScore' - The level of confidence that Comprehend Medical has in the accuracy of the detection.
--
-- * 'eTraits' - Contextual information for the entity
--
-- * 'eAttributes' - The extracted attributes that relate to this entity.
--
-- * 'eEndOffset' - The 0-based character offset in the input text that shows where the entity ends. The offset returns the UTF-8 code point in the string. 
--
-- * 'eId' - The numeric identifier for the entity. This is a monotonically increasing id unique within this response rather than a global unique identifier. 
--
-- * 'eType' - Describes the specific type of entity with category of entities. 
entity
    :: Entity
entity =
  Entity'
    { _eBeginOffset = Nothing
    , _eText = Nothing
    , _eCategory = Nothing
    , _eScore = Nothing
    , _eTraits = Nothing
    , _eAttributes = Nothing
    , _eEndOffset = Nothing
    , _eId = Nothing
    , _eType = Nothing
    }


-- | The 0-based character offset in the input text that shows where the entity begins. The offset returns the UTF-8 code point in the string. 
eBeginOffset :: Lens' Entity (Maybe Int)
eBeginOffset = lens _eBeginOffset (\ s a -> s{_eBeginOffset = a})

-- | The segment of input text extracted as this entity.
eText :: Lens' Entity (Maybe Text)
eText = lens _eText (\ s a -> s{_eText = a})

-- | The category of the entity.
eCategory :: Lens' Entity (Maybe EntityType)
eCategory = lens _eCategory (\ s a -> s{_eCategory = a})

-- | The level of confidence that Comprehend Medical has in the accuracy of the detection.
eScore :: Lens' Entity (Maybe Double)
eScore = lens _eScore (\ s a -> s{_eScore = a})

-- | Contextual information for the entity
eTraits :: Lens' Entity [Trait]
eTraits = lens _eTraits (\ s a -> s{_eTraits = a}) . _Default . _Coerce

-- | The extracted attributes that relate to this entity.
eAttributes :: Lens' Entity [Attribute]
eAttributes = lens _eAttributes (\ s a -> s{_eAttributes = a}) . _Default . _Coerce

-- | The 0-based character offset in the input text that shows where the entity ends. The offset returns the UTF-8 code point in the string. 
eEndOffset :: Lens' Entity (Maybe Int)
eEndOffset = lens _eEndOffset (\ s a -> s{_eEndOffset = a})

-- | The numeric identifier for the entity. This is a monotonically increasing id unique within this response rather than a global unique identifier. 
eId :: Lens' Entity (Maybe Int)
eId = lens _eId (\ s a -> s{_eId = a})

-- | Describes the specific type of entity with category of entities. 
eType :: Lens' Entity (Maybe EntitySubType)
eType = lens _eType (\ s a -> s{_eType = a})

instance FromJSON Entity where
        parseJSON
          = withObject "Entity"
              (\ x ->
                 Entity' <$>
                   (x .:? "BeginOffset") <*> (x .:? "Text") <*>
                     (x .:? "Category")
                     <*> (x .:? "Score")
                     <*> (x .:? "Traits" .!= mempty)
                     <*> (x .:? "Attributes" .!= mempty)
                     <*> (x .:? "EndOffset")
                     <*> (x .:? "Id")
                     <*> (x .:? "Type"))

instance Hashable Entity where

instance NFData Entity where

-- | Provides contextual information about the extracted entity. 
--
--
--
-- /See:/ 'trait' smart constructor.
data Trait = Trait'
  { _tScore :: !(Maybe Double)
  , _tName :: !(Maybe AttributeName)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Trait' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tScore' - The level of confidence that Comprehend Medical has in the accuracy of this trait.
--
-- * 'tName' - Provides a name or contextual description about the trait. 
trait
    :: Trait
trait = Trait' {_tScore = Nothing, _tName = Nothing}


-- | The level of confidence that Comprehend Medical has in the accuracy of this trait.
tScore :: Lens' Trait (Maybe Double)
tScore = lens _tScore (\ s a -> s{_tScore = a})

-- | Provides a name or contextual description about the trait. 
tName :: Lens' Trait (Maybe AttributeName)
tName = lens _tName (\ s a -> s{_tName = a})

instance FromJSON Trait where
        parseJSON
          = withObject "Trait"
              (\ x ->
                 Trait' <$> (x .:? "Score") <*> (x .:? "Name"))

instance Hashable Trait where

instance NFData Trait where

-- | An attribute that we extracted, but were unable to relate to an entity. 
--
--
--
-- /See:/ 'unmappedAttribute' smart constructor.
data UnmappedAttribute = UnmappedAttribute'
  { _uaAttribute :: !(Maybe Attribute)
  , _uaType :: !(Maybe EntityType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnmappedAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaAttribute' - The specific attribute that has been extracted but not mapped to an entity. 
--
-- * 'uaType' - The type of the attribute, could be one of the following values: "MEDICATION", "MEDICAL_CONDITION", "ANATOMY", "TEST_AND_TREATMENT_PROCEDURE" or "PERSONAL_HEALTH_INFORMATION". 
unmappedAttribute
    :: UnmappedAttribute
unmappedAttribute =
  UnmappedAttribute' {_uaAttribute = Nothing, _uaType = Nothing}


-- | The specific attribute that has been extracted but not mapped to an entity. 
uaAttribute :: Lens' UnmappedAttribute (Maybe Attribute)
uaAttribute = lens _uaAttribute (\ s a -> s{_uaAttribute = a})

-- | The type of the attribute, could be one of the following values: "MEDICATION", "MEDICAL_CONDITION", "ANATOMY", "TEST_AND_TREATMENT_PROCEDURE" or "PERSONAL_HEALTH_INFORMATION". 
uaType :: Lens' UnmappedAttribute (Maybe EntityType)
uaType = lens _uaType (\ s a -> s{_uaType = a})

instance FromJSON UnmappedAttribute where
        parseJSON
          = withObject "UnmappedAttribute"
              (\ x ->
                 UnmappedAttribute' <$>
                   (x .:? "Attribute") <*> (x .:? "Type"))

instance Hashable UnmappedAttribute where

instance NFData UnmappedAttribute where
