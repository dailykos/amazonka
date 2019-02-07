{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DLM.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DLM.Types.Product where

import Network.AWS.DLM.Internal
import Network.AWS.DLM.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies when to create snapshots of EBS volumes.
--
--
--
-- /See:/ 'createRule' smart constructor.
data CreateRule = CreateRule'
  { _crTimes :: !(Maybe [Text])
  , _crInterval :: !Nat
  , _crIntervalUnit :: !IntervalUnitValues
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crTimes' - The time, in UTC, to start the operation. The operation occurs within a one-hour window following the specified time.
--
-- * 'crInterval' - The interval. The supported values are 12 and 24.
--
-- * 'crIntervalUnit' - The interval unit.
createRule
    :: Natural -- ^ 'crInterval'
    -> IntervalUnitValues -- ^ 'crIntervalUnit'
    -> CreateRule
createRule pInterval_ pIntervalUnit_ =
  CreateRule'
    { _crTimes = Nothing
    , _crInterval = _Nat # pInterval_
    , _crIntervalUnit = pIntervalUnit_
    }


-- | The time, in UTC, to start the operation. The operation occurs within a one-hour window following the specified time.
crTimes :: Lens' CreateRule [Text]
crTimes = lens _crTimes (\ s a -> s{_crTimes = a}) . _Default . _Coerce

-- | The interval. The supported values are 12 and 24.
crInterval :: Lens' CreateRule Natural
crInterval = lens _crInterval (\ s a -> s{_crInterval = a}) . _Nat

-- | The interval unit.
crIntervalUnit :: Lens' CreateRule IntervalUnitValues
crIntervalUnit = lens _crIntervalUnit (\ s a -> s{_crIntervalUnit = a})

instance FromJSON CreateRule where
        parseJSON
          = withObject "CreateRule"
              (\ x ->
                 CreateRule' <$>
                   (x .:? "Times" .!= mempty) <*> (x .: "Interval") <*>
                     (x .: "IntervalUnit"))

instance Hashable CreateRule where

instance NFData CreateRule where

instance ToJSON CreateRule where
        toJSON CreateRule'{..}
          = object
              (catMaybes
                 [("Times" .=) <$> _crTimes,
                  Just ("Interval" .= _crInterval),
                  Just ("IntervalUnit" .= _crIntervalUnit)])

-- | Detailed information about a lifecycle policy.
--
--
--
-- /See:/ 'lifecyclePolicy' smart constructor.
data LifecyclePolicy = LifecyclePolicy'
  { _lpState :: !(Maybe GettablePolicyStateValues)
  , _lpPolicyDetails :: !(Maybe PolicyDetails)
  , _lpPolicyId :: !(Maybe Text)
  , _lpExecutionRoleARN :: !(Maybe Text)
  , _lpDateCreated :: !(Maybe POSIX)
  , _lpDateModified :: !(Maybe POSIX)
  , _lpDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LifecyclePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpState' - The activation state of the lifecycle policy.
--
-- * 'lpPolicyDetails' - The configuration of the lifecycle policy
--
-- * 'lpPolicyId' - The identifier of the lifecycle policy.
--
-- * 'lpExecutionRoleARN' - The Amazon Resource Name (ARN) of the IAM role used to run the operations specified by the lifecycle policy.
--
-- * 'lpDateCreated' - The local date and time when the lifecycle policy was created.
--
-- * 'lpDateModified' - The local date and time when the lifecycle policy was last modified.
--
-- * 'lpDescription' - The description of the lifecycle policy.
lifecyclePolicy
    :: LifecyclePolicy
lifecyclePolicy =
  LifecyclePolicy'
    { _lpState = Nothing
    , _lpPolicyDetails = Nothing
    , _lpPolicyId = Nothing
    , _lpExecutionRoleARN = Nothing
    , _lpDateCreated = Nothing
    , _lpDateModified = Nothing
    , _lpDescription = Nothing
    }


-- | The activation state of the lifecycle policy.
lpState :: Lens' LifecyclePolicy (Maybe GettablePolicyStateValues)
lpState = lens _lpState (\ s a -> s{_lpState = a})

-- | The configuration of the lifecycle policy
lpPolicyDetails :: Lens' LifecyclePolicy (Maybe PolicyDetails)
lpPolicyDetails = lens _lpPolicyDetails (\ s a -> s{_lpPolicyDetails = a})

-- | The identifier of the lifecycle policy.
lpPolicyId :: Lens' LifecyclePolicy (Maybe Text)
lpPolicyId = lens _lpPolicyId (\ s a -> s{_lpPolicyId = a})

-- | The Amazon Resource Name (ARN) of the IAM role used to run the operations specified by the lifecycle policy.
lpExecutionRoleARN :: Lens' LifecyclePolicy (Maybe Text)
lpExecutionRoleARN = lens _lpExecutionRoleARN (\ s a -> s{_lpExecutionRoleARN = a})

-- | The local date and time when the lifecycle policy was created.
lpDateCreated :: Lens' LifecyclePolicy (Maybe UTCTime)
lpDateCreated = lens _lpDateCreated (\ s a -> s{_lpDateCreated = a}) . mapping _Time

-- | The local date and time when the lifecycle policy was last modified.
lpDateModified :: Lens' LifecyclePolicy (Maybe UTCTime)
lpDateModified = lens _lpDateModified (\ s a -> s{_lpDateModified = a}) . mapping _Time

-- | The description of the lifecycle policy.
lpDescription :: Lens' LifecyclePolicy (Maybe Text)
lpDescription = lens _lpDescription (\ s a -> s{_lpDescription = a})

instance FromJSON LifecyclePolicy where
        parseJSON
          = withObject "LifecyclePolicy"
              (\ x ->
                 LifecyclePolicy' <$>
                   (x .:? "State") <*> (x .:? "PolicyDetails") <*>
                     (x .:? "PolicyId")
                     <*> (x .:? "ExecutionRoleArn")
                     <*> (x .:? "DateCreated")
                     <*> (x .:? "DateModified")
                     <*> (x .:? "Description"))

instance Hashable LifecyclePolicy where

instance NFData LifecyclePolicy where

-- | Summary information about a lifecycle policy.
--
--
--
-- /See:/ 'lifecyclePolicySummary' smart constructor.
data LifecyclePolicySummary = LifecyclePolicySummary'
  { _lpsState :: !(Maybe GettablePolicyStateValues)
  , _lpsPolicyId :: !(Maybe Text)
  , _lpsDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LifecyclePolicySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpsState' - The activation state of the lifecycle policy.
--
-- * 'lpsPolicyId' - The identifier of the lifecycle policy.
--
-- * 'lpsDescription' - The description of the lifecycle policy.
lifecyclePolicySummary
    :: LifecyclePolicySummary
lifecyclePolicySummary =
  LifecyclePolicySummary'
    {_lpsState = Nothing, _lpsPolicyId = Nothing, _lpsDescription = Nothing}


-- | The activation state of the lifecycle policy.
lpsState :: Lens' LifecyclePolicySummary (Maybe GettablePolicyStateValues)
lpsState = lens _lpsState (\ s a -> s{_lpsState = a})

-- | The identifier of the lifecycle policy.
lpsPolicyId :: Lens' LifecyclePolicySummary (Maybe Text)
lpsPolicyId = lens _lpsPolicyId (\ s a -> s{_lpsPolicyId = a})

-- | The description of the lifecycle policy.
lpsDescription :: Lens' LifecyclePolicySummary (Maybe Text)
lpsDescription = lens _lpsDescription (\ s a -> s{_lpsDescription = a})

instance FromJSON LifecyclePolicySummary where
        parseJSON
          = withObject "LifecyclePolicySummary"
              (\ x ->
                 LifecyclePolicySummary' <$>
                   (x .:? "State") <*> (x .:? "PolicyId") <*>
                     (x .:? "Description"))

instance Hashable LifecyclePolicySummary where

instance NFData LifecyclePolicySummary where

-- | Specifies the configuration of a lifecycle policy.
--
--
--
-- /See:/ 'policyDetails' smart constructor.
data PolicyDetails = PolicyDetails'
  { _pdTargetTags :: !(Maybe (List1 Tag))
  , _pdSchedules :: !(Maybe (List1 Schedule))
  , _pdResourceTypes :: !(Maybe (List1 ResourceTypeValues))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PolicyDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdTargetTags' - The single tag that identifies targeted resources for this policy.
--
-- * 'pdSchedules' - The schedule of policy-defined actions.
--
-- * 'pdResourceTypes' - The resource type.
policyDetails
    :: PolicyDetails
policyDetails =
  PolicyDetails'
    { _pdTargetTags = Nothing
    , _pdSchedules = Nothing
    , _pdResourceTypes = Nothing
    }


-- | The single tag that identifies targeted resources for this policy.
pdTargetTags :: Lens' PolicyDetails (Maybe (NonEmpty Tag))
pdTargetTags = lens _pdTargetTags (\ s a -> s{_pdTargetTags = a}) . mapping _List1

-- | The schedule of policy-defined actions.
pdSchedules :: Lens' PolicyDetails (Maybe (NonEmpty Schedule))
pdSchedules = lens _pdSchedules (\ s a -> s{_pdSchedules = a}) . mapping _List1

-- | The resource type.
pdResourceTypes :: Lens' PolicyDetails (Maybe (NonEmpty ResourceTypeValues))
pdResourceTypes = lens _pdResourceTypes (\ s a -> s{_pdResourceTypes = a}) . mapping _List1

instance FromJSON PolicyDetails where
        parseJSON
          = withObject "PolicyDetails"
              (\ x ->
                 PolicyDetails' <$>
                   (x .:? "TargetTags") <*> (x .:? "Schedules") <*>
                     (x .:? "ResourceTypes"))

instance Hashable PolicyDetails where

instance NFData PolicyDetails where

instance ToJSON PolicyDetails where
        toJSON PolicyDetails'{..}
          = object
              (catMaybes
                 [("TargetTags" .=) <$> _pdTargetTags,
                  ("Schedules" .=) <$> _pdSchedules,
                  ("ResourceTypes" .=) <$> _pdResourceTypes])

-- | Specifies the number of snapshots to keep for each EBS volume.
--
--
--
-- /See:/ 'retainRule' smart constructor.
newtype RetainRule = RetainRule'
  { _rrCount :: Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RetainRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrCount' - The number of snapshots to keep for each volume, up to a maximum of 1000.
retainRule
    :: Natural -- ^ 'rrCount'
    -> RetainRule
retainRule pCount_ = RetainRule' {_rrCount = _Nat # pCount_}


-- | The number of snapshots to keep for each volume, up to a maximum of 1000.
rrCount :: Lens' RetainRule Natural
rrCount = lens _rrCount (\ s a -> s{_rrCount = a}) . _Nat

instance FromJSON RetainRule where
        parseJSON
          = withObject "RetainRule"
              (\ x -> RetainRule' <$> (x .: "Count"))

instance Hashable RetainRule where

instance NFData RetainRule where

instance ToJSON RetainRule where
        toJSON RetainRule'{..}
          = object (catMaybes [Just ("Count" .= _rrCount)])

-- | Specifies a schedule.
--
--
--
-- /See:/ 'schedule' smart constructor.
data Schedule = Schedule'
  { _sCreateRule :: !(Maybe CreateRule)
  , _sCopyTags :: !(Maybe Bool)
  , _sName :: !(Maybe Text)
  , _sTagsToAdd :: !(Maybe [Tag])
  , _sRetainRule :: !(Maybe RetainRule)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Schedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sCreateRule' - The create rule.
--
-- * 'sCopyTags' - Undocumented member.
--
-- * 'sName' - The name of the schedule.
--
-- * 'sTagsToAdd' - The tags to apply to policy-created resources. These user-defined tags are in addition to the AWS-added lifecycle tags.
--
-- * 'sRetainRule' - The retain rule.
schedule
    :: Schedule
schedule =
  Schedule'
    { _sCreateRule = Nothing
    , _sCopyTags = Nothing
    , _sName = Nothing
    , _sTagsToAdd = Nothing
    , _sRetainRule = Nothing
    }


-- | The create rule.
sCreateRule :: Lens' Schedule (Maybe CreateRule)
sCreateRule = lens _sCreateRule (\ s a -> s{_sCreateRule = a})

-- | Undocumented member.
sCopyTags :: Lens' Schedule (Maybe Bool)
sCopyTags = lens _sCopyTags (\ s a -> s{_sCopyTags = a})

-- | The name of the schedule.
sName :: Lens' Schedule (Maybe Text)
sName = lens _sName (\ s a -> s{_sName = a})

-- | The tags to apply to policy-created resources. These user-defined tags are in addition to the AWS-added lifecycle tags.
sTagsToAdd :: Lens' Schedule [Tag]
sTagsToAdd = lens _sTagsToAdd (\ s a -> s{_sTagsToAdd = a}) . _Default . _Coerce

-- | The retain rule.
sRetainRule :: Lens' Schedule (Maybe RetainRule)
sRetainRule = lens _sRetainRule (\ s a -> s{_sRetainRule = a})

instance FromJSON Schedule where
        parseJSON
          = withObject "Schedule"
              (\ x ->
                 Schedule' <$>
                   (x .:? "CreateRule") <*> (x .:? "CopyTags") <*>
                     (x .:? "Name")
                     <*> (x .:? "TagsToAdd" .!= mempty)
                     <*> (x .:? "RetainRule"))

instance Hashable Schedule where

instance NFData Schedule where

instance ToJSON Schedule where
        toJSON Schedule'{..}
          = object
              (catMaybes
                 [("CreateRule" .=) <$> _sCreateRule,
                  ("CopyTags" .=) <$> _sCopyTags,
                  ("Name" .=) <$> _sName,
                  ("TagsToAdd" .=) <$> _sTagsToAdd,
                  ("RetainRule" .=) <$> _sRetainRule])

-- | Specifies a tag for a resource.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagKey :: !Text
  , _tagValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - The tag key.
--
-- * 'tagValue' - The tag value.
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}


-- | The tag key.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

-- | The tag value.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .: "Key") <*> (x .: "Value"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _tagKey),
                  Just ("Value" .= _tagValue)])
