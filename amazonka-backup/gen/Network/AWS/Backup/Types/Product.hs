{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Backup.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Backup.Types.Product where

import Network.AWS.Backup.Internal
import Network.AWS.Backup.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains detailed information about a backup job.
--
--
--
-- /See:/ 'backupJob' smart constructor.
data BackupJob = BackupJob'
  { _bjIAMRoleARN :: !(Maybe Text)
  , _bjState :: !(Maybe BackupJobState)
  , _bjResourceType :: !(Maybe Text)
  , _bjPercentDone :: !(Maybe Text)
  , _bjStartBy :: !(Maybe POSIX)
  , _bjCreatedBy :: !(Maybe RecoveryPointCreator)
  , _bjExpectedCompletionDate :: !(Maybe POSIX)
  , _bjBytesTransferred :: !(Maybe Integer)
  , _bjBackupVaultARN :: !(Maybe Text)
  , _bjBackupJobId :: !(Maybe Text)
  , _bjResourceARN :: !(Maybe Text)
  , _bjStatusMessage :: !(Maybe Text)
  , _bjRecoveryPointARN :: !(Maybe Text)
  , _bjBackupSizeInBytes :: !(Maybe Integer)
  , _bjCreationDate :: !(Maybe POSIX)
  , _bjCompletionDate :: !(Maybe POSIX)
  , _bjBackupVaultName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BackupJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bjIAMRoleARN' - Specifies the IAM role ARN used to create the target recovery point; for example, @arn:aws:iam::123456789012:role/S3Access@ .
--
-- * 'bjState' - The current state of a resource recovery point.
--
-- * 'bjResourceType' - The type of AWS resource to be backed-up; for example, an Amazon Elastic Block Store (Amazon EBS) volume or an Amazon Relational Database Service (Amazon RDS) database.
--
-- * 'bjPercentDone' - Contains an estimated percentage complete of a job at the time the job status was queried.
--
-- * 'bjStartBy' - Specifies the time in Unix format and Coordinated Universal Time (UTC) when a backup job must be started before it is canceled. The value is calculated by adding the start window to the scheduled time. So if the scheduled time were 6:00 PM and the start window is 2 hours, the @StartBy@ time would be 8:00 PM on the date specified. The value of @StartBy@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'bjCreatedBy' - Contains identifying information about the creation of a backup job, including the @BackupPlanArn@ , @BackupPlanId@ , @BackupPlanVersion@ , and @BackupRuleId@ of the backup plan used to create it.
--
-- * 'bjExpectedCompletionDate' - The date and time a job to back up resources is expected to be completed, in Unix format and Coordinated Universal Time (UTC). The value of @ExpectedCompletionDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'bjBytesTransferred' - The size in bytes transferred to a backup vault at the time that the job status was queried.
--
-- * 'bjBackupVaultARN' - An Amazon Resource Name (ARN) that uniquely identifies a backup vault; for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@ .
--
-- * 'bjBackupJobId' - Uniquely identifies a request to AWS Backup to back up a resource.
--
-- * 'bjResourceARN' - An ARN that uniquely identifies a resource. The format of the ARN depends on the resource type.
--
-- * 'bjStatusMessage' - A detailed message explaining the status of the job to back up a resource.
--
-- * 'bjRecoveryPointARN' - An ARN that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
--
-- * 'bjBackupSizeInBytes' - The size, in bytes, of a backup.
--
-- * 'bjCreationDate' - The date and time a backup job is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'bjCompletionDate' - The date and time a job to create a backup job is completed, in Unix format and Coordinated Universal Time (UTC). The value of @CompletionDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'bjBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
backupJob
    :: BackupJob
backupJob =
  BackupJob'
    { _bjIAMRoleARN = Nothing
    , _bjState = Nothing
    , _bjResourceType = Nothing
    , _bjPercentDone = Nothing
    , _bjStartBy = Nothing
    , _bjCreatedBy = Nothing
    , _bjExpectedCompletionDate = Nothing
    , _bjBytesTransferred = Nothing
    , _bjBackupVaultARN = Nothing
    , _bjBackupJobId = Nothing
    , _bjResourceARN = Nothing
    , _bjStatusMessage = Nothing
    , _bjRecoveryPointARN = Nothing
    , _bjBackupSizeInBytes = Nothing
    , _bjCreationDate = Nothing
    , _bjCompletionDate = Nothing
    , _bjBackupVaultName = Nothing
    }


-- | Specifies the IAM role ARN used to create the target recovery point; for example, @arn:aws:iam::123456789012:role/S3Access@ .
bjIAMRoleARN :: Lens' BackupJob (Maybe Text)
bjIAMRoleARN = lens _bjIAMRoleARN (\ s a -> s{_bjIAMRoleARN = a})

-- | The current state of a resource recovery point.
bjState :: Lens' BackupJob (Maybe BackupJobState)
bjState = lens _bjState (\ s a -> s{_bjState = a})

-- | The type of AWS resource to be backed-up; for example, an Amazon Elastic Block Store (Amazon EBS) volume or an Amazon Relational Database Service (Amazon RDS) database.
bjResourceType :: Lens' BackupJob (Maybe Text)
bjResourceType = lens _bjResourceType (\ s a -> s{_bjResourceType = a})

-- | Contains an estimated percentage complete of a job at the time the job status was queried.
bjPercentDone :: Lens' BackupJob (Maybe Text)
bjPercentDone = lens _bjPercentDone (\ s a -> s{_bjPercentDone = a})

-- | Specifies the time in Unix format and Coordinated Universal Time (UTC) when a backup job must be started before it is canceled. The value is calculated by adding the start window to the scheduled time. So if the scheduled time were 6:00 PM and the start window is 2 hours, the @StartBy@ time would be 8:00 PM on the date specified. The value of @StartBy@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
bjStartBy :: Lens' BackupJob (Maybe UTCTime)
bjStartBy = lens _bjStartBy (\ s a -> s{_bjStartBy = a}) . mapping _Time

-- | Contains identifying information about the creation of a backup job, including the @BackupPlanArn@ , @BackupPlanId@ , @BackupPlanVersion@ , and @BackupRuleId@ of the backup plan used to create it.
bjCreatedBy :: Lens' BackupJob (Maybe RecoveryPointCreator)
bjCreatedBy = lens _bjCreatedBy (\ s a -> s{_bjCreatedBy = a})

-- | The date and time a job to back up resources is expected to be completed, in Unix format and Coordinated Universal Time (UTC). The value of @ExpectedCompletionDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
bjExpectedCompletionDate :: Lens' BackupJob (Maybe UTCTime)
bjExpectedCompletionDate = lens _bjExpectedCompletionDate (\ s a -> s{_bjExpectedCompletionDate = a}) . mapping _Time

-- | The size in bytes transferred to a backup vault at the time that the job status was queried.
bjBytesTransferred :: Lens' BackupJob (Maybe Integer)
bjBytesTransferred = lens _bjBytesTransferred (\ s a -> s{_bjBytesTransferred = a})

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault; for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@ .
bjBackupVaultARN :: Lens' BackupJob (Maybe Text)
bjBackupVaultARN = lens _bjBackupVaultARN (\ s a -> s{_bjBackupVaultARN = a})

-- | Uniquely identifies a request to AWS Backup to back up a resource.
bjBackupJobId :: Lens' BackupJob (Maybe Text)
bjBackupJobId = lens _bjBackupJobId (\ s a -> s{_bjBackupJobId = a})

-- | An ARN that uniquely identifies a resource. The format of the ARN depends on the resource type.
bjResourceARN :: Lens' BackupJob (Maybe Text)
bjResourceARN = lens _bjResourceARN (\ s a -> s{_bjResourceARN = a})

-- | A detailed message explaining the status of the job to back up a resource.
bjStatusMessage :: Lens' BackupJob (Maybe Text)
bjStatusMessage = lens _bjStatusMessage (\ s a -> s{_bjStatusMessage = a})

-- | An ARN that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
bjRecoveryPointARN :: Lens' BackupJob (Maybe Text)
bjRecoveryPointARN = lens _bjRecoveryPointARN (\ s a -> s{_bjRecoveryPointARN = a})

-- | The size, in bytes, of a backup.
bjBackupSizeInBytes :: Lens' BackupJob (Maybe Integer)
bjBackupSizeInBytes = lens _bjBackupSizeInBytes (\ s a -> s{_bjBackupSizeInBytes = a})

-- | The date and time a backup job is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
bjCreationDate :: Lens' BackupJob (Maybe UTCTime)
bjCreationDate = lens _bjCreationDate (\ s a -> s{_bjCreationDate = a}) . mapping _Time

-- | The date and time a job to create a backup job is completed, in Unix format and Coordinated Universal Time (UTC). The value of @CompletionDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
bjCompletionDate :: Lens' BackupJob (Maybe UTCTime)
bjCompletionDate = lens _bjCompletionDate (\ s a -> s{_bjCompletionDate = a}) . mapping _Time

-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
bjBackupVaultName :: Lens' BackupJob (Maybe Text)
bjBackupVaultName = lens _bjBackupVaultName (\ s a -> s{_bjBackupVaultName = a})

instance FromJSON BackupJob where
        parseJSON
          = withObject "BackupJob"
              (\ x ->
                 BackupJob' <$>
                   (x .:? "IamRoleArn") <*> (x .:? "State") <*>
                     (x .:? "ResourceType")
                     <*> (x .:? "PercentDone")
                     <*> (x .:? "StartBy")
                     <*> (x .:? "CreatedBy")
                     <*> (x .:? "ExpectedCompletionDate")
                     <*> (x .:? "BytesTransferred")
                     <*> (x .:? "BackupVaultArn")
                     <*> (x .:? "BackupJobId")
                     <*> (x .:? "ResourceArn")
                     <*> (x .:? "StatusMessage")
                     <*> (x .:? "RecoveryPointArn")
                     <*> (x .:? "BackupSizeInBytes")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "CompletionDate")
                     <*> (x .:? "BackupVaultName"))

instance Hashable BackupJob where

instance NFData BackupJob where

-- | Contains an optional backup plan display name and an array of @BackupRule@ objects, each of which specifies a backup rule. Each rule in a backup plan is a separate scheduled task and can back up a different selection of AWS resources.
--
--
--
-- /See:/ 'backupPlan' smart constructor.
data BackupPlan = BackupPlan'
  { _bpBackupPlanName :: !Text
  , _bpRules :: ![BackupRule]
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'BackupPlan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpBackupPlanName' - The display name of a backup plan.
--
-- * 'bpRules' - An array of @BackupRule@ objects, each of which specifies a scheduled task that is used to back up a selection of resources.
backupPlan
    :: Text -- ^ 'bpBackupPlanName'
    -> BackupPlan
backupPlan pBackupPlanName_ =
  BackupPlan' {_bpBackupPlanName = pBackupPlanName_, _bpRules = mempty}


-- | The display name of a backup plan.
bpBackupPlanName :: Lens' BackupPlan Text
bpBackupPlanName = lens _bpBackupPlanName (\ s a -> s{_bpBackupPlanName = a})

-- | An array of @BackupRule@ objects, each of which specifies a scheduled task that is used to back up a selection of resources.
bpRules :: Lens' BackupPlan [BackupRule]
bpRules = lens _bpRules (\ s a -> s{_bpRules = a}) . _Coerce

instance FromJSON BackupPlan where
        parseJSON
          = withObject "BackupPlan"
              (\ x ->
                 BackupPlan' <$>
                   (x .: "BackupPlanName") <*>
                     (x .:? "Rules" .!= mempty))

instance Hashable BackupPlan where

instance NFData BackupPlan where

-- | Contains an optional backup plan display name and an array of @BackupRule@ objects, each of which specifies a backup rule. Each rule in a backup plan is a separate scheduled task and can back up a different selection of AWS resources.
--
--
--
-- /See:/ 'backupPlanInput' smart constructor.
data BackupPlanInput = BackupPlanInput'
  { _bpiBackupPlanName :: !Text
  , _bpiRules :: ![BackupRuleInput]
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'BackupPlanInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpiBackupPlanName' - The display name of a backup plan.
--
-- * 'bpiRules' - An array of @BackupRule@ objects, each of which specifies a scheduled task that is used to back up a selection of resources.
backupPlanInput
    :: Text -- ^ 'bpiBackupPlanName'
    -> BackupPlanInput
backupPlanInput pBackupPlanName_ =
  BackupPlanInput' {_bpiBackupPlanName = pBackupPlanName_, _bpiRules = mempty}


-- | The display name of a backup plan.
bpiBackupPlanName :: Lens' BackupPlanInput Text
bpiBackupPlanName = lens _bpiBackupPlanName (\ s a -> s{_bpiBackupPlanName = a})

-- | An array of @BackupRule@ objects, each of which specifies a scheduled task that is used to back up a selection of resources.
bpiRules :: Lens' BackupPlanInput [BackupRuleInput]
bpiRules = lens _bpiRules (\ s a -> s{_bpiRules = a}) . _Coerce

instance Hashable BackupPlanInput where

instance NFData BackupPlanInput where

instance ToJSON BackupPlanInput where
        toJSON BackupPlanInput'{..}
          = object
              (catMaybes
                 [Just ("BackupPlanName" .= _bpiBackupPlanName),
                  Just ("Rules" .= _bpiRules)])

-- | An object specifying metadata associated with a backup plan template.
--
--
--
-- /See:/ 'backupPlanTemplatesListMember' smart constructor.
data BackupPlanTemplatesListMember = BackupPlanTemplatesListMember'
  { _bptlmBackupPlanTemplateName :: !(Maybe Text)
  , _bptlmBackupPlanTemplateId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BackupPlanTemplatesListMember' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bptlmBackupPlanTemplateName' - The optional display name of a backup plan template.
--
-- * 'bptlmBackupPlanTemplateId' - Uniquely identifies a stored backup plan template.
backupPlanTemplatesListMember
    :: BackupPlanTemplatesListMember
backupPlanTemplatesListMember =
  BackupPlanTemplatesListMember'
    { _bptlmBackupPlanTemplateName = Nothing
    , _bptlmBackupPlanTemplateId = Nothing
    }


-- | The optional display name of a backup plan template.
bptlmBackupPlanTemplateName :: Lens' BackupPlanTemplatesListMember (Maybe Text)
bptlmBackupPlanTemplateName = lens _bptlmBackupPlanTemplateName (\ s a -> s{_bptlmBackupPlanTemplateName = a})

-- | Uniquely identifies a stored backup plan template.
bptlmBackupPlanTemplateId :: Lens' BackupPlanTemplatesListMember (Maybe Text)
bptlmBackupPlanTemplateId = lens _bptlmBackupPlanTemplateId (\ s a -> s{_bptlmBackupPlanTemplateId = a})

instance FromJSON BackupPlanTemplatesListMember where
        parseJSON
          = withObject "BackupPlanTemplatesListMember"
              (\ x ->
                 BackupPlanTemplatesListMember' <$>
                   (x .:? "BackupPlanTemplateName") <*>
                     (x .:? "BackupPlanTemplateId"))

instance Hashable BackupPlanTemplatesListMember where

instance NFData BackupPlanTemplatesListMember where

-- | Contains metadata about a backup plan.
--
--
--
-- /See:/ 'backupPlansListMember' smart constructor.
data BackupPlansListMember = BackupPlansListMember'
  { _bplmVersionId :: !(Maybe Text)
  , _bplmBackupPlanName :: !(Maybe Text)
  , _bplmBackupPlanId :: !(Maybe Text)
  , _bplmCreatorRequestId :: !(Maybe Text)
  , _bplmBackupPlanARN :: !(Maybe Text)
  , _bplmLastExecutionDate :: !(Maybe POSIX)
  , _bplmCreationDate :: !(Maybe POSIX)
  , _bplmDeletionDate :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BackupPlansListMember' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bplmVersionId' - Unique, randomly generated, Unicode, UTF-8 encoded strings that are at most 1,024 bytes long. Version IDs cannot be edited.
--
-- * 'bplmBackupPlanName' - The display name of a saved backup plan.
--
-- * 'bplmBackupPlanId' - Uniquely identifies a backup plan.
--
-- * 'bplmCreatorRequestId' - A unique string that identifies the request and allows failed requests to be retried without the risk of executing the operation twice.
--
-- * 'bplmBackupPlanARN' - An Amazon Resource Name (ARN) that uniquely identifies a backup plan; for example, @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@ .
--
-- * 'bplmLastExecutionDate' - The last time a job to back up resources was executed with this rule. A date and time, in Unix format and Coordinated Universal Time (UTC). The value of @LastExecutionDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'bplmCreationDate' - The date and time a resource backup plan is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'bplmDeletionDate' - The date and time a backup plan is deleted, in Unix format and Coordinated Universal Time (UTC). The value of @DeletionDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
backupPlansListMember
    :: BackupPlansListMember
backupPlansListMember =
  BackupPlansListMember'
    { _bplmVersionId = Nothing
    , _bplmBackupPlanName = Nothing
    , _bplmBackupPlanId = Nothing
    , _bplmCreatorRequestId = Nothing
    , _bplmBackupPlanARN = Nothing
    , _bplmLastExecutionDate = Nothing
    , _bplmCreationDate = Nothing
    , _bplmDeletionDate = Nothing
    }


-- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at most 1,024 bytes long. Version IDs cannot be edited.
bplmVersionId :: Lens' BackupPlansListMember (Maybe Text)
bplmVersionId = lens _bplmVersionId (\ s a -> s{_bplmVersionId = a})

-- | The display name of a saved backup plan.
bplmBackupPlanName :: Lens' BackupPlansListMember (Maybe Text)
bplmBackupPlanName = lens _bplmBackupPlanName (\ s a -> s{_bplmBackupPlanName = a})

-- | Uniquely identifies a backup plan.
bplmBackupPlanId :: Lens' BackupPlansListMember (Maybe Text)
bplmBackupPlanId = lens _bplmBackupPlanId (\ s a -> s{_bplmBackupPlanId = a})

-- | A unique string that identifies the request and allows failed requests to be retried without the risk of executing the operation twice.
bplmCreatorRequestId :: Lens' BackupPlansListMember (Maybe Text)
bplmCreatorRequestId = lens _bplmCreatorRequestId (\ s a -> s{_bplmCreatorRequestId = a})

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan; for example, @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@ .
bplmBackupPlanARN :: Lens' BackupPlansListMember (Maybe Text)
bplmBackupPlanARN = lens _bplmBackupPlanARN (\ s a -> s{_bplmBackupPlanARN = a})

-- | The last time a job to back up resources was executed with this rule. A date and time, in Unix format and Coordinated Universal Time (UTC). The value of @LastExecutionDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
bplmLastExecutionDate :: Lens' BackupPlansListMember (Maybe UTCTime)
bplmLastExecutionDate = lens _bplmLastExecutionDate (\ s a -> s{_bplmLastExecutionDate = a}) . mapping _Time

-- | The date and time a resource backup plan is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
bplmCreationDate :: Lens' BackupPlansListMember (Maybe UTCTime)
bplmCreationDate = lens _bplmCreationDate (\ s a -> s{_bplmCreationDate = a}) . mapping _Time

-- | The date and time a backup plan is deleted, in Unix format and Coordinated Universal Time (UTC). The value of @DeletionDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
bplmDeletionDate :: Lens' BackupPlansListMember (Maybe UTCTime)
bplmDeletionDate = lens _bplmDeletionDate (\ s a -> s{_bplmDeletionDate = a}) . mapping _Time

instance FromJSON BackupPlansListMember where
        parseJSON
          = withObject "BackupPlansListMember"
              (\ x ->
                 BackupPlansListMember' <$>
                   (x .:? "VersionId") <*> (x .:? "BackupPlanName") <*>
                     (x .:? "BackupPlanId")
                     <*> (x .:? "CreatorRequestId")
                     <*> (x .:? "BackupPlanArn")
                     <*> (x .:? "LastExecutionDate")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "DeletionDate"))

instance Hashable BackupPlansListMember where

instance NFData BackupPlansListMember where

-- | Specifies a scheduled task used to back up a selection of resources.
--
--
--
-- /See:/ 'backupRule' smart constructor.
data BackupRule = BackupRule'
  { _brRuleId :: !(Maybe Text)
  , _brLifecycle :: !(Maybe Lifecycle)
  , _brRecoveryPointTags :: !(Maybe (Sensitive (Map Text Text)))
  , _brScheduleExpression :: !(Maybe Text)
  , _brCompletionWindowMinutes :: !(Maybe Integer)
  , _brStartWindowMinutes :: !(Maybe Integer)
  , _brRuleName :: !Text
  , _brTargetBackupVaultName :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'BackupRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brRuleId' - Uniquely identifies a rule that is used to schedule the backup of a selection of resources.
--
-- * 'brLifecycle' - The lifecycle defines when a protected resource is transitioned to cold storage and when it expires. AWS Backup transitions and expires backups automatically according to the lifecycle that you define.  Backups transitioned to cold storage must be stored in cold storage for a minimum of 90 days. Therefore, the “expire after days” setting must be 90 days greater than the “transition to cold after days” setting. The “transition to cold after days” setting cannot be changed after a backup has been transitioned to cold. 
--
-- * 'brRecoveryPointTags' - An array of key-value pair strings that are assigned to resources that are associated with this rule when restored from backup.
--
-- * 'brScheduleExpression' - A CRON expression specifying when AWS Backup initiates a backup job.
--
-- * 'brCompletionWindowMinutes' - A value in minutes after a backup job is successfully started before it must be completed or it is canceled by AWS Backup. This value is optional.
--
-- * 'brStartWindowMinutes' - An optional value that specifies a period of time in minutes after a backup is scheduled before a job is canceled if it doesn't start successfully.
--
-- * 'brRuleName' - An optional display name for a backup rule.
--
-- * 'brTargetBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
backupRule
    :: Text -- ^ 'brRuleName'
    -> Text -- ^ 'brTargetBackupVaultName'
    -> BackupRule
backupRule pRuleName_ pTargetBackupVaultName_ =
  BackupRule'
    { _brRuleId = Nothing
    , _brLifecycle = Nothing
    , _brRecoveryPointTags = Nothing
    , _brScheduleExpression = Nothing
    , _brCompletionWindowMinutes = Nothing
    , _brStartWindowMinutes = Nothing
    , _brRuleName = pRuleName_
    , _brTargetBackupVaultName = pTargetBackupVaultName_
    }


-- | Uniquely identifies a rule that is used to schedule the backup of a selection of resources.
brRuleId :: Lens' BackupRule (Maybe Text)
brRuleId = lens _brRuleId (\ s a -> s{_brRuleId = a})

-- | The lifecycle defines when a protected resource is transitioned to cold storage and when it expires. AWS Backup transitions and expires backups automatically according to the lifecycle that you define.  Backups transitioned to cold storage must be stored in cold storage for a minimum of 90 days. Therefore, the “expire after days” setting must be 90 days greater than the “transition to cold after days” setting. The “transition to cold after days” setting cannot be changed after a backup has been transitioned to cold. 
brLifecycle :: Lens' BackupRule (Maybe Lifecycle)
brLifecycle = lens _brLifecycle (\ s a -> s{_brLifecycle = a})

-- | An array of key-value pair strings that are assigned to resources that are associated with this rule when restored from backup.
brRecoveryPointTags :: Lens' BackupRule (Maybe (HashMap Text Text))
brRecoveryPointTags = lens _brRecoveryPointTags (\ s a -> s{_brRecoveryPointTags = a}) . mapping (_Sensitive . _Map)

-- | A CRON expression specifying when AWS Backup initiates a backup job.
brScheduleExpression :: Lens' BackupRule (Maybe Text)
brScheduleExpression = lens _brScheduleExpression (\ s a -> s{_brScheduleExpression = a})

-- | A value in minutes after a backup job is successfully started before it must be completed or it is canceled by AWS Backup. This value is optional.
brCompletionWindowMinutes :: Lens' BackupRule (Maybe Integer)
brCompletionWindowMinutes = lens _brCompletionWindowMinutes (\ s a -> s{_brCompletionWindowMinutes = a})

-- | An optional value that specifies a period of time in minutes after a backup is scheduled before a job is canceled if it doesn't start successfully.
brStartWindowMinutes :: Lens' BackupRule (Maybe Integer)
brStartWindowMinutes = lens _brStartWindowMinutes (\ s a -> s{_brStartWindowMinutes = a})

-- | An optional display name for a backup rule.
brRuleName :: Lens' BackupRule Text
brRuleName = lens _brRuleName (\ s a -> s{_brRuleName = a})

-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
brTargetBackupVaultName :: Lens' BackupRule Text
brTargetBackupVaultName = lens _brTargetBackupVaultName (\ s a -> s{_brTargetBackupVaultName = a})

instance FromJSON BackupRule where
        parseJSON
          = withObject "BackupRule"
              (\ x ->
                 BackupRule' <$>
                   (x .:? "RuleId") <*> (x .:? "Lifecycle") <*>
                     (x .:? "RecoveryPointTags" .!= mempty)
                     <*> (x .:? "ScheduleExpression")
                     <*> (x .:? "CompletionWindowMinutes")
                     <*> (x .:? "StartWindowMinutes")
                     <*> (x .: "RuleName")
                     <*> (x .: "TargetBackupVaultName"))

instance Hashable BackupRule where

instance NFData BackupRule where

-- | Specifies a scheduled task used to back up a selection of resources.
--
--
--
-- /See:/ 'backupRuleInput' smart constructor.
data BackupRuleInput = BackupRuleInput'
  { _briLifecycle :: !(Maybe Lifecycle)
  , _briRecoveryPointTags :: !(Maybe (Sensitive (Map Text Text)))
  , _briScheduleExpression :: !(Maybe Text)
  , _briCompletionWindowMinutes :: !(Maybe Integer)
  , _briStartWindowMinutes :: !(Maybe Integer)
  , _briRuleName :: !Text
  , _briTargetBackupVaultName :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'BackupRuleInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'briLifecycle' - The lifecycle defines when a protected resource is transitioned to cold storage and when it expires. AWS Backup will transition and expire backups automatically according to the lifecycle that you define.  Backups transitioned to cold storage must be stored in cold storage for a minimum of 90 days. Therefore, the “expire after days” setting must be 90 days greater than the “transition to cold after days”. The “transition to cold after days” setting cannot be changed after a backup has been transitioned to cold. 
--
-- * 'briRecoveryPointTags' - To help organize your resources, you can assign your own metadata to the resources that you create. Each tag is a key-value pair.
--
-- * 'briScheduleExpression' - A CRON expression specifying when AWS Backup initiates a backup job.
--
-- * 'briCompletionWindowMinutes' - The amount of time AWS Backup attempts a backup before canceling the job and returning an error.
--
-- * 'briStartWindowMinutes' - The amount of time in minutes before beginning a backup.
--
-- * 'briRuleName' - >An optional display name for a backup rule.
--
-- * 'briTargetBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
backupRuleInput
    :: Text -- ^ 'briRuleName'
    -> Text -- ^ 'briTargetBackupVaultName'
    -> BackupRuleInput
backupRuleInput pRuleName_ pTargetBackupVaultName_ =
  BackupRuleInput'
    { _briLifecycle = Nothing
    , _briRecoveryPointTags = Nothing
    , _briScheduleExpression = Nothing
    , _briCompletionWindowMinutes = Nothing
    , _briStartWindowMinutes = Nothing
    , _briRuleName = pRuleName_
    , _briTargetBackupVaultName = pTargetBackupVaultName_
    }


-- | The lifecycle defines when a protected resource is transitioned to cold storage and when it expires. AWS Backup will transition and expire backups automatically according to the lifecycle that you define.  Backups transitioned to cold storage must be stored in cold storage for a minimum of 90 days. Therefore, the “expire after days” setting must be 90 days greater than the “transition to cold after days”. The “transition to cold after days” setting cannot be changed after a backup has been transitioned to cold. 
briLifecycle :: Lens' BackupRuleInput (Maybe Lifecycle)
briLifecycle = lens _briLifecycle (\ s a -> s{_briLifecycle = a})

-- | To help organize your resources, you can assign your own metadata to the resources that you create. Each tag is a key-value pair.
briRecoveryPointTags :: Lens' BackupRuleInput (Maybe (HashMap Text Text))
briRecoveryPointTags = lens _briRecoveryPointTags (\ s a -> s{_briRecoveryPointTags = a}) . mapping (_Sensitive . _Map)

-- | A CRON expression specifying when AWS Backup initiates a backup job.
briScheduleExpression :: Lens' BackupRuleInput (Maybe Text)
briScheduleExpression = lens _briScheduleExpression (\ s a -> s{_briScheduleExpression = a})

-- | The amount of time AWS Backup attempts a backup before canceling the job and returning an error.
briCompletionWindowMinutes :: Lens' BackupRuleInput (Maybe Integer)
briCompletionWindowMinutes = lens _briCompletionWindowMinutes (\ s a -> s{_briCompletionWindowMinutes = a})

-- | The amount of time in minutes before beginning a backup.
briStartWindowMinutes :: Lens' BackupRuleInput (Maybe Integer)
briStartWindowMinutes = lens _briStartWindowMinutes (\ s a -> s{_briStartWindowMinutes = a})

-- | >An optional display name for a backup rule.
briRuleName :: Lens' BackupRuleInput Text
briRuleName = lens _briRuleName (\ s a -> s{_briRuleName = a})

-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
briTargetBackupVaultName :: Lens' BackupRuleInput Text
briTargetBackupVaultName = lens _briTargetBackupVaultName (\ s a -> s{_briTargetBackupVaultName = a})

instance Hashable BackupRuleInput where

instance NFData BackupRuleInput where

instance ToJSON BackupRuleInput where
        toJSON BackupRuleInput'{..}
          = object
              (catMaybes
                 [("Lifecycle" .=) <$> _briLifecycle,
                  ("RecoveryPointTags" .=) <$> _briRecoveryPointTags,
                  ("ScheduleExpression" .=) <$> _briScheduleExpression,
                  ("CompletionWindowMinutes" .=) <$>
                    _briCompletionWindowMinutes,
                  ("StartWindowMinutes" .=) <$> _briStartWindowMinutes,
                  Just ("RuleName" .= _briRuleName),
                  Just
                    ("TargetBackupVaultName" .=
                       _briTargetBackupVaultName)])

-- | Used to specify a set of resources to a backup plan.
--
--
--
-- /See:/ 'backupSelection' smart constructor.
data BackupSelection = BackupSelection'
  { _bsResources :: !(Maybe [Text])
  , _bsListOfTags :: !(Maybe [Condition])
  , _bsSelectionName :: !Text
  , _bsIAMRoleARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BackupSelection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsResources' - An array of strings that either contain Amazon Resource Names (ARNs) or match patterns such as "@arn:aws:ec2:us-east-1:123456789012:volume/*@ " of resources to assign to a backup plan.
--
-- * 'bsListOfTags' - An array of conditions used to specify a set of resources to assign to a backup plan; for example, @"StringEquals": {"ec2:ResourceTag/Department": "accounting"@ .
--
-- * 'bsSelectionName' - The display name of a resource selection document.
--
-- * 'bsIAMRoleARN' - The ARN of the IAM role that AWS Backup uses to authenticate when restoring the target resource; for example, @arn:aws:iam::123456789012:role/S3Access@ .
backupSelection
    :: Text -- ^ 'bsSelectionName'
    -> Text -- ^ 'bsIAMRoleARN'
    -> BackupSelection
backupSelection pSelectionName_ pIAMRoleARN_ =
  BackupSelection'
    { _bsResources = Nothing
    , _bsListOfTags = Nothing
    , _bsSelectionName = pSelectionName_
    , _bsIAMRoleARN = pIAMRoleARN_
    }


-- | An array of strings that either contain Amazon Resource Names (ARNs) or match patterns such as "@arn:aws:ec2:us-east-1:123456789012:volume/*@ " of resources to assign to a backup plan.
bsResources :: Lens' BackupSelection [Text]
bsResources = lens _bsResources (\ s a -> s{_bsResources = a}) . _Default . _Coerce

-- | An array of conditions used to specify a set of resources to assign to a backup plan; for example, @"StringEquals": {"ec2:ResourceTag/Department": "accounting"@ .
bsListOfTags :: Lens' BackupSelection [Condition]
bsListOfTags = lens _bsListOfTags (\ s a -> s{_bsListOfTags = a}) . _Default . _Coerce

-- | The display name of a resource selection document.
bsSelectionName :: Lens' BackupSelection Text
bsSelectionName = lens _bsSelectionName (\ s a -> s{_bsSelectionName = a})

-- | The ARN of the IAM role that AWS Backup uses to authenticate when restoring the target resource; for example, @arn:aws:iam::123456789012:role/S3Access@ .
bsIAMRoleARN :: Lens' BackupSelection Text
bsIAMRoleARN = lens _bsIAMRoleARN (\ s a -> s{_bsIAMRoleARN = a})

instance FromJSON BackupSelection where
        parseJSON
          = withObject "BackupSelection"
              (\ x ->
                 BackupSelection' <$>
                   (x .:? "Resources" .!= mempty) <*>
                     (x .:? "ListOfTags" .!= mempty)
                     <*> (x .: "SelectionName")
                     <*> (x .: "IamRoleArn"))

instance Hashable BackupSelection where

instance NFData BackupSelection where

instance ToJSON BackupSelection where
        toJSON BackupSelection'{..}
          = object
              (catMaybes
                 [("Resources" .=) <$> _bsResources,
                  ("ListOfTags" .=) <$> _bsListOfTags,
                  Just ("SelectionName" .= _bsSelectionName),
                  Just ("IamRoleArn" .= _bsIAMRoleARN)])

-- | Contains metadata about a @BackupSelection@ object.
--
--
--
-- /See:/ 'backupSelectionsListMember' smart constructor.
data BackupSelectionsListMember = BackupSelectionsListMember'
  { _bslmIAMRoleARN :: !(Maybe Text)
  , _bslmSelectionName :: !(Maybe Text)
  , _bslmSelectionId :: !(Maybe Text)
  , _bslmBackupPlanId :: !(Maybe Text)
  , _bslmCreatorRequestId :: !(Maybe Text)
  , _bslmCreationDate :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BackupSelectionsListMember' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bslmIAMRoleARN' - Specifies the IAM role Amazon Resource Name (ARN) to create the target recovery point; for example, @arn:aws:iam::123456789012:role/S3Access@ .
--
-- * 'bslmSelectionName' - The display name of a resource selection document.
--
-- * 'bslmSelectionId' - Uniquely identifies a request to assign a set of resources to a backup plan.
--
-- * 'bslmBackupPlanId' - Uniquely identifies a backup plan.
--
-- * 'bslmCreatorRequestId' - A unique string that identifies the request and allows failed requests to be retried without the risk of executing the operation twice.
--
-- * 'bslmCreationDate' - The date and time a backup plan is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
backupSelectionsListMember
    :: BackupSelectionsListMember
backupSelectionsListMember =
  BackupSelectionsListMember'
    { _bslmIAMRoleARN = Nothing
    , _bslmSelectionName = Nothing
    , _bslmSelectionId = Nothing
    , _bslmBackupPlanId = Nothing
    , _bslmCreatorRequestId = Nothing
    , _bslmCreationDate = Nothing
    }


-- | Specifies the IAM role Amazon Resource Name (ARN) to create the target recovery point; for example, @arn:aws:iam::123456789012:role/S3Access@ .
bslmIAMRoleARN :: Lens' BackupSelectionsListMember (Maybe Text)
bslmIAMRoleARN = lens _bslmIAMRoleARN (\ s a -> s{_bslmIAMRoleARN = a})

-- | The display name of a resource selection document.
bslmSelectionName :: Lens' BackupSelectionsListMember (Maybe Text)
bslmSelectionName = lens _bslmSelectionName (\ s a -> s{_bslmSelectionName = a})

-- | Uniquely identifies a request to assign a set of resources to a backup plan.
bslmSelectionId :: Lens' BackupSelectionsListMember (Maybe Text)
bslmSelectionId = lens _bslmSelectionId (\ s a -> s{_bslmSelectionId = a})

-- | Uniquely identifies a backup plan.
bslmBackupPlanId :: Lens' BackupSelectionsListMember (Maybe Text)
bslmBackupPlanId = lens _bslmBackupPlanId (\ s a -> s{_bslmBackupPlanId = a})

-- | A unique string that identifies the request and allows failed requests to be retried without the risk of executing the operation twice.
bslmCreatorRequestId :: Lens' BackupSelectionsListMember (Maybe Text)
bslmCreatorRequestId = lens _bslmCreatorRequestId (\ s a -> s{_bslmCreatorRequestId = a})

-- | The date and time a backup plan is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
bslmCreationDate :: Lens' BackupSelectionsListMember (Maybe UTCTime)
bslmCreationDate = lens _bslmCreationDate (\ s a -> s{_bslmCreationDate = a}) . mapping _Time

instance FromJSON BackupSelectionsListMember where
        parseJSON
          = withObject "BackupSelectionsListMember"
              (\ x ->
                 BackupSelectionsListMember' <$>
                   (x .:? "IamRoleArn") <*> (x .:? "SelectionName") <*>
                     (x .:? "SelectionId")
                     <*> (x .:? "BackupPlanId")
                     <*> (x .:? "CreatorRequestId")
                     <*> (x .:? "CreationDate"))

instance Hashable BackupSelectionsListMember where

instance NFData BackupSelectionsListMember where

-- | Contains metadata about a backup vault.
--
--
--
-- /See:/ 'backupVaultListMember' smart constructor.
data BackupVaultListMember = BackupVaultListMember'
  { _bvlmCreatorRequestId :: !(Maybe Text)
  , _bvlmNumberOfRecoveryPoints :: !(Maybe Integer)
  , _bvlmBackupVaultARN :: !(Maybe Text)
  , _bvlmEncryptionKeyARN :: !(Maybe Text)
  , _bvlmCreationDate :: !(Maybe POSIX)
  , _bvlmBackupVaultName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BackupVaultListMember' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bvlmCreatorRequestId' - A unique string that identifies the request and allows failed requests to be retried without the risk of executing the operation twice.
--
-- * 'bvlmNumberOfRecoveryPoints' - The number of recovery points that are stored in a backup vault.
--
-- * 'bvlmBackupVaultARN' - An Amazon Resource Name (ARN) that uniquely identifies a backup vault; for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@ .
--
-- * 'bvlmEncryptionKeyARN' - The server-side encryption key that is used to protect your backups; for example, @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ .
--
-- * 'bvlmCreationDate' - The date and time a resource backup is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'bvlmBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
backupVaultListMember
    :: BackupVaultListMember
backupVaultListMember =
  BackupVaultListMember'
    { _bvlmCreatorRequestId = Nothing
    , _bvlmNumberOfRecoveryPoints = Nothing
    , _bvlmBackupVaultARN = Nothing
    , _bvlmEncryptionKeyARN = Nothing
    , _bvlmCreationDate = Nothing
    , _bvlmBackupVaultName = Nothing
    }


-- | A unique string that identifies the request and allows failed requests to be retried without the risk of executing the operation twice.
bvlmCreatorRequestId :: Lens' BackupVaultListMember (Maybe Text)
bvlmCreatorRequestId = lens _bvlmCreatorRequestId (\ s a -> s{_bvlmCreatorRequestId = a})

-- | The number of recovery points that are stored in a backup vault.
bvlmNumberOfRecoveryPoints :: Lens' BackupVaultListMember (Maybe Integer)
bvlmNumberOfRecoveryPoints = lens _bvlmNumberOfRecoveryPoints (\ s a -> s{_bvlmNumberOfRecoveryPoints = a})

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault; for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@ .
bvlmBackupVaultARN :: Lens' BackupVaultListMember (Maybe Text)
bvlmBackupVaultARN = lens _bvlmBackupVaultARN (\ s a -> s{_bvlmBackupVaultARN = a})

-- | The server-side encryption key that is used to protect your backups; for example, @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ .
bvlmEncryptionKeyARN :: Lens' BackupVaultListMember (Maybe Text)
bvlmEncryptionKeyARN = lens _bvlmEncryptionKeyARN (\ s a -> s{_bvlmEncryptionKeyARN = a})

-- | The date and time a resource backup is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
bvlmCreationDate :: Lens' BackupVaultListMember (Maybe UTCTime)
bvlmCreationDate = lens _bvlmCreationDate (\ s a -> s{_bvlmCreationDate = a}) . mapping _Time

-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
bvlmBackupVaultName :: Lens' BackupVaultListMember (Maybe Text)
bvlmBackupVaultName = lens _bvlmBackupVaultName (\ s a -> s{_bvlmBackupVaultName = a})

instance FromJSON BackupVaultListMember where
        parseJSON
          = withObject "BackupVaultListMember"
              (\ x ->
                 BackupVaultListMember' <$>
                   (x .:? "CreatorRequestId") <*>
                     (x .:? "NumberOfRecoveryPoints")
                     <*> (x .:? "BackupVaultArn")
                     <*> (x .:? "EncryptionKeyArn")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "BackupVaultName"))

instance Hashable BackupVaultListMember where

instance NFData BackupVaultListMember where

-- | Contains @DeleteAt@ and @MoveToColdStorageAt@ timestamps, which are used to specify a lifecycle for a recovery point.
--
--
-- The lifecycle defines when a protected resource is transitioned to cold storage and when it expires. AWS Backup transitions and expires backups automatically according to the lifecycle that you define. 
--
-- Backups transitioned to cold storage must be stored in cold storage for a minimum of 90 days. Therefore, the “expire after days” setting must be 90 days greater than the “transition to cold after days” setting. The “transition to cold after days” setting cannot be changed after a backup has been transitioned to cold. 
--
--
-- /See:/ 'calculatedLifecycle' smart constructor.
data CalculatedLifecycle = CalculatedLifecycle'
  { _clDeleteAt :: !(Maybe POSIX)
  , _clMoveToColdStorageAt :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CalculatedLifecycle' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clDeleteAt' - A timestamp that specifies when to delete a recovery point.
--
-- * 'clMoveToColdStorageAt' - A timestamp that specifies when to transition a recovery point to cold storage.
calculatedLifecycle
    :: CalculatedLifecycle
calculatedLifecycle =
  CalculatedLifecycle' {_clDeleteAt = Nothing, _clMoveToColdStorageAt = Nothing}


-- | A timestamp that specifies when to delete a recovery point.
clDeleteAt :: Lens' CalculatedLifecycle (Maybe UTCTime)
clDeleteAt = lens _clDeleteAt (\ s a -> s{_clDeleteAt = a}) . mapping _Time

-- | A timestamp that specifies when to transition a recovery point to cold storage.
clMoveToColdStorageAt :: Lens' CalculatedLifecycle (Maybe UTCTime)
clMoveToColdStorageAt = lens _clMoveToColdStorageAt (\ s a -> s{_clMoveToColdStorageAt = a}) . mapping _Time

instance FromJSON CalculatedLifecycle where
        parseJSON
          = withObject "CalculatedLifecycle"
              (\ x ->
                 CalculatedLifecycle' <$>
                   (x .:? "DeleteAt") <*> (x .:? "MoveToColdStorageAt"))

instance Hashable CalculatedLifecycle where

instance NFData CalculatedLifecycle where

-- | Contains an array of triplets made up of a condition type (such as @StringEquals@ ), a key, and a value. Conditions are used to filter resources in a selection that is assigned to a backup plan.
--
--
--
-- /See:/ 'condition' smart constructor.
data Condition = Condition'
  { _cConditionType :: !ConditionType
  , _cConditionKey :: !Text
  , _cConditionValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Condition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cConditionType' - An operation, such as @StringEquals@ , that is applied to a key-value pair used to filter resources in a selection.
--
-- * 'cConditionKey' - The key in a key-value pair. For example, in @"ec2:ResourceTag/Department": "accounting"@ , @"ec2:ResourceTag/Department"@ is the key.
--
-- * 'cConditionValue' - The value in a key-value pair. For example, in @"ec2:ResourceTag/Department": "accounting"@ , @"accounting"@ is the value.
condition
    :: ConditionType -- ^ 'cConditionType'
    -> Text -- ^ 'cConditionKey'
    -> Text -- ^ 'cConditionValue'
    -> Condition
condition pConditionType_ pConditionKey_ pConditionValue_ =
  Condition'
    { _cConditionType = pConditionType_
    , _cConditionKey = pConditionKey_
    , _cConditionValue = pConditionValue_
    }


-- | An operation, such as @StringEquals@ , that is applied to a key-value pair used to filter resources in a selection.
cConditionType :: Lens' Condition ConditionType
cConditionType = lens _cConditionType (\ s a -> s{_cConditionType = a})

-- | The key in a key-value pair. For example, in @"ec2:ResourceTag/Department": "accounting"@ , @"ec2:ResourceTag/Department"@ is the key.
cConditionKey :: Lens' Condition Text
cConditionKey = lens _cConditionKey (\ s a -> s{_cConditionKey = a})

-- | The value in a key-value pair. For example, in @"ec2:ResourceTag/Department": "accounting"@ , @"accounting"@ is the value.
cConditionValue :: Lens' Condition Text
cConditionValue = lens _cConditionValue (\ s a -> s{_cConditionValue = a})

instance FromJSON Condition where
        parseJSON
          = withObject "Condition"
              (\ x ->
                 Condition' <$>
                   (x .: "ConditionType") <*> (x .: "ConditionKey") <*>
                     (x .: "ConditionValue"))

instance Hashable Condition where

instance NFData Condition where

instance ToJSON Condition where
        toJSON Condition'{..}
          = object
              (catMaybes
                 [Just ("ConditionType" .= _cConditionType),
                  Just ("ConditionKey" .= _cConditionKey),
                  Just ("ConditionValue" .= _cConditionValue)])

-- | Contains an array of @Transition@ objects specifying how long in days before a recovery point transitions to cold storage or is deleted.
--
--
--
-- /See:/ 'lifecycle' smart constructor.
data Lifecycle = Lifecycle'
  { _lMoveToColdStorageAfterDays :: !(Maybe Integer)
  , _lDeleteAfterDays :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Lifecycle' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lMoveToColdStorageAfterDays' - Specifies the number of days after creation that a recovery point is moved to cold storage.
--
-- * 'lDeleteAfterDays' - Specifies the number of days after creation that a recovery point is deleted. Must be greater than @MoveToColdStorageAfterDays@ .
lifecycle
    :: Lifecycle
lifecycle =
  Lifecycle'
    {_lMoveToColdStorageAfterDays = Nothing, _lDeleteAfterDays = Nothing}


-- | Specifies the number of days after creation that a recovery point is moved to cold storage.
lMoveToColdStorageAfterDays :: Lens' Lifecycle (Maybe Integer)
lMoveToColdStorageAfterDays = lens _lMoveToColdStorageAfterDays (\ s a -> s{_lMoveToColdStorageAfterDays = a})

-- | Specifies the number of days after creation that a recovery point is deleted. Must be greater than @MoveToColdStorageAfterDays@ .
lDeleteAfterDays :: Lens' Lifecycle (Maybe Integer)
lDeleteAfterDays = lens _lDeleteAfterDays (\ s a -> s{_lDeleteAfterDays = a})

instance FromJSON Lifecycle where
        parseJSON
          = withObject "Lifecycle"
              (\ x ->
                 Lifecycle' <$>
                   (x .:? "MoveToColdStorageAfterDays") <*>
                     (x .:? "DeleteAfterDays"))

instance Hashable Lifecycle where

instance NFData Lifecycle where

instance ToJSON Lifecycle where
        toJSON Lifecycle'{..}
          = object
              (catMaybes
                 [("MoveToColdStorageAfterDays" .=) <$>
                    _lMoveToColdStorageAfterDays,
                  ("DeleteAfterDays" .=) <$> _lDeleteAfterDays])

-- | A structure that contains information about a backed-up resource.
--
--
--
-- /See:/ 'protectedResource' smart constructor.
data ProtectedResource = ProtectedResource'
  { _prResourceType :: !(Maybe Text)
  , _prLastBackupTime :: !(Maybe POSIX)
  , _prResourceARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProtectedResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prResourceType' - The type of AWS resource; for example, an Amazon Elastic Block Store (Amazon EBS) volume or an Amazon Relational Database Service (Amazon RDS) database.
--
-- * 'prLastBackupTime' - The date and time a resource was last backed up, in Unix format and Coordinated Universal Time (UTC). The value of @LastBackupTime@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'prResourceARN' - An Amazon Resource Name (ARN) that uniquely identifies a resource. The format of the ARN depends on the resource type.
protectedResource
    :: ProtectedResource
protectedResource =
  ProtectedResource'
    { _prResourceType = Nothing
    , _prLastBackupTime = Nothing
    , _prResourceARN = Nothing
    }


-- | The type of AWS resource; for example, an Amazon Elastic Block Store (Amazon EBS) volume or an Amazon Relational Database Service (Amazon RDS) database.
prResourceType :: Lens' ProtectedResource (Maybe Text)
prResourceType = lens _prResourceType (\ s a -> s{_prResourceType = a})

-- | The date and time a resource was last backed up, in Unix format and Coordinated Universal Time (UTC). The value of @LastBackupTime@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
prLastBackupTime :: Lens' ProtectedResource (Maybe UTCTime)
prLastBackupTime = lens _prLastBackupTime (\ s a -> s{_prLastBackupTime = a}) . mapping _Time

-- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The format of the ARN depends on the resource type.
prResourceARN :: Lens' ProtectedResource (Maybe Text)
prResourceARN = lens _prResourceARN (\ s a -> s{_prResourceARN = a})

instance FromJSON ProtectedResource where
        parseJSON
          = withObject "ProtectedResource"
              (\ x ->
                 ProtectedResource' <$>
                   (x .:? "ResourceType") <*> (x .:? "LastBackupTime")
                     <*> (x .:? "ResourceArn"))

instance Hashable ProtectedResource where

instance NFData ProtectedResource where

-- | Contains detailed information about the recovery points stored in a backup vault.
--
--
--
-- /See:/ 'recoveryPointByBackupVault' smart constructor.
data RecoveryPointByBackupVault = RecoveryPointByBackupVault'
  { _rpbbvIsEncrypted :: !(Maybe Bool)
  , _rpbbvStatus :: !(Maybe RecoveryPointStatus)
  , _rpbbvIAMRoleARN :: !(Maybe Text)
  , _rpbbvResourceType :: !(Maybe Text)
  , _rpbbvCreatedBy :: !(Maybe RecoveryPointCreator)
  , _rpbbvCalculatedLifecycle :: !(Maybe CalculatedLifecycle)
  , _rpbbvLifecycle :: !(Maybe Lifecycle)
  , _rpbbvBackupVaultARN :: !(Maybe Text)
  , _rpbbvLastRestoreTime :: !(Maybe POSIX)
  , _rpbbvResourceARN :: !(Maybe Text)
  , _rpbbvRecoveryPointARN :: !(Maybe Text)
  , _rpbbvEncryptionKeyARN :: !(Maybe Text)
  , _rpbbvBackupSizeInBytes :: !(Maybe Integer)
  , _rpbbvCreationDate :: !(Maybe POSIX)
  , _rpbbvCompletionDate :: !(Maybe POSIX)
  , _rpbbvBackupVaultName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecoveryPointByBackupVault' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpbbvIsEncrypted' - A Boolean value that is returned as @TRUE@ if the specified recovery point is encrypted, or @FALSE@ if the recovery point is not encrypted.
--
-- * 'rpbbvStatus' - A status code specifying the state of the recovery point.
--
-- * 'rpbbvIAMRoleARN' - Specifies the IAM role ARN used to create the target recovery point; for example, @arn:aws:iam::123456789012:role/S3Access@ .
--
-- * 'rpbbvResourceType' - The type of AWS resource saved as a recovery point; for example, an Amazon Elastic Block Store (Amazon EBS) volume or an Amazon Relational Database Service (Amazon RDS) database.
--
-- * 'rpbbvCreatedBy' - Contains identifying information about the creation of a recovery point, including the @BackupPlanArn@ , @BackupPlanId@ , @BackupPlanVersion@ , and @BackupRuleId@ of the backup plan that is used to create it.
--
-- * 'rpbbvCalculatedLifecycle' - A @CalculatedLifecycle@ object containing @DeleteAt@ and @MoveToColdStorageAt@ timestamps.
--
-- * 'rpbbvLifecycle' - The lifecycle defines when a protected resource is transitioned to cold storage and when it expires. AWS Backup transitions and expires backups automatically according to the lifecycle that you define.  Backups transitioned to cold storage must be stored in cold storage for a minimum of 90 days. Therefore, the “expire after days” setting must be 90 days greater than the “transition to cold after days” setting. The “transition to cold after days” setting cannot be changed after a backup has been transitioned to cold. 
--
-- * 'rpbbvBackupVaultARN' - An ARN that uniquely identifies a backup vault; for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@ .
--
-- * 'rpbbvLastRestoreTime' - The date and time a recovery point was last restored, in Unix format and Coordinated Universal Time (UTC). The value of @LastRestoreTime@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'rpbbvResourceARN' - An ARN that uniquely identifies a resource. The format of the ARN depends on the resource type.
--
-- * 'rpbbvRecoveryPointARN' - An Amazon Resource Name (ARN) that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
--
-- * 'rpbbvEncryptionKeyARN' - The server-side encryption key that is used to protect your backups; for example, @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ .
--
-- * 'rpbbvBackupSizeInBytes' - The size, in bytes, of a backup.
--
-- * 'rpbbvCreationDate' - The date and time a recovery point is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'rpbbvCompletionDate' - The date and time a job to restore a recovery point is completed, in Unix format and Coordinated Universal Time (UTC). The value of @CompletionDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'rpbbvBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
recoveryPointByBackupVault
    :: RecoveryPointByBackupVault
recoveryPointByBackupVault =
  RecoveryPointByBackupVault'
    { _rpbbvIsEncrypted = Nothing
    , _rpbbvStatus = Nothing
    , _rpbbvIAMRoleARN = Nothing
    , _rpbbvResourceType = Nothing
    , _rpbbvCreatedBy = Nothing
    , _rpbbvCalculatedLifecycle = Nothing
    , _rpbbvLifecycle = Nothing
    , _rpbbvBackupVaultARN = Nothing
    , _rpbbvLastRestoreTime = Nothing
    , _rpbbvResourceARN = Nothing
    , _rpbbvRecoveryPointARN = Nothing
    , _rpbbvEncryptionKeyARN = Nothing
    , _rpbbvBackupSizeInBytes = Nothing
    , _rpbbvCreationDate = Nothing
    , _rpbbvCompletionDate = Nothing
    , _rpbbvBackupVaultName = Nothing
    }


-- | A Boolean value that is returned as @TRUE@ if the specified recovery point is encrypted, or @FALSE@ if the recovery point is not encrypted.
rpbbvIsEncrypted :: Lens' RecoveryPointByBackupVault (Maybe Bool)
rpbbvIsEncrypted = lens _rpbbvIsEncrypted (\ s a -> s{_rpbbvIsEncrypted = a})

-- | A status code specifying the state of the recovery point.
rpbbvStatus :: Lens' RecoveryPointByBackupVault (Maybe RecoveryPointStatus)
rpbbvStatus = lens _rpbbvStatus (\ s a -> s{_rpbbvStatus = a})

-- | Specifies the IAM role ARN used to create the target recovery point; for example, @arn:aws:iam::123456789012:role/S3Access@ .
rpbbvIAMRoleARN :: Lens' RecoveryPointByBackupVault (Maybe Text)
rpbbvIAMRoleARN = lens _rpbbvIAMRoleARN (\ s a -> s{_rpbbvIAMRoleARN = a})

-- | The type of AWS resource saved as a recovery point; for example, an Amazon Elastic Block Store (Amazon EBS) volume or an Amazon Relational Database Service (Amazon RDS) database.
rpbbvResourceType :: Lens' RecoveryPointByBackupVault (Maybe Text)
rpbbvResourceType = lens _rpbbvResourceType (\ s a -> s{_rpbbvResourceType = a})

-- | Contains identifying information about the creation of a recovery point, including the @BackupPlanArn@ , @BackupPlanId@ , @BackupPlanVersion@ , and @BackupRuleId@ of the backup plan that is used to create it.
rpbbvCreatedBy :: Lens' RecoveryPointByBackupVault (Maybe RecoveryPointCreator)
rpbbvCreatedBy = lens _rpbbvCreatedBy (\ s a -> s{_rpbbvCreatedBy = a})

-- | A @CalculatedLifecycle@ object containing @DeleteAt@ and @MoveToColdStorageAt@ timestamps.
rpbbvCalculatedLifecycle :: Lens' RecoveryPointByBackupVault (Maybe CalculatedLifecycle)
rpbbvCalculatedLifecycle = lens _rpbbvCalculatedLifecycle (\ s a -> s{_rpbbvCalculatedLifecycle = a})

-- | The lifecycle defines when a protected resource is transitioned to cold storage and when it expires. AWS Backup transitions and expires backups automatically according to the lifecycle that you define.  Backups transitioned to cold storage must be stored in cold storage for a minimum of 90 days. Therefore, the “expire after days” setting must be 90 days greater than the “transition to cold after days” setting. The “transition to cold after days” setting cannot be changed after a backup has been transitioned to cold. 
rpbbvLifecycle :: Lens' RecoveryPointByBackupVault (Maybe Lifecycle)
rpbbvLifecycle = lens _rpbbvLifecycle (\ s a -> s{_rpbbvLifecycle = a})

-- | An ARN that uniquely identifies a backup vault; for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@ .
rpbbvBackupVaultARN :: Lens' RecoveryPointByBackupVault (Maybe Text)
rpbbvBackupVaultARN = lens _rpbbvBackupVaultARN (\ s a -> s{_rpbbvBackupVaultARN = a})

-- | The date and time a recovery point was last restored, in Unix format and Coordinated Universal Time (UTC). The value of @LastRestoreTime@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
rpbbvLastRestoreTime :: Lens' RecoveryPointByBackupVault (Maybe UTCTime)
rpbbvLastRestoreTime = lens _rpbbvLastRestoreTime (\ s a -> s{_rpbbvLastRestoreTime = a}) . mapping _Time

-- | An ARN that uniquely identifies a resource. The format of the ARN depends on the resource type.
rpbbvResourceARN :: Lens' RecoveryPointByBackupVault (Maybe Text)
rpbbvResourceARN = lens _rpbbvResourceARN (\ s a -> s{_rpbbvResourceARN = a})

-- | An Amazon Resource Name (ARN) that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
rpbbvRecoveryPointARN :: Lens' RecoveryPointByBackupVault (Maybe Text)
rpbbvRecoveryPointARN = lens _rpbbvRecoveryPointARN (\ s a -> s{_rpbbvRecoveryPointARN = a})

-- | The server-side encryption key that is used to protect your backups; for example, @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ .
rpbbvEncryptionKeyARN :: Lens' RecoveryPointByBackupVault (Maybe Text)
rpbbvEncryptionKeyARN = lens _rpbbvEncryptionKeyARN (\ s a -> s{_rpbbvEncryptionKeyARN = a})

-- | The size, in bytes, of a backup.
rpbbvBackupSizeInBytes :: Lens' RecoveryPointByBackupVault (Maybe Integer)
rpbbvBackupSizeInBytes = lens _rpbbvBackupSizeInBytes (\ s a -> s{_rpbbvBackupSizeInBytes = a})

-- | The date and time a recovery point is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
rpbbvCreationDate :: Lens' RecoveryPointByBackupVault (Maybe UTCTime)
rpbbvCreationDate = lens _rpbbvCreationDate (\ s a -> s{_rpbbvCreationDate = a}) . mapping _Time

-- | The date and time a job to restore a recovery point is completed, in Unix format and Coordinated Universal Time (UTC). The value of @CompletionDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
rpbbvCompletionDate :: Lens' RecoveryPointByBackupVault (Maybe UTCTime)
rpbbvCompletionDate = lens _rpbbvCompletionDate (\ s a -> s{_rpbbvCompletionDate = a}) . mapping _Time

-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
rpbbvBackupVaultName :: Lens' RecoveryPointByBackupVault (Maybe Text)
rpbbvBackupVaultName = lens _rpbbvBackupVaultName (\ s a -> s{_rpbbvBackupVaultName = a})

instance FromJSON RecoveryPointByBackupVault where
        parseJSON
          = withObject "RecoveryPointByBackupVault"
              (\ x ->
                 RecoveryPointByBackupVault' <$>
                   (x .:? "IsEncrypted") <*> (x .:? "Status") <*>
                     (x .:? "IamRoleArn")
                     <*> (x .:? "ResourceType")
                     <*> (x .:? "CreatedBy")
                     <*> (x .:? "CalculatedLifecycle")
                     <*> (x .:? "Lifecycle")
                     <*> (x .:? "BackupVaultArn")
                     <*> (x .:? "LastRestoreTime")
                     <*> (x .:? "ResourceArn")
                     <*> (x .:? "RecoveryPointArn")
                     <*> (x .:? "EncryptionKeyArn")
                     <*> (x .:? "BackupSizeInBytes")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "CompletionDate")
                     <*> (x .:? "BackupVaultName"))

instance Hashable RecoveryPointByBackupVault where

instance NFData RecoveryPointByBackupVault where

-- | Contains detailed information about a saved recovery point.
--
--
--
-- /See:/ 'recoveryPointByResource' smart constructor.
data RecoveryPointByResource = RecoveryPointByResource'
  { _rpbrStatus :: !(Maybe RecoveryPointStatus)
  , _rpbrRecoveryPointARN :: !(Maybe Text)
  , _rpbrBackupSizeBytes :: !(Maybe Integer)
  , _rpbrEncryptionKeyARN :: !(Maybe Text)
  , _rpbrCreationDate :: !(Maybe POSIX)
  , _rpbrBackupVaultName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecoveryPointByResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpbrStatus' - A status code specifying the state of the recovery point.
--
-- * 'rpbrRecoveryPointARN' - An Amazon Resource Name (ARN) that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
--
-- * 'rpbrBackupSizeBytes' - The size, in bytes, of a backup.
--
-- * 'rpbrEncryptionKeyARN' - The server-side encryption key that is used to protect your backups; for example, @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ .
--
-- * 'rpbrCreationDate' - The date and time a recovery point is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'rpbrBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
recoveryPointByResource
    :: RecoveryPointByResource
recoveryPointByResource =
  RecoveryPointByResource'
    { _rpbrStatus = Nothing
    , _rpbrRecoveryPointARN = Nothing
    , _rpbrBackupSizeBytes = Nothing
    , _rpbrEncryptionKeyARN = Nothing
    , _rpbrCreationDate = Nothing
    , _rpbrBackupVaultName = Nothing
    }


-- | A status code specifying the state of the recovery point.
rpbrStatus :: Lens' RecoveryPointByResource (Maybe RecoveryPointStatus)
rpbrStatus = lens _rpbrStatus (\ s a -> s{_rpbrStatus = a})

-- | An Amazon Resource Name (ARN) that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
rpbrRecoveryPointARN :: Lens' RecoveryPointByResource (Maybe Text)
rpbrRecoveryPointARN = lens _rpbrRecoveryPointARN (\ s a -> s{_rpbrRecoveryPointARN = a})

-- | The size, in bytes, of a backup.
rpbrBackupSizeBytes :: Lens' RecoveryPointByResource (Maybe Integer)
rpbrBackupSizeBytes = lens _rpbrBackupSizeBytes (\ s a -> s{_rpbrBackupSizeBytes = a})

-- | The server-side encryption key that is used to protect your backups; for example, @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ .
rpbrEncryptionKeyARN :: Lens' RecoveryPointByResource (Maybe Text)
rpbrEncryptionKeyARN = lens _rpbrEncryptionKeyARN (\ s a -> s{_rpbrEncryptionKeyARN = a})

-- | The date and time a recovery point is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
rpbrCreationDate :: Lens' RecoveryPointByResource (Maybe UTCTime)
rpbrCreationDate = lens _rpbrCreationDate (\ s a -> s{_rpbrCreationDate = a}) . mapping _Time

-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
rpbrBackupVaultName :: Lens' RecoveryPointByResource (Maybe Text)
rpbrBackupVaultName = lens _rpbrBackupVaultName (\ s a -> s{_rpbrBackupVaultName = a})

instance FromJSON RecoveryPointByResource where
        parseJSON
          = withObject "RecoveryPointByResource"
              (\ x ->
                 RecoveryPointByResource' <$>
                   (x .:? "Status") <*> (x .:? "RecoveryPointArn") <*>
                     (x .:? "BackupSizeBytes")
                     <*> (x .:? "EncryptionKeyArn")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "BackupVaultName"))

instance Hashable RecoveryPointByResource where

instance NFData RecoveryPointByResource where

-- | Contains information about the backup plan and rule that AWS Backup used to initiate the recovery point backup.
--
--
--
-- /See:/ 'recoveryPointCreator' smart constructor.
data RecoveryPointCreator = RecoveryPointCreator'
  { _rpcBackupPlanId :: !(Maybe Text)
  , _rpcBackupPlanARN :: !(Maybe Text)
  , _rpcBackupPlanVersion :: !(Maybe Text)
  , _rpcBackupRuleId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecoveryPointCreator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpcBackupPlanId' - Uniquely identifies a backup plan.
--
-- * 'rpcBackupPlanARN' - An Amazon Resource Name (ARN) that uniquely identifies a backup plan; for example, @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@ .
--
-- * 'rpcBackupPlanVersion' - Version IDs are unique, randomly generated, Unicode, UTF-8 encoded strings that are at most 1,024 bytes long. They cannot be edited.
--
-- * 'rpcBackupRuleId' - Uniquely identifies a rule used to schedule the backup of a selection of resources.
recoveryPointCreator
    :: RecoveryPointCreator
recoveryPointCreator =
  RecoveryPointCreator'
    { _rpcBackupPlanId = Nothing
    , _rpcBackupPlanARN = Nothing
    , _rpcBackupPlanVersion = Nothing
    , _rpcBackupRuleId = Nothing
    }


-- | Uniquely identifies a backup plan.
rpcBackupPlanId :: Lens' RecoveryPointCreator (Maybe Text)
rpcBackupPlanId = lens _rpcBackupPlanId (\ s a -> s{_rpcBackupPlanId = a})

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan; for example, @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@ .
rpcBackupPlanARN :: Lens' RecoveryPointCreator (Maybe Text)
rpcBackupPlanARN = lens _rpcBackupPlanARN (\ s a -> s{_rpcBackupPlanARN = a})

-- | Version IDs are unique, randomly generated, Unicode, UTF-8 encoded strings that are at most 1,024 bytes long. They cannot be edited.
rpcBackupPlanVersion :: Lens' RecoveryPointCreator (Maybe Text)
rpcBackupPlanVersion = lens _rpcBackupPlanVersion (\ s a -> s{_rpcBackupPlanVersion = a})

-- | Uniquely identifies a rule used to schedule the backup of a selection of resources.
rpcBackupRuleId :: Lens' RecoveryPointCreator (Maybe Text)
rpcBackupRuleId = lens _rpcBackupRuleId (\ s a -> s{_rpcBackupRuleId = a})

instance FromJSON RecoveryPointCreator where
        parseJSON
          = withObject "RecoveryPointCreator"
              (\ x ->
                 RecoveryPointCreator' <$>
                   (x .:? "BackupPlanId") <*> (x .:? "BackupPlanArn")
                     <*> (x .:? "BackupPlanVersion")
                     <*> (x .:? "BackupRuleId"))

instance Hashable RecoveryPointCreator where

instance NFData RecoveryPointCreator where

-- | Contains metadata about a restore job.
--
--
--
-- /See:/ 'restoreJobsListMember' smart constructor.
data RestoreJobsListMember = RestoreJobsListMember'
  { _rjlmStatus :: !(Maybe RestoreJobStatus)
  , _rjlmIAMRoleARN :: !(Maybe Text)
  , _rjlmExpectedCompletionTimeMinutes :: !(Maybe Integer)
  , _rjlmRestoreJobId :: !(Maybe Text)
  , _rjlmPercentDone :: !(Maybe Text)
  , _rjlmCreatedResourceARN :: !(Maybe Text)
  , _rjlmStatusMessage :: !(Maybe Text)
  , _rjlmRecoveryPointARN :: !(Maybe Text)
  , _rjlmBackupSizeInBytes :: !(Maybe Integer)
  , _rjlmCreationDate :: !(Maybe POSIX)
  , _rjlmCompletionDate :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreJobsListMember' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rjlmStatus' - A status code specifying the state of the job initiated by AWS Backup to restore a recovery point.
--
-- * 'rjlmIAMRoleARN' - Specifies the IAM role ARN used to create the target recovery point; for example, @arn:aws:iam::123456789012:role/S3Access@ .
--
-- * 'rjlmExpectedCompletionTimeMinutes' - The amount of time in minutes that a job restoring a recovery point is expected to take.
--
-- * 'rjlmRestoreJobId' - Uniquely identifies the job that restores a recovery point.
--
-- * 'rjlmPercentDone' - Contains an estimated percentage complete of a job at the time the job status was queried.
--
-- * 'rjlmCreatedResourceARN' - An Amazon Resource Name (ARN) that uniquely identifies a resource. The format of the ARN depends on the resource type.
--
-- * 'rjlmStatusMessage' - A detailed message explaining the status of the job to restore a recovery point.
--
-- * 'rjlmRecoveryPointARN' - An ARN that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
--
-- * 'rjlmBackupSizeInBytes' - The size, in bytes, of the restored resource.
--
-- * 'rjlmCreationDate' - The date and time a restore job is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'rjlmCompletionDate' - The date and time a job to restore a recovery point is completed, in Unix format and Coordinated Universal Time (UTC). The value of @CompletionDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
restoreJobsListMember
    :: RestoreJobsListMember
restoreJobsListMember =
  RestoreJobsListMember'
    { _rjlmStatus = Nothing
    , _rjlmIAMRoleARN = Nothing
    , _rjlmExpectedCompletionTimeMinutes = Nothing
    , _rjlmRestoreJobId = Nothing
    , _rjlmPercentDone = Nothing
    , _rjlmCreatedResourceARN = Nothing
    , _rjlmStatusMessage = Nothing
    , _rjlmRecoveryPointARN = Nothing
    , _rjlmBackupSizeInBytes = Nothing
    , _rjlmCreationDate = Nothing
    , _rjlmCompletionDate = Nothing
    }


-- | A status code specifying the state of the job initiated by AWS Backup to restore a recovery point.
rjlmStatus :: Lens' RestoreJobsListMember (Maybe RestoreJobStatus)
rjlmStatus = lens _rjlmStatus (\ s a -> s{_rjlmStatus = a})

-- | Specifies the IAM role ARN used to create the target recovery point; for example, @arn:aws:iam::123456789012:role/S3Access@ .
rjlmIAMRoleARN :: Lens' RestoreJobsListMember (Maybe Text)
rjlmIAMRoleARN = lens _rjlmIAMRoleARN (\ s a -> s{_rjlmIAMRoleARN = a})

-- | The amount of time in minutes that a job restoring a recovery point is expected to take.
rjlmExpectedCompletionTimeMinutes :: Lens' RestoreJobsListMember (Maybe Integer)
rjlmExpectedCompletionTimeMinutes = lens _rjlmExpectedCompletionTimeMinutes (\ s a -> s{_rjlmExpectedCompletionTimeMinutes = a})

-- | Uniquely identifies the job that restores a recovery point.
rjlmRestoreJobId :: Lens' RestoreJobsListMember (Maybe Text)
rjlmRestoreJobId = lens _rjlmRestoreJobId (\ s a -> s{_rjlmRestoreJobId = a})

-- | Contains an estimated percentage complete of a job at the time the job status was queried.
rjlmPercentDone :: Lens' RestoreJobsListMember (Maybe Text)
rjlmPercentDone = lens _rjlmPercentDone (\ s a -> s{_rjlmPercentDone = a})

-- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The format of the ARN depends on the resource type.
rjlmCreatedResourceARN :: Lens' RestoreJobsListMember (Maybe Text)
rjlmCreatedResourceARN = lens _rjlmCreatedResourceARN (\ s a -> s{_rjlmCreatedResourceARN = a})

-- | A detailed message explaining the status of the job to restore a recovery point.
rjlmStatusMessage :: Lens' RestoreJobsListMember (Maybe Text)
rjlmStatusMessage = lens _rjlmStatusMessage (\ s a -> s{_rjlmStatusMessage = a})

-- | An ARN that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
rjlmRecoveryPointARN :: Lens' RestoreJobsListMember (Maybe Text)
rjlmRecoveryPointARN = lens _rjlmRecoveryPointARN (\ s a -> s{_rjlmRecoveryPointARN = a})

-- | The size, in bytes, of the restored resource.
rjlmBackupSizeInBytes :: Lens' RestoreJobsListMember (Maybe Integer)
rjlmBackupSizeInBytes = lens _rjlmBackupSizeInBytes (\ s a -> s{_rjlmBackupSizeInBytes = a})

-- | The date and time a restore job is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
rjlmCreationDate :: Lens' RestoreJobsListMember (Maybe UTCTime)
rjlmCreationDate = lens _rjlmCreationDate (\ s a -> s{_rjlmCreationDate = a}) . mapping _Time

-- | The date and time a job to restore a recovery point is completed, in Unix format and Coordinated Universal Time (UTC). The value of @CompletionDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
rjlmCompletionDate :: Lens' RestoreJobsListMember (Maybe UTCTime)
rjlmCompletionDate = lens _rjlmCompletionDate (\ s a -> s{_rjlmCompletionDate = a}) . mapping _Time

instance FromJSON RestoreJobsListMember where
        parseJSON
          = withObject "RestoreJobsListMember"
              (\ x ->
                 RestoreJobsListMember' <$>
                   (x .:? "Status") <*> (x .:? "IamRoleArn") <*>
                     (x .:? "ExpectedCompletionTimeMinutes")
                     <*> (x .:? "RestoreJobId")
                     <*> (x .:? "PercentDone")
                     <*> (x .:? "CreatedResourceArn")
                     <*> (x .:? "StatusMessage")
                     <*> (x .:? "RecoveryPointArn")
                     <*> (x .:? "BackupSizeInBytes")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "CompletionDate"))

instance Hashable RestoreJobsListMember where

instance NFData RestoreJobsListMember where
