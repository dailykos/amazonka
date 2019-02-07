{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Backup.DescribeBackupJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata associated with creating a backup of a resource.
--
--
module Network.AWS.Backup.DescribeBackupJob
    (
    -- * Creating a Request
      describeBackupJob
    , DescribeBackupJob
    -- * Request Lenses
    , dbjBackupJobId

    -- * Destructuring the Response
    , describeBackupJobResponse
    , DescribeBackupJobResponse
    -- * Response Lenses
    , dbjrsIAMRoleARN
    , dbjrsState
    , dbjrsResourceType
    , dbjrsPercentDone
    , dbjrsStartBy
    , dbjrsCreatedBy
    , dbjrsExpectedCompletionDate
    , dbjrsBytesTransferred
    , dbjrsBackupVaultARN
    , dbjrsBackupJobId
    , dbjrsResourceARN
    , dbjrsStatusMessage
    , dbjrsRecoveryPointARN
    , dbjrsBackupSizeInBytes
    , dbjrsCreationDate
    , dbjrsCompletionDate
    , dbjrsBackupVaultName
    , dbjrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeBackupJob' smart constructor.
newtype DescribeBackupJob = DescribeBackupJob'
  { _dbjBackupJobId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBackupJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbjBackupJobId' - Uniquely identifies a request to AWS Backup to back up a resource.
describeBackupJob
    :: Text -- ^ 'dbjBackupJobId'
    -> DescribeBackupJob
describeBackupJob pBackupJobId_ =
  DescribeBackupJob' {_dbjBackupJobId = pBackupJobId_}


-- | Uniquely identifies a request to AWS Backup to back up a resource.
dbjBackupJobId :: Lens' DescribeBackupJob Text
dbjBackupJobId = lens _dbjBackupJobId (\ s a -> s{_dbjBackupJobId = a})

instance AWSRequest DescribeBackupJob where
        type Rs DescribeBackupJob = DescribeBackupJobResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 DescribeBackupJobResponse' <$>
                   (x .?> "IamRoleArn") <*> (x .?> "State") <*>
                     (x .?> "ResourceType")
                     <*> (x .?> "PercentDone")
                     <*> (x .?> "StartBy")
                     <*> (x .?> "CreatedBy")
                     <*> (x .?> "ExpectedCompletionDate")
                     <*> (x .?> "BytesTransferred")
                     <*> (x .?> "BackupVaultArn")
                     <*> (x .?> "BackupJobId")
                     <*> (x .?> "ResourceArn")
                     <*> (x .?> "StatusMessage")
                     <*> (x .?> "RecoveryPointArn")
                     <*> (x .?> "BackupSizeInBytes")
                     <*> (x .?> "CreationDate")
                     <*> (x .?> "CompletionDate")
                     <*> (x .?> "BackupVaultName")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeBackupJob where

instance NFData DescribeBackupJob where

instance ToHeaders DescribeBackupJob where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeBackupJob where
        toPath DescribeBackupJob'{..}
          = mconcat ["/backup-jobs/", toBS _dbjBackupJobId]

instance ToQuery DescribeBackupJob where
        toQuery = const mempty

-- | /See:/ 'describeBackupJobResponse' smart constructor.
data DescribeBackupJobResponse = DescribeBackupJobResponse'
  { _dbjrsIAMRoleARN :: !(Maybe Text)
  , _dbjrsState :: !(Maybe BackupJobState)
  , _dbjrsResourceType :: !(Maybe Text)
  , _dbjrsPercentDone :: !(Maybe Text)
  , _dbjrsStartBy :: !(Maybe POSIX)
  , _dbjrsCreatedBy :: !(Maybe RecoveryPointCreator)
  , _dbjrsExpectedCompletionDate :: !(Maybe POSIX)
  , _dbjrsBytesTransferred :: !(Maybe Integer)
  , _dbjrsBackupVaultARN :: !(Maybe Text)
  , _dbjrsBackupJobId :: !(Maybe Text)
  , _dbjrsResourceARN :: !(Maybe Text)
  , _dbjrsStatusMessage :: !(Maybe Text)
  , _dbjrsRecoveryPointARN :: !(Maybe Text)
  , _dbjrsBackupSizeInBytes :: !(Maybe Integer)
  , _dbjrsCreationDate :: !(Maybe POSIX)
  , _dbjrsCompletionDate :: !(Maybe POSIX)
  , _dbjrsBackupVaultName :: !(Maybe Text)
  , _dbjrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBackupJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbjrsIAMRoleARN' - Specifies the IAM role ARN used to create the target recovery point; for example, @arn:aws:iam::123456789012:role/S3Access@ .
--
-- * 'dbjrsState' - The current state of a resource recovery point.
--
-- * 'dbjrsResourceType' - The type of AWS resource to be backed-up; for example, an Amazon Elastic Block Store (Amazon EBS) volume or an Amazon Relational Database Service (Amazon RDS) database.
--
-- * 'dbjrsPercentDone' - Contains an estimated percentage that is complete of a job at the time the job status was queried.
--
-- * 'dbjrsStartBy' - Specifies the time in Unix format and Coordinated Universal Time (UTC) when a backup job must be started before it is canceled. The value is calculated by adding the start window to the scheduled time. So if the scheduled time were 6:00 PM and the start window is 2 hours, the @StartBy@ time would be 8:00 PM on the date specified. The value of @StartBy@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'dbjrsCreatedBy' - Contains identifying information about the creation of a backup job, including the @BackupPlanArn@ , @BackupPlanId@ , @BackupPlanVersion@ , and @BackupRuleId@ of the backup plan that is used to create it.
--
-- * 'dbjrsExpectedCompletionDate' - The date and time that a job to back up resources is expected to be completed, in Unix format and Coordinated Universal Time (UTC). The value of @ExpectedCompletionDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'dbjrsBytesTransferred' - The size in bytes transferred to a backup vault at the time that the job status was queried.
--
-- * 'dbjrsBackupVaultARN' - An Amazon Resource Name (ARN) that uniquely identifies a backup vault; for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@ .
--
-- * 'dbjrsBackupJobId' - Uniquely identifies a request to AWS Backup to back up a resource.
--
-- * 'dbjrsResourceARN' - An ARN that uniquely identifies a saved resource. The format of the ARN depends on the resource type.
--
-- * 'dbjrsStatusMessage' - A detailed message explaining the status of the job to back up a resource.
--
-- * 'dbjrsRecoveryPointARN' - An ARN that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
--
-- * 'dbjrsBackupSizeInBytes' - The size, in bytes, of a backup.
--
-- * 'dbjrsCreationDate' - The date and time that a backup job is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'dbjrsCompletionDate' - The date and time that a job to create a backup job is completed, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'dbjrsBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
--
-- * 'dbjrsResponseStatus' - -- | The response status code.
describeBackupJobResponse
    :: Int -- ^ 'dbjrsResponseStatus'
    -> DescribeBackupJobResponse
describeBackupJobResponse pResponseStatus_ =
  DescribeBackupJobResponse'
    { _dbjrsIAMRoleARN = Nothing
    , _dbjrsState = Nothing
    , _dbjrsResourceType = Nothing
    , _dbjrsPercentDone = Nothing
    , _dbjrsStartBy = Nothing
    , _dbjrsCreatedBy = Nothing
    , _dbjrsExpectedCompletionDate = Nothing
    , _dbjrsBytesTransferred = Nothing
    , _dbjrsBackupVaultARN = Nothing
    , _dbjrsBackupJobId = Nothing
    , _dbjrsResourceARN = Nothing
    , _dbjrsStatusMessage = Nothing
    , _dbjrsRecoveryPointARN = Nothing
    , _dbjrsBackupSizeInBytes = Nothing
    , _dbjrsCreationDate = Nothing
    , _dbjrsCompletionDate = Nothing
    , _dbjrsBackupVaultName = Nothing
    , _dbjrsResponseStatus = pResponseStatus_
    }


-- | Specifies the IAM role ARN used to create the target recovery point; for example, @arn:aws:iam::123456789012:role/S3Access@ .
dbjrsIAMRoleARN :: Lens' DescribeBackupJobResponse (Maybe Text)
dbjrsIAMRoleARN = lens _dbjrsIAMRoleARN (\ s a -> s{_dbjrsIAMRoleARN = a})

-- | The current state of a resource recovery point.
dbjrsState :: Lens' DescribeBackupJobResponse (Maybe BackupJobState)
dbjrsState = lens _dbjrsState (\ s a -> s{_dbjrsState = a})

-- | The type of AWS resource to be backed-up; for example, an Amazon Elastic Block Store (Amazon EBS) volume or an Amazon Relational Database Service (Amazon RDS) database.
dbjrsResourceType :: Lens' DescribeBackupJobResponse (Maybe Text)
dbjrsResourceType = lens _dbjrsResourceType (\ s a -> s{_dbjrsResourceType = a})

-- | Contains an estimated percentage that is complete of a job at the time the job status was queried.
dbjrsPercentDone :: Lens' DescribeBackupJobResponse (Maybe Text)
dbjrsPercentDone = lens _dbjrsPercentDone (\ s a -> s{_dbjrsPercentDone = a})

-- | Specifies the time in Unix format and Coordinated Universal Time (UTC) when a backup job must be started before it is canceled. The value is calculated by adding the start window to the scheduled time. So if the scheduled time were 6:00 PM and the start window is 2 hours, the @StartBy@ time would be 8:00 PM on the date specified. The value of @StartBy@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
dbjrsStartBy :: Lens' DescribeBackupJobResponse (Maybe UTCTime)
dbjrsStartBy = lens _dbjrsStartBy (\ s a -> s{_dbjrsStartBy = a}) . mapping _Time

-- | Contains identifying information about the creation of a backup job, including the @BackupPlanArn@ , @BackupPlanId@ , @BackupPlanVersion@ , and @BackupRuleId@ of the backup plan that is used to create it.
dbjrsCreatedBy :: Lens' DescribeBackupJobResponse (Maybe RecoveryPointCreator)
dbjrsCreatedBy = lens _dbjrsCreatedBy (\ s a -> s{_dbjrsCreatedBy = a})

-- | The date and time that a job to back up resources is expected to be completed, in Unix format and Coordinated Universal Time (UTC). The value of @ExpectedCompletionDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
dbjrsExpectedCompletionDate :: Lens' DescribeBackupJobResponse (Maybe UTCTime)
dbjrsExpectedCompletionDate = lens _dbjrsExpectedCompletionDate (\ s a -> s{_dbjrsExpectedCompletionDate = a}) . mapping _Time

-- | The size in bytes transferred to a backup vault at the time that the job status was queried.
dbjrsBytesTransferred :: Lens' DescribeBackupJobResponse (Maybe Integer)
dbjrsBytesTransferred = lens _dbjrsBytesTransferred (\ s a -> s{_dbjrsBytesTransferred = a})

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault; for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@ .
dbjrsBackupVaultARN :: Lens' DescribeBackupJobResponse (Maybe Text)
dbjrsBackupVaultARN = lens _dbjrsBackupVaultARN (\ s a -> s{_dbjrsBackupVaultARN = a})

-- | Uniquely identifies a request to AWS Backup to back up a resource.
dbjrsBackupJobId :: Lens' DescribeBackupJobResponse (Maybe Text)
dbjrsBackupJobId = lens _dbjrsBackupJobId (\ s a -> s{_dbjrsBackupJobId = a})

-- | An ARN that uniquely identifies a saved resource. The format of the ARN depends on the resource type.
dbjrsResourceARN :: Lens' DescribeBackupJobResponse (Maybe Text)
dbjrsResourceARN = lens _dbjrsResourceARN (\ s a -> s{_dbjrsResourceARN = a})

-- | A detailed message explaining the status of the job to back up a resource.
dbjrsStatusMessage :: Lens' DescribeBackupJobResponse (Maybe Text)
dbjrsStatusMessage = lens _dbjrsStatusMessage (\ s a -> s{_dbjrsStatusMessage = a})

-- | An ARN that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
dbjrsRecoveryPointARN :: Lens' DescribeBackupJobResponse (Maybe Text)
dbjrsRecoveryPointARN = lens _dbjrsRecoveryPointARN (\ s a -> s{_dbjrsRecoveryPointARN = a})

-- | The size, in bytes, of a backup.
dbjrsBackupSizeInBytes :: Lens' DescribeBackupJobResponse (Maybe Integer)
dbjrsBackupSizeInBytes = lens _dbjrsBackupSizeInBytes (\ s a -> s{_dbjrsBackupSizeInBytes = a})

-- | The date and time that a backup job is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
dbjrsCreationDate :: Lens' DescribeBackupJobResponse (Maybe UTCTime)
dbjrsCreationDate = lens _dbjrsCreationDate (\ s a -> s{_dbjrsCreationDate = a}) . mapping _Time

-- | The date and time that a job to create a backup job is completed, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
dbjrsCompletionDate :: Lens' DescribeBackupJobResponse (Maybe UTCTime)
dbjrsCompletionDate = lens _dbjrsCompletionDate (\ s a -> s{_dbjrsCompletionDate = a}) . mapping _Time

-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
dbjrsBackupVaultName :: Lens' DescribeBackupJobResponse (Maybe Text)
dbjrsBackupVaultName = lens _dbjrsBackupVaultName (\ s a -> s{_dbjrsBackupVaultName = a})

-- | -- | The response status code.
dbjrsResponseStatus :: Lens' DescribeBackupJobResponse Int
dbjrsResponseStatus = lens _dbjrsResponseStatus (\ s a -> s{_dbjrsResponseStatus = a})

instance NFData DescribeBackupJobResponse where
