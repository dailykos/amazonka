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
-- Module      : Network.AWS.Backup.DescribeRecoveryPoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata associated with a recovery point, including ID, status, encryption, and lifecycle.
--
--
module Network.AWS.Backup.DescribeRecoveryPoint
    (
    -- * Creating a Request
      describeRecoveryPoint
    , DescribeRecoveryPoint
    -- * Request Lenses
    , drpBackupVaultName
    , drpRecoveryPointARN

    -- * Destructuring the Response
    , describeRecoveryPointResponse
    , DescribeRecoveryPointResponse
    -- * Response Lenses
    , drprsIsEncrypted
    , drprsStatus
    , drprsIAMRoleARN
    , drprsResourceType
    , drprsCreatedBy
    , drprsCalculatedLifecycle
    , drprsLifecycle
    , drprsBackupVaultARN
    , drprsLastRestoreTime
    , drprsResourceARN
    , drprsStorageClass
    , drprsRecoveryPointARN
    , drprsEncryptionKeyARN
    , drprsBackupSizeInBytes
    , drprsCreationDate
    , drprsCompletionDate
    , drprsBackupVaultName
    , drprsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeRecoveryPoint' smart constructor.
data DescribeRecoveryPoint = DescribeRecoveryPoint'
  { _drpBackupVaultName :: !Text
  , _drpRecoveryPointARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRecoveryPoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drpBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
--
-- * 'drpRecoveryPointARN' - An Amazon Resource Name (ARN) that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
describeRecoveryPoint
    :: Text -- ^ 'drpBackupVaultName'
    -> Text -- ^ 'drpRecoveryPointARN'
    -> DescribeRecoveryPoint
describeRecoveryPoint pBackupVaultName_ pRecoveryPointARN_ =
  DescribeRecoveryPoint'
    { _drpBackupVaultName = pBackupVaultName_
    , _drpRecoveryPointARN = pRecoveryPointARN_
    }


-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
drpBackupVaultName :: Lens' DescribeRecoveryPoint Text
drpBackupVaultName = lens _drpBackupVaultName (\ s a -> s{_drpBackupVaultName = a})

-- | An Amazon Resource Name (ARN) that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
drpRecoveryPointARN :: Lens' DescribeRecoveryPoint Text
drpRecoveryPointARN = lens _drpRecoveryPointARN (\ s a -> s{_drpRecoveryPointARN = a})

instance AWSRequest DescribeRecoveryPoint where
        type Rs DescribeRecoveryPoint =
             DescribeRecoveryPointResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 DescribeRecoveryPointResponse' <$>
                   (x .?> "IsEncrypted") <*> (x .?> "Status") <*>
                     (x .?> "IamRoleArn")
                     <*> (x .?> "ResourceType")
                     <*> (x .?> "CreatedBy")
                     <*> (x .?> "CalculatedLifecycle")
                     <*> (x .?> "Lifecycle")
                     <*> (x .?> "BackupVaultArn")
                     <*> (x .?> "LastRestoreTime")
                     <*> (x .?> "ResourceArn")
                     <*> (x .?> "StorageClass")
                     <*> (x .?> "RecoveryPointArn")
                     <*> (x .?> "EncryptionKeyArn")
                     <*> (x .?> "BackupSizeInBytes")
                     <*> (x .?> "CreationDate")
                     <*> (x .?> "CompletionDate")
                     <*> (x .?> "BackupVaultName")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeRecoveryPoint where

instance NFData DescribeRecoveryPoint where

instance ToHeaders DescribeRecoveryPoint where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeRecoveryPoint where
        toPath DescribeRecoveryPoint'{..}
          = mconcat
              ["/backup-vaults/", toBS _drpBackupVaultName,
               "/recovery-points/", toBS _drpRecoveryPointARN]

instance ToQuery DescribeRecoveryPoint where
        toQuery = const mempty

-- | /See:/ 'describeRecoveryPointResponse' smart constructor.
data DescribeRecoveryPointResponse = DescribeRecoveryPointResponse'
  { _drprsIsEncrypted :: !(Maybe Bool)
  , _drprsStatus :: !(Maybe RecoveryPointStatus)
  , _drprsIAMRoleARN :: !(Maybe Text)
  , _drprsResourceType :: !(Maybe Text)
  , _drprsCreatedBy :: !(Maybe RecoveryPointCreator)
  , _drprsCalculatedLifecycle :: !(Maybe CalculatedLifecycle)
  , _drprsLifecycle :: !(Maybe Lifecycle)
  , _drprsBackupVaultARN :: !(Maybe Text)
  , _drprsLastRestoreTime :: !(Maybe POSIX)
  , _drprsResourceARN :: !(Maybe Text)
  , _drprsStorageClass :: !(Maybe StorageClass)
  , _drprsRecoveryPointARN :: !(Maybe Text)
  , _drprsEncryptionKeyARN :: !(Maybe Text)
  , _drprsBackupSizeInBytes :: !(Maybe Integer)
  , _drprsCreationDate :: !(Maybe POSIX)
  , _drprsCompletionDate :: !(Maybe POSIX)
  , _drprsBackupVaultName :: !(Maybe Text)
  , _drprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRecoveryPointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drprsIsEncrypted' - A Boolean value that is returned as @TRUE@ if the specified recovery point is encrypted, or @FALSE@ if the recovery point is not encrypted.
--
-- * 'drprsStatus' - A status code specifying the state of the recovery point.
--
-- * 'drprsIAMRoleARN' - Specifies the IAM role ARN used to create the target recovery point; for example, @arn:aws:iam::123456789012:role/S3Access@ .
--
-- * 'drprsResourceType' - The type of AWS resource to save as a recovery point; for example, an Amazon Elastic Block Store (Amazon EBS) volume or an Amazon Relational Database Service (Amazon RDS) database.
--
-- * 'drprsCreatedBy' - Contains identifying information about the creation of a recovery point, including the @BackupPlanArn@ , @BackupPlanId@ , @BackupPlanVersion@ , and @BackupRuleId@ of the backup plan used to create it.
--
-- * 'drprsCalculatedLifecycle' - A @CalculatedLifecycle@ object containing @DeleteAt@ and @MoveToColdStorageAt@ timestamps.
--
-- * 'drprsLifecycle' - The lifecycle defines when a protected resource is transitioned to cold storage and when it expires. AWS Backup transitions and expires backups automatically according to the lifecycle that you define.  Backups that are transitioned to cold storage must be stored in cold storage for a minimum of 90 days. Therefore, the “expire after days” setting must be 90 days greater than the “transition to cold after days” setting. The “transition to cold after days” setting cannot be changed after a backup has been transitioned to cold. 
--
-- * 'drprsBackupVaultARN' - An ARN that uniquely identifies a backup vault; for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@ .
--
-- * 'drprsLastRestoreTime' - The date and time that a recovery point was last restored, in Unix format and Coordinated Universal Time (UTC). The value of @LastRestoreTime@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'drprsResourceARN' - An ARN that uniquely identifies a saved resource. The format of the ARN depends on the resource type.
--
-- * 'drprsStorageClass' - Specifies the storage class of the recovery point. Valid values are @WARM@ or @COLD@ .
--
-- * 'drprsRecoveryPointARN' - An ARN that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
--
-- * 'drprsEncryptionKeyARN' - The server-side encryption key used to protect your backups; for example, @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ .
--
-- * 'drprsBackupSizeInBytes' - The size, in bytes, of a backup.
--
-- * 'drprsCreationDate' - The date and time that a recovery point is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'drprsCompletionDate' - The date and time that a job to create a recovery point is completed, in Unix format and Coordinated Universal Time (UTC). The value of @CompletionDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'drprsBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the Region where they are created. They consist of lowercase letters, numbers, and hyphens.
--
-- * 'drprsResponseStatus' - -- | The response status code.
describeRecoveryPointResponse
    :: Int -- ^ 'drprsResponseStatus'
    -> DescribeRecoveryPointResponse
describeRecoveryPointResponse pResponseStatus_ =
  DescribeRecoveryPointResponse'
    { _drprsIsEncrypted = Nothing
    , _drprsStatus = Nothing
    , _drprsIAMRoleARN = Nothing
    , _drprsResourceType = Nothing
    , _drprsCreatedBy = Nothing
    , _drprsCalculatedLifecycle = Nothing
    , _drprsLifecycle = Nothing
    , _drprsBackupVaultARN = Nothing
    , _drprsLastRestoreTime = Nothing
    , _drprsResourceARN = Nothing
    , _drprsStorageClass = Nothing
    , _drprsRecoveryPointARN = Nothing
    , _drprsEncryptionKeyARN = Nothing
    , _drprsBackupSizeInBytes = Nothing
    , _drprsCreationDate = Nothing
    , _drprsCompletionDate = Nothing
    , _drprsBackupVaultName = Nothing
    , _drprsResponseStatus = pResponseStatus_
    }


-- | A Boolean value that is returned as @TRUE@ if the specified recovery point is encrypted, or @FALSE@ if the recovery point is not encrypted.
drprsIsEncrypted :: Lens' DescribeRecoveryPointResponse (Maybe Bool)
drprsIsEncrypted = lens _drprsIsEncrypted (\ s a -> s{_drprsIsEncrypted = a})

-- | A status code specifying the state of the recovery point.
drprsStatus :: Lens' DescribeRecoveryPointResponse (Maybe RecoveryPointStatus)
drprsStatus = lens _drprsStatus (\ s a -> s{_drprsStatus = a})

-- | Specifies the IAM role ARN used to create the target recovery point; for example, @arn:aws:iam::123456789012:role/S3Access@ .
drprsIAMRoleARN :: Lens' DescribeRecoveryPointResponse (Maybe Text)
drprsIAMRoleARN = lens _drprsIAMRoleARN (\ s a -> s{_drprsIAMRoleARN = a})

-- | The type of AWS resource to save as a recovery point; for example, an Amazon Elastic Block Store (Amazon EBS) volume or an Amazon Relational Database Service (Amazon RDS) database.
drprsResourceType :: Lens' DescribeRecoveryPointResponse (Maybe Text)
drprsResourceType = lens _drprsResourceType (\ s a -> s{_drprsResourceType = a})

-- | Contains identifying information about the creation of a recovery point, including the @BackupPlanArn@ , @BackupPlanId@ , @BackupPlanVersion@ , and @BackupRuleId@ of the backup plan used to create it.
drprsCreatedBy :: Lens' DescribeRecoveryPointResponse (Maybe RecoveryPointCreator)
drprsCreatedBy = lens _drprsCreatedBy (\ s a -> s{_drprsCreatedBy = a})

-- | A @CalculatedLifecycle@ object containing @DeleteAt@ and @MoveToColdStorageAt@ timestamps.
drprsCalculatedLifecycle :: Lens' DescribeRecoveryPointResponse (Maybe CalculatedLifecycle)
drprsCalculatedLifecycle = lens _drprsCalculatedLifecycle (\ s a -> s{_drprsCalculatedLifecycle = a})

-- | The lifecycle defines when a protected resource is transitioned to cold storage and when it expires. AWS Backup transitions and expires backups automatically according to the lifecycle that you define.  Backups that are transitioned to cold storage must be stored in cold storage for a minimum of 90 days. Therefore, the “expire after days” setting must be 90 days greater than the “transition to cold after days” setting. The “transition to cold after days” setting cannot be changed after a backup has been transitioned to cold. 
drprsLifecycle :: Lens' DescribeRecoveryPointResponse (Maybe Lifecycle)
drprsLifecycle = lens _drprsLifecycle (\ s a -> s{_drprsLifecycle = a})

-- | An ARN that uniquely identifies a backup vault; for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@ .
drprsBackupVaultARN :: Lens' DescribeRecoveryPointResponse (Maybe Text)
drprsBackupVaultARN = lens _drprsBackupVaultARN (\ s a -> s{_drprsBackupVaultARN = a})

-- | The date and time that a recovery point was last restored, in Unix format and Coordinated Universal Time (UTC). The value of @LastRestoreTime@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
drprsLastRestoreTime :: Lens' DescribeRecoveryPointResponse (Maybe UTCTime)
drprsLastRestoreTime = lens _drprsLastRestoreTime (\ s a -> s{_drprsLastRestoreTime = a}) . mapping _Time

-- | An ARN that uniquely identifies a saved resource. The format of the ARN depends on the resource type.
drprsResourceARN :: Lens' DescribeRecoveryPointResponse (Maybe Text)
drprsResourceARN = lens _drprsResourceARN (\ s a -> s{_drprsResourceARN = a})

-- | Specifies the storage class of the recovery point. Valid values are @WARM@ or @COLD@ .
drprsStorageClass :: Lens' DescribeRecoveryPointResponse (Maybe StorageClass)
drprsStorageClass = lens _drprsStorageClass (\ s a -> s{_drprsStorageClass = a})

-- | An ARN that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
drprsRecoveryPointARN :: Lens' DescribeRecoveryPointResponse (Maybe Text)
drprsRecoveryPointARN = lens _drprsRecoveryPointARN (\ s a -> s{_drprsRecoveryPointARN = a})

-- | The server-side encryption key used to protect your backups; for example, @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ .
drprsEncryptionKeyARN :: Lens' DescribeRecoveryPointResponse (Maybe Text)
drprsEncryptionKeyARN = lens _drprsEncryptionKeyARN (\ s a -> s{_drprsEncryptionKeyARN = a})

-- | The size, in bytes, of a backup.
drprsBackupSizeInBytes :: Lens' DescribeRecoveryPointResponse (Maybe Integer)
drprsBackupSizeInBytes = lens _drprsBackupSizeInBytes (\ s a -> s{_drprsBackupSizeInBytes = a})

-- | The date and time that a recovery point is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
drprsCreationDate :: Lens' DescribeRecoveryPointResponse (Maybe UTCTime)
drprsCreationDate = lens _drprsCreationDate (\ s a -> s{_drprsCreationDate = a}) . mapping _Time

-- | The date and time that a job to create a recovery point is completed, in Unix format and Coordinated Universal Time (UTC). The value of @CompletionDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
drprsCompletionDate :: Lens' DescribeRecoveryPointResponse (Maybe UTCTime)
drprsCompletionDate = lens _drprsCompletionDate (\ s a -> s{_drprsCompletionDate = a}) . mapping _Time

-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the Region where they are created. They consist of lowercase letters, numbers, and hyphens.
drprsBackupVaultName :: Lens' DescribeRecoveryPointResponse (Maybe Text)
drprsBackupVaultName = lens _drprsBackupVaultName (\ s a -> s{_drprsBackupVaultName = a})

-- | -- | The response status code.
drprsResponseStatus :: Lens' DescribeRecoveryPointResponse Int
drprsResponseStatus = lens _drprsResponseStatus (\ s a -> s{_drprsResponseStatus = a})

instance NFData DescribeRecoveryPointResponse where
