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
-- Module      : Network.AWS.Backup.StartBackupJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a job to create a one-time backup of the specified resource.
--
--
module Network.AWS.Backup.StartBackupJob
    (
    -- * Creating a Request
      startBackupJob
    , StartBackupJob
    -- * Request Lenses
    , sbjIdempotencyToken
    , sbjLifecycle
    , sbjRecoveryPointTags
    , sbjCompleteWindowMinutes
    , sbjStartWindowMinutes
    , sbjBackupVaultName
    , sbjResourceARN
    , sbjIAMRoleARN

    -- * Destructuring the Response
    , startBackupJobResponse
    , StartBackupJobResponse
    -- * Response Lenses
    , sbjrsBackupJobId
    , sbjrsRecoveryPointARN
    , sbjrsCreationDate
    , sbjrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startBackupJob' smart constructor.
data StartBackupJob = StartBackupJob'
  { _sbjIdempotencyToken :: !(Maybe Text)
  , _sbjLifecycle :: !(Maybe Lifecycle)
  , _sbjRecoveryPointTags :: !(Maybe (Sensitive (Map Text Text)))
  , _sbjCompleteWindowMinutes :: !(Maybe Integer)
  , _sbjStartWindowMinutes :: !(Maybe Integer)
  , _sbjBackupVaultName :: !Text
  , _sbjResourceARN :: !Text
  , _sbjIAMRoleARN :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartBackupJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbjIdempotencyToken' - A customer chosen string that can be used to distinguish between calls to @StartBackupJob@ . Idempotency tokens time out after one hour. Therefore, if you call @StartBackupJob@ multiple times with the same idempotency token within one hour, AWS Backup recognizes that you are requesting only one backup job and initiates only one. If you change the idempotency token for each call, AWS Backup recognizes that you are requesting to start multiple backups.
--
-- * 'sbjLifecycle' - The lifecycle defines when a protected resource is transitioned to cold storage and when it expires. AWS Backup will transition and expire backups automatically according to the lifecycle that you define.  Backups transitioned to cold storage must be stored in cold storage for a minimum of 90 days. Therefore, the “expire after days” setting must be 90 days greater than the “transition to cold after days” setting. The “transition to cold after days” setting cannot be changed after a backup has been transitioned to cold. 
--
-- * 'sbjRecoveryPointTags' - To help organize your resources, you can assign your own metadata to the resources that you create. Each tag is a key-value pair.
--
-- * 'sbjCompleteWindowMinutes' - The amount of time AWS Backup attempts a backup before canceling the job and returning an error.
--
-- * 'sbjStartWindowMinutes' - The amount of time in minutes before beginning a backup.
--
-- * 'sbjBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
--
-- * 'sbjResourceARN' - An Amazon Resource Name (ARN) that uniquely identifies a resource. The format of the ARN depends on the resource type.
--
-- * 'sbjIAMRoleARN' - Specifies the IAM role ARN used to create the target recovery point; for example, @arn:aws:iam::123456789012:role/S3Access@ .
startBackupJob
    :: Text -- ^ 'sbjBackupVaultName'
    -> Text -- ^ 'sbjResourceARN'
    -> Text -- ^ 'sbjIAMRoleARN'
    -> StartBackupJob
startBackupJob pBackupVaultName_ pResourceARN_ pIAMRoleARN_ =
  StartBackupJob'
    { _sbjIdempotencyToken = Nothing
    , _sbjLifecycle = Nothing
    , _sbjRecoveryPointTags = Nothing
    , _sbjCompleteWindowMinutes = Nothing
    , _sbjStartWindowMinutes = Nothing
    , _sbjBackupVaultName = pBackupVaultName_
    , _sbjResourceARN = pResourceARN_
    , _sbjIAMRoleARN = pIAMRoleARN_
    }


-- | A customer chosen string that can be used to distinguish between calls to @StartBackupJob@ . Idempotency tokens time out after one hour. Therefore, if you call @StartBackupJob@ multiple times with the same idempotency token within one hour, AWS Backup recognizes that you are requesting only one backup job and initiates only one. If you change the idempotency token for each call, AWS Backup recognizes that you are requesting to start multiple backups.
sbjIdempotencyToken :: Lens' StartBackupJob (Maybe Text)
sbjIdempotencyToken = lens _sbjIdempotencyToken (\ s a -> s{_sbjIdempotencyToken = a})

-- | The lifecycle defines when a protected resource is transitioned to cold storage and when it expires. AWS Backup will transition and expire backups automatically according to the lifecycle that you define.  Backups transitioned to cold storage must be stored in cold storage for a minimum of 90 days. Therefore, the “expire after days” setting must be 90 days greater than the “transition to cold after days” setting. The “transition to cold after days” setting cannot be changed after a backup has been transitioned to cold. 
sbjLifecycle :: Lens' StartBackupJob (Maybe Lifecycle)
sbjLifecycle = lens _sbjLifecycle (\ s a -> s{_sbjLifecycle = a})

-- | To help organize your resources, you can assign your own metadata to the resources that you create. Each tag is a key-value pair.
sbjRecoveryPointTags :: Lens' StartBackupJob (Maybe (HashMap Text Text))
sbjRecoveryPointTags = lens _sbjRecoveryPointTags (\ s a -> s{_sbjRecoveryPointTags = a}) . mapping (_Sensitive . _Map)

-- | The amount of time AWS Backup attempts a backup before canceling the job and returning an error.
sbjCompleteWindowMinutes :: Lens' StartBackupJob (Maybe Integer)
sbjCompleteWindowMinutes = lens _sbjCompleteWindowMinutes (\ s a -> s{_sbjCompleteWindowMinutes = a})

-- | The amount of time in minutes before beginning a backup.
sbjStartWindowMinutes :: Lens' StartBackupJob (Maybe Integer)
sbjStartWindowMinutes = lens _sbjStartWindowMinutes (\ s a -> s{_sbjStartWindowMinutes = a})

-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
sbjBackupVaultName :: Lens' StartBackupJob Text
sbjBackupVaultName = lens _sbjBackupVaultName (\ s a -> s{_sbjBackupVaultName = a})

-- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The format of the ARN depends on the resource type.
sbjResourceARN :: Lens' StartBackupJob Text
sbjResourceARN = lens _sbjResourceARN (\ s a -> s{_sbjResourceARN = a})

-- | Specifies the IAM role ARN used to create the target recovery point; for example, @arn:aws:iam::123456789012:role/S3Access@ .
sbjIAMRoleARN :: Lens' StartBackupJob Text
sbjIAMRoleARN = lens _sbjIAMRoleARN (\ s a -> s{_sbjIAMRoleARN = a})

instance AWSRequest StartBackupJob where
        type Rs StartBackupJob = StartBackupJobResponse
        request = putJSON backup
        response
          = receiveJSON
              (\ s h x ->
                 StartBackupJobResponse' <$>
                   (x .?> "BackupJobId") <*> (x .?> "RecoveryPointArn")
                     <*> (x .?> "CreationDate")
                     <*> (pure (fromEnum s)))

instance Hashable StartBackupJob where

instance NFData StartBackupJob where

instance ToHeaders StartBackupJob where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartBackupJob where
        toJSON StartBackupJob'{..}
          = object
              (catMaybes
                 [("IdempotencyToken" .=) <$> _sbjIdempotencyToken,
                  ("Lifecycle" .=) <$> _sbjLifecycle,
                  ("RecoveryPointTags" .=) <$> _sbjRecoveryPointTags,
                  ("CompleteWindowMinutes" .=) <$>
                    _sbjCompleteWindowMinutes,
                  ("StartWindowMinutes" .=) <$> _sbjStartWindowMinutes,
                  Just ("BackupVaultName" .= _sbjBackupVaultName),
                  Just ("ResourceArn" .= _sbjResourceARN),
                  Just ("IamRoleArn" .= _sbjIAMRoleARN)])

instance ToPath StartBackupJob where
        toPath = const "/backup-jobs"

instance ToQuery StartBackupJob where
        toQuery = const mempty

-- | /See:/ 'startBackupJobResponse' smart constructor.
data StartBackupJobResponse = StartBackupJobResponse'
  { _sbjrsBackupJobId :: !(Maybe Text)
  , _sbjrsRecoveryPointARN :: !(Maybe Text)
  , _sbjrsCreationDate :: !(Maybe POSIX)
  , _sbjrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartBackupJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbjrsBackupJobId' - Uniquely identifies a request to AWS Backup to back up a resource.
--
-- * 'sbjrsRecoveryPointARN' - An ARN that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
--
-- * 'sbjrsCreationDate' - The date and time that a backup job is started, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'sbjrsResponseStatus' - -- | The response status code.
startBackupJobResponse
    :: Int -- ^ 'sbjrsResponseStatus'
    -> StartBackupJobResponse
startBackupJobResponse pResponseStatus_ =
  StartBackupJobResponse'
    { _sbjrsBackupJobId = Nothing
    , _sbjrsRecoveryPointARN = Nothing
    , _sbjrsCreationDate = Nothing
    , _sbjrsResponseStatus = pResponseStatus_
    }


-- | Uniquely identifies a request to AWS Backup to back up a resource.
sbjrsBackupJobId :: Lens' StartBackupJobResponse (Maybe Text)
sbjrsBackupJobId = lens _sbjrsBackupJobId (\ s a -> s{_sbjrsBackupJobId = a})

-- | An ARN that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
sbjrsRecoveryPointARN :: Lens' StartBackupJobResponse (Maybe Text)
sbjrsRecoveryPointARN = lens _sbjrsRecoveryPointARN (\ s a -> s{_sbjrsRecoveryPointARN = a})

-- | The date and time that a backup job is started, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
sbjrsCreationDate :: Lens' StartBackupJobResponse (Maybe UTCTime)
sbjrsCreationDate = lens _sbjrsCreationDate (\ s a -> s{_sbjrsCreationDate = a}) . mapping _Time

-- | -- | The response status code.
sbjrsResponseStatus :: Lens' StartBackupJobResponse Int
sbjrsResponseStatus = lens _sbjrsResponseStatus (\ s a -> s{_sbjrsResponseStatus = a})

instance NFData StartBackupJobResponse where
