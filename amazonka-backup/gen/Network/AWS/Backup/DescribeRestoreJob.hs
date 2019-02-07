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
-- Module      : Network.AWS.Backup.DescribeRestoreJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata associated with a restore job that is specified by a job ID.
--
--
module Network.AWS.Backup.DescribeRestoreJob
    (
    -- * Creating a Request
      describeRestoreJob
    , DescribeRestoreJob
    -- * Request Lenses
    , drjRestoreJobId

    -- * Destructuring the Response
    , describeRestoreJobResponse
    , DescribeRestoreJobResponse
    -- * Response Lenses
    , drjrsStatus
    , drjrsIAMRoleARN
    , drjrsExpectedCompletionTimeMinutes
    , drjrsRestoreJobId
    , drjrsPercentDone
    , drjrsCreatedResourceARN
    , drjrsStatusMessage
    , drjrsRecoveryPointARN
    , drjrsBackupSizeInBytes
    , drjrsCreationDate
    , drjrsCompletionDate
    , drjrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeRestoreJob' smart constructor.
newtype DescribeRestoreJob = DescribeRestoreJob'
  { _drjRestoreJobId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRestoreJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drjRestoreJobId' - Uniquely identifies the job that restores a recovery point.
describeRestoreJob
    :: Text -- ^ 'drjRestoreJobId'
    -> DescribeRestoreJob
describeRestoreJob pRestoreJobId_ =
  DescribeRestoreJob' {_drjRestoreJobId = pRestoreJobId_}


-- | Uniquely identifies the job that restores a recovery point.
drjRestoreJobId :: Lens' DescribeRestoreJob Text
drjRestoreJobId = lens _drjRestoreJobId (\ s a -> s{_drjRestoreJobId = a})

instance AWSRequest DescribeRestoreJob where
        type Rs DescribeRestoreJob =
             DescribeRestoreJobResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 DescribeRestoreJobResponse' <$>
                   (x .?> "Status") <*> (x .?> "IamRoleArn") <*>
                     (x .?> "ExpectedCompletionTimeMinutes")
                     <*> (x .?> "RestoreJobId")
                     <*> (x .?> "PercentDone")
                     <*> (x .?> "CreatedResourceArn")
                     <*> (x .?> "StatusMessage")
                     <*> (x .?> "RecoveryPointArn")
                     <*> (x .?> "BackupSizeInBytes")
                     <*> (x .?> "CreationDate")
                     <*> (x .?> "CompletionDate")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeRestoreJob where

instance NFData DescribeRestoreJob where

instance ToHeaders DescribeRestoreJob where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeRestoreJob where
        toPath DescribeRestoreJob'{..}
          = mconcat ["/restore-jobs/", toBS _drjRestoreJobId]

instance ToQuery DescribeRestoreJob where
        toQuery = const mempty

-- | /See:/ 'describeRestoreJobResponse' smart constructor.
data DescribeRestoreJobResponse = DescribeRestoreJobResponse'
  { _drjrsStatus :: !(Maybe RestoreJobStatus)
  , _drjrsIAMRoleARN :: !(Maybe Text)
  , _drjrsExpectedCompletionTimeMinutes :: !(Maybe Integer)
  , _drjrsRestoreJobId :: !(Maybe Text)
  , _drjrsPercentDone :: !(Maybe Text)
  , _drjrsCreatedResourceARN :: !(Maybe Text)
  , _drjrsStatusMessage :: !(Maybe Text)
  , _drjrsRecoveryPointARN :: !(Maybe Text)
  , _drjrsBackupSizeInBytes :: !(Maybe Integer)
  , _drjrsCreationDate :: !(Maybe POSIX)
  , _drjrsCompletionDate :: !(Maybe POSIX)
  , _drjrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRestoreJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drjrsStatus' - Status code specifying the state of the job that is initiated by AWS Backup to restore a recovery point.
--
-- * 'drjrsIAMRoleARN' - Specifies the IAM role ARN used to create the target recovery point; for example, @arn:aws:iam::123456789012:role/S3Access@ .
--
-- * 'drjrsExpectedCompletionTimeMinutes' - The amount of time in minutes that a job restoring a recovery point is expected to take.
--
-- * 'drjrsRestoreJobId' - Uniquely identifies the job that restores a recovery point.
--
-- * 'drjrsPercentDone' - Contains an estimated percentage that is complete of a job at the time the job status was queried.
--
-- * 'drjrsCreatedResourceARN' - An Amazon Resource Name (ARN) that uniquely identifies a resource whose recovery point is being restored. The format of the ARN depends on the resource type of the backed-up resource.
--
-- * 'drjrsStatusMessage' - A detailed message explaining the status of a job to restore a recovery point.
--
-- * 'drjrsRecoveryPointARN' - An ARN that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
--
-- * 'drjrsBackupSizeInBytes' - The size, in bytes, of the restored resource.
--
-- * 'drjrsCreationDate' - The date and time that a restore job is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'drjrsCompletionDate' - The date and time that a job to restore a recovery point is completed, in Unix format and Coordinated Universal Time (UTC). The value of @CompletionDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'drjrsResponseStatus' - -- | The response status code.
describeRestoreJobResponse
    :: Int -- ^ 'drjrsResponseStatus'
    -> DescribeRestoreJobResponse
describeRestoreJobResponse pResponseStatus_ =
  DescribeRestoreJobResponse'
    { _drjrsStatus = Nothing
    , _drjrsIAMRoleARN = Nothing
    , _drjrsExpectedCompletionTimeMinutes = Nothing
    , _drjrsRestoreJobId = Nothing
    , _drjrsPercentDone = Nothing
    , _drjrsCreatedResourceARN = Nothing
    , _drjrsStatusMessage = Nothing
    , _drjrsRecoveryPointARN = Nothing
    , _drjrsBackupSizeInBytes = Nothing
    , _drjrsCreationDate = Nothing
    , _drjrsCompletionDate = Nothing
    , _drjrsResponseStatus = pResponseStatus_
    }


-- | Status code specifying the state of the job that is initiated by AWS Backup to restore a recovery point.
drjrsStatus :: Lens' DescribeRestoreJobResponse (Maybe RestoreJobStatus)
drjrsStatus = lens _drjrsStatus (\ s a -> s{_drjrsStatus = a})

-- | Specifies the IAM role ARN used to create the target recovery point; for example, @arn:aws:iam::123456789012:role/S3Access@ .
drjrsIAMRoleARN :: Lens' DescribeRestoreJobResponse (Maybe Text)
drjrsIAMRoleARN = lens _drjrsIAMRoleARN (\ s a -> s{_drjrsIAMRoleARN = a})

-- | The amount of time in minutes that a job restoring a recovery point is expected to take.
drjrsExpectedCompletionTimeMinutes :: Lens' DescribeRestoreJobResponse (Maybe Integer)
drjrsExpectedCompletionTimeMinutes = lens _drjrsExpectedCompletionTimeMinutes (\ s a -> s{_drjrsExpectedCompletionTimeMinutes = a})

-- | Uniquely identifies the job that restores a recovery point.
drjrsRestoreJobId :: Lens' DescribeRestoreJobResponse (Maybe Text)
drjrsRestoreJobId = lens _drjrsRestoreJobId (\ s a -> s{_drjrsRestoreJobId = a})

-- | Contains an estimated percentage that is complete of a job at the time the job status was queried.
drjrsPercentDone :: Lens' DescribeRestoreJobResponse (Maybe Text)
drjrsPercentDone = lens _drjrsPercentDone (\ s a -> s{_drjrsPercentDone = a})

-- | An Amazon Resource Name (ARN) that uniquely identifies a resource whose recovery point is being restored. The format of the ARN depends on the resource type of the backed-up resource.
drjrsCreatedResourceARN :: Lens' DescribeRestoreJobResponse (Maybe Text)
drjrsCreatedResourceARN = lens _drjrsCreatedResourceARN (\ s a -> s{_drjrsCreatedResourceARN = a})

-- | A detailed message explaining the status of a job to restore a recovery point.
drjrsStatusMessage :: Lens' DescribeRestoreJobResponse (Maybe Text)
drjrsStatusMessage = lens _drjrsStatusMessage (\ s a -> s{_drjrsStatusMessage = a})

-- | An ARN that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
drjrsRecoveryPointARN :: Lens' DescribeRestoreJobResponse (Maybe Text)
drjrsRecoveryPointARN = lens _drjrsRecoveryPointARN (\ s a -> s{_drjrsRecoveryPointARN = a})

-- | The size, in bytes, of the restored resource.
drjrsBackupSizeInBytes :: Lens' DescribeRestoreJobResponse (Maybe Integer)
drjrsBackupSizeInBytes = lens _drjrsBackupSizeInBytes (\ s a -> s{_drjrsBackupSizeInBytes = a})

-- | The date and time that a restore job is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
drjrsCreationDate :: Lens' DescribeRestoreJobResponse (Maybe UTCTime)
drjrsCreationDate = lens _drjrsCreationDate (\ s a -> s{_drjrsCreationDate = a}) . mapping _Time

-- | The date and time that a job to restore a recovery point is completed, in Unix format and Coordinated Universal Time (UTC). The value of @CompletionDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
drjrsCompletionDate :: Lens' DescribeRestoreJobResponse (Maybe UTCTime)
drjrsCompletionDate = lens _drjrsCompletionDate (\ s a -> s{_drjrsCompletionDate = a}) . mapping _Time

-- | -- | The response status code.
drjrsResponseStatus :: Lens' DescribeRestoreJobResponse Int
drjrsResponseStatus = lens _drjrsResponseStatus (\ s a -> s{_drjrsResponseStatus = a})

instance NFData DescribeRestoreJobResponse where
