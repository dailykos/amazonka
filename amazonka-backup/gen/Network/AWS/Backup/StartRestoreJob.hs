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
-- Module      : Network.AWS.Backup.StartRestoreJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Recovers the saved resource identified by an Amazon Resource Name (ARN). 
--
--
-- If the resource ARN is included in the request, then the last complete backup of that resource is recovered. If the ARN of a recovery point is supplied, then that recovery point is restored.
--
module Network.AWS.Backup.StartRestoreJob
    (
    -- * Creating a Request
      startRestoreJob
    , StartRestoreJob
    -- * Request Lenses
    , srjIdempotencyToken
    , srjResourceType
    , srjRecoveryPointARN
    , srjMetadata
    , srjIAMRoleARN

    -- * Destructuring the Response
    , startRestoreJobResponse
    , StartRestoreJobResponse
    -- * Response Lenses
    , srjrsRestoreJobId
    , srjrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startRestoreJob' smart constructor.
data StartRestoreJob = StartRestoreJob'
  { _srjIdempotencyToken :: !(Maybe Text)
  , _srjResourceType :: !(Maybe Text)
  , _srjRecoveryPointARN :: !Text
  , _srjMetadata :: !(Map Text Text)
  , _srjIAMRoleARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartRestoreJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srjIdempotencyToken' - A customer chosen string that can be used to distinguish between calls to @StartRestoreJob@ . Idempotency tokens time out after one hour. Therefore, if you call @StartRestoreJob@ multiple times with the same idempotency token within one hour, AWS Backup recognizes that you are requesting only one restore job and initiates only one. If you change the idempotency token for each call, AWS Backup recognizes that you are requesting to start multiple restores. 
--
-- * 'srjResourceType' - Starts a job to restore a recovery point for one of the following resources:     * @EBS@ for Amazon Elastic Block Store     * @SGW@ for AWS Storage Gateway     * @RDS@ for Amazon Relational Database Service     * @DDB@ for Amazon DynamoDB     * @EFS@ for Amazon Elastic File System
--
-- * 'srjRecoveryPointARN' - An ARN that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
--
-- * 'srjMetadata' - A set of metadata key-value pairs. Lists the metadata that the recovery point was created with.
--
-- * 'srjIAMRoleARN' - The Amazon Resource Name (ARN) of the IAM role that AWS Backup uses to create the target recovery point; for example, @arn:aws:iam::123456789012:role/S3Access@ .
startRestoreJob
    :: Text -- ^ 'srjRecoveryPointARN'
    -> Text -- ^ 'srjIAMRoleARN'
    -> StartRestoreJob
startRestoreJob pRecoveryPointARN_ pIAMRoleARN_ =
  StartRestoreJob'
    { _srjIdempotencyToken = Nothing
    , _srjResourceType = Nothing
    , _srjRecoveryPointARN = pRecoveryPointARN_
    , _srjMetadata = mempty
    , _srjIAMRoleARN = pIAMRoleARN_
    }


-- | A customer chosen string that can be used to distinguish between calls to @StartRestoreJob@ . Idempotency tokens time out after one hour. Therefore, if you call @StartRestoreJob@ multiple times with the same idempotency token within one hour, AWS Backup recognizes that you are requesting only one restore job and initiates only one. If you change the idempotency token for each call, AWS Backup recognizes that you are requesting to start multiple restores. 
srjIdempotencyToken :: Lens' StartRestoreJob (Maybe Text)
srjIdempotencyToken = lens _srjIdempotencyToken (\ s a -> s{_srjIdempotencyToken = a})

-- | Starts a job to restore a recovery point for one of the following resources:     * @EBS@ for Amazon Elastic Block Store     * @SGW@ for AWS Storage Gateway     * @RDS@ for Amazon Relational Database Service     * @DDB@ for Amazon DynamoDB     * @EFS@ for Amazon Elastic File System
srjResourceType :: Lens' StartRestoreJob (Maybe Text)
srjResourceType = lens _srjResourceType (\ s a -> s{_srjResourceType = a})

-- | An ARN that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
srjRecoveryPointARN :: Lens' StartRestoreJob Text
srjRecoveryPointARN = lens _srjRecoveryPointARN (\ s a -> s{_srjRecoveryPointARN = a})

-- | A set of metadata key-value pairs. Lists the metadata that the recovery point was created with.
srjMetadata :: Lens' StartRestoreJob (HashMap Text Text)
srjMetadata = lens _srjMetadata (\ s a -> s{_srjMetadata = a}) . _Map

-- | The Amazon Resource Name (ARN) of the IAM role that AWS Backup uses to create the target recovery point; for example, @arn:aws:iam::123456789012:role/S3Access@ .
srjIAMRoleARN :: Lens' StartRestoreJob Text
srjIAMRoleARN = lens _srjIAMRoleARN (\ s a -> s{_srjIAMRoleARN = a})

instance AWSRequest StartRestoreJob where
        type Rs StartRestoreJob = StartRestoreJobResponse
        request = putJSON backup
        response
          = receiveJSON
              (\ s h x ->
                 StartRestoreJobResponse' <$>
                   (x .?> "RestoreJobId") <*> (pure (fromEnum s)))

instance Hashable StartRestoreJob where

instance NFData StartRestoreJob where

instance ToHeaders StartRestoreJob where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartRestoreJob where
        toJSON StartRestoreJob'{..}
          = object
              (catMaybes
                 [("IdempotencyToken" .=) <$> _srjIdempotencyToken,
                  ("ResourceType" .=) <$> _srjResourceType,
                  Just ("RecoveryPointArn" .= _srjRecoveryPointARN),
                  Just ("Metadata" .= _srjMetadata),
                  Just ("IamRoleArn" .= _srjIAMRoleARN)])

instance ToPath StartRestoreJob where
        toPath = const "/restore-jobs"

instance ToQuery StartRestoreJob where
        toQuery = const mempty

-- | /See:/ 'startRestoreJobResponse' smart constructor.
data StartRestoreJobResponse = StartRestoreJobResponse'
  { _srjrsRestoreJobId :: !(Maybe Text)
  , _srjrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartRestoreJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srjrsRestoreJobId' - Uniquely identifies the job that restores a recovery point.
--
-- * 'srjrsResponseStatus' - -- | The response status code.
startRestoreJobResponse
    :: Int -- ^ 'srjrsResponseStatus'
    -> StartRestoreJobResponse
startRestoreJobResponse pResponseStatus_ =
  StartRestoreJobResponse'
    {_srjrsRestoreJobId = Nothing, _srjrsResponseStatus = pResponseStatus_}


-- | Uniquely identifies the job that restores a recovery point.
srjrsRestoreJobId :: Lens' StartRestoreJobResponse (Maybe Text)
srjrsRestoreJobId = lens _srjrsRestoreJobId (\ s a -> s{_srjrsRestoreJobId = a})

-- | -- | The response status code.
srjrsResponseStatus :: Lens' StartRestoreJobResponse Int
srjrsResponseStatus = lens _srjrsResponseStatus (\ s a -> s{_srjrsResponseStatus = a})

instance NFData StartRestoreJobResponse where
