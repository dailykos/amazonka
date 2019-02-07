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
-- Module      : Network.AWS.Backup.UpdateRecoveryPointLifecycle
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the transition lifecycle of a recovery point.
--
--
-- The lifecycle defines when a protected resource is transitioned to cold storage and when it expires. AWS Backup transitions and expires backups automatically according to the lifecycle that you define. 
--
-- Backups transitioned to cold storage must be stored in cold storage for a minimum of 90 days. Therefore, the “expire after days” setting must be 90 days greater than the “transition to cold after days” setting. The “transition to cold after days” setting cannot be changed after a backup has been transitioned to cold. 
--
module Network.AWS.Backup.UpdateRecoveryPointLifecycle
    (
    -- * Creating a Request
      updateRecoveryPointLifecycle
    , UpdateRecoveryPointLifecycle
    -- * Request Lenses
    , urplLifecycle
    , urplBackupVaultName
    , urplRecoveryPointARN

    -- * Destructuring the Response
    , updateRecoveryPointLifecycleResponse
    , UpdateRecoveryPointLifecycleResponse
    -- * Response Lenses
    , urplrsCalculatedLifecycle
    , urplrsLifecycle
    , urplrsBackupVaultARN
    , urplrsRecoveryPointARN
    , urplrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateRecoveryPointLifecycle' smart constructor.
data UpdateRecoveryPointLifecycle = UpdateRecoveryPointLifecycle'
  { _urplLifecycle :: !(Maybe Lifecycle)
  , _urplBackupVaultName :: !Text
  , _urplRecoveryPointARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRecoveryPointLifecycle' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urplLifecycle' - The lifecycle defines when a protected resource is transitioned to cold storage and when it expires. AWS Backup transitions and expires backups automatically according to the lifecycle that you define.  Backups transitioned to cold storage must be stored in cold storage for a minimum of 90 days. Therefore, the “expire after days” setting must be 90 days greater than the “transition to cold after days” setting. The “transition to cold after days” setting cannot be changed after a backup has been transitioned to cold. 
--
-- * 'urplBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
--
-- * 'urplRecoveryPointARN' - An Amazon Resource Name (ARN) that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
updateRecoveryPointLifecycle
    :: Text -- ^ 'urplBackupVaultName'
    -> Text -- ^ 'urplRecoveryPointARN'
    -> UpdateRecoveryPointLifecycle
updateRecoveryPointLifecycle pBackupVaultName_ pRecoveryPointARN_ =
  UpdateRecoveryPointLifecycle'
    { _urplLifecycle = Nothing
    , _urplBackupVaultName = pBackupVaultName_
    , _urplRecoveryPointARN = pRecoveryPointARN_
    }


-- | The lifecycle defines when a protected resource is transitioned to cold storage and when it expires. AWS Backup transitions and expires backups automatically according to the lifecycle that you define.  Backups transitioned to cold storage must be stored in cold storage for a minimum of 90 days. Therefore, the “expire after days” setting must be 90 days greater than the “transition to cold after days” setting. The “transition to cold after days” setting cannot be changed after a backup has been transitioned to cold. 
urplLifecycle :: Lens' UpdateRecoveryPointLifecycle (Maybe Lifecycle)
urplLifecycle = lens _urplLifecycle (\ s a -> s{_urplLifecycle = a})

-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
urplBackupVaultName :: Lens' UpdateRecoveryPointLifecycle Text
urplBackupVaultName = lens _urplBackupVaultName (\ s a -> s{_urplBackupVaultName = a})

-- | An Amazon Resource Name (ARN) that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
urplRecoveryPointARN :: Lens' UpdateRecoveryPointLifecycle Text
urplRecoveryPointARN = lens _urplRecoveryPointARN (\ s a -> s{_urplRecoveryPointARN = a})

instance AWSRequest UpdateRecoveryPointLifecycle
         where
        type Rs UpdateRecoveryPointLifecycle =
             UpdateRecoveryPointLifecycleResponse
        request = postJSON backup
        response
          = receiveJSON
              (\ s h x ->
                 UpdateRecoveryPointLifecycleResponse' <$>
                   (x .?> "CalculatedLifecycle") <*> (x .?> "Lifecycle")
                     <*> (x .?> "BackupVaultArn")
                     <*> (x .?> "RecoveryPointArn")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateRecoveryPointLifecycle where

instance NFData UpdateRecoveryPointLifecycle where

instance ToHeaders UpdateRecoveryPointLifecycle where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateRecoveryPointLifecycle where
        toJSON UpdateRecoveryPointLifecycle'{..}
          = object
              (catMaybes [("Lifecycle" .=) <$> _urplLifecycle])

instance ToPath UpdateRecoveryPointLifecycle where
        toPath UpdateRecoveryPointLifecycle'{..}
          = mconcat
              ["/backup-vaults/", toBS _urplBackupVaultName,
               "/recovery-points/", toBS _urplRecoveryPointARN]

instance ToQuery UpdateRecoveryPointLifecycle where
        toQuery = const mempty

-- | /See:/ 'updateRecoveryPointLifecycleResponse' smart constructor.
data UpdateRecoveryPointLifecycleResponse = UpdateRecoveryPointLifecycleResponse'
  { _urplrsCalculatedLifecycle :: !(Maybe CalculatedLifecycle)
  , _urplrsLifecycle :: !(Maybe Lifecycle)
  , _urplrsBackupVaultARN :: !(Maybe Text)
  , _urplrsRecoveryPointARN :: !(Maybe Text)
  , _urplrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRecoveryPointLifecycleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urplrsCalculatedLifecycle' - A @CalculatedLifecycle@ object containing @DeleteAt@ and @MoveToColdStorageAt@ timestamps.
--
-- * 'urplrsLifecycle' - The lifecycle defines when a protected resource is transitioned to cold storage and when it expires. AWS Backup transitions and expires backups automatically according to the lifecycle that you define.  Backups transitioned to cold storage must be stored in cold storage for a minimum of 90 days. Therefore, the “expire after days” setting must be 90 days greater than the “transition to cold after days” setting. The “transition to cold after days” setting cannot be changed after a backup has been transitioned to cold. 
--
-- * 'urplrsBackupVaultARN' - An ARN that uniquely identifies a backup vault; for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@ .
--
-- * 'urplrsRecoveryPointARN' - An Amazon Resource Name (ARN) that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
--
-- * 'urplrsResponseStatus' - -- | The response status code.
updateRecoveryPointLifecycleResponse
    :: Int -- ^ 'urplrsResponseStatus'
    -> UpdateRecoveryPointLifecycleResponse
updateRecoveryPointLifecycleResponse pResponseStatus_ =
  UpdateRecoveryPointLifecycleResponse'
    { _urplrsCalculatedLifecycle = Nothing
    , _urplrsLifecycle = Nothing
    , _urplrsBackupVaultARN = Nothing
    , _urplrsRecoveryPointARN = Nothing
    , _urplrsResponseStatus = pResponseStatus_
    }


-- | A @CalculatedLifecycle@ object containing @DeleteAt@ and @MoveToColdStorageAt@ timestamps.
urplrsCalculatedLifecycle :: Lens' UpdateRecoveryPointLifecycleResponse (Maybe CalculatedLifecycle)
urplrsCalculatedLifecycle = lens _urplrsCalculatedLifecycle (\ s a -> s{_urplrsCalculatedLifecycle = a})

-- | The lifecycle defines when a protected resource is transitioned to cold storage and when it expires. AWS Backup transitions and expires backups automatically according to the lifecycle that you define.  Backups transitioned to cold storage must be stored in cold storage for a minimum of 90 days. Therefore, the “expire after days” setting must be 90 days greater than the “transition to cold after days” setting. The “transition to cold after days” setting cannot be changed after a backup has been transitioned to cold. 
urplrsLifecycle :: Lens' UpdateRecoveryPointLifecycleResponse (Maybe Lifecycle)
urplrsLifecycle = lens _urplrsLifecycle (\ s a -> s{_urplrsLifecycle = a})

-- | An ARN that uniquely identifies a backup vault; for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@ .
urplrsBackupVaultARN :: Lens' UpdateRecoveryPointLifecycleResponse (Maybe Text)
urplrsBackupVaultARN = lens _urplrsBackupVaultARN (\ s a -> s{_urplrsBackupVaultARN = a})

-- | An Amazon Resource Name (ARN) that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
urplrsRecoveryPointARN :: Lens' UpdateRecoveryPointLifecycleResponse (Maybe Text)
urplrsRecoveryPointARN = lens _urplrsRecoveryPointARN (\ s a -> s{_urplrsRecoveryPointARN = a})

-- | -- | The response status code.
urplrsResponseStatus :: Lens' UpdateRecoveryPointLifecycleResponse Int
urplrsResponseStatus = lens _urplrsResponseStatus (\ s a -> s{_urplrsResponseStatus = a})

instance NFData UpdateRecoveryPointLifecycleResponse
         where
