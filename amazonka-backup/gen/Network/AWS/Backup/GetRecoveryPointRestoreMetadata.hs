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
-- Module      : Network.AWS.Backup.GetRecoveryPointRestoreMetadata
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns two sets of metadata key-value pairs. The first set lists the metadata that the recovery point was created with. The second set lists the metadata key-value pairs that are required to restore the recovery point.
--
--
-- These sets can be the same, or the restore metadata set can contain different values if the target service to be restored has changed since the recovery point was created and now requires additional or different information in order to be restored.
--
module Network.AWS.Backup.GetRecoveryPointRestoreMetadata
    (
    -- * Creating a Request
      getRecoveryPointRestoreMetadata
    , GetRecoveryPointRestoreMetadata
    -- * Request Lenses
    , grprmBackupVaultName
    , grprmRecoveryPointARN

    -- * Destructuring the Response
    , getRecoveryPointRestoreMetadataResponse
    , GetRecoveryPointRestoreMetadataResponse
    -- * Response Lenses
    , grprmrsBackupVaultARN
    , grprmrsRecoveryPointARN
    , grprmrsRestoreMetadata
    , grprmrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRecoveryPointRestoreMetadata' smart constructor.
data GetRecoveryPointRestoreMetadata = GetRecoveryPointRestoreMetadata'
  { _grprmBackupVaultName :: !Text
  , _grprmRecoveryPointARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRecoveryPointRestoreMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grprmBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
--
-- * 'grprmRecoveryPointARN' - An Amazon Resource Name (ARN) that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
getRecoveryPointRestoreMetadata
    :: Text -- ^ 'grprmBackupVaultName'
    -> Text -- ^ 'grprmRecoveryPointARN'
    -> GetRecoveryPointRestoreMetadata
getRecoveryPointRestoreMetadata pBackupVaultName_ pRecoveryPointARN_ =
  GetRecoveryPointRestoreMetadata'
    { _grprmBackupVaultName = pBackupVaultName_
    , _grprmRecoveryPointARN = pRecoveryPointARN_
    }


-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
grprmBackupVaultName :: Lens' GetRecoveryPointRestoreMetadata Text
grprmBackupVaultName = lens _grprmBackupVaultName (\ s a -> s{_grprmBackupVaultName = a})

-- | An Amazon Resource Name (ARN) that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
grprmRecoveryPointARN :: Lens' GetRecoveryPointRestoreMetadata Text
grprmRecoveryPointARN = lens _grprmRecoveryPointARN (\ s a -> s{_grprmRecoveryPointARN = a})

instance AWSRequest GetRecoveryPointRestoreMetadata
         where
        type Rs GetRecoveryPointRestoreMetadata =
             GetRecoveryPointRestoreMetadataResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 GetRecoveryPointRestoreMetadataResponse' <$>
                   (x .?> "BackupVaultArn") <*>
                     (x .?> "RecoveryPointArn")
                     <*> (x .?> "RestoreMetadata" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetRecoveryPointRestoreMetadata
         where

instance NFData GetRecoveryPointRestoreMetadata where

instance ToHeaders GetRecoveryPointRestoreMetadata
         where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetRecoveryPointRestoreMetadata where
        toPath GetRecoveryPointRestoreMetadata'{..}
          = mconcat
              ["/backup-vaults/", toBS _grprmBackupVaultName,
               "/recovery-points/", toBS _grprmRecoveryPointARN,
               "/restore-metadata"]

instance ToQuery GetRecoveryPointRestoreMetadata
         where
        toQuery = const mempty

-- | /See:/ 'getRecoveryPointRestoreMetadataResponse' smart constructor.
data GetRecoveryPointRestoreMetadataResponse = GetRecoveryPointRestoreMetadataResponse'
  { _grprmrsBackupVaultARN :: !(Maybe Text)
  , _grprmrsRecoveryPointARN :: !(Maybe Text)
  , _grprmrsRestoreMetadata :: !(Maybe (Map Text Text))
  , _grprmrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRecoveryPointRestoreMetadataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grprmrsBackupVaultARN' - An ARN that uniquely identifies a backup vault; for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@ .
--
-- * 'grprmrsRecoveryPointARN' - An ARN that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
--
-- * 'grprmrsRestoreMetadata' - A set of metadata key-value pairs that lists the metadata key-value pairs that are required to restore the recovery point.
--
-- * 'grprmrsResponseStatus' - -- | The response status code.
getRecoveryPointRestoreMetadataResponse
    :: Int -- ^ 'grprmrsResponseStatus'
    -> GetRecoveryPointRestoreMetadataResponse
getRecoveryPointRestoreMetadataResponse pResponseStatus_ =
  GetRecoveryPointRestoreMetadataResponse'
    { _grprmrsBackupVaultARN = Nothing
    , _grprmrsRecoveryPointARN = Nothing
    , _grprmrsRestoreMetadata = Nothing
    , _grprmrsResponseStatus = pResponseStatus_
    }


-- | An ARN that uniquely identifies a backup vault; for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@ .
grprmrsBackupVaultARN :: Lens' GetRecoveryPointRestoreMetadataResponse (Maybe Text)
grprmrsBackupVaultARN = lens _grprmrsBackupVaultARN (\ s a -> s{_grprmrsBackupVaultARN = a})

-- | An ARN that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
grprmrsRecoveryPointARN :: Lens' GetRecoveryPointRestoreMetadataResponse (Maybe Text)
grprmrsRecoveryPointARN = lens _grprmrsRecoveryPointARN (\ s a -> s{_grprmrsRecoveryPointARN = a})

-- | A set of metadata key-value pairs that lists the metadata key-value pairs that are required to restore the recovery point.
grprmrsRestoreMetadata :: Lens' GetRecoveryPointRestoreMetadataResponse (HashMap Text Text)
grprmrsRestoreMetadata = lens _grprmrsRestoreMetadata (\ s a -> s{_grprmrsRestoreMetadata = a}) . _Default . _Map

-- | -- | The response status code.
grprmrsResponseStatus :: Lens' GetRecoveryPointRestoreMetadataResponse Int
grprmrsResponseStatus = lens _grprmrsResponseStatus (\ s a -> s{_grprmrsResponseStatus = a})

instance NFData
           GetRecoveryPointRestoreMetadataResponse
         where
