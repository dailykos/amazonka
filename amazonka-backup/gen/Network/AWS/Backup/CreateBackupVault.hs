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
-- Module      : Network.AWS.Backup.CreateBackupVault
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a logical container where backups are stored. A @CreateBackupVault@ request includes a name, optionally one or more resource tags, an encryption key, and a request ID.
--
--
module Network.AWS.Backup.CreateBackupVault
    (
    -- * Creating a Request
      createBackupVault
    , CreateBackupVault
    -- * Request Lenses
    , cbvCreatorRequestId
    , cbvEncryptionKeyARN
    , cbvBackupVaultTags
    , cbvBackupVaultName

    -- * Destructuring the Response
    , createBackupVaultResponse
    , CreateBackupVaultResponse
    -- * Response Lenses
    , cbvrsBackupVaultARN
    , cbvrsCreationDate
    , cbvrsBackupVaultName
    , cbvrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createBackupVault' smart constructor.
data CreateBackupVault = CreateBackupVault'
  { _cbvCreatorRequestId :: !(Maybe Text)
  , _cbvEncryptionKeyARN :: !(Maybe Text)
  , _cbvBackupVaultTags :: !(Maybe (Sensitive (Map Text Text)))
  , _cbvBackupVaultName :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBackupVault' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbvCreatorRequestId' - A unique string that identifies the request and allows failed requests to be retried without the risk of executing the operation twice.
--
-- * 'cbvEncryptionKeyARN' - The server-side encryption key that is used to protect your backups; for example, @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ .
--
-- * 'cbvBackupVaultTags' - Metadata that you can assign to help organize the resources that you create. Each tag is a key-value pair.
--
-- * 'cbvBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
createBackupVault
    :: Text -- ^ 'cbvBackupVaultName'
    -> CreateBackupVault
createBackupVault pBackupVaultName_ =
  CreateBackupVault'
    { _cbvCreatorRequestId = Nothing
    , _cbvEncryptionKeyARN = Nothing
    , _cbvBackupVaultTags = Nothing
    , _cbvBackupVaultName = pBackupVaultName_
    }


-- | A unique string that identifies the request and allows failed requests to be retried without the risk of executing the operation twice.
cbvCreatorRequestId :: Lens' CreateBackupVault (Maybe Text)
cbvCreatorRequestId = lens _cbvCreatorRequestId (\ s a -> s{_cbvCreatorRequestId = a})

-- | The server-side encryption key that is used to protect your backups; for example, @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ .
cbvEncryptionKeyARN :: Lens' CreateBackupVault (Maybe Text)
cbvEncryptionKeyARN = lens _cbvEncryptionKeyARN (\ s a -> s{_cbvEncryptionKeyARN = a})

-- | Metadata that you can assign to help organize the resources that you create. Each tag is a key-value pair.
cbvBackupVaultTags :: Lens' CreateBackupVault (Maybe (HashMap Text Text))
cbvBackupVaultTags = lens _cbvBackupVaultTags (\ s a -> s{_cbvBackupVaultTags = a}) . mapping (_Sensitive . _Map)

-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
cbvBackupVaultName :: Lens' CreateBackupVault Text
cbvBackupVaultName = lens _cbvBackupVaultName (\ s a -> s{_cbvBackupVaultName = a})

instance AWSRequest CreateBackupVault where
        type Rs CreateBackupVault = CreateBackupVaultResponse
        request = putJSON backup
        response
          = receiveJSON
              (\ s h x ->
                 CreateBackupVaultResponse' <$>
                   (x .?> "BackupVaultArn") <*> (x .?> "CreationDate")
                     <*> (x .?> "BackupVaultName")
                     <*> (pure (fromEnum s)))

instance Hashable CreateBackupVault where

instance NFData CreateBackupVault where

instance ToHeaders CreateBackupVault where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateBackupVault where
        toJSON CreateBackupVault'{..}
          = object
              (catMaybes
                 [("CreatorRequestId" .=) <$> _cbvCreatorRequestId,
                  ("EncryptionKeyArn" .=) <$> _cbvEncryptionKeyARN,
                  ("BackupVaultTags" .=) <$> _cbvBackupVaultTags])

instance ToPath CreateBackupVault where
        toPath CreateBackupVault'{..}
          = mconcat
              ["/backup-vaults/", toBS _cbvBackupVaultName]

instance ToQuery CreateBackupVault where
        toQuery = const mempty

-- | /See:/ 'createBackupVaultResponse' smart constructor.
data CreateBackupVaultResponse = CreateBackupVaultResponse'
  { _cbvrsBackupVaultARN :: !(Maybe Text)
  , _cbvrsCreationDate :: !(Maybe POSIX)
  , _cbvrsBackupVaultName :: !(Maybe Text)
  , _cbvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBackupVaultResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbvrsBackupVaultARN' - An Amazon Resource Name (ARN) that uniquely identifies a backup vault; for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@ .
--
-- * 'cbvrsCreationDate' - The date and time a backup vault is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'cbvrsBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the Region where they are created. They consist of lowercase letters, numbers, and hyphens.
--
-- * 'cbvrsResponseStatus' - -- | The response status code.
createBackupVaultResponse
    :: Int -- ^ 'cbvrsResponseStatus'
    -> CreateBackupVaultResponse
createBackupVaultResponse pResponseStatus_ =
  CreateBackupVaultResponse'
    { _cbvrsBackupVaultARN = Nothing
    , _cbvrsCreationDate = Nothing
    , _cbvrsBackupVaultName = Nothing
    , _cbvrsResponseStatus = pResponseStatus_
    }


-- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault; for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@ .
cbvrsBackupVaultARN :: Lens' CreateBackupVaultResponse (Maybe Text)
cbvrsBackupVaultARN = lens _cbvrsBackupVaultARN (\ s a -> s{_cbvrsBackupVaultARN = a})

-- | The date and time a backup vault is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
cbvrsCreationDate :: Lens' CreateBackupVaultResponse (Maybe UTCTime)
cbvrsCreationDate = lens _cbvrsCreationDate (\ s a -> s{_cbvrsCreationDate = a}) . mapping _Time

-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the Region where they are created. They consist of lowercase letters, numbers, and hyphens.
cbvrsBackupVaultName :: Lens' CreateBackupVaultResponse (Maybe Text)
cbvrsBackupVaultName = lens _cbvrsBackupVaultName (\ s a -> s{_cbvrsBackupVaultName = a})

-- | -- | The response status code.
cbvrsResponseStatus :: Lens' CreateBackupVaultResponse Int
cbvrsResponseStatus = lens _cbvrsResponseStatus (\ s a -> s{_cbvrsResponseStatus = a})

instance NFData CreateBackupVaultResponse where
