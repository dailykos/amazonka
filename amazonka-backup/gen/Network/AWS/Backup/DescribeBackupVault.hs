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
-- Module      : Network.AWS.Backup.DescribeBackupVault
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata about a backup vault specified by its name.
--
--
module Network.AWS.Backup.DescribeBackupVault
    (
    -- * Creating a Request
      describeBackupVault
    , DescribeBackupVault
    -- * Request Lenses
    , dBackupVaultName

    -- * Destructuring the Response
    , describeBackupVaultResponse
    , DescribeBackupVaultResponse
    -- * Response Lenses
    , dbvrsCreatorRequestId
    , dbvrsNumberOfRecoveryPoints
    , dbvrsBackupVaultARN
    , dbvrsEncryptionKeyARN
    , dbvrsCreationDate
    , dbvrsBackupVaultName
    , dbvrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeBackupVault' smart constructor.
newtype DescribeBackupVault = DescribeBackupVault'
  { _dBackupVaultName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBackupVault' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
describeBackupVault
    :: Text -- ^ 'dBackupVaultName'
    -> DescribeBackupVault
describeBackupVault pBackupVaultName_ =
  DescribeBackupVault' {_dBackupVaultName = pBackupVaultName_}


-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
dBackupVaultName :: Lens' DescribeBackupVault Text
dBackupVaultName = lens _dBackupVaultName (\ s a -> s{_dBackupVaultName = a})

instance AWSRequest DescribeBackupVault where
        type Rs DescribeBackupVault =
             DescribeBackupVaultResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 DescribeBackupVaultResponse' <$>
                   (x .?> "CreatorRequestId") <*>
                     (x .?> "NumberOfRecoveryPoints")
                     <*> (x .?> "BackupVaultArn")
                     <*> (x .?> "EncryptionKeyArn")
                     <*> (x .?> "CreationDate")
                     <*> (x .?> "BackupVaultName")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeBackupVault where

instance NFData DescribeBackupVault where

instance ToHeaders DescribeBackupVault where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeBackupVault where
        toPath DescribeBackupVault'{..}
          = mconcat ["/backup-vaults/", toBS _dBackupVaultName]

instance ToQuery DescribeBackupVault where
        toQuery = const mempty

-- | /See:/ 'describeBackupVaultResponse' smart constructor.
data DescribeBackupVaultResponse = DescribeBackupVaultResponse'
  { _dbvrsCreatorRequestId :: !(Maybe Text)
  , _dbvrsNumberOfRecoveryPoints :: !(Maybe Integer)
  , _dbvrsBackupVaultARN :: !(Maybe Text)
  , _dbvrsEncryptionKeyARN :: !(Maybe Text)
  , _dbvrsCreationDate :: !(Maybe POSIX)
  , _dbvrsBackupVaultName :: !(Maybe Text)
  , _dbvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBackupVaultResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbvrsCreatorRequestId' - A unique string that identifies the request and allows failed requests to be retried without the risk of executing the operation twice.
--
-- * 'dbvrsNumberOfRecoveryPoints' - The number of recovery points that are stored in a backup vault.
--
-- * 'dbvrsBackupVaultARN' - An Amazon Resource Name (ARN) that uniquely identifies a backup vault; for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@ .
--
-- * 'dbvrsEncryptionKeyARN' - The server-side encryption key that is used to protect your backups; for example, @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ .
--
-- * 'dbvrsCreationDate' - The date and time that a backup vault is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'dbvrsBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the Region where they are created. They consist of lowercase letters, numbers, and hyphens.
--
-- * 'dbvrsResponseStatus' - -- | The response status code.
describeBackupVaultResponse
    :: Int -- ^ 'dbvrsResponseStatus'
    -> DescribeBackupVaultResponse
describeBackupVaultResponse pResponseStatus_ =
  DescribeBackupVaultResponse'
    { _dbvrsCreatorRequestId = Nothing
    , _dbvrsNumberOfRecoveryPoints = Nothing
    , _dbvrsBackupVaultARN = Nothing
    , _dbvrsEncryptionKeyARN = Nothing
    , _dbvrsCreationDate = Nothing
    , _dbvrsBackupVaultName = Nothing
    , _dbvrsResponseStatus = pResponseStatus_
    }


-- | A unique string that identifies the request and allows failed requests to be retried without the risk of executing the operation twice.
dbvrsCreatorRequestId :: Lens' DescribeBackupVaultResponse (Maybe Text)
dbvrsCreatorRequestId = lens _dbvrsCreatorRequestId (\ s a -> s{_dbvrsCreatorRequestId = a})

-- | The number of recovery points that are stored in a backup vault.
dbvrsNumberOfRecoveryPoints :: Lens' DescribeBackupVaultResponse (Maybe Integer)
dbvrsNumberOfRecoveryPoints = lens _dbvrsNumberOfRecoveryPoints (\ s a -> s{_dbvrsNumberOfRecoveryPoints = a})

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault; for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@ .
dbvrsBackupVaultARN :: Lens' DescribeBackupVaultResponse (Maybe Text)
dbvrsBackupVaultARN = lens _dbvrsBackupVaultARN (\ s a -> s{_dbvrsBackupVaultARN = a})

-- | The server-side encryption key that is used to protect your backups; for example, @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ .
dbvrsEncryptionKeyARN :: Lens' DescribeBackupVaultResponse (Maybe Text)
dbvrsEncryptionKeyARN = lens _dbvrsEncryptionKeyARN (\ s a -> s{_dbvrsEncryptionKeyARN = a})

-- | The date and time that a backup vault is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
dbvrsCreationDate :: Lens' DescribeBackupVaultResponse (Maybe UTCTime)
dbvrsCreationDate = lens _dbvrsCreationDate (\ s a -> s{_dbvrsCreationDate = a}) . mapping _Time

-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the Region where they are created. They consist of lowercase letters, numbers, and hyphens.
dbvrsBackupVaultName :: Lens' DescribeBackupVaultResponse (Maybe Text)
dbvrsBackupVaultName = lens _dbvrsBackupVaultName (\ s a -> s{_dbvrsBackupVaultName = a})

-- | -- | The response status code.
dbvrsResponseStatus :: Lens' DescribeBackupVaultResponse Int
dbvrsResponseStatus = lens _dbvrsResponseStatus (\ s a -> s{_dbvrsResponseStatus = a})

instance NFData DescribeBackupVaultResponse where
