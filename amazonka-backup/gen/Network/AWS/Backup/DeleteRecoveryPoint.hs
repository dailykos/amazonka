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
-- Module      : Network.AWS.Backup.DeleteRecoveryPoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the recovery point specified by a recovery point ID.
--
--
module Network.AWS.Backup.DeleteRecoveryPoint
    (
    -- * Creating a Request
      deleteRecoveryPoint
    , DeleteRecoveryPoint
    -- * Request Lenses
    , delBackupVaultName
    , delRecoveryPointARN

    -- * Destructuring the Response
    , deleteRecoveryPointResponse
    , DeleteRecoveryPointResponse
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRecoveryPoint' smart constructor.
data DeleteRecoveryPoint = DeleteRecoveryPoint'
  { _delBackupVaultName :: !Text
  , _delRecoveryPointARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRecoveryPoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
--
-- * 'delRecoveryPointARN' - An Amazon Resource Name (ARN) that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
deleteRecoveryPoint
    :: Text -- ^ 'delBackupVaultName'
    -> Text -- ^ 'delRecoveryPointARN'
    -> DeleteRecoveryPoint
deleteRecoveryPoint pBackupVaultName_ pRecoveryPointARN_ =
  DeleteRecoveryPoint'
    { _delBackupVaultName = pBackupVaultName_
    , _delRecoveryPointARN = pRecoveryPointARN_
    }


-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
delBackupVaultName :: Lens' DeleteRecoveryPoint Text
delBackupVaultName = lens _delBackupVaultName (\ s a -> s{_delBackupVaultName = a})

-- | An Amazon Resource Name (ARN) that uniquely identifies a recovery point; for example, @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@ .
delRecoveryPointARN :: Lens' DeleteRecoveryPoint Text
delRecoveryPointARN = lens _delRecoveryPointARN (\ s a -> s{_delRecoveryPointARN = a})

instance AWSRequest DeleteRecoveryPoint where
        type Rs DeleteRecoveryPoint =
             DeleteRecoveryPointResponse
        request = delete backup
        response = receiveNull DeleteRecoveryPointResponse'

instance Hashable DeleteRecoveryPoint where

instance NFData DeleteRecoveryPoint where

instance ToHeaders DeleteRecoveryPoint where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteRecoveryPoint where
        toPath DeleteRecoveryPoint'{..}
          = mconcat
              ["/backup-vaults/", toBS _delBackupVaultName,
               "/recovery-points/", toBS _delRecoveryPointARN]

instance ToQuery DeleteRecoveryPoint where
        toQuery = const mempty

-- | /See:/ 'deleteRecoveryPointResponse' smart constructor.
data DeleteRecoveryPointResponse =
  DeleteRecoveryPointResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRecoveryPointResponse' with the minimum fields required to make a request.
--
deleteRecoveryPointResponse
    :: DeleteRecoveryPointResponse
deleteRecoveryPointResponse = DeleteRecoveryPointResponse'


instance NFData DeleteRecoveryPointResponse where
