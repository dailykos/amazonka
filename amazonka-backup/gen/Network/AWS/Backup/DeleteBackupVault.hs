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
-- Module      : Network.AWS.Backup.DeleteBackupVault
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the backup vault identified by its name. A vault can be deleted only if it is empty.
--
--
module Network.AWS.Backup.DeleteBackupVault
    (
    -- * Creating a Request
      deleteBackupVault
    , DeleteBackupVault
    -- * Request Lenses
    , dbvBackupVaultName

    -- * Destructuring the Response
    , deleteBackupVaultResponse
    , DeleteBackupVaultResponse
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteBackupVault' smart constructor.
newtype DeleteBackupVault = DeleteBackupVault'
  { _dbvBackupVaultName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBackupVault' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbvBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and theAWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
deleteBackupVault
    :: Text -- ^ 'dbvBackupVaultName'
    -> DeleteBackupVault
deleteBackupVault pBackupVaultName_ =
  DeleteBackupVault' {_dbvBackupVaultName = pBackupVaultName_}


-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and theAWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
dbvBackupVaultName :: Lens' DeleteBackupVault Text
dbvBackupVaultName = lens _dbvBackupVaultName (\ s a -> s{_dbvBackupVaultName = a})

instance AWSRequest DeleteBackupVault where
        type Rs DeleteBackupVault = DeleteBackupVaultResponse
        request = delete backup
        response = receiveNull DeleteBackupVaultResponse'

instance Hashable DeleteBackupVault where

instance NFData DeleteBackupVault where

instance ToHeaders DeleteBackupVault where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteBackupVault where
        toPath DeleteBackupVault'{..}
          = mconcat
              ["/backup-vaults/", toBS _dbvBackupVaultName]

instance ToQuery DeleteBackupVault where
        toQuery = const mempty

-- | /See:/ 'deleteBackupVaultResponse' smart constructor.
data DeleteBackupVaultResponse =
  DeleteBackupVaultResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBackupVaultResponse' with the minimum fields required to make a request.
--
deleteBackupVaultResponse
    :: DeleteBackupVaultResponse
deleteBackupVaultResponse = DeleteBackupVaultResponse'


instance NFData DeleteBackupVaultResponse where
