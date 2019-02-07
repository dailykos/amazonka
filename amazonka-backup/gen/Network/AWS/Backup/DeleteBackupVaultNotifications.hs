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
-- Module      : Network.AWS.Backup.DeleteBackupVaultNotifications
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes event notifications for the specified backup vault.
--
--
module Network.AWS.Backup.DeleteBackupVaultNotifications
    (
    -- * Creating a Request
      deleteBackupVaultNotifications
    , DeleteBackupVaultNotifications
    -- * Request Lenses
    , dbvnBackupVaultName

    -- * Destructuring the Response
    , deleteBackupVaultNotificationsResponse
    , DeleteBackupVaultNotificationsResponse
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteBackupVaultNotifications' smart constructor.
newtype DeleteBackupVaultNotifications = DeleteBackupVaultNotifications'
  { _dbvnBackupVaultName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBackupVaultNotifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbvnBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the Region where they are created. They consist of lowercase letters, numbers, and hyphens.
deleteBackupVaultNotifications
    :: Text -- ^ 'dbvnBackupVaultName'
    -> DeleteBackupVaultNotifications
deleteBackupVaultNotifications pBackupVaultName_ =
  DeleteBackupVaultNotifications' {_dbvnBackupVaultName = pBackupVaultName_}


-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the Region where they are created. They consist of lowercase letters, numbers, and hyphens.
dbvnBackupVaultName :: Lens' DeleteBackupVaultNotifications Text
dbvnBackupVaultName = lens _dbvnBackupVaultName (\ s a -> s{_dbvnBackupVaultName = a})

instance AWSRequest DeleteBackupVaultNotifications
         where
        type Rs DeleteBackupVaultNotifications =
             DeleteBackupVaultNotificationsResponse
        request = delete backup
        response
          = receiveNull DeleteBackupVaultNotificationsResponse'

instance Hashable DeleteBackupVaultNotifications
         where

instance NFData DeleteBackupVaultNotifications where

instance ToHeaders DeleteBackupVaultNotifications
         where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteBackupVaultNotifications where
        toPath DeleteBackupVaultNotifications'{..}
          = mconcat
              ["/backup-vaults/", toBS _dbvnBackupVaultName,
               "/notification-configuration"]

instance ToQuery DeleteBackupVaultNotifications where
        toQuery = const mempty

-- | /See:/ 'deleteBackupVaultNotificationsResponse' smart constructor.
data DeleteBackupVaultNotificationsResponse =
  DeleteBackupVaultNotificationsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBackupVaultNotificationsResponse' with the minimum fields required to make a request.
--
deleteBackupVaultNotificationsResponse
    :: DeleteBackupVaultNotificationsResponse
deleteBackupVaultNotificationsResponse = DeleteBackupVaultNotificationsResponse'


instance NFData
           DeleteBackupVaultNotificationsResponse
         where
