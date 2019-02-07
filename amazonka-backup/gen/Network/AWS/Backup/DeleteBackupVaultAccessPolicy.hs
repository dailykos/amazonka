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
-- Module      : Network.AWS.Backup.DeleteBackupVaultAccessPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the policy document that manages permissions on a backup vault.
--
--
module Network.AWS.Backup.DeleteBackupVaultAccessPolicy
    (
    -- * Creating a Request
      deleteBackupVaultAccessPolicy
    , DeleteBackupVaultAccessPolicy
    -- * Request Lenses
    , dbvapBackupVaultName

    -- * Destructuring the Response
    , deleteBackupVaultAccessPolicyResponse
    , DeleteBackupVaultAccessPolicyResponse
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteBackupVaultAccessPolicy' smart constructor.
newtype DeleteBackupVaultAccessPolicy = DeleteBackupVaultAccessPolicy'
  { _dbvapBackupVaultName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBackupVaultAccessPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbvapBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
deleteBackupVaultAccessPolicy
    :: Text -- ^ 'dbvapBackupVaultName'
    -> DeleteBackupVaultAccessPolicy
deleteBackupVaultAccessPolicy pBackupVaultName_ =
  DeleteBackupVaultAccessPolicy' {_dbvapBackupVaultName = pBackupVaultName_}


-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
dbvapBackupVaultName :: Lens' DeleteBackupVaultAccessPolicy Text
dbvapBackupVaultName = lens _dbvapBackupVaultName (\ s a -> s{_dbvapBackupVaultName = a})

instance AWSRequest DeleteBackupVaultAccessPolicy
         where
        type Rs DeleteBackupVaultAccessPolicy =
             DeleteBackupVaultAccessPolicyResponse
        request = delete backup
        response
          = receiveNull DeleteBackupVaultAccessPolicyResponse'

instance Hashable DeleteBackupVaultAccessPolicy where

instance NFData DeleteBackupVaultAccessPolicy where

instance ToHeaders DeleteBackupVaultAccessPolicy
         where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteBackupVaultAccessPolicy where
        toPath DeleteBackupVaultAccessPolicy'{..}
          = mconcat
              ["/backup-vaults/", toBS _dbvapBackupVaultName,
               "/access-policy"]

instance ToQuery DeleteBackupVaultAccessPolicy where
        toQuery = const mempty

-- | /See:/ 'deleteBackupVaultAccessPolicyResponse' smart constructor.
data DeleteBackupVaultAccessPolicyResponse =
  DeleteBackupVaultAccessPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBackupVaultAccessPolicyResponse' with the minimum fields required to make a request.
--
deleteBackupVaultAccessPolicyResponse
    :: DeleteBackupVaultAccessPolicyResponse
deleteBackupVaultAccessPolicyResponse = DeleteBackupVaultAccessPolicyResponse'


instance NFData DeleteBackupVaultAccessPolicyResponse
         where
