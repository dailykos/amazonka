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
-- Module      : Network.AWS.Backup.PutBackupVaultAccessPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets a resource-based policy that is used to manage access permissions on the target backup vault. Requires a backup vault name and an access policy document in JSON format.
--
--
module Network.AWS.Backup.PutBackupVaultAccessPolicy
    (
    -- * Creating a Request
      putBackupVaultAccessPolicy
    , PutBackupVaultAccessPolicy
    -- * Request Lenses
    , pbvapPolicy
    , pbvapBackupVaultName

    -- * Destructuring the Response
    , putBackupVaultAccessPolicyResponse
    , PutBackupVaultAccessPolicyResponse
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putBackupVaultAccessPolicy' smart constructor.
data PutBackupVaultAccessPolicy = PutBackupVaultAccessPolicy'
  { _pbvapPolicy :: !(Maybe Text)
  , _pbvapBackupVaultName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBackupVaultAccessPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbvapPolicy' - The backup vault access policy document in JSON format.
--
-- * 'pbvapBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
putBackupVaultAccessPolicy
    :: Text -- ^ 'pbvapBackupVaultName'
    -> PutBackupVaultAccessPolicy
putBackupVaultAccessPolicy pBackupVaultName_ =
  PutBackupVaultAccessPolicy'
    {_pbvapPolicy = Nothing, _pbvapBackupVaultName = pBackupVaultName_}


-- | The backup vault access policy document in JSON format.
pbvapPolicy :: Lens' PutBackupVaultAccessPolicy (Maybe Text)
pbvapPolicy = lens _pbvapPolicy (\ s a -> s{_pbvapPolicy = a})

-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
pbvapBackupVaultName :: Lens' PutBackupVaultAccessPolicy Text
pbvapBackupVaultName = lens _pbvapBackupVaultName (\ s a -> s{_pbvapBackupVaultName = a})

instance AWSRequest PutBackupVaultAccessPolicy where
        type Rs PutBackupVaultAccessPolicy =
             PutBackupVaultAccessPolicyResponse
        request = putJSON backup
        response
          = receiveNull PutBackupVaultAccessPolicyResponse'

instance Hashable PutBackupVaultAccessPolicy where

instance NFData PutBackupVaultAccessPolicy where

instance ToHeaders PutBackupVaultAccessPolicy where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutBackupVaultAccessPolicy where
        toJSON PutBackupVaultAccessPolicy'{..}
          = object (catMaybes [("Policy" .=) <$> _pbvapPolicy])

instance ToPath PutBackupVaultAccessPolicy where
        toPath PutBackupVaultAccessPolicy'{..}
          = mconcat
              ["/backup-vaults/", toBS _pbvapBackupVaultName,
               "/access-policy"]

instance ToQuery PutBackupVaultAccessPolicy where
        toQuery = const mempty

-- | /See:/ 'putBackupVaultAccessPolicyResponse' smart constructor.
data PutBackupVaultAccessPolicyResponse =
  PutBackupVaultAccessPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBackupVaultAccessPolicyResponse' with the minimum fields required to make a request.
--
putBackupVaultAccessPolicyResponse
    :: PutBackupVaultAccessPolicyResponse
putBackupVaultAccessPolicyResponse = PutBackupVaultAccessPolicyResponse'


instance NFData PutBackupVaultAccessPolicyResponse
         where
