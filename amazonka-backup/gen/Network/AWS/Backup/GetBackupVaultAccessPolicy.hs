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
-- Module      : Network.AWS.Backup.GetBackupVaultAccessPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the access policy document that is associated with the named backup vault.
--
--
module Network.AWS.Backup.GetBackupVaultAccessPolicy
    (
    -- * Creating a Request
      getBackupVaultAccessPolicy
    , GetBackupVaultAccessPolicy
    -- * Request Lenses
    , gbvapBackupVaultName

    -- * Destructuring the Response
    , getBackupVaultAccessPolicyResponse
    , GetBackupVaultAccessPolicyResponse
    -- * Response Lenses
    , gbvaprsBackupVaultARN
    , gbvaprsPolicy
    , gbvaprsBackupVaultName
    , gbvaprsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getBackupVaultAccessPolicy' smart constructor.
newtype GetBackupVaultAccessPolicy = GetBackupVaultAccessPolicy'
  { _gbvapBackupVaultName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBackupVaultAccessPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbvapBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
getBackupVaultAccessPolicy
    :: Text -- ^ 'gbvapBackupVaultName'
    -> GetBackupVaultAccessPolicy
getBackupVaultAccessPolicy pBackupVaultName_ =
  GetBackupVaultAccessPolicy' {_gbvapBackupVaultName = pBackupVaultName_}


-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
gbvapBackupVaultName :: Lens' GetBackupVaultAccessPolicy Text
gbvapBackupVaultName = lens _gbvapBackupVaultName (\ s a -> s{_gbvapBackupVaultName = a})

instance AWSRequest GetBackupVaultAccessPolicy where
        type Rs GetBackupVaultAccessPolicy =
             GetBackupVaultAccessPolicyResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 GetBackupVaultAccessPolicyResponse' <$>
                   (x .?> "BackupVaultArn") <*> (x .?> "Policy") <*>
                     (x .?> "BackupVaultName")
                     <*> (pure (fromEnum s)))

instance Hashable GetBackupVaultAccessPolicy where

instance NFData GetBackupVaultAccessPolicy where

instance ToHeaders GetBackupVaultAccessPolicy where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetBackupVaultAccessPolicy where
        toPath GetBackupVaultAccessPolicy'{..}
          = mconcat
              ["/backup-vaults/", toBS _gbvapBackupVaultName,
               "/access-policy"]

instance ToQuery GetBackupVaultAccessPolicy where
        toQuery = const mempty

-- | /See:/ 'getBackupVaultAccessPolicyResponse' smart constructor.
data GetBackupVaultAccessPolicyResponse = GetBackupVaultAccessPolicyResponse'
  { _gbvaprsBackupVaultARN :: !(Maybe Text)
  , _gbvaprsPolicy :: !(Maybe Text)
  , _gbvaprsBackupVaultName :: !(Maybe Text)
  , _gbvaprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBackupVaultAccessPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbvaprsBackupVaultARN' - An Amazon Resource Name (ARN) that uniquely identifies a backup vault; for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@ .
--
-- * 'gbvaprsPolicy' - The backup vault access policy document in JSON format.
--
-- * 'gbvaprsBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the Region where they are created. They consist of lowercase letters, numbers, and hyphens.
--
-- * 'gbvaprsResponseStatus' - -- | The response status code.
getBackupVaultAccessPolicyResponse
    :: Int -- ^ 'gbvaprsResponseStatus'
    -> GetBackupVaultAccessPolicyResponse
getBackupVaultAccessPolicyResponse pResponseStatus_ =
  GetBackupVaultAccessPolicyResponse'
    { _gbvaprsBackupVaultARN = Nothing
    , _gbvaprsPolicy = Nothing
    , _gbvaprsBackupVaultName = Nothing
    , _gbvaprsResponseStatus = pResponseStatus_
    }


-- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault; for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@ .
gbvaprsBackupVaultARN :: Lens' GetBackupVaultAccessPolicyResponse (Maybe Text)
gbvaprsBackupVaultARN = lens _gbvaprsBackupVaultARN (\ s a -> s{_gbvaprsBackupVaultARN = a})

-- | The backup vault access policy document in JSON format.
gbvaprsPolicy :: Lens' GetBackupVaultAccessPolicyResponse (Maybe Text)
gbvaprsPolicy = lens _gbvaprsPolicy (\ s a -> s{_gbvaprsPolicy = a})

-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the Region where they are created. They consist of lowercase letters, numbers, and hyphens.
gbvaprsBackupVaultName :: Lens' GetBackupVaultAccessPolicyResponse (Maybe Text)
gbvaprsBackupVaultName = lens _gbvaprsBackupVaultName (\ s a -> s{_gbvaprsBackupVaultName = a})

-- | -- | The response status code.
gbvaprsResponseStatus :: Lens' GetBackupVaultAccessPolicyResponse Int
gbvaprsResponseStatus = lens _gbvaprsResponseStatus (\ s a -> s{_gbvaprsResponseStatus = a})

instance NFData GetBackupVaultAccessPolicyResponse
         where
