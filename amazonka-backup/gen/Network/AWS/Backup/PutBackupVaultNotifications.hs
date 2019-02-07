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
-- Module      : Network.AWS.Backup.PutBackupVaultNotifications
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Turns on notifications on a backup vault for the specified topic and events.
--
--
module Network.AWS.Backup.PutBackupVaultNotifications
    (
    -- * Creating a Request
      putBackupVaultNotifications
    , PutBackupVaultNotifications
    -- * Request Lenses
    , pbvnBackupVaultName
    , pbvnSNSTopicARN
    , pbvnBackupVaultEvents

    -- * Destructuring the Response
    , putBackupVaultNotificationsResponse
    , PutBackupVaultNotificationsResponse
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putBackupVaultNotifications' smart constructor.
data PutBackupVaultNotifications = PutBackupVaultNotifications'
  { _pbvnBackupVaultName :: !Text
  , _pbvnSNSTopicARN :: !Text
  , _pbvnBackupVaultEvents :: ![BackupVaultEvent]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBackupVaultNotifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbvnBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
--
-- * 'pbvnSNSTopicARN' - The Amazon Resource Name (ARN) that specifies the topic for a backup vault’s events; for example, @arn:aws:sns:us-west-2:111122223333:MyVaultTopic@ .
--
-- * 'pbvnBackupVaultEvents' - An array of events that indicate the status of jobs to back up resources to the backup vault.
putBackupVaultNotifications
    :: Text -- ^ 'pbvnBackupVaultName'
    -> Text -- ^ 'pbvnSNSTopicARN'
    -> PutBackupVaultNotifications
putBackupVaultNotifications pBackupVaultName_ pSNSTopicARN_ =
  PutBackupVaultNotifications'
    { _pbvnBackupVaultName = pBackupVaultName_
    , _pbvnSNSTopicARN = pSNSTopicARN_
    , _pbvnBackupVaultEvents = mempty
    }


-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
pbvnBackupVaultName :: Lens' PutBackupVaultNotifications Text
pbvnBackupVaultName = lens _pbvnBackupVaultName (\ s a -> s{_pbvnBackupVaultName = a})

-- | The Amazon Resource Name (ARN) that specifies the topic for a backup vault’s events; for example, @arn:aws:sns:us-west-2:111122223333:MyVaultTopic@ .
pbvnSNSTopicARN :: Lens' PutBackupVaultNotifications Text
pbvnSNSTopicARN = lens _pbvnSNSTopicARN (\ s a -> s{_pbvnSNSTopicARN = a})

-- | An array of events that indicate the status of jobs to back up resources to the backup vault.
pbvnBackupVaultEvents :: Lens' PutBackupVaultNotifications [BackupVaultEvent]
pbvnBackupVaultEvents = lens _pbvnBackupVaultEvents (\ s a -> s{_pbvnBackupVaultEvents = a}) . _Coerce

instance AWSRequest PutBackupVaultNotifications where
        type Rs PutBackupVaultNotifications =
             PutBackupVaultNotificationsResponse
        request = putJSON backup
        response
          = receiveNull PutBackupVaultNotificationsResponse'

instance Hashable PutBackupVaultNotifications where

instance NFData PutBackupVaultNotifications where

instance ToHeaders PutBackupVaultNotifications where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutBackupVaultNotifications where
        toJSON PutBackupVaultNotifications'{..}
          = object
              (catMaybes
                 [Just ("SNSTopicArn" .= _pbvnSNSTopicARN),
                  Just
                    ("BackupVaultEvents" .= _pbvnBackupVaultEvents)])

instance ToPath PutBackupVaultNotifications where
        toPath PutBackupVaultNotifications'{..}
          = mconcat
              ["/backup-vaults/", toBS _pbvnBackupVaultName,
               "/notification-configuration"]

instance ToQuery PutBackupVaultNotifications where
        toQuery = const mempty

-- | /See:/ 'putBackupVaultNotificationsResponse' smart constructor.
data PutBackupVaultNotificationsResponse =
  PutBackupVaultNotificationsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBackupVaultNotificationsResponse' with the minimum fields required to make a request.
--
putBackupVaultNotificationsResponse
    :: PutBackupVaultNotificationsResponse
putBackupVaultNotificationsResponse = PutBackupVaultNotificationsResponse'


instance NFData PutBackupVaultNotificationsResponse
         where
