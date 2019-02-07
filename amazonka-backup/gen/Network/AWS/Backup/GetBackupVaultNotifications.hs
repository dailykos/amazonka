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
-- Module      : Network.AWS.Backup.GetBackupVaultNotifications
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns event notifications for the specified backup vault.
--
--
module Network.AWS.Backup.GetBackupVaultNotifications
    (
    -- * Creating a Request
      getBackupVaultNotifications
    , GetBackupVaultNotifications
    -- * Request Lenses
    , gbvnBackupVaultName

    -- * Destructuring the Response
    , getBackupVaultNotificationsResponse
    , GetBackupVaultNotificationsResponse
    -- * Response Lenses
    , gbvnrsSNSTopicARN
    , gbvnrsBackupVaultARN
    , gbvnrsBackupVaultName
    , gbvnrsBackupVaultEvents
    , gbvnrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getBackupVaultNotifications' smart constructor.
newtype GetBackupVaultNotifications = GetBackupVaultNotifications'
  { _gbvnBackupVaultName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBackupVaultNotifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbvnBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
getBackupVaultNotifications
    :: Text -- ^ 'gbvnBackupVaultName'
    -> GetBackupVaultNotifications
getBackupVaultNotifications pBackupVaultName_ =
  GetBackupVaultNotifications' {_gbvnBackupVaultName = pBackupVaultName_}


-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
gbvnBackupVaultName :: Lens' GetBackupVaultNotifications Text
gbvnBackupVaultName = lens _gbvnBackupVaultName (\ s a -> s{_gbvnBackupVaultName = a})

instance AWSRequest GetBackupVaultNotifications where
        type Rs GetBackupVaultNotifications =
             GetBackupVaultNotificationsResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 GetBackupVaultNotificationsResponse' <$>
                   (x .?> "SNSTopicArn") <*> (x .?> "BackupVaultArn")
                     <*> (x .?> "BackupVaultName")
                     <*> (x .?> "BackupVaultEvents" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetBackupVaultNotifications where

instance NFData GetBackupVaultNotifications where

instance ToHeaders GetBackupVaultNotifications where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetBackupVaultNotifications where
        toPath GetBackupVaultNotifications'{..}
          = mconcat
              ["/backup-vaults/", toBS _gbvnBackupVaultName,
               "/notification-configuration"]

instance ToQuery GetBackupVaultNotifications where
        toQuery = const mempty

-- | /See:/ 'getBackupVaultNotificationsResponse' smart constructor.
data GetBackupVaultNotificationsResponse = GetBackupVaultNotificationsResponse'
  { _gbvnrsSNSTopicARN :: !(Maybe Text)
  , _gbvnrsBackupVaultARN :: !(Maybe Text)
  , _gbvnrsBackupVaultName :: !(Maybe Text)
  , _gbvnrsBackupVaultEvents :: !(Maybe [BackupVaultEvent])
  , _gbvnrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBackupVaultNotificationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbvnrsSNSTopicARN' - An ARN that uniquely identifies an Amazon Simple Notification Service (Amazon SNS) topic; for example, @arn:aws:sns:us-west-2:111122223333:MyTopic@ .
--
-- * 'gbvnrsBackupVaultARN' - An Amazon Resource Name (ARN) that uniquely identifies a backup vault; for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@ .
--
-- * 'gbvnrsBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the Region where they are created. They consist of lowercase letters, numbers, and hyphens.
--
-- * 'gbvnrsBackupVaultEvents' - An array of events that indicate the status of jobs to back up resources to the backup vault.
--
-- * 'gbvnrsResponseStatus' - -- | The response status code.
getBackupVaultNotificationsResponse
    :: Int -- ^ 'gbvnrsResponseStatus'
    -> GetBackupVaultNotificationsResponse
getBackupVaultNotificationsResponse pResponseStatus_ =
  GetBackupVaultNotificationsResponse'
    { _gbvnrsSNSTopicARN = Nothing
    , _gbvnrsBackupVaultARN = Nothing
    , _gbvnrsBackupVaultName = Nothing
    , _gbvnrsBackupVaultEvents = Nothing
    , _gbvnrsResponseStatus = pResponseStatus_
    }


-- | An ARN that uniquely identifies an Amazon Simple Notification Service (Amazon SNS) topic; for example, @arn:aws:sns:us-west-2:111122223333:MyTopic@ .
gbvnrsSNSTopicARN :: Lens' GetBackupVaultNotificationsResponse (Maybe Text)
gbvnrsSNSTopicARN = lens _gbvnrsSNSTopicARN (\ s a -> s{_gbvnrsSNSTopicARN = a})

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault; for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@ .
gbvnrsBackupVaultARN :: Lens' GetBackupVaultNotificationsResponse (Maybe Text)
gbvnrsBackupVaultARN = lens _gbvnrsBackupVaultARN (\ s a -> s{_gbvnrsBackupVaultARN = a})

-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the Region where they are created. They consist of lowercase letters, numbers, and hyphens.
gbvnrsBackupVaultName :: Lens' GetBackupVaultNotificationsResponse (Maybe Text)
gbvnrsBackupVaultName = lens _gbvnrsBackupVaultName (\ s a -> s{_gbvnrsBackupVaultName = a})

-- | An array of events that indicate the status of jobs to back up resources to the backup vault.
gbvnrsBackupVaultEvents :: Lens' GetBackupVaultNotificationsResponse [BackupVaultEvent]
gbvnrsBackupVaultEvents = lens _gbvnrsBackupVaultEvents (\ s a -> s{_gbvnrsBackupVaultEvents = a}) . _Default . _Coerce

-- | -- | The response status code.
gbvnrsResponseStatus :: Lens' GetBackupVaultNotificationsResponse Int
gbvnrsResponseStatus = lens _gbvnrsResponseStatus (\ s a -> s{_gbvnrsResponseStatus = a})

instance NFData GetBackupVaultNotificationsResponse
         where
