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
-- Module      : Network.AWS.Backup.UpdateBackupPlan
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the body of a saved backup plan identified by its @backupPlanId@ with the input document in JSON format. The new version is uniquely identified by a @VersionId@ .
--
--
module Network.AWS.Backup.UpdateBackupPlan
    (
    -- * Creating a Request
      updateBackupPlan
    , UpdateBackupPlan
    -- * Request Lenses
    , ubpBackupPlanId
    , ubpBackupPlan

    -- * Destructuring the Response
    , updateBackupPlanResponse
    , UpdateBackupPlanResponse
    -- * Response Lenses
    , ubprsVersionId
    , ubprsBackupPlanId
    , ubprsBackupPlanARN
    , ubprsCreationDate
    , ubprsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateBackupPlan' smart constructor.
data UpdateBackupPlan = UpdateBackupPlan'
  { _ubpBackupPlanId :: !Text
  , _ubpBackupPlan :: !BackupPlanInput
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateBackupPlan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubpBackupPlanId' - Uniquely identifies a backup plan.
--
-- * 'ubpBackupPlan' - Specifies the body of a backup plan. Includes a @BackupPlanName@ and one or more sets of @Rules@ .
updateBackupPlan
    :: Text -- ^ 'ubpBackupPlanId'
    -> BackupPlanInput -- ^ 'ubpBackupPlan'
    -> UpdateBackupPlan
updateBackupPlan pBackupPlanId_ pBackupPlan_ =
  UpdateBackupPlan'
    {_ubpBackupPlanId = pBackupPlanId_, _ubpBackupPlan = pBackupPlan_}


-- | Uniquely identifies a backup plan.
ubpBackupPlanId :: Lens' UpdateBackupPlan Text
ubpBackupPlanId = lens _ubpBackupPlanId (\ s a -> s{_ubpBackupPlanId = a})

-- | Specifies the body of a backup plan. Includes a @BackupPlanName@ and one or more sets of @Rules@ .
ubpBackupPlan :: Lens' UpdateBackupPlan BackupPlanInput
ubpBackupPlan = lens _ubpBackupPlan (\ s a -> s{_ubpBackupPlan = a})

instance AWSRequest UpdateBackupPlan where
        type Rs UpdateBackupPlan = UpdateBackupPlanResponse
        request = postJSON backup
        response
          = receiveJSON
              (\ s h x ->
                 UpdateBackupPlanResponse' <$>
                   (x .?> "VersionId") <*> (x .?> "BackupPlanId") <*>
                     (x .?> "BackupPlanArn")
                     <*> (x .?> "CreationDate")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateBackupPlan where

instance NFData UpdateBackupPlan where

instance ToHeaders UpdateBackupPlan where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateBackupPlan where
        toJSON UpdateBackupPlan'{..}
          = object
              (catMaybes [Just ("BackupPlan" .= _ubpBackupPlan)])

instance ToPath UpdateBackupPlan where
        toPath UpdateBackupPlan'{..}
          = mconcat ["/backup/plans/", toBS _ubpBackupPlanId]

instance ToQuery UpdateBackupPlan where
        toQuery = const mempty

-- | /See:/ 'updateBackupPlanResponse' smart constructor.
data UpdateBackupPlanResponse = UpdateBackupPlanResponse'
  { _ubprsVersionId :: !(Maybe Text)
  , _ubprsBackupPlanId :: !(Maybe Text)
  , _ubprsBackupPlanARN :: !(Maybe Text)
  , _ubprsCreationDate :: !(Maybe POSIX)
  , _ubprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateBackupPlanResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubprsVersionId' - Unique, randomly generated, Unicode, UTF-8 encoded strings that are at most 1,024 bytes long. Version Ids cannot be edited.
--
-- * 'ubprsBackupPlanId' - Uniquely identifies a backup plan.
--
-- * 'ubprsBackupPlanARN' - An Amazon Resource Name (ARN) that uniquely identifies a backup plan; for example, @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@ .
--
-- * 'ubprsCreationDate' - The date and time a backup plan is updated, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'ubprsResponseStatus' - -- | The response status code.
updateBackupPlanResponse
    :: Int -- ^ 'ubprsResponseStatus'
    -> UpdateBackupPlanResponse
updateBackupPlanResponse pResponseStatus_ =
  UpdateBackupPlanResponse'
    { _ubprsVersionId = Nothing
    , _ubprsBackupPlanId = Nothing
    , _ubprsBackupPlanARN = Nothing
    , _ubprsCreationDate = Nothing
    , _ubprsResponseStatus = pResponseStatus_
    }


-- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at most 1,024 bytes long. Version Ids cannot be edited.
ubprsVersionId :: Lens' UpdateBackupPlanResponse (Maybe Text)
ubprsVersionId = lens _ubprsVersionId (\ s a -> s{_ubprsVersionId = a})

-- | Uniquely identifies a backup plan.
ubprsBackupPlanId :: Lens' UpdateBackupPlanResponse (Maybe Text)
ubprsBackupPlanId = lens _ubprsBackupPlanId (\ s a -> s{_ubprsBackupPlanId = a})

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan; for example, @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@ .
ubprsBackupPlanARN :: Lens' UpdateBackupPlanResponse (Maybe Text)
ubprsBackupPlanARN = lens _ubprsBackupPlanARN (\ s a -> s{_ubprsBackupPlanARN = a})

-- | The date and time a backup plan is updated, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
ubprsCreationDate :: Lens' UpdateBackupPlanResponse (Maybe UTCTime)
ubprsCreationDate = lens _ubprsCreationDate (\ s a -> s{_ubprsCreationDate = a}) . mapping _Time

-- | -- | The response status code.
ubprsResponseStatus :: Lens' UpdateBackupPlanResponse Int
ubprsResponseStatus = lens _ubprsResponseStatus (\ s a -> s{_ubprsResponseStatus = a})

instance NFData UpdateBackupPlanResponse where
