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
-- Module      : Network.AWS.Backup.GetBackupPlan
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the body of a backup plan in JSON format, in addition to plan metadata.
--
--
module Network.AWS.Backup.GetBackupPlan
    (
    -- * Creating a Request
      getBackupPlan
    , GetBackupPlan
    -- * Request Lenses
    , gbpVersionId
    , gbpBackupPlanId

    -- * Destructuring the Response
    , getBackupPlanResponse
    , GetBackupPlanResponse
    -- * Response Lenses
    , gbprsVersionId
    , gbprsBackupPlanId
    , gbprsCreatorRequestId
    , gbprsBackupPlanARN
    , gbprsLastExecutionDate
    , gbprsBackupPlan
    , gbprsCreationDate
    , gbprsDeletionDate
    , gbprsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getBackupPlan' smart constructor.
data GetBackupPlan = GetBackupPlan'
  { _gbpVersionId :: !(Maybe Text)
  , _gbpBackupPlanId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBackupPlan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbpVersionId' - Unique, randomly generated, Unicode, UTF-8 encoded strings that are at most 1,024 bytes long. Version IDs cannot be edited.
--
-- * 'gbpBackupPlanId' - Uniquely identifies a backup plan.
getBackupPlan
    :: Text -- ^ 'gbpBackupPlanId'
    -> GetBackupPlan
getBackupPlan pBackupPlanId_ =
  GetBackupPlan' {_gbpVersionId = Nothing, _gbpBackupPlanId = pBackupPlanId_}


-- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at most 1,024 bytes long. Version IDs cannot be edited.
gbpVersionId :: Lens' GetBackupPlan (Maybe Text)
gbpVersionId = lens _gbpVersionId (\ s a -> s{_gbpVersionId = a})

-- | Uniquely identifies a backup plan.
gbpBackupPlanId :: Lens' GetBackupPlan Text
gbpBackupPlanId = lens _gbpBackupPlanId (\ s a -> s{_gbpBackupPlanId = a})

instance AWSRequest GetBackupPlan where
        type Rs GetBackupPlan = GetBackupPlanResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 GetBackupPlanResponse' <$>
                   (x .?> "VersionId") <*> (x .?> "BackupPlanId") <*>
                     (x .?> "CreatorRequestId")
                     <*> (x .?> "BackupPlanArn")
                     <*> (x .?> "LastExecutionDate")
                     <*> (x .?> "BackupPlan")
                     <*> (x .?> "CreationDate")
                     <*> (x .?> "DeletionDate")
                     <*> (pure (fromEnum s)))

instance Hashable GetBackupPlan where

instance NFData GetBackupPlan where

instance ToHeaders GetBackupPlan where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetBackupPlan where
        toPath GetBackupPlan'{..}
          = mconcat
              ["/backup/plans/", toBS _gbpBackupPlanId, "/"]

instance ToQuery GetBackupPlan where
        toQuery GetBackupPlan'{..}
          = mconcat ["versionId" =: _gbpVersionId]

-- | /See:/ 'getBackupPlanResponse' smart constructor.
data GetBackupPlanResponse = GetBackupPlanResponse'
  { _gbprsVersionId :: !(Maybe Text)
  , _gbprsBackupPlanId :: !(Maybe Text)
  , _gbprsCreatorRequestId :: !(Maybe Text)
  , _gbprsBackupPlanARN :: !(Maybe Text)
  , _gbprsLastExecutionDate :: !(Maybe POSIX)
  , _gbprsBackupPlan :: !(Maybe BackupPlan)
  , _gbprsCreationDate :: !(Maybe POSIX)
  , _gbprsDeletionDate :: !(Maybe POSIX)
  , _gbprsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBackupPlanResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbprsVersionId' - Unique, randomly generated, Unicode, UTF-8 encoded strings that are at most 1,024 bytes long. Version IDs cannot be edited.
--
-- * 'gbprsBackupPlanId' - Uniquely identifies a backup plan.
--
-- * 'gbprsCreatorRequestId' - A unique string that identifies the request and allows failed requests to be retried without the risk of executing the operation twice.
--
-- * 'gbprsBackupPlanARN' - An Amazon Resource Name (ARN) that uniquely identifies a backup plan; for example, @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@ .
--
-- * 'gbprsLastExecutionDate' - The last time a job to back up resources was executed with this backup plan. A date and time, in Unix format and Coordinated Universal Time (UTC). The value of @LastExecutionDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'gbprsBackupPlan' - Specifies the body of a backup plan. Includes a @BackupPlanName@ and one or more sets of @Rules@ .
--
-- * 'gbprsCreationDate' - The date and time that a backup plan is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'gbprsDeletionDate' - The date and time that a backup plan is deleted, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'gbprsResponseStatus' - -- | The response status code.
getBackupPlanResponse
    :: Int -- ^ 'gbprsResponseStatus'
    -> GetBackupPlanResponse
getBackupPlanResponse pResponseStatus_ =
  GetBackupPlanResponse'
    { _gbprsVersionId = Nothing
    , _gbprsBackupPlanId = Nothing
    , _gbprsCreatorRequestId = Nothing
    , _gbprsBackupPlanARN = Nothing
    , _gbprsLastExecutionDate = Nothing
    , _gbprsBackupPlan = Nothing
    , _gbprsCreationDate = Nothing
    , _gbprsDeletionDate = Nothing
    , _gbprsResponseStatus = pResponseStatus_
    }


-- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at most 1,024 bytes long. Version IDs cannot be edited.
gbprsVersionId :: Lens' GetBackupPlanResponse (Maybe Text)
gbprsVersionId = lens _gbprsVersionId (\ s a -> s{_gbprsVersionId = a})

-- | Uniquely identifies a backup plan.
gbprsBackupPlanId :: Lens' GetBackupPlanResponse (Maybe Text)
gbprsBackupPlanId = lens _gbprsBackupPlanId (\ s a -> s{_gbprsBackupPlanId = a})

-- | A unique string that identifies the request and allows failed requests to be retried without the risk of executing the operation twice.
gbprsCreatorRequestId :: Lens' GetBackupPlanResponse (Maybe Text)
gbprsCreatorRequestId = lens _gbprsCreatorRequestId (\ s a -> s{_gbprsCreatorRequestId = a})

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan; for example, @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@ .
gbprsBackupPlanARN :: Lens' GetBackupPlanResponse (Maybe Text)
gbprsBackupPlanARN = lens _gbprsBackupPlanARN (\ s a -> s{_gbprsBackupPlanARN = a})

-- | The last time a job to back up resources was executed with this backup plan. A date and time, in Unix format and Coordinated Universal Time (UTC). The value of @LastExecutionDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
gbprsLastExecutionDate :: Lens' GetBackupPlanResponse (Maybe UTCTime)
gbprsLastExecutionDate = lens _gbprsLastExecutionDate (\ s a -> s{_gbprsLastExecutionDate = a}) . mapping _Time

-- | Specifies the body of a backup plan. Includes a @BackupPlanName@ and one or more sets of @Rules@ .
gbprsBackupPlan :: Lens' GetBackupPlanResponse (Maybe BackupPlan)
gbprsBackupPlan = lens _gbprsBackupPlan (\ s a -> s{_gbprsBackupPlan = a})

-- | The date and time that a backup plan is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
gbprsCreationDate :: Lens' GetBackupPlanResponse (Maybe UTCTime)
gbprsCreationDate = lens _gbprsCreationDate (\ s a -> s{_gbprsCreationDate = a}) . mapping _Time

-- | The date and time that a backup plan is deleted, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
gbprsDeletionDate :: Lens' GetBackupPlanResponse (Maybe UTCTime)
gbprsDeletionDate = lens _gbprsDeletionDate (\ s a -> s{_gbprsDeletionDate = a}) . mapping _Time

-- | -- | The response status code.
gbprsResponseStatus :: Lens' GetBackupPlanResponse Int
gbprsResponseStatus = lens _gbprsResponseStatus (\ s a -> s{_gbprsResponseStatus = a})

instance NFData GetBackupPlanResponse where
