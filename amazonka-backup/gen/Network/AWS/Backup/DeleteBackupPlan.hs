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
-- Module      : Network.AWS.Backup.DeleteBackupPlan
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a backup plan. A backup plan can only be deleted after all associated selections of resources have been deleted. Deleting a backup plan deletes the current version of a backup plan. Previous versions, if any, will still exist.
--
--
module Network.AWS.Backup.DeleteBackupPlan
    (
    -- * Creating a Request
      deleteBackupPlan
    , DeleteBackupPlan
    -- * Request Lenses
    , dbpBackupPlanId

    -- * Destructuring the Response
    , deleteBackupPlanResponse
    , DeleteBackupPlanResponse
    -- * Response Lenses
    , dbprsVersionId
    , dbprsBackupPlanId
    , dbprsBackupPlanARN
    , dbprsDeletionDate
    , dbprsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteBackupPlan' smart constructor.
newtype DeleteBackupPlan = DeleteBackupPlan'
  { _dbpBackupPlanId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBackupPlan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbpBackupPlanId' - Uniquely identifies a backup plan.
deleteBackupPlan
    :: Text -- ^ 'dbpBackupPlanId'
    -> DeleteBackupPlan
deleteBackupPlan pBackupPlanId_ =
  DeleteBackupPlan' {_dbpBackupPlanId = pBackupPlanId_}


-- | Uniquely identifies a backup plan.
dbpBackupPlanId :: Lens' DeleteBackupPlan Text
dbpBackupPlanId = lens _dbpBackupPlanId (\ s a -> s{_dbpBackupPlanId = a})

instance AWSRequest DeleteBackupPlan where
        type Rs DeleteBackupPlan = DeleteBackupPlanResponse
        request = delete backup
        response
          = receiveJSON
              (\ s h x ->
                 DeleteBackupPlanResponse' <$>
                   (x .?> "VersionId") <*> (x .?> "BackupPlanId") <*>
                     (x .?> "BackupPlanArn")
                     <*> (x .?> "DeletionDate")
                     <*> (pure (fromEnum s)))

instance Hashable DeleteBackupPlan where

instance NFData DeleteBackupPlan where

instance ToHeaders DeleteBackupPlan where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteBackupPlan where
        toPath DeleteBackupPlan'{..}
          = mconcat ["/backup/plans/", toBS _dbpBackupPlanId]

instance ToQuery DeleteBackupPlan where
        toQuery = const mempty

-- | /See:/ 'deleteBackupPlanResponse' smart constructor.
data DeleteBackupPlanResponse = DeleteBackupPlanResponse'
  { _dbprsVersionId :: !(Maybe Text)
  , _dbprsBackupPlanId :: !(Maybe Text)
  , _dbprsBackupPlanARN :: !(Maybe Text)
  , _dbprsDeletionDate :: !(Maybe POSIX)
  , _dbprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBackupPlanResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbprsVersionId' - Unique, randomly generated, Unicode, UTF-8 encoded strings that are at most 1,024 bytes long. Version Ids cannot be edited.
--
-- * 'dbprsBackupPlanId' - Uniquely identifies a backup plan.
--
-- * 'dbprsBackupPlanARN' - An Amazon Resource Name (ARN) that uniquely identifies a backup plan; for example, @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@ .
--
-- * 'dbprsDeletionDate' - The date and time a backup plan is deleted, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'dbprsResponseStatus' - -- | The response status code.
deleteBackupPlanResponse
    :: Int -- ^ 'dbprsResponseStatus'
    -> DeleteBackupPlanResponse
deleteBackupPlanResponse pResponseStatus_ =
  DeleteBackupPlanResponse'
    { _dbprsVersionId = Nothing
    , _dbprsBackupPlanId = Nothing
    , _dbprsBackupPlanARN = Nothing
    , _dbprsDeletionDate = Nothing
    , _dbprsResponseStatus = pResponseStatus_
    }


-- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at most 1,024 bytes long. Version Ids cannot be edited.
dbprsVersionId :: Lens' DeleteBackupPlanResponse (Maybe Text)
dbprsVersionId = lens _dbprsVersionId (\ s a -> s{_dbprsVersionId = a})

-- | Uniquely identifies a backup plan.
dbprsBackupPlanId :: Lens' DeleteBackupPlanResponse (Maybe Text)
dbprsBackupPlanId = lens _dbprsBackupPlanId (\ s a -> s{_dbprsBackupPlanId = a})

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan; for example, @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@ .
dbprsBackupPlanARN :: Lens' DeleteBackupPlanResponse (Maybe Text)
dbprsBackupPlanARN = lens _dbprsBackupPlanARN (\ s a -> s{_dbprsBackupPlanARN = a})

-- | The date and time a backup plan is deleted, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
dbprsDeletionDate :: Lens' DeleteBackupPlanResponse (Maybe UTCTime)
dbprsDeletionDate = lens _dbprsDeletionDate (\ s a -> s{_dbprsDeletionDate = a}) . mapping _Time

-- | -- | The response status code.
dbprsResponseStatus :: Lens' DeleteBackupPlanResponse Int
dbprsResponseStatus = lens _dbprsResponseStatus (\ s a -> s{_dbprsResponseStatus = a})

instance NFData DeleteBackupPlanResponse where
