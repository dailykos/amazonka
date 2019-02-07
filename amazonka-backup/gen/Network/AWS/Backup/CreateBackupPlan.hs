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
-- Module      : Network.AWS.Backup.CreateBackupPlan
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Backup plans are documents that contain information that AWS Backup uses to schedule tasks that create recovery points of resources.
--
--
-- If you call @CreateBackupPlan@ with a plan that already exists, the existing @backupPlanId@ is returned.
--
module Network.AWS.Backup.CreateBackupPlan
    (
    -- * Creating a Request
      createBackupPlan
    , CreateBackupPlan
    -- * Request Lenses
    , cbpBackupPlanTags
    , cbpCreatorRequestId
    , cbpBackupPlan

    -- * Destructuring the Response
    , createBackupPlanResponse
    , CreateBackupPlanResponse
    -- * Response Lenses
    , cbprsVersionId
    , cbprsBackupPlanId
    , cbprsBackupPlanARN
    , cbprsCreationDate
    , cbprsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createBackupPlan' smart constructor.
data CreateBackupPlan = CreateBackupPlan'
  { _cbpBackupPlanTags :: !(Maybe (Sensitive (Map Text Text)))
  , _cbpCreatorRequestId :: !(Maybe Text)
  , _cbpBackupPlan :: !BackupPlanInput
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBackupPlan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbpBackupPlanTags' - To help organize your resources, you can assign your own metadata to the resources that you create. Each tag is a key-value pair. The specified tags are assigned to all backups created with this plan.
--
-- * 'cbpCreatorRequestId' - Identifies the request and allows failed requests to be retried without the risk of executing the operation twice. If the request includes a @CreatorRequestId@ that matches an existing backup plan, that plan is returned. This parameter is optional.
--
-- * 'cbpBackupPlan' - Specifies the body of a backup plan. Includes a @BackupPlanName@ and one or more sets of @Rules@ .
createBackupPlan
    :: BackupPlanInput -- ^ 'cbpBackupPlan'
    -> CreateBackupPlan
createBackupPlan pBackupPlan_ =
  CreateBackupPlan'
    { _cbpBackupPlanTags = Nothing
    , _cbpCreatorRequestId = Nothing
    , _cbpBackupPlan = pBackupPlan_
    }


-- | To help organize your resources, you can assign your own metadata to the resources that you create. Each tag is a key-value pair. The specified tags are assigned to all backups created with this plan.
cbpBackupPlanTags :: Lens' CreateBackupPlan (Maybe (HashMap Text Text))
cbpBackupPlanTags = lens _cbpBackupPlanTags (\ s a -> s{_cbpBackupPlanTags = a}) . mapping (_Sensitive . _Map)

-- | Identifies the request and allows failed requests to be retried without the risk of executing the operation twice. If the request includes a @CreatorRequestId@ that matches an existing backup plan, that plan is returned. This parameter is optional.
cbpCreatorRequestId :: Lens' CreateBackupPlan (Maybe Text)
cbpCreatorRequestId = lens _cbpCreatorRequestId (\ s a -> s{_cbpCreatorRequestId = a})

-- | Specifies the body of a backup plan. Includes a @BackupPlanName@ and one or more sets of @Rules@ .
cbpBackupPlan :: Lens' CreateBackupPlan BackupPlanInput
cbpBackupPlan = lens _cbpBackupPlan (\ s a -> s{_cbpBackupPlan = a})

instance AWSRequest CreateBackupPlan where
        type Rs CreateBackupPlan = CreateBackupPlanResponse
        request = putJSON backup
        response
          = receiveJSON
              (\ s h x ->
                 CreateBackupPlanResponse' <$>
                   (x .?> "VersionId") <*> (x .?> "BackupPlanId") <*>
                     (x .?> "BackupPlanArn")
                     <*> (x .?> "CreationDate")
                     <*> (pure (fromEnum s)))

instance Hashable CreateBackupPlan where

instance NFData CreateBackupPlan where

instance ToHeaders CreateBackupPlan where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateBackupPlan where
        toJSON CreateBackupPlan'{..}
          = object
              (catMaybes
                 [("BackupPlanTags" .=) <$> _cbpBackupPlanTags,
                  ("CreatorRequestId" .=) <$> _cbpCreatorRequestId,
                  Just ("BackupPlan" .= _cbpBackupPlan)])

instance ToPath CreateBackupPlan where
        toPath = const "/backup/plans/"

instance ToQuery CreateBackupPlan where
        toQuery = const mempty

-- | /See:/ 'createBackupPlanResponse' smart constructor.
data CreateBackupPlanResponse = CreateBackupPlanResponse'
  { _cbprsVersionId :: !(Maybe Text)
  , _cbprsBackupPlanId :: !(Maybe Text)
  , _cbprsBackupPlanARN :: !(Maybe Text)
  , _cbprsCreationDate :: !(Maybe POSIX)
  , _cbprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBackupPlanResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbprsVersionId' - Unique, randomly generated, Unicode, UTF-8 encoded strings that are at most 1024 bytes long. They cannot be edited.
--
-- * 'cbprsBackupPlanId' - Uniquely identifies a backup plan.
--
-- * 'cbprsBackupPlanARN' - An Amazon Resource Name (ARN) that uniquely identifies a backup plan; for example, @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@ .
--
-- * 'cbprsCreationDate' - The date and time that a backup plan is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'cbprsResponseStatus' - -- | The response status code.
createBackupPlanResponse
    :: Int -- ^ 'cbprsResponseStatus'
    -> CreateBackupPlanResponse
createBackupPlanResponse pResponseStatus_ =
  CreateBackupPlanResponse'
    { _cbprsVersionId = Nothing
    , _cbprsBackupPlanId = Nothing
    , _cbprsBackupPlanARN = Nothing
    , _cbprsCreationDate = Nothing
    , _cbprsResponseStatus = pResponseStatus_
    }


-- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at most 1024 bytes long. They cannot be edited.
cbprsVersionId :: Lens' CreateBackupPlanResponse (Maybe Text)
cbprsVersionId = lens _cbprsVersionId (\ s a -> s{_cbprsVersionId = a})

-- | Uniquely identifies a backup plan.
cbprsBackupPlanId :: Lens' CreateBackupPlanResponse (Maybe Text)
cbprsBackupPlanId = lens _cbprsBackupPlanId (\ s a -> s{_cbprsBackupPlanId = a})

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan; for example, @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@ .
cbprsBackupPlanARN :: Lens' CreateBackupPlanResponse (Maybe Text)
cbprsBackupPlanARN = lens _cbprsBackupPlanARN (\ s a -> s{_cbprsBackupPlanARN = a})

-- | The date and time that a backup plan is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
cbprsCreationDate :: Lens' CreateBackupPlanResponse (Maybe UTCTime)
cbprsCreationDate = lens _cbprsCreationDate (\ s a -> s{_cbprsCreationDate = a}) . mapping _Time

-- | -- | The response status code.
cbprsResponseStatus :: Lens' CreateBackupPlanResponse Int
cbprsResponseStatus = lens _cbprsResponseStatus (\ s a -> s{_cbprsResponseStatus = a})

instance NFData CreateBackupPlanResponse where
