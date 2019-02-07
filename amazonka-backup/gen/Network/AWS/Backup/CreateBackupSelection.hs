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
-- Module      : Network.AWS.Backup.CreateBackupSelection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a JSON document that specifies a set of resources to assign to a backup plan. Resources can be included by specifying patterns for a @ListOfTags@ and selected @Resources@ . 
--
--
-- For example, consider the following patterns:
--
--     * @Resources: "arn:aws:ec2:region:account-id:volume/volume-id"@ 
--
--     * @ConditionKey:"department"@ 
--
-- @ConditionValue:"finance"@ 
--
-- @ConditionType:"StringEquals"@ 
--
--     * @ConditionKey:"importance"@ 
--
-- @ConditionValue:"critical"@ 
--
-- @ConditionType:"StringEquals"@ 
--
--
--
-- Using these patterns would back up all Amazon Elastic Block Store (Amazon EBS) volumes that are tagged as @"department=finance"@ , @"importance=critical"@ , in addition to an EBS volume with the specified volume Id.
--
-- Resources and conditions are additive in that all resources that match the pattern are selected. This shouldn't be confused with a logical AND, where all conditions must match. The matching patterns are logically 'put together using the OR operator. In other words, all patterns that match are selected for backup.
--
module Network.AWS.Backup.CreateBackupSelection
    (
    -- * Creating a Request
      createBackupSelection
    , CreateBackupSelection
    -- * Request Lenses
    , cbsCreatorRequestId
    , cbsBackupPlanId
    , cbsBackupSelection

    -- * Destructuring the Response
    , createBackupSelectionResponse
    , CreateBackupSelectionResponse
    -- * Response Lenses
    , cbsrsSelectionId
    , cbsrsBackupPlanId
    , cbsrsCreationDate
    , cbsrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createBackupSelection' smart constructor.
data CreateBackupSelection = CreateBackupSelection'
  { _cbsCreatorRequestId :: !(Maybe Text)
  , _cbsBackupPlanId :: !Text
  , _cbsBackupSelection :: !BackupSelection
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBackupSelection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbsCreatorRequestId' - A unique string that identifies the request and allows failed requests to be retried without the risk of executing the operation twice.
--
-- * 'cbsBackupPlanId' - Uniquely identifies the backup plan to be associated with the selection of resources.
--
-- * 'cbsBackupSelection' - Specifies the body of a request to assign a set of resources to a backup plan. It includes an array of resources, an optional array of patterns to exclude resources, an optional role to provide access to the AWS service the resource belongs to, and an optional array of tags used to identify a set of resources.
createBackupSelection
    :: Text -- ^ 'cbsBackupPlanId'
    -> BackupSelection -- ^ 'cbsBackupSelection'
    -> CreateBackupSelection
createBackupSelection pBackupPlanId_ pBackupSelection_ =
  CreateBackupSelection'
    { _cbsCreatorRequestId = Nothing
    , _cbsBackupPlanId = pBackupPlanId_
    , _cbsBackupSelection = pBackupSelection_
    }


-- | A unique string that identifies the request and allows failed requests to be retried without the risk of executing the operation twice.
cbsCreatorRequestId :: Lens' CreateBackupSelection (Maybe Text)
cbsCreatorRequestId = lens _cbsCreatorRequestId (\ s a -> s{_cbsCreatorRequestId = a})

-- | Uniquely identifies the backup plan to be associated with the selection of resources.
cbsBackupPlanId :: Lens' CreateBackupSelection Text
cbsBackupPlanId = lens _cbsBackupPlanId (\ s a -> s{_cbsBackupPlanId = a})

-- | Specifies the body of a request to assign a set of resources to a backup plan. It includes an array of resources, an optional array of patterns to exclude resources, an optional role to provide access to the AWS service the resource belongs to, and an optional array of tags used to identify a set of resources.
cbsBackupSelection :: Lens' CreateBackupSelection BackupSelection
cbsBackupSelection = lens _cbsBackupSelection (\ s a -> s{_cbsBackupSelection = a})

instance AWSRequest CreateBackupSelection where
        type Rs CreateBackupSelection =
             CreateBackupSelectionResponse
        request = putJSON backup
        response
          = receiveJSON
              (\ s h x ->
                 CreateBackupSelectionResponse' <$>
                   (x .?> "SelectionId") <*> (x .?> "BackupPlanId") <*>
                     (x .?> "CreationDate")
                     <*> (pure (fromEnum s)))

instance Hashable CreateBackupSelection where

instance NFData CreateBackupSelection where

instance ToHeaders CreateBackupSelection where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateBackupSelection where
        toJSON CreateBackupSelection'{..}
          = object
              (catMaybes
                 [("CreatorRequestId" .=) <$> _cbsCreatorRequestId,
                  Just ("BackupSelection" .= _cbsBackupSelection)])

instance ToPath CreateBackupSelection where
        toPath CreateBackupSelection'{..}
          = mconcat
              ["/backup/plans/", toBS _cbsBackupPlanId,
               "/selections/"]

instance ToQuery CreateBackupSelection where
        toQuery = const mempty

-- | /See:/ 'createBackupSelectionResponse' smart constructor.
data CreateBackupSelectionResponse = CreateBackupSelectionResponse'
  { _cbsrsSelectionId :: !(Maybe Text)
  , _cbsrsBackupPlanId :: !(Maybe Text)
  , _cbsrsCreationDate :: !(Maybe POSIX)
  , _cbsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBackupSelectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbsrsSelectionId' - Uniquely identifies the body of a request to assign a set of resources to a backup plan.
--
-- * 'cbsrsBackupPlanId' - Uniquely identifies a backup plan.
--
-- * 'cbsrsCreationDate' - The date and time a backup selection is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'cbsrsResponseStatus' - -- | The response status code.
createBackupSelectionResponse
    :: Int -- ^ 'cbsrsResponseStatus'
    -> CreateBackupSelectionResponse
createBackupSelectionResponse pResponseStatus_ =
  CreateBackupSelectionResponse'
    { _cbsrsSelectionId = Nothing
    , _cbsrsBackupPlanId = Nothing
    , _cbsrsCreationDate = Nothing
    , _cbsrsResponseStatus = pResponseStatus_
    }


-- | Uniquely identifies the body of a request to assign a set of resources to a backup plan.
cbsrsSelectionId :: Lens' CreateBackupSelectionResponse (Maybe Text)
cbsrsSelectionId = lens _cbsrsSelectionId (\ s a -> s{_cbsrsSelectionId = a})

-- | Uniquely identifies a backup plan.
cbsrsBackupPlanId :: Lens' CreateBackupSelectionResponse (Maybe Text)
cbsrsBackupPlanId = lens _cbsrsBackupPlanId (\ s a -> s{_cbsrsBackupPlanId = a})

-- | The date and time a backup selection is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
cbsrsCreationDate :: Lens' CreateBackupSelectionResponse (Maybe UTCTime)
cbsrsCreationDate = lens _cbsrsCreationDate (\ s a -> s{_cbsrsCreationDate = a}) . mapping _Time

-- | -- | The response status code.
cbsrsResponseStatus :: Lens' CreateBackupSelectionResponse Int
cbsrsResponseStatus = lens _cbsrsResponseStatus (\ s a -> s{_cbsrsResponseStatus = a})

instance NFData CreateBackupSelectionResponse where
