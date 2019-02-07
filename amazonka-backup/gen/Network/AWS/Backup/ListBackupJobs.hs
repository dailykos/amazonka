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
-- Module      : Network.AWS.Backup.ListBackupJobs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata about your backup jobs.
--
--
module Network.AWS.Backup.ListBackupJobs
    (
    -- * Creating a Request
      listBackupJobs
    , ListBackupJobs
    -- * Request Lenses
    , lbjByResourceARN
    , lbjByCreatedAfter
    , lbjByCreatedBefore
    , lbjByBackupVaultName
    , lbjByResourceType
    , lbjNextToken
    , lbjByState
    , lbjMaxResults

    -- * Destructuring the Response
    , listBackupJobsResponse
    , ListBackupJobsResponse
    -- * Response Lenses
    , lbjrsBackupJobs
    , lbjrsNextToken
    , lbjrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listBackupJobs' smart constructor.
data ListBackupJobs = ListBackupJobs'
  { _lbjByResourceARN :: !(Maybe Text)
  , _lbjByCreatedAfter :: !(Maybe POSIX)
  , _lbjByCreatedBefore :: !(Maybe POSIX)
  , _lbjByBackupVaultName :: !(Maybe Text)
  , _lbjByResourceType :: !(Maybe Text)
  , _lbjNextToken :: !(Maybe Text)
  , _lbjByState :: !(Maybe BackupJobState)
  , _lbjMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBackupJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbjByResourceARN' - Returns only backup jobs that match the specified resource Amazon Resource Name (ARN).
--
-- * 'lbjByCreatedAfter' - Returns only backup jobs that were created after the specified date.
--
-- * 'lbjByCreatedBefore' - Returns only backup jobs that were created before the specified date.
--
-- * 'lbjByBackupVaultName' - Returns only backup jobs that will be stored in the specified backup vault. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
--
-- * 'lbjByResourceType' - Returns only backup jobs for the specified resources:     * @EBS@ for Amazon Elastic Block Store     * @SGW@ for AWS Storage Gateway     * @RDS@ for Amazon Relational Database Service     * @DDB@ for Amazon DynamoDB     * @EFS@ for Amazon Elastic File System
--
-- * 'lbjNextToken' - The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
--
-- * 'lbjByState' - Returns only backup jobs that are in the specified state.
--
-- * 'lbjMaxResults' - The maximum number of items to be returned.
listBackupJobs
    :: ListBackupJobs
listBackupJobs =
  ListBackupJobs'
    { _lbjByResourceARN = Nothing
    , _lbjByCreatedAfter = Nothing
    , _lbjByCreatedBefore = Nothing
    , _lbjByBackupVaultName = Nothing
    , _lbjByResourceType = Nothing
    , _lbjNextToken = Nothing
    , _lbjByState = Nothing
    , _lbjMaxResults = Nothing
    }


-- | Returns only backup jobs that match the specified resource Amazon Resource Name (ARN).
lbjByResourceARN :: Lens' ListBackupJobs (Maybe Text)
lbjByResourceARN = lens _lbjByResourceARN (\ s a -> s{_lbjByResourceARN = a})

-- | Returns only backup jobs that were created after the specified date.
lbjByCreatedAfter :: Lens' ListBackupJobs (Maybe UTCTime)
lbjByCreatedAfter = lens _lbjByCreatedAfter (\ s a -> s{_lbjByCreatedAfter = a}) . mapping _Time

-- | Returns only backup jobs that were created before the specified date.
lbjByCreatedBefore :: Lens' ListBackupJobs (Maybe UTCTime)
lbjByCreatedBefore = lens _lbjByCreatedBefore (\ s a -> s{_lbjByCreatedBefore = a}) . mapping _Time

-- | Returns only backup jobs that will be stored in the specified backup vault. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
lbjByBackupVaultName :: Lens' ListBackupJobs (Maybe Text)
lbjByBackupVaultName = lens _lbjByBackupVaultName (\ s a -> s{_lbjByBackupVaultName = a})

-- | Returns only backup jobs for the specified resources:     * @EBS@ for Amazon Elastic Block Store     * @SGW@ for AWS Storage Gateway     * @RDS@ for Amazon Relational Database Service     * @DDB@ for Amazon DynamoDB     * @EFS@ for Amazon Elastic File System
lbjByResourceType :: Lens' ListBackupJobs (Maybe Text)
lbjByResourceType = lens _lbjByResourceType (\ s a -> s{_lbjByResourceType = a})

-- | The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
lbjNextToken :: Lens' ListBackupJobs (Maybe Text)
lbjNextToken = lens _lbjNextToken (\ s a -> s{_lbjNextToken = a})

-- | Returns only backup jobs that are in the specified state.
lbjByState :: Lens' ListBackupJobs (Maybe BackupJobState)
lbjByState = lens _lbjByState (\ s a -> s{_lbjByState = a})

-- | The maximum number of items to be returned.
lbjMaxResults :: Lens' ListBackupJobs (Maybe Natural)
lbjMaxResults = lens _lbjMaxResults (\ s a -> s{_lbjMaxResults = a}) . mapping _Nat

instance AWSRequest ListBackupJobs where
        type Rs ListBackupJobs = ListBackupJobsResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 ListBackupJobsResponse' <$>
                   (x .?> "BackupJobs" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListBackupJobs where

instance NFData ListBackupJobs where

instance ToHeaders ListBackupJobs where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListBackupJobs where
        toPath = const "/backup-jobs/"

instance ToQuery ListBackupJobs where
        toQuery ListBackupJobs'{..}
          = mconcat
              ["resourceArn" =: _lbjByResourceARN,
               "createdAfter" =: _lbjByCreatedAfter,
               "createdBefore" =: _lbjByCreatedBefore,
               "backupVaultName" =: _lbjByBackupVaultName,
               "resourceType" =: _lbjByResourceType,
               "nextToken" =: _lbjNextToken, "state" =: _lbjByState,
               "maxResults" =: _lbjMaxResults]

-- | /See:/ 'listBackupJobsResponse' smart constructor.
data ListBackupJobsResponse = ListBackupJobsResponse'
  { _lbjrsBackupJobs :: !(Maybe [BackupJob])
  , _lbjrsNextToken :: !(Maybe Text)
  , _lbjrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBackupJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbjrsBackupJobs' - An array of structures containing metadata about your backup jobs returned in JSON format.
--
-- * 'lbjrsNextToken' - The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
--
-- * 'lbjrsResponseStatus' - -- | The response status code.
listBackupJobsResponse
    :: Int -- ^ 'lbjrsResponseStatus'
    -> ListBackupJobsResponse
listBackupJobsResponse pResponseStatus_ =
  ListBackupJobsResponse'
    { _lbjrsBackupJobs = Nothing
    , _lbjrsNextToken = Nothing
    , _lbjrsResponseStatus = pResponseStatus_
    }


-- | An array of structures containing metadata about your backup jobs returned in JSON format.
lbjrsBackupJobs :: Lens' ListBackupJobsResponse [BackupJob]
lbjrsBackupJobs = lens _lbjrsBackupJobs (\ s a -> s{_lbjrsBackupJobs = a}) . _Default . _Coerce

-- | The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
lbjrsNextToken :: Lens' ListBackupJobsResponse (Maybe Text)
lbjrsNextToken = lens _lbjrsNextToken (\ s a -> s{_lbjrsNextToken = a})

-- | -- | The response status code.
lbjrsResponseStatus :: Lens' ListBackupJobsResponse Int
lbjrsResponseStatus = lens _lbjrsResponseStatus (\ s a -> s{_lbjrsResponseStatus = a})

instance NFData ListBackupJobsResponse where
