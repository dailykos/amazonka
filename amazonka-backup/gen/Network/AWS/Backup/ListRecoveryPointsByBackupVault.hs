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
-- Module      : Network.AWS.Backup.ListRecoveryPointsByBackupVault
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about the recovery points stored in a backup vault.
--
--
module Network.AWS.Backup.ListRecoveryPointsByBackupVault
    (
    -- * Creating a Request
      listRecoveryPointsByBackupVault
    , ListRecoveryPointsByBackupVault
    -- * Request Lenses
    , lrpbbvByResourceARN
    , lrpbbvByCreatedAfter
    , lrpbbvByCreatedBefore
    , lrpbbvByBackupPlanId
    , lrpbbvByResourceType
    , lrpbbvNextToken
    , lrpbbvMaxResults
    , lrpbbvBackupVaultName

    -- * Destructuring the Response
    , listRecoveryPointsByBackupVaultResponse
    , ListRecoveryPointsByBackupVaultResponse
    -- * Response Lenses
    , lrpbbvrsRecoveryPoints
    , lrpbbvrsNextToken
    , lrpbbvrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listRecoveryPointsByBackupVault' smart constructor.
data ListRecoveryPointsByBackupVault = ListRecoveryPointsByBackupVault'
  { _lrpbbvByResourceARN :: !(Maybe Text)
  , _lrpbbvByCreatedAfter :: !(Maybe POSIX)
  , _lrpbbvByCreatedBefore :: !(Maybe POSIX)
  , _lrpbbvByBackupPlanId :: !(Maybe Text)
  , _lrpbbvByResourceType :: !(Maybe Text)
  , _lrpbbvNextToken :: !(Maybe Text)
  , _lrpbbvMaxResults :: !(Maybe Nat)
  , _lrpbbvBackupVaultName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRecoveryPointsByBackupVault' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrpbbvByResourceARN' - Returns only recovery points that match the specified resource Amazon Resource Name (ARN).
--
-- * 'lrpbbvByCreatedAfter' - Returns only recovery points that were created after the specified timestamp.
--
-- * 'lrpbbvByCreatedBefore' - Returns only recovery points that were created before the specified timestamp.
--
-- * 'lrpbbvByBackupPlanId' - Returns only recovery points that match the specified backup plan ID.
--
-- * 'lrpbbvByResourceType' - Returns only recovery points that match the specified resource type.
--
-- * 'lrpbbvNextToken' - The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
--
-- * 'lrpbbvMaxResults' - The maximum number of items to be returned.
--
-- * 'lrpbbvBackupVaultName' - The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
listRecoveryPointsByBackupVault
    :: Text -- ^ 'lrpbbvBackupVaultName'
    -> ListRecoveryPointsByBackupVault
listRecoveryPointsByBackupVault pBackupVaultName_ =
  ListRecoveryPointsByBackupVault'
    { _lrpbbvByResourceARN = Nothing
    , _lrpbbvByCreatedAfter = Nothing
    , _lrpbbvByCreatedBefore = Nothing
    , _lrpbbvByBackupPlanId = Nothing
    , _lrpbbvByResourceType = Nothing
    , _lrpbbvNextToken = Nothing
    , _lrpbbvMaxResults = Nothing
    , _lrpbbvBackupVaultName = pBackupVaultName_
    }


-- | Returns only recovery points that match the specified resource Amazon Resource Name (ARN).
lrpbbvByResourceARN :: Lens' ListRecoveryPointsByBackupVault (Maybe Text)
lrpbbvByResourceARN = lens _lrpbbvByResourceARN (\ s a -> s{_lrpbbvByResourceARN = a})

-- | Returns only recovery points that were created after the specified timestamp.
lrpbbvByCreatedAfter :: Lens' ListRecoveryPointsByBackupVault (Maybe UTCTime)
lrpbbvByCreatedAfter = lens _lrpbbvByCreatedAfter (\ s a -> s{_lrpbbvByCreatedAfter = a}) . mapping _Time

-- | Returns only recovery points that were created before the specified timestamp.
lrpbbvByCreatedBefore :: Lens' ListRecoveryPointsByBackupVault (Maybe UTCTime)
lrpbbvByCreatedBefore = lens _lrpbbvByCreatedBefore (\ s a -> s{_lrpbbvByCreatedBefore = a}) . mapping _Time

-- | Returns only recovery points that match the specified backup plan ID.
lrpbbvByBackupPlanId :: Lens' ListRecoveryPointsByBackupVault (Maybe Text)
lrpbbvByBackupPlanId = lens _lrpbbvByBackupPlanId (\ s a -> s{_lrpbbvByBackupPlanId = a})

-- | Returns only recovery points that match the specified resource type.
lrpbbvByResourceType :: Lens' ListRecoveryPointsByBackupVault (Maybe Text)
lrpbbvByResourceType = lens _lrpbbvByResourceType (\ s a -> s{_lrpbbvByResourceType = a})

-- | The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
lrpbbvNextToken :: Lens' ListRecoveryPointsByBackupVault (Maybe Text)
lrpbbvNextToken = lens _lrpbbvNextToken (\ s a -> s{_lrpbbvNextToken = a})

-- | The maximum number of items to be returned.
lrpbbvMaxResults :: Lens' ListRecoveryPointsByBackupVault (Maybe Natural)
lrpbbvMaxResults = lens _lrpbbvMaxResults (\ s a -> s{_lrpbbvMaxResults = a}) . mapping _Nat

-- | The name of a logical container where backups are stored. Backup vaults are identified by names that are unique to the account used to create them and the AWS Region where they are created. They consist of lowercase letters, numbers, and hyphens.
lrpbbvBackupVaultName :: Lens' ListRecoveryPointsByBackupVault Text
lrpbbvBackupVaultName = lens _lrpbbvBackupVaultName (\ s a -> s{_lrpbbvBackupVaultName = a})

instance AWSRequest ListRecoveryPointsByBackupVault
         where
        type Rs ListRecoveryPointsByBackupVault =
             ListRecoveryPointsByBackupVaultResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 ListRecoveryPointsByBackupVaultResponse' <$>
                   (x .?> "RecoveryPoints" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListRecoveryPointsByBackupVault
         where

instance NFData ListRecoveryPointsByBackupVault where

instance ToHeaders ListRecoveryPointsByBackupVault
         where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListRecoveryPointsByBackupVault where
        toPath ListRecoveryPointsByBackupVault'{..}
          = mconcat
              ["/backup-vaults/", toBS _lrpbbvBackupVaultName,
               "/recovery-points/"]

instance ToQuery ListRecoveryPointsByBackupVault
         where
        toQuery ListRecoveryPointsByBackupVault'{..}
          = mconcat
              ["resourceArn" =: _lrpbbvByResourceARN,
               "createdAfter" =: _lrpbbvByCreatedAfter,
               "createdBefore" =: _lrpbbvByCreatedBefore,
               "backupPlanId" =: _lrpbbvByBackupPlanId,
               "resourceType" =: _lrpbbvByResourceType,
               "nextToken" =: _lrpbbvNextToken,
               "maxResults" =: _lrpbbvMaxResults]

-- | /See:/ 'listRecoveryPointsByBackupVaultResponse' smart constructor.
data ListRecoveryPointsByBackupVaultResponse = ListRecoveryPointsByBackupVaultResponse'
  { _lrpbbvrsRecoveryPoints :: !(Maybe [RecoveryPointByBackupVault])
  , _lrpbbvrsNextToken :: !(Maybe Text)
  , _lrpbbvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRecoveryPointsByBackupVaultResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrpbbvrsRecoveryPoints' - An array of objects that contain detailed information about recovery points saved in a backup vault.
--
-- * 'lrpbbvrsNextToken' - The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
--
-- * 'lrpbbvrsResponseStatus' - -- | The response status code.
listRecoveryPointsByBackupVaultResponse
    :: Int -- ^ 'lrpbbvrsResponseStatus'
    -> ListRecoveryPointsByBackupVaultResponse
listRecoveryPointsByBackupVaultResponse pResponseStatus_ =
  ListRecoveryPointsByBackupVaultResponse'
    { _lrpbbvrsRecoveryPoints = Nothing
    , _lrpbbvrsNextToken = Nothing
    , _lrpbbvrsResponseStatus = pResponseStatus_
    }


-- | An array of objects that contain detailed information about recovery points saved in a backup vault.
lrpbbvrsRecoveryPoints :: Lens' ListRecoveryPointsByBackupVaultResponse [RecoveryPointByBackupVault]
lrpbbvrsRecoveryPoints = lens _lrpbbvrsRecoveryPoints (\ s a -> s{_lrpbbvrsRecoveryPoints = a}) . _Default . _Coerce

-- | The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
lrpbbvrsNextToken :: Lens' ListRecoveryPointsByBackupVaultResponse (Maybe Text)
lrpbbvrsNextToken = lens _lrpbbvrsNextToken (\ s a -> s{_lrpbbvrsNextToken = a})

-- | -- | The response status code.
lrpbbvrsResponseStatus :: Lens' ListRecoveryPointsByBackupVaultResponse Int
lrpbbvrsResponseStatus = lens _lrpbbvrsResponseStatus (\ s a -> s{_lrpbbvrsResponseStatus = a})

instance NFData
           ListRecoveryPointsByBackupVaultResponse
         where
