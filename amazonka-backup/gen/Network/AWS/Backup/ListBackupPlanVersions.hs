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
-- Module      : Network.AWS.Backup.ListBackupPlanVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns version metadata of your backup plans, including Amazon Resource Names (ARNs), backup plan IDs, creation and deletion dates, plan names, and version IDs.
--
--
module Network.AWS.Backup.ListBackupPlanVersions
    (
    -- * Creating a Request
      listBackupPlanVersions
    , ListBackupPlanVersions
    -- * Request Lenses
    , lbpvNextToken
    , lbpvMaxResults
    , lbpvBackupPlanId

    -- * Destructuring the Response
    , listBackupPlanVersionsResponse
    , ListBackupPlanVersionsResponse
    -- * Response Lenses
    , lbpvrsBackupPlanVersionsList
    , lbpvrsNextToken
    , lbpvrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listBackupPlanVersions' smart constructor.
data ListBackupPlanVersions = ListBackupPlanVersions'
  { _lbpvNextToken :: !(Maybe Text)
  , _lbpvMaxResults :: !(Maybe Nat)
  , _lbpvBackupPlanId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBackupPlanVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbpvNextToken' - The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
--
-- * 'lbpvMaxResults' - The maximum number of items to be returned.
--
-- * 'lbpvBackupPlanId' - Uniquely identifies a backup plan.
listBackupPlanVersions
    :: Text -- ^ 'lbpvBackupPlanId'
    -> ListBackupPlanVersions
listBackupPlanVersions pBackupPlanId_ =
  ListBackupPlanVersions'
    { _lbpvNextToken = Nothing
    , _lbpvMaxResults = Nothing
    , _lbpvBackupPlanId = pBackupPlanId_
    }


-- | The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
lbpvNextToken :: Lens' ListBackupPlanVersions (Maybe Text)
lbpvNextToken = lens _lbpvNextToken (\ s a -> s{_lbpvNextToken = a})

-- | The maximum number of items to be returned.
lbpvMaxResults :: Lens' ListBackupPlanVersions (Maybe Natural)
lbpvMaxResults = lens _lbpvMaxResults (\ s a -> s{_lbpvMaxResults = a}) . mapping _Nat

-- | Uniquely identifies a backup plan.
lbpvBackupPlanId :: Lens' ListBackupPlanVersions Text
lbpvBackupPlanId = lens _lbpvBackupPlanId (\ s a -> s{_lbpvBackupPlanId = a})

instance AWSRequest ListBackupPlanVersions where
        type Rs ListBackupPlanVersions =
             ListBackupPlanVersionsResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 ListBackupPlanVersionsResponse' <$>
                   (x .?> "BackupPlanVersionsList" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListBackupPlanVersions where

instance NFData ListBackupPlanVersions where

instance ToHeaders ListBackupPlanVersions where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListBackupPlanVersions where
        toPath ListBackupPlanVersions'{..}
          = mconcat
              ["/backup/plans/", toBS _lbpvBackupPlanId,
               "/versions/"]

instance ToQuery ListBackupPlanVersions where
        toQuery ListBackupPlanVersions'{..}
          = mconcat
              ["nextToken" =: _lbpvNextToken,
               "maxResults" =: _lbpvMaxResults]

-- | /See:/ 'listBackupPlanVersionsResponse' smart constructor.
data ListBackupPlanVersionsResponse = ListBackupPlanVersionsResponse'
  { _lbpvrsBackupPlanVersionsList :: !(Maybe [BackupPlansListMember])
  , _lbpvrsNextToken :: !(Maybe Text)
  , _lbpvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBackupPlanVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbpvrsBackupPlanVersionsList' - An array of version list items containing metadata about your backup plans.
--
-- * 'lbpvrsNextToken' - The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
--
-- * 'lbpvrsResponseStatus' - -- | The response status code.
listBackupPlanVersionsResponse
    :: Int -- ^ 'lbpvrsResponseStatus'
    -> ListBackupPlanVersionsResponse
listBackupPlanVersionsResponse pResponseStatus_ =
  ListBackupPlanVersionsResponse'
    { _lbpvrsBackupPlanVersionsList = Nothing
    , _lbpvrsNextToken = Nothing
    , _lbpvrsResponseStatus = pResponseStatus_
    }


-- | An array of version list items containing metadata about your backup plans.
lbpvrsBackupPlanVersionsList :: Lens' ListBackupPlanVersionsResponse [BackupPlansListMember]
lbpvrsBackupPlanVersionsList = lens _lbpvrsBackupPlanVersionsList (\ s a -> s{_lbpvrsBackupPlanVersionsList = a}) . _Default . _Coerce

-- | The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
lbpvrsNextToken :: Lens' ListBackupPlanVersionsResponse (Maybe Text)
lbpvrsNextToken = lens _lbpvrsNextToken (\ s a -> s{_lbpvrsNextToken = a})

-- | -- | The response status code.
lbpvrsResponseStatus :: Lens' ListBackupPlanVersionsResponse Int
lbpvrsResponseStatus = lens _lbpvrsResponseStatus (\ s a -> s{_lbpvrsResponseStatus = a})

instance NFData ListBackupPlanVersionsResponse where
