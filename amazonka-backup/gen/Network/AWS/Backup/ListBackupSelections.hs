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
-- Module      : Network.AWS.Backup.ListBackupSelections
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array containing metadata of the resources associated with the target backup plan.
--
--
module Network.AWS.Backup.ListBackupSelections
    (
    -- * Creating a Request
      listBackupSelections
    , ListBackupSelections
    -- * Request Lenses
    , lbsNextToken
    , lbsMaxResults
    , lbsBackupPlanId

    -- * Destructuring the Response
    , listBackupSelectionsResponse
    , ListBackupSelectionsResponse
    -- * Response Lenses
    , lbsrsNextToken
    , lbsrsBackupSelectionsList
    , lbsrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listBackupSelections' smart constructor.
data ListBackupSelections = ListBackupSelections'
  { _lbsNextToken :: !(Maybe Text)
  , _lbsMaxResults :: !(Maybe Nat)
  , _lbsBackupPlanId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBackupSelections' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbsNextToken' - The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
--
-- * 'lbsMaxResults' - The maximum number of items to be returned.
--
-- * 'lbsBackupPlanId' - Uniquely identifies a backup plan.
listBackupSelections
    :: Text -- ^ 'lbsBackupPlanId'
    -> ListBackupSelections
listBackupSelections pBackupPlanId_ =
  ListBackupSelections'
    { _lbsNextToken = Nothing
    , _lbsMaxResults = Nothing
    , _lbsBackupPlanId = pBackupPlanId_
    }


-- | The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
lbsNextToken :: Lens' ListBackupSelections (Maybe Text)
lbsNextToken = lens _lbsNextToken (\ s a -> s{_lbsNextToken = a})

-- | The maximum number of items to be returned.
lbsMaxResults :: Lens' ListBackupSelections (Maybe Natural)
lbsMaxResults = lens _lbsMaxResults (\ s a -> s{_lbsMaxResults = a}) . mapping _Nat

-- | Uniquely identifies a backup plan.
lbsBackupPlanId :: Lens' ListBackupSelections Text
lbsBackupPlanId = lens _lbsBackupPlanId (\ s a -> s{_lbsBackupPlanId = a})

instance AWSRequest ListBackupSelections where
        type Rs ListBackupSelections =
             ListBackupSelectionsResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 ListBackupSelectionsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "BackupSelectionsList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListBackupSelections where

instance NFData ListBackupSelections where

instance ToHeaders ListBackupSelections where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListBackupSelections where
        toPath ListBackupSelections'{..}
          = mconcat
              ["/backup/plans/", toBS _lbsBackupPlanId,
               "/selections/"]

instance ToQuery ListBackupSelections where
        toQuery ListBackupSelections'{..}
          = mconcat
              ["nextToken" =: _lbsNextToken,
               "maxResults" =: _lbsMaxResults]

-- | /See:/ 'listBackupSelectionsResponse' smart constructor.
data ListBackupSelectionsResponse = ListBackupSelectionsResponse'
  { _lbsrsNextToken :: !(Maybe Text)
  , _lbsrsBackupSelectionsList :: !(Maybe [BackupSelectionsListMember])
  , _lbsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBackupSelectionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbsrsNextToken' - The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
--
-- * 'lbsrsBackupSelectionsList' - An array of backup selection list items containing metadata about each resource in the list.
--
-- * 'lbsrsResponseStatus' - -- | The response status code.
listBackupSelectionsResponse
    :: Int -- ^ 'lbsrsResponseStatus'
    -> ListBackupSelectionsResponse
listBackupSelectionsResponse pResponseStatus_ =
  ListBackupSelectionsResponse'
    { _lbsrsNextToken = Nothing
    , _lbsrsBackupSelectionsList = Nothing
    , _lbsrsResponseStatus = pResponseStatus_
    }


-- | The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
lbsrsNextToken :: Lens' ListBackupSelectionsResponse (Maybe Text)
lbsrsNextToken = lens _lbsrsNextToken (\ s a -> s{_lbsrsNextToken = a})

-- | An array of backup selection list items containing metadata about each resource in the list.
lbsrsBackupSelectionsList :: Lens' ListBackupSelectionsResponse [BackupSelectionsListMember]
lbsrsBackupSelectionsList = lens _lbsrsBackupSelectionsList (\ s a -> s{_lbsrsBackupSelectionsList = a}) . _Default . _Coerce

-- | -- | The response status code.
lbsrsResponseStatus :: Lens' ListBackupSelectionsResponse Int
lbsrsResponseStatus = lens _lbsrsResponseStatus (\ s a -> s{_lbsrsResponseStatus = a})

instance NFData ListBackupSelectionsResponse where
