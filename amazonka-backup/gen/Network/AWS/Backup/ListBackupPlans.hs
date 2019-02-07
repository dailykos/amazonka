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
-- Module      : Network.AWS.Backup.ListBackupPlans
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata of your saved backup plans, including Amazon Resource Names (ARNs), plan IDs, creation and deletion dates, version IDs, plan names, and creator request IDs.
--
--
module Network.AWS.Backup.ListBackupPlans
    (
    -- * Creating a Request
      listBackupPlans
    , ListBackupPlans
    -- * Request Lenses
    , lbpNextToken
    , lbpMaxResults
    , lbpIncludeDeleted

    -- * Destructuring the Response
    , listBackupPlansResponse
    , ListBackupPlansResponse
    -- * Response Lenses
    , lbprsNextToken
    , lbprsBackupPlansList
    , lbprsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listBackupPlans' smart constructor.
data ListBackupPlans = ListBackupPlans'
  { _lbpNextToken :: !(Maybe Text)
  , _lbpMaxResults :: !(Maybe Nat)
  , _lbpIncludeDeleted :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBackupPlans' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbpNextToken' - The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
--
-- * 'lbpMaxResults' - The maximum number of items to be returned.
--
-- * 'lbpIncludeDeleted' - A Boolean value with a default value of @FALSE@ that returns deleted backup plans when set to @TRUE@ .
listBackupPlans
    :: ListBackupPlans
listBackupPlans =
  ListBackupPlans'
    { _lbpNextToken = Nothing
    , _lbpMaxResults = Nothing
    , _lbpIncludeDeleted = Nothing
    }


-- | The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
lbpNextToken :: Lens' ListBackupPlans (Maybe Text)
lbpNextToken = lens _lbpNextToken (\ s a -> s{_lbpNextToken = a})

-- | The maximum number of items to be returned.
lbpMaxResults :: Lens' ListBackupPlans (Maybe Natural)
lbpMaxResults = lens _lbpMaxResults (\ s a -> s{_lbpMaxResults = a}) . mapping _Nat

-- | A Boolean value with a default value of @FALSE@ that returns deleted backup plans when set to @TRUE@ .
lbpIncludeDeleted :: Lens' ListBackupPlans (Maybe Bool)
lbpIncludeDeleted = lens _lbpIncludeDeleted (\ s a -> s{_lbpIncludeDeleted = a})

instance AWSRequest ListBackupPlans where
        type Rs ListBackupPlans = ListBackupPlansResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 ListBackupPlansResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "BackupPlansList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListBackupPlans where

instance NFData ListBackupPlans where

instance ToHeaders ListBackupPlans where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListBackupPlans where
        toPath = const "/backup/plans/"

instance ToQuery ListBackupPlans where
        toQuery ListBackupPlans'{..}
          = mconcat
              ["nextToken" =: _lbpNextToken,
               "maxResults" =: _lbpMaxResults,
               "includeDeleted" =: _lbpIncludeDeleted]

-- | /See:/ 'listBackupPlansResponse' smart constructor.
data ListBackupPlansResponse = ListBackupPlansResponse'
  { _lbprsNextToken :: !(Maybe Text)
  , _lbprsBackupPlansList :: !(Maybe [BackupPlansListMember])
  , _lbprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBackupPlansResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbprsNextToken' - The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
--
-- * 'lbprsBackupPlansList' - An array of backup plan list items containing metadata about your saved backup plans.
--
-- * 'lbprsResponseStatus' - -- | The response status code.
listBackupPlansResponse
    :: Int -- ^ 'lbprsResponseStatus'
    -> ListBackupPlansResponse
listBackupPlansResponse pResponseStatus_ =
  ListBackupPlansResponse'
    { _lbprsNextToken = Nothing
    , _lbprsBackupPlansList = Nothing
    , _lbprsResponseStatus = pResponseStatus_
    }


-- | The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
lbprsNextToken :: Lens' ListBackupPlansResponse (Maybe Text)
lbprsNextToken = lens _lbprsNextToken (\ s a -> s{_lbprsNextToken = a})

-- | An array of backup plan list items containing metadata about your saved backup plans.
lbprsBackupPlansList :: Lens' ListBackupPlansResponse [BackupPlansListMember]
lbprsBackupPlansList = lens _lbprsBackupPlansList (\ s a -> s{_lbprsBackupPlansList = a}) . _Default . _Coerce

-- | -- | The response status code.
lbprsResponseStatus :: Lens' ListBackupPlansResponse Int
lbprsResponseStatus = lens _lbprsResponseStatus (\ s a -> s{_lbprsResponseStatus = a})

instance NFData ListBackupPlansResponse where
