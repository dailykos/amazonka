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
-- Module      : Network.AWS.Backup.ListBackupVaults
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of recovery point storage containers along with information about them.
--
--
module Network.AWS.Backup.ListBackupVaults
    (
    -- * Creating a Request
      listBackupVaults
    , ListBackupVaults
    -- * Request Lenses
    , lbvNextToken
    , lbvMaxResults

    -- * Destructuring the Response
    , listBackupVaultsResponse
    , ListBackupVaultsResponse
    -- * Response Lenses
    , lbvrsNextToken
    , lbvrsBackupVaultList
    , lbvrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listBackupVaults' smart constructor.
data ListBackupVaults = ListBackupVaults'
  { _lbvNextToken :: !(Maybe Text)
  , _lbvMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBackupVaults' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbvNextToken' - The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
--
-- * 'lbvMaxResults' - The maximum number of items to be returned.
listBackupVaults
    :: ListBackupVaults
listBackupVaults =
  ListBackupVaults' {_lbvNextToken = Nothing, _lbvMaxResults = Nothing}


-- | The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
lbvNextToken :: Lens' ListBackupVaults (Maybe Text)
lbvNextToken = lens _lbvNextToken (\ s a -> s{_lbvNextToken = a})

-- | The maximum number of items to be returned.
lbvMaxResults :: Lens' ListBackupVaults (Maybe Natural)
lbvMaxResults = lens _lbvMaxResults (\ s a -> s{_lbvMaxResults = a}) . mapping _Nat

instance AWSRequest ListBackupVaults where
        type Rs ListBackupVaults = ListBackupVaultsResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 ListBackupVaultsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "BackupVaultList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListBackupVaults where

instance NFData ListBackupVaults where

instance ToHeaders ListBackupVaults where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListBackupVaults where
        toPath = const "/backup-vaults/"

instance ToQuery ListBackupVaults where
        toQuery ListBackupVaults'{..}
          = mconcat
              ["nextToken" =: _lbvNextToken,
               "maxResults" =: _lbvMaxResults]

-- | /See:/ 'listBackupVaultsResponse' smart constructor.
data ListBackupVaultsResponse = ListBackupVaultsResponse'
  { _lbvrsNextToken :: !(Maybe Text)
  , _lbvrsBackupVaultList :: !(Maybe [BackupVaultListMember])
  , _lbvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBackupVaultsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbvrsNextToken' - The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
--
-- * 'lbvrsBackupVaultList' - An array of backup vault list members containing vault metadata, including Amazon Resource Name (ARN), display name, creation date, number of saved recovery points, and encryption information if the resources saved in the backup vault are encrypted.
--
-- * 'lbvrsResponseStatus' - -- | The response status code.
listBackupVaultsResponse
    :: Int -- ^ 'lbvrsResponseStatus'
    -> ListBackupVaultsResponse
listBackupVaultsResponse pResponseStatus_ =
  ListBackupVaultsResponse'
    { _lbvrsNextToken = Nothing
    , _lbvrsBackupVaultList = Nothing
    , _lbvrsResponseStatus = pResponseStatus_
    }


-- | The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
lbvrsNextToken :: Lens' ListBackupVaultsResponse (Maybe Text)
lbvrsNextToken = lens _lbvrsNextToken (\ s a -> s{_lbvrsNextToken = a})

-- | An array of backup vault list members containing vault metadata, including Amazon Resource Name (ARN), display name, creation date, number of saved recovery points, and encryption information if the resources saved in the backup vault are encrypted.
lbvrsBackupVaultList :: Lens' ListBackupVaultsResponse [BackupVaultListMember]
lbvrsBackupVaultList = lens _lbvrsBackupVaultList (\ s a -> s{_lbvrsBackupVaultList = a}) . _Default . _Coerce

-- | -- | The response status code.
lbvrsResponseStatus :: Lens' ListBackupVaultsResponse Int
lbvrsResponseStatus = lens _lbvrsResponseStatus (\ s a -> s{_lbvrsResponseStatus = a})

instance NFData ListBackupVaultsResponse where
