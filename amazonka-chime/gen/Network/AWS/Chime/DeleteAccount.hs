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
-- Module      : Network.AWS.Chime.DeleteAccount
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Amazon Chime account. You must suspend all users before deleting a @Team@ account. You can use the 'BatchSuspendUser' action to do so.
--
--
-- For @EnterpriseLWA@ and @EnterpriseAD@ accounts, you must release the claimed domains for your Amazon Chime account before deletion. As soon as you release the domain, all users under that account are suspended.
--
-- Deleted accounts appear in your @Disabled@ accounts list for 90 days. To restore a deleted account from your @Disabled@ accounts list, you must contact AWS Support.
--
-- After 90 days, deleted accounts are permanently removed from your @Disabled@ accounts list.
--
module Network.AWS.Chime.DeleteAccount
    (
    -- * Creating a Request
      deleteAccount
    , DeleteAccount
    -- * Request Lenses
    , daAccountId

    -- * Destructuring the Response
    , deleteAccountResponse
    , DeleteAccountResponse
    -- * Response Lenses
    , darsResponseStatus
    ) where

import Network.AWS.Chime.Types
import Network.AWS.Chime.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAccount' smart constructor.
newtype DeleteAccount = DeleteAccount'
  { _daAccountId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daAccountId' - The Amazon Chime account ID.
deleteAccount
    :: Text -- ^ 'daAccountId'
    -> DeleteAccount
deleteAccount pAccountId_ = DeleteAccount' {_daAccountId = pAccountId_}


-- | The Amazon Chime account ID.
daAccountId :: Lens' DeleteAccount Text
daAccountId = lens _daAccountId (\ s a -> s{_daAccountId = a})

instance AWSRequest DeleteAccount where
        type Rs DeleteAccount = DeleteAccountResponse
        request = delete chime
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteAccountResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteAccount where

instance NFData DeleteAccount where

instance ToHeaders DeleteAccount where
        toHeaders = const mempty

instance ToPath DeleteAccount where
        toPath DeleteAccount'{..}
          = mconcat ["/console/accounts/", toBS _daAccountId]

instance ToQuery DeleteAccount where
        toQuery = const mempty

-- | /See:/ 'deleteAccountResponse' smart constructor.
newtype DeleteAccountResponse = DeleteAccountResponse'
  { _darsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAccountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsResponseStatus' - -- | The response status code.
deleteAccountResponse
    :: Int -- ^ 'darsResponseStatus'
    -> DeleteAccountResponse
deleteAccountResponse pResponseStatus_ =
  DeleteAccountResponse' {_darsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
darsResponseStatus :: Lens' DeleteAccountResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a})

instance NFData DeleteAccountResponse where
