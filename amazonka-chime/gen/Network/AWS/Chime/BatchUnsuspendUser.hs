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
-- Module      : Network.AWS.Chime.BatchUnsuspendUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the suspension from up to 50 previously suspended users for the specified Amazon Chime @EnterpriseLWA@ account. Only users on @EnterpriseLWA@ accounts can be unsuspended using this action. For more information about different account types, see <http://docs.aws.amazon.com/chime/latest/ag/manage-chime-account.html Managing Your Amazon Chime Accounts> in the /Amazon Chime Administration Guide/ .
--
--
-- Previously suspended users who are unsuspended using this action are returned to @Registered@ status. Users who are not previously suspended are ignored.
--
module Network.AWS.Chime.BatchUnsuspendUser
    (
    -- * Creating a Request
      batchUnsuspendUser
    , BatchUnsuspendUser
    -- * Request Lenses
    , bAccountId
    , bUserIdList

    -- * Destructuring the Response
    , batchUnsuspendUserResponse
    , BatchUnsuspendUserResponse
    -- * Response Lenses
    , buursUserErrors
    , buursResponseStatus
    ) where

import Network.AWS.Chime.Types
import Network.AWS.Chime.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchUnsuspendUser' smart constructor.
data BatchUnsuspendUser = BatchUnsuspendUser'
  { _bAccountId :: !Text
  , _bUserIdList :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchUnsuspendUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bAccountId' - The Amazon Chime account ID.
--
-- * 'bUserIdList' - The request containing the user IDs to unsuspend.
batchUnsuspendUser
    :: Text -- ^ 'bAccountId'
    -> BatchUnsuspendUser
batchUnsuspendUser pAccountId_ =
  BatchUnsuspendUser' {_bAccountId = pAccountId_, _bUserIdList = mempty}


-- | The Amazon Chime account ID.
bAccountId :: Lens' BatchUnsuspendUser Text
bAccountId = lens _bAccountId (\ s a -> s{_bAccountId = a})

-- | The request containing the user IDs to unsuspend.
bUserIdList :: Lens' BatchUnsuspendUser [Text]
bUserIdList = lens _bUserIdList (\ s a -> s{_bUserIdList = a}) . _Coerce

instance AWSRequest BatchUnsuspendUser where
        type Rs BatchUnsuspendUser =
             BatchUnsuspendUserResponse
        request = postJSON chime
        response
          = receiveJSON
              (\ s h x ->
                 BatchUnsuspendUserResponse' <$>
                   (x .?> "UserErrors" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable BatchUnsuspendUser where

instance NFData BatchUnsuspendUser where

instance ToHeaders BatchUnsuspendUser where
        toHeaders = const mempty

instance ToJSON BatchUnsuspendUser where
        toJSON BatchUnsuspendUser'{..}
          = object
              (catMaybes [Just ("UserIdList" .= _bUserIdList)])

instance ToPath BatchUnsuspendUser where
        toPath BatchUnsuspendUser'{..}
          = mconcat
              ["/console/accounts/", toBS _bAccountId, "/users"]

instance ToQuery BatchUnsuspendUser where
        toQuery = const (mconcat ["operation=unsuspend"])

-- | /See:/ 'batchUnsuspendUserResponse' smart constructor.
data BatchUnsuspendUserResponse = BatchUnsuspendUserResponse'
  { _buursUserErrors :: !(Maybe [UserError])
  , _buursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchUnsuspendUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'buursUserErrors' - If the 'BatchUnsuspendUser' action fails for one or more of the user IDs in the request, a list of the user IDs is returned, along with error codes and error messages.
--
-- * 'buursResponseStatus' - -- | The response status code.
batchUnsuspendUserResponse
    :: Int -- ^ 'buursResponseStatus'
    -> BatchUnsuspendUserResponse
batchUnsuspendUserResponse pResponseStatus_ =
  BatchUnsuspendUserResponse'
    {_buursUserErrors = Nothing, _buursResponseStatus = pResponseStatus_}


-- | If the 'BatchUnsuspendUser' action fails for one or more of the user IDs in the request, a list of the user IDs is returned, along with error codes and error messages.
buursUserErrors :: Lens' BatchUnsuspendUserResponse [UserError]
buursUserErrors = lens _buursUserErrors (\ s a -> s{_buursUserErrors = a}) . _Default . _Coerce

-- | -- | The response status code.
buursResponseStatus :: Lens' BatchUnsuspendUserResponse Int
buursResponseStatus = lens _buursResponseStatus (\ s a -> s{_buursResponseStatus = a})

instance NFData BatchUnsuspendUserResponse where
