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
-- Module      : Network.AWS.Chime.BatchSuspendUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Suspends up to 50 users from a @Team@ or @EnterpriseLWA@ Amazon Chime account. For more information about different account types, see <http://docs.aws.amazon.com/chime/latest/ag/manage-chime-account.html Managing Your Amazon Chime Accounts> in the /Amazon Chime Administration Guide/ .
--
--
-- Users suspended from a @Team@ account are dissociated from the account, but they can continue to use Amazon Chime as free users. To remove the suspension from suspended @Team@ account users, invite them to the @Team@ account again. You can use the 'InviteUsers' action to do so.
--
-- Users suspended from an @EnterpriseLWA@ account are immediately signed out of Amazon Chime and are no longer able to sign in. To remove the suspension from suspended @EnterpriseLWA@ account users, use the 'BatchUnsuspendUser' action. 
--
-- To sign out users without suspending them, use the 'LogoutUser' action.
--
module Network.AWS.Chime.BatchSuspendUser
    (
    -- * Creating a Request
      batchSuspendUser
    , BatchSuspendUser
    -- * Request Lenses
    , bsuAccountId
    , bsuUserIdList

    -- * Destructuring the Response
    , batchSuspendUserResponse
    , BatchSuspendUserResponse
    -- * Response Lenses
    , bsursUserErrors
    , bsursResponseStatus
    ) where

import Network.AWS.Chime.Types
import Network.AWS.Chime.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchSuspendUser' smart constructor.
data BatchSuspendUser = BatchSuspendUser'
  { _bsuAccountId :: !Text
  , _bsuUserIdList :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchSuspendUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsuAccountId' - The Amazon Chime account ID.
--
-- * 'bsuUserIdList' - The request containing the user IDs to suspend.
batchSuspendUser
    :: Text -- ^ 'bsuAccountId'
    -> BatchSuspendUser
batchSuspendUser pAccountId_ =
  BatchSuspendUser' {_bsuAccountId = pAccountId_, _bsuUserIdList = mempty}


-- | The Amazon Chime account ID.
bsuAccountId :: Lens' BatchSuspendUser Text
bsuAccountId = lens _bsuAccountId (\ s a -> s{_bsuAccountId = a})

-- | The request containing the user IDs to suspend.
bsuUserIdList :: Lens' BatchSuspendUser [Text]
bsuUserIdList = lens _bsuUserIdList (\ s a -> s{_bsuUserIdList = a}) . _Coerce

instance AWSRequest BatchSuspendUser where
        type Rs BatchSuspendUser = BatchSuspendUserResponse
        request = postJSON chime
        response
          = receiveJSON
              (\ s h x ->
                 BatchSuspendUserResponse' <$>
                   (x .?> "UserErrors" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable BatchSuspendUser where

instance NFData BatchSuspendUser where

instance ToHeaders BatchSuspendUser where
        toHeaders = const mempty

instance ToJSON BatchSuspendUser where
        toJSON BatchSuspendUser'{..}
          = object
              (catMaybes [Just ("UserIdList" .= _bsuUserIdList)])

instance ToPath BatchSuspendUser where
        toPath BatchSuspendUser'{..}
          = mconcat
              ["/console/accounts/", toBS _bsuAccountId, "/users"]

instance ToQuery BatchSuspendUser where
        toQuery = const (mconcat ["operation=suspend"])

-- | /See:/ 'batchSuspendUserResponse' smart constructor.
data BatchSuspendUserResponse = BatchSuspendUserResponse'
  { _bsursUserErrors :: !(Maybe [UserError])
  , _bsursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchSuspendUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsursUserErrors' - If the 'BatchSuspendUser' action fails for one or more of the user IDs in the request, a list of the user IDs is returned, along with error codes and error messages.
--
-- * 'bsursResponseStatus' - -- | The response status code.
batchSuspendUserResponse
    :: Int -- ^ 'bsursResponseStatus'
    -> BatchSuspendUserResponse
batchSuspendUserResponse pResponseStatus_ =
  BatchSuspendUserResponse'
    {_bsursUserErrors = Nothing, _bsursResponseStatus = pResponseStatus_}


-- | If the 'BatchSuspendUser' action fails for one or more of the user IDs in the request, a list of the user IDs is returned, along with error codes and error messages.
bsursUserErrors :: Lens' BatchSuspendUserResponse [UserError]
bsursUserErrors = lens _bsursUserErrors (\ s a -> s{_bsursUserErrors = a}) . _Default . _Coerce

-- | -- | The response status code.
bsursResponseStatus :: Lens' BatchSuspendUserResponse Int
bsursResponseStatus = lens _bsursResponseStatus (\ s a -> s{_bsursResponseStatus = a})

instance NFData BatchSuspendUserResponse where
