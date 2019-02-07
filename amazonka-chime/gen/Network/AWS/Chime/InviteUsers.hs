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
-- Module      : Network.AWS.Chime.InviteUsers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends email invites to as many as 50 users, inviting them to the specified Amazon Chime @Team@ account. Only @Team@ account types are currently supported for this action. 
--
--
module Network.AWS.Chime.InviteUsers
    (
    -- * Creating a Request
      inviteUsers
    , InviteUsers
    -- * Request Lenses
    , iuAccountId
    , iuUserEmailList

    -- * Destructuring the Response
    , inviteUsersResponse
    , InviteUsersResponse
    -- * Response Lenses
    , iursInvites
    , iursResponseStatus
    ) where

import Network.AWS.Chime.Types
import Network.AWS.Chime.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'inviteUsers' smart constructor.
data InviteUsers = InviteUsers'
  { _iuAccountId :: !Text
  , _iuUserEmailList :: ![Sensitive Text]
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'InviteUsers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iuAccountId' - The Amazon Chime account ID.
--
-- * 'iuUserEmailList' - The user email addresses to which to send the invite.
inviteUsers
    :: Text -- ^ 'iuAccountId'
    -> InviteUsers
inviteUsers pAccountId_ =
  InviteUsers' {_iuAccountId = pAccountId_, _iuUserEmailList = mempty}


-- | The Amazon Chime account ID.
iuAccountId :: Lens' InviteUsers Text
iuAccountId = lens _iuAccountId (\ s a -> s{_iuAccountId = a})

-- | The user email addresses to which to send the invite.
iuUserEmailList :: Lens' InviteUsers [Text]
iuUserEmailList = lens _iuUserEmailList (\ s a -> s{_iuUserEmailList = a}) . _Coerce

instance AWSRequest InviteUsers where
        type Rs InviteUsers = InviteUsersResponse
        request = postJSON chime
        response
          = receiveJSON
              (\ s h x ->
                 InviteUsersResponse' <$>
                   (x .?> "Invites" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable InviteUsers where

instance NFData InviteUsers where

instance ToHeaders InviteUsers where
        toHeaders = const mempty

instance ToJSON InviteUsers where
        toJSON InviteUsers'{..}
          = object
              (catMaybes
                 [Just ("UserEmailList" .= _iuUserEmailList)])

instance ToPath InviteUsers where
        toPath InviteUsers'{..}
          = mconcat
              ["/console/accounts/", toBS _iuAccountId, "/users"]

instance ToQuery InviteUsers where
        toQuery = const (mconcat ["operation=add"])

-- | /See:/ 'inviteUsersResponse' smart constructor.
data InviteUsersResponse = InviteUsersResponse'
  { _iursInvites :: !(Maybe [Invite])
  , _iursResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'InviteUsersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iursInvites' - The invite details.
--
-- * 'iursResponseStatus' - -- | The response status code.
inviteUsersResponse
    :: Int -- ^ 'iursResponseStatus'
    -> InviteUsersResponse
inviteUsersResponse pResponseStatus_ =
  InviteUsersResponse'
    {_iursInvites = Nothing, _iursResponseStatus = pResponseStatus_}


-- | The invite details.
iursInvites :: Lens' InviteUsersResponse [Invite]
iursInvites = lens _iursInvites (\ s a -> s{_iursInvites = a}) . _Default . _Coerce

-- | -- | The response status code.
iursResponseStatus :: Lens' InviteUsersResponse Int
iursResponseStatus = lens _iursResponseStatus (\ s a -> s{_iursResponseStatus = a})

instance NFData InviteUsersResponse where
