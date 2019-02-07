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
-- Module      : Network.AWS.Chime.ResetPersonalPIN
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the personal meeting PIN for the specified user on an Amazon Chime account. Returns the 'User' object with the updated personal meeting PIN.
--
--
module Network.AWS.Chime.ResetPersonalPIN
    (
    -- * Creating a Request
      resetPersonalPIN
    , ResetPersonalPIN
    -- * Request Lenses
    , rppinAccountId
    , rppinUserId

    -- * Destructuring the Response
    , resetPersonalPINResponse
    , ResetPersonalPINResponse
    -- * Response Lenses
    , rppinrsUser
    , rppinrsResponseStatus
    ) where

import Network.AWS.Chime.Types
import Network.AWS.Chime.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'resetPersonalPIN' smart constructor.
data ResetPersonalPIN = ResetPersonalPIN'
  { _rppinAccountId :: !Text
  , _rppinUserId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResetPersonalPIN' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rppinAccountId' - The Amazon Chime account ID.
--
-- * 'rppinUserId' - The user ID.
resetPersonalPIN
    :: Text -- ^ 'rppinAccountId'
    -> Text -- ^ 'rppinUserId'
    -> ResetPersonalPIN
resetPersonalPIN pAccountId_ pUserId_ =
  ResetPersonalPIN' {_rppinAccountId = pAccountId_, _rppinUserId = pUserId_}


-- | The Amazon Chime account ID.
rppinAccountId :: Lens' ResetPersonalPIN Text
rppinAccountId = lens _rppinAccountId (\ s a -> s{_rppinAccountId = a})

-- | The user ID.
rppinUserId :: Lens' ResetPersonalPIN Text
rppinUserId = lens _rppinUserId (\ s a -> s{_rppinUserId = a})

instance AWSRequest ResetPersonalPIN where
        type Rs ResetPersonalPIN = ResetPersonalPINResponse
        request = postJSON chime
        response
          = receiveJSON
              (\ s h x ->
                 ResetPersonalPINResponse' <$>
                   (x .?> "User") <*> (pure (fromEnum s)))

instance Hashable ResetPersonalPIN where

instance NFData ResetPersonalPIN where

instance ToHeaders ResetPersonalPIN where
        toHeaders = const mempty

instance ToJSON ResetPersonalPIN where
        toJSON = const (Object mempty)

instance ToPath ResetPersonalPIN where
        toPath ResetPersonalPIN'{..}
          = mconcat
              ["/console/accounts/", toBS _rppinAccountId,
               "/users/", toBS _rppinUserId]

instance ToQuery ResetPersonalPIN where
        toQuery
          = const (mconcat ["operation=reset-personal-pin"])

-- | /See:/ 'resetPersonalPINResponse' smart constructor.
data ResetPersonalPINResponse = ResetPersonalPINResponse'
  { _rppinrsUser :: !(Maybe User)
  , _rppinrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResetPersonalPINResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rppinrsUser' - The user details and new personal meeting PIN.
--
-- * 'rppinrsResponseStatus' - -- | The response status code.
resetPersonalPINResponse
    :: Int -- ^ 'rppinrsResponseStatus'
    -> ResetPersonalPINResponse
resetPersonalPINResponse pResponseStatus_ =
  ResetPersonalPINResponse'
    {_rppinrsUser = Nothing, _rppinrsResponseStatus = pResponseStatus_}


-- | The user details and new personal meeting PIN.
rppinrsUser :: Lens' ResetPersonalPINResponse (Maybe User)
rppinrsUser = lens _rppinrsUser (\ s a -> s{_rppinrsUser = a})

-- | -- | The response status code.
rppinrsResponseStatus :: Lens' ResetPersonalPINResponse Int
rppinrsResponseStatus = lens _rppinrsResponseStatus (\ s a -> s{_rppinrsResponseStatus = a})

instance NFData ResetPersonalPINResponse where
