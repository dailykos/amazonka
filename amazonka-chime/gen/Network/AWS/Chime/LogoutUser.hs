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
-- Module      : Network.AWS.Chime.LogoutUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Logs out the specified user from all of the devices they are currently logged into.
--
--
module Network.AWS.Chime.LogoutUser
    (
    -- * Creating a Request
      logoutUser
    , LogoutUser
    -- * Request Lenses
    , lAccountId
    , lUserId

    -- * Destructuring the Response
    , logoutUserResponse
    , LogoutUserResponse
    -- * Response Lenses
    , lursResponseStatus
    ) where

import Network.AWS.Chime.Types
import Network.AWS.Chime.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'logoutUser' smart constructor.
data LogoutUser = LogoutUser'
  { _lAccountId :: !Text
  , _lUserId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LogoutUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lAccountId' - The Amazon Chime account ID.
--
-- * 'lUserId' - The user ID.
logoutUser
    :: Text -- ^ 'lAccountId'
    -> Text -- ^ 'lUserId'
    -> LogoutUser
logoutUser pAccountId_ pUserId_ =
  LogoutUser' {_lAccountId = pAccountId_, _lUserId = pUserId_}


-- | The Amazon Chime account ID.
lAccountId :: Lens' LogoutUser Text
lAccountId = lens _lAccountId (\ s a -> s{_lAccountId = a})

-- | The user ID.
lUserId :: Lens' LogoutUser Text
lUserId = lens _lUserId (\ s a -> s{_lUserId = a})

instance AWSRequest LogoutUser where
        type Rs LogoutUser = LogoutUserResponse
        request = postJSON chime
        response
          = receiveEmpty
              (\ s h x ->
                 LogoutUserResponse' <$> (pure (fromEnum s)))

instance Hashable LogoutUser where

instance NFData LogoutUser where

instance ToHeaders LogoutUser where
        toHeaders = const mempty

instance ToJSON LogoutUser where
        toJSON = const (Object mempty)

instance ToPath LogoutUser where
        toPath LogoutUser'{..}
          = mconcat
              ["/console/accounts/", toBS _lAccountId, "/users/",
               toBS _lUserId]

instance ToQuery LogoutUser where
        toQuery = const (mconcat ["operation=logout"])

-- | /See:/ 'logoutUserResponse' smart constructor.
newtype LogoutUserResponse = LogoutUserResponse'
  { _lursResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LogoutUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lursResponseStatus' - -- | The response status code.
logoutUserResponse
    :: Int -- ^ 'lursResponseStatus'
    -> LogoutUserResponse
logoutUserResponse pResponseStatus_ =
  LogoutUserResponse' {_lursResponseStatus = pResponseStatus_}


-- | -- | The response status code.
lursResponseStatus :: Lens' LogoutUserResponse Int
lursResponseStatus = lens _lursResponseStatus (\ s a -> s{_lursResponseStatus = a})

instance NFData LogoutUserResponse where
