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
-- Module      : Network.AWS.Chime.GetUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details for the specified user ID, such as primary email address, license type, and personal meeting PIN.
--
--
-- To retrieve user details with an email address instead of a user ID, use the 'ListUsers' action, and then filter by email address.
--
module Network.AWS.Chime.GetUser
    (
    -- * Creating a Request
      getUser
    , GetUser
    -- * Request Lenses
    , guAccountId
    , guUserId

    -- * Destructuring the Response
    , getUserResponse
    , GetUserResponse
    -- * Response Lenses
    , gursUser
    , gursResponseStatus
    ) where

import Network.AWS.Chime.Types
import Network.AWS.Chime.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getUser' smart constructor.
data GetUser = GetUser'
  { _guAccountId :: !Text
  , _guUserId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'guAccountId' - The Amazon Chime account ID.
--
-- * 'guUserId' - The user ID.
getUser
    :: Text -- ^ 'guAccountId'
    -> Text -- ^ 'guUserId'
    -> GetUser
getUser pAccountId_ pUserId_ =
  GetUser' {_guAccountId = pAccountId_, _guUserId = pUserId_}


-- | The Amazon Chime account ID.
guAccountId :: Lens' GetUser Text
guAccountId = lens _guAccountId (\ s a -> s{_guAccountId = a})

-- | The user ID.
guUserId :: Lens' GetUser Text
guUserId = lens _guUserId (\ s a -> s{_guUserId = a})

instance AWSRequest GetUser where
        type Rs GetUser = GetUserResponse
        request = get chime
        response
          = receiveJSON
              (\ s h x ->
                 GetUserResponse' <$>
                   (x .?> "User") <*> (pure (fromEnum s)))

instance Hashable GetUser where

instance NFData GetUser where

instance ToHeaders GetUser where
        toHeaders = const mempty

instance ToPath GetUser where
        toPath GetUser'{..}
          = mconcat
              ["/console/accounts/", toBS _guAccountId, "/users/",
               toBS _guUserId]

instance ToQuery GetUser where
        toQuery = const mempty

-- | /See:/ 'getUserResponse' smart constructor.
data GetUserResponse = GetUserResponse'
  { _gursUser :: !(Maybe User)
  , _gursResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gursUser' - The user details.
--
-- * 'gursResponseStatus' - -- | The response status code.
getUserResponse
    :: Int -- ^ 'gursResponseStatus'
    -> GetUserResponse
getUserResponse pResponseStatus_ =
  GetUserResponse' {_gursUser = Nothing, _gursResponseStatus = pResponseStatus_}


-- | The user details.
gursUser :: Lens' GetUserResponse (Maybe User)
gursUser = lens _gursUser (\ s a -> s{_gursUser = a})

-- | -- | The response status code.
gursResponseStatus :: Lens' GetUserResponse Int
gursResponseStatus = lens _gursResponseStatus (\ s a -> s{_gursResponseStatus = a})

instance NFData GetUserResponse where
