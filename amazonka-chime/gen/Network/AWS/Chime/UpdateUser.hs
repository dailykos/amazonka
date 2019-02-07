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
-- Module      : Network.AWS.Chime.UpdateUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates user details for a specified user ID. Currently, only @LicenseType@ updates are supported for this action.
--
--
module Network.AWS.Chime.UpdateUser
    (
    -- * Creating a Request
      updateUser
    , UpdateUser
    -- * Request Lenses
    , uuLicenseType
    , uuAccountId
    , uuUserId

    -- * Destructuring the Response
    , updateUserResponse
    , UpdateUserResponse
    -- * Response Lenses
    , uursUser
    , uursResponseStatus
    ) where

import Network.AWS.Chime.Types
import Network.AWS.Chime.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateUser' smart constructor.
data UpdateUser = UpdateUser'
  { _uuLicenseType :: !(Maybe License)
  , _uuAccountId :: !Text
  , _uuUserId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uuLicenseType' - The user license type to update. This must be a supported license type for the Amazon Chime account that the user belongs to.
--
-- * 'uuAccountId' - The Amazon Chime account ID.
--
-- * 'uuUserId' - The user ID.
updateUser
    :: Text -- ^ 'uuAccountId'
    -> Text -- ^ 'uuUserId'
    -> UpdateUser
updateUser pAccountId_ pUserId_ =
  UpdateUser'
    {_uuLicenseType = Nothing, _uuAccountId = pAccountId_, _uuUserId = pUserId_}


-- | The user license type to update. This must be a supported license type for the Amazon Chime account that the user belongs to.
uuLicenseType :: Lens' UpdateUser (Maybe License)
uuLicenseType = lens _uuLicenseType (\ s a -> s{_uuLicenseType = a})

-- | The Amazon Chime account ID.
uuAccountId :: Lens' UpdateUser Text
uuAccountId = lens _uuAccountId (\ s a -> s{_uuAccountId = a})

-- | The user ID.
uuUserId :: Lens' UpdateUser Text
uuUserId = lens _uuUserId (\ s a -> s{_uuUserId = a})

instance AWSRequest UpdateUser where
        type Rs UpdateUser = UpdateUserResponse
        request = postJSON chime
        response
          = receiveJSON
              (\ s h x ->
                 UpdateUserResponse' <$>
                   (x .?> "User") <*> (pure (fromEnum s)))

instance Hashable UpdateUser where

instance NFData UpdateUser where

instance ToHeaders UpdateUser where
        toHeaders = const mempty

instance ToJSON UpdateUser where
        toJSON UpdateUser'{..}
          = object
              (catMaybes [("LicenseType" .=) <$> _uuLicenseType])

instance ToPath UpdateUser where
        toPath UpdateUser'{..}
          = mconcat
              ["/console/accounts/", toBS _uuAccountId, "/users/",
               toBS _uuUserId]

instance ToQuery UpdateUser where
        toQuery = const mempty

-- | /See:/ 'updateUserResponse' smart constructor.
data UpdateUserResponse = UpdateUserResponse'
  { _uursUser :: !(Maybe User)
  , _uursResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uursUser' - The updated user details.
--
-- * 'uursResponseStatus' - -- | The response status code.
updateUserResponse
    :: Int -- ^ 'uursResponseStatus'
    -> UpdateUserResponse
updateUserResponse pResponseStatus_ =
  UpdateUserResponse'
    {_uursUser = Nothing, _uursResponseStatus = pResponseStatus_}


-- | The updated user details.
uursUser :: Lens' UpdateUserResponse (Maybe User)
uursUser = lens _uursUser (\ s a -> s{_uursUser = a})

-- | -- | The response status code.
uursResponseStatus :: Lens' UpdateUserResponse Int
uursResponseStatus = lens _uursResponseStatus (\ s a -> s{_uursResponseStatus = a})

instance NFData UpdateUserResponse where
