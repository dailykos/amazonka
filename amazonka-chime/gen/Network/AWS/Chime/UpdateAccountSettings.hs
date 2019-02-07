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
-- Module      : Network.AWS.Chime.UpdateAccountSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings for the specified Amazon Chime account. You can update settings for remote control of shared screens, or for the dial-out option. For more information about these settings, see <http://docs.aws.amazon.com/chime/latest/ag/policies.html Use the Policies Page> in the /Amazon Chime Administration Guide/ .
--
--
module Network.AWS.Chime.UpdateAccountSettings
    (
    -- * Creating a Request
      updateAccountSettings
    , UpdateAccountSettings
    -- * Request Lenses
    , uasAccountId
    , uasAccountSettings

    -- * Destructuring the Response
    , updateAccountSettingsResponse
    , UpdateAccountSettingsResponse
    -- * Response Lenses
    , uasrsResponseStatus
    ) where

import Network.AWS.Chime.Types
import Network.AWS.Chime.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateAccountSettings' smart constructor.
data UpdateAccountSettings = UpdateAccountSettings'
  { _uasAccountId :: !Text
  , _uasAccountSettings :: !AccountSettings
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAccountSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uasAccountId' - The Amazon Chime account ID.
--
-- * 'uasAccountSettings' - The Amazon Chime account settings to update.
updateAccountSettings
    :: Text -- ^ 'uasAccountId'
    -> AccountSettings -- ^ 'uasAccountSettings'
    -> UpdateAccountSettings
updateAccountSettings pAccountId_ pAccountSettings_ =
  UpdateAccountSettings'
    {_uasAccountId = pAccountId_, _uasAccountSettings = pAccountSettings_}


-- | The Amazon Chime account ID.
uasAccountId :: Lens' UpdateAccountSettings Text
uasAccountId = lens _uasAccountId (\ s a -> s{_uasAccountId = a})

-- | The Amazon Chime account settings to update.
uasAccountSettings :: Lens' UpdateAccountSettings AccountSettings
uasAccountSettings = lens _uasAccountSettings (\ s a -> s{_uasAccountSettings = a})

instance AWSRequest UpdateAccountSettings where
        type Rs UpdateAccountSettings =
             UpdateAccountSettingsResponse
        request = putJSON chime
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateAccountSettingsResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateAccountSettings where

instance NFData UpdateAccountSettings where

instance ToHeaders UpdateAccountSettings where
        toHeaders = const mempty

instance ToJSON UpdateAccountSettings where
        toJSON UpdateAccountSettings'{..}
          = object
              (catMaybes
                 [Just ("AccountSettings" .= _uasAccountSettings)])

instance ToPath UpdateAccountSettings where
        toPath UpdateAccountSettings'{..}
          = mconcat
              ["/console/accounts/", toBS _uasAccountId,
               "/settings"]

instance ToQuery UpdateAccountSettings where
        toQuery = const mempty

-- | /See:/ 'updateAccountSettingsResponse' smart constructor.
newtype UpdateAccountSettingsResponse = UpdateAccountSettingsResponse'
  { _uasrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAccountSettingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uasrsResponseStatus' - -- | The response status code.
updateAccountSettingsResponse
    :: Int -- ^ 'uasrsResponseStatus'
    -> UpdateAccountSettingsResponse
updateAccountSettingsResponse pResponseStatus_ =
  UpdateAccountSettingsResponse' {_uasrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uasrsResponseStatus :: Lens' UpdateAccountSettingsResponse Int
uasrsResponseStatus = lens _uasrsResponseStatus (\ s a -> s{_uasrsResponseStatus = a})

instance NFData UpdateAccountSettingsResponse where
