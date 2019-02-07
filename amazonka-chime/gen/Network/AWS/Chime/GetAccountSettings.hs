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
-- Module      : Network.AWS.Chime.GetAccountSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves account settings for the specified Amazon Chime account ID, such as remote control and dial out settings. For more information about these settings, see <http://docs.aws.amazon.com/chime/latest/ag/policies.html Use the Policies Page> in the /Amazon Chime Administration Guide/ .
--
--
module Network.AWS.Chime.GetAccountSettings
    (
    -- * Creating a Request
      getAccountSettings
    , GetAccountSettings
    -- * Request Lenses
    , gasAccountId

    -- * Destructuring the Response
    , getAccountSettingsResponse
    , GetAccountSettingsResponse
    -- * Response Lenses
    , gasrsAccountSettings
    , gasrsResponseStatus
    ) where

import Network.AWS.Chime.Types
import Network.AWS.Chime.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAccountSettings' smart constructor.
newtype GetAccountSettings = GetAccountSettings'
  { _gasAccountId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAccountSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gasAccountId' - The Amazon Chime account ID.
getAccountSettings
    :: Text -- ^ 'gasAccountId'
    -> GetAccountSettings
getAccountSettings pAccountId_ =
  GetAccountSettings' {_gasAccountId = pAccountId_}


-- | The Amazon Chime account ID.
gasAccountId :: Lens' GetAccountSettings Text
gasAccountId = lens _gasAccountId (\ s a -> s{_gasAccountId = a})

instance AWSRequest GetAccountSettings where
        type Rs GetAccountSettings =
             GetAccountSettingsResponse
        request = get chime
        response
          = receiveJSON
              (\ s h x ->
                 GetAccountSettingsResponse' <$>
                   (x .?> "AccountSettings") <*> (pure (fromEnum s)))

instance Hashable GetAccountSettings where

instance NFData GetAccountSettings where

instance ToHeaders GetAccountSettings where
        toHeaders = const mempty

instance ToPath GetAccountSettings where
        toPath GetAccountSettings'{..}
          = mconcat
              ["/console/accounts/", toBS _gasAccountId,
               "/settings"]

instance ToQuery GetAccountSettings where
        toQuery = const mempty

-- | /See:/ 'getAccountSettingsResponse' smart constructor.
data GetAccountSettingsResponse = GetAccountSettingsResponse'
  { _gasrsAccountSettings :: !(Maybe AccountSettings)
  , _gasrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAccountSettingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gasrsAccountSettings' - The Amazon Chime account settings.
--
-- * 'gasrsResponseStatus' - -- | The response status code.
getAccountSettingsResponse
    :: Int -- ^ 'gasrsResponseStatus'
    -> GetAccountSettingsResponse
getAccountSettingsResponse pResponseStatus_ =
  GetAccountSettingsResponse'
    {_gasrsAccountSettings = Nothing, _gasrsResponseStatus = pResponseStatus_}


-- | The Amazon Chime account settings.
gasrsAccountSettings :: Lens' GetAccountSettingsResponse (Maybe AccountSettings)
gasrsAccountSettings = lens _gasrsAccountSettings (\ s a -> s{_gasrsAccountSettings = a})

-- | -- | The response status code.
gasrsResponseStatus :: Lens' GetAccountSettingsResponse Int
gasrsResponseStatus = lens _gasrsResponseStatus (\ s a -> s{_gasrsResponseStatus = a})

instance NFData GetAccountSettingsResponse where
