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
-- Module      : Network.AWS.Amplify.GetApp
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an existing Amplify App by appId. 
--
--
module Network.AWS.Amplify.GetApp
    (
    -- * Creating a Request
      getApp
    , GetApp
    -- * Request Lenses
    , gaAppId

    -- * Destructuring the Response
    , getAppResponse
    , GetAppResponse
    -- * Response Lenses
    , garsResponseStatus
    , garsApp
    ) where

import Network.AWS.Amplify.Types
import Network.AWS.Amplify.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure for get App request. 
--
--
--
-- /See:/ 'getApp' smart constructor.
newtype GetApp = GetApp'
  { _gaAppId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetApp' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaAppId' - Unique Id for an Amplify App. 
getApp
    :: Text -- ^ 'gaAppId'
    -> GetApp
getApp pAppId_ = GetApp' {_gaAppId = pAppId_}


-- | Unique Id for an Amplify App. 
gaAppId :: Lens' GetApp Text
gaAppId = lens _gaAppId (\ s a -> s{_gaAppId = a})

instance AWSRequest GetApp where
        type Rs GetApp = GetAppResponse
        request = get amplify
        response
          = receiveJSON
              (\ s h x ->
                 GetAppResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "app"))

instance Hashable GetApp where

instance NFData GetApp where

instance ToHeaders GetApp where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetApp where
        toPath GetApp'{..}
          = mconcat ["/apps/", toBS _gaAppId]

instance ToQuery GetApp where
        toQuery = const mempty

-- | /See:/ 'getAppResponse' smart constructor.
data GetAppResponse = GetAppResponse'
  { _garsResponseStatus :: !Int
  , _garsApp :: !App
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAppResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'garsResponseStatus' - -- | The response status code.
--
-- * 'garsApp' - Undocumented member.
getAppResponse
    :: Int -- ^ 'garsResponseStatus'
    -> App -- ^ 'garsApp'
    -> GetAppResponse
getAppResponse pResponseStatus_ pApp_ =
  GetAppResponse' {_garsResponseStatus = pResponseStatus_, _garsApp = pApp_}


-- | -- | The response status code.
garsResponseStatus :: Lens' GetAppResponse Int
garsResponseStatus = lens _garsResponseStatus (\ s a -> s{_garsResponseStatus = a})

-- | Undocumented member.
garsApp :: Lens' GetAppResponse App
garsApp = lens _garsApp (\ s a -> s{_garsApp = a})

instance NFData GetAppResponse where
