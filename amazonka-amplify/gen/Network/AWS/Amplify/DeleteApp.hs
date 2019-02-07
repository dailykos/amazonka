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
-- Module      : Network.AWS.Amplify.DeleteApp
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an existing Amplify App by appId. 
--
--
module Network.AWS.Amplify.DeleteApp
    (
    -- * Creating a Request
      deleteApp
    , DeleteApp
    -- * Request Lenses
    , daAppId

    -- * Destructuring the Response
    , deleteAppResponse
    , DeleteAppResponse
    -- * Response Lenses
    , darsResponseStatus
    , darsApp
    ) where

import Network.AWS.Amplify.Types
import Network.AWS.Amplify.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure for an Amplify App delete request. 
--
--
--
-- /See:/ 'deleteApp' smart constructor.
newtype DeleteApp = DeleteApp'
  { _daAppId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteApp' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daAppId' - Unique Id for an Amplify App. 
deleteApp
    :: Text -- ^ 'daAppId'
    -> DeleteApp
deleteApp pAppId_ = DeleteApp' {_daAppId = pAppId_}


-- | Unique Id for an Amplify App. 
daAppId :: Lens' DeleteApp Text
daAppId = lens _daAppId (\ s a -> s{_daAppId = a})

instance AWSRequest DeleteApp where
        type Rs DeleteApp = DeleteAppResponse
        request = delete amplify
        response
          = receiveJSON
              (\ s h x ->
                 DeleteAppResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "app"))

instance Hashable DeleteApp where

instance NFData DeleteApp where

instance ToHeaders DeleteApp where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteApp where
        toPath DeleteApp'{..}
          = mconcat ["/apps/", toBS _daAppId]

instance ToQuery DeleteApp where
        toQuery = const mempty

-- | Result structure for an Amplify App delete request. 
--
--
--
-- /See:/ 'deleteAppResponse' smart constructor.
data DeleteAppResponse = DeleteAppResponse'
  { _darsResponseStatus :: !Int
  , _darsApp :: !App
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAppResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsResponseStatus' - -- | The response status code.
--
-- * 'darsApp' - Undocumented member.
deleteAppResponse
    :: Int -- ^ 'darsResponseStatus'
    -> App -- ^ 'darsApp'
    -> DeleteAppResponse
deleteAppResponse pResponseStatus_ pApp_ =
  DeleteAppResponse' {_darsResponseStatus = pResponseStatus_, _darsApp = pApp_}


-- | -- | The response status code.
darsResponseStatus :: Lens' DeleteAppResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a})

-- | Undocumented member.
darsApp :: Lens' DeleteAppResponse App
darsApp = lens _darsApp (\ s a -> s{_darsApp = a})

instance NFData DeleteAppResponse where
