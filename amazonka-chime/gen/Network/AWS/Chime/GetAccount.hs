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
-- Module      : Network.AWS.Chime.GetAccount
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details for the specified Amazon Chime account, such as account type and supported licenses.
--
--
module Network.AWS.Chime.GetAccount
    (
    -- * Creating a Request
      getAccount
    , GetAccount
    -- * Request Lenses
    , gaAccountId

    -- * Destructuring the Response
    , getAccountResponse
    , GetAccountResponse
    -- * Response Lenses
    , garsAccount
    , garsResponseStatus
    ) where

import Network.AWS.Chime.Types
import Network.AWS.Chime.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAccount' smart constructor.
newtype GetAccount = GetAccount'
  { _gaAccountId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaAccountId' - The Amazon Chime account ID.
getAccount
    :: Text -- ^ 'gaAccountId'
    -> GetAccount
getAccount pAccountId_ = GetAccount' {_gaAccountId = pAccountId_}


-- | The Amazon Chime account ID.
gaAccountId :: Lens' GetAccount Text
gaAccountId = lens _gaAccountId (\ s a -> s{_gaAccountId = a})

instance AWSRequest GetAccount where
        type Rs GetAccount = GetAccountResponse
        request = get chime
        response
          = receiveJSON
              (\ s h x ->
                 GetAccountResponse' <$>
                   (x .?> "Account") <*> (pure (fromEnum s)))

instance Hashable GetAccount where

instance NFData GetAccount where

instance ToHeaders GetAccount where
        toHeaders = const mempty

instance ToPath GetAccount where
        toPath GetAccount'{..}
          = mconcat ["/console/accounts/", toBS _gaAccountId]

instance ToQuery GetAccount where
        toQuery = const mempty

-- | /See:/ 'getAccountResponse' smart constructor.
data GetAccountResponse = GetAccountResponse'
  { _garsAccount :: !(Maybe Account)
  , _garsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAccountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'garsAccount' - The Amazon Chime account details.
--
-- * 'garsResponseStatus' - -- | The response status code.
getAccountResponse
    :: Int -- ^ 'garsResponseStatus'
    -> GetAccountResponse
getAccountResponse pResponseStatus_ =
  GetAccountResponse'
    {_garsAccount = Nothing, _garsResponseStatus = pResponseStatus_}


-- | The Amazon Chime account details.
garsAccount :: Lens' GetAccountResponse (Maybe Account)
garsAccount = lens _garsAccount (\ s a -> s{_garsAccount = a})

-- | -- | The response status code.
garsResponseStatus :: Lens' GetAccountResponse Int
garsResponseStatus = lens _garsResponseStatus (\ s a -> s{_garsResponseStatus = a})

instance NFData GetAccountResponse where
