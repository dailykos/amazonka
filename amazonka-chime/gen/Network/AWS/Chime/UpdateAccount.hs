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
-- Module      : Network.AWS.Chime.UpdateAccount
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates account details for the specified Amazon Chime account. Currently, only account name updates are supported for this action.
--
--
module Network.AWS.Chime.UpdateAccount
    (
    -- * Creating a Request
      updateAccount
    , UpdateAccount
    -- * Request Lenses
    , uaName
    , uaAccountId

    -- * Destructuring the Response
    , updateAccountResponse
    , UpdateAccountResponse
    -- * Response Lenses
    , uarsAccount
    , uarsResponseStatus
    ) where

import Network.AWS.Chime.Types
import Network.AWS.Chime.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateAccount' smart constructor.
data UpdateAccount = UpdateAccount'
  { _uaName :: !(Maybe Text)
  , _uaAccountId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaName' - The new name for the specified Amazon Chime account.
--
-- * 'uaAccountId' - The Amazon Chime account ID.
updateAccount
    :: Text -- ^ 'uaAccountId'
    -> UpdateAccount
updateAccount pAccountId_ =
  UpdateAccount' {_uaName = Nothing, _uaAccountId = pAccountId_}


-- | The new name for the specified Amazon Chime account.
uaName :: Lens' UpdateAccount (Maybe Text)
uaName = lens _uaName (\ s a -> s{_uaName = a})

-- | The Amazon Chime account ID.
uaAccountId :: Lens' UpdateAccount Text
uaAccountId = lens _uaAccountId (\ s a -> s{_uaAccountId = a})

instance AWSRequest UpdateAccount where
        type Rs UpdateAccount = UpdateAccountResponse
        request = postJSON chime
        response
          = receiveJSON
              (\ s h x ->
                 UpdateAccountResponse' <$>
                   (x .?> "Account") <*> (pure (fromEnum s)))

instance Hashable UpdateAccount where

instance NFData UpdateAccount where

instance ToHeaders UpdateAccount where
        toHeaders = const mempty

instance ToJSON UpdateAccount where
        toJSON UpdateAccount'{..}
          = object (catMaybes [("Name" .=) <$> _uaName])

instance ToPath UpdateAccount where
        toPath UpdateAccount'{..}
          = mconcat ["/console/accounts/", toBS _uaAccountId]

instance ToQuery UpdateAccount where
        toQuery = const mempty

-- | /See:/ 'updateAccountResponse' smart constructor.
data UpdateAccountResponse = UpdateAccountResponse'
  { _uarsAccount :: !(Maybe Account)
  , _uarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAccountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uarsAccount' - The updated Amazon Chime account details.
--
-- * 'uarsResponseStatus' - -- | The response status code.
updateAccountResponse
    :: Int -- ^ 'uarsResponseStatus'
    -> UpdateAccountResponse
updateAccountResponse pResponseStatus_ =
  UpdateAccountResponse'
    {_uarsAccount = Nothing, _uarsResponseStatus = pResponseStatus_}


-- | The updated Amazon Chime account details.
uarsAccount :: Lens' UpdateAccountResponse (Maybe Account)
uarsAccount = lens _uarsAccount (\ s a -> s{_uarsAccount = a})

-- | -- | The response status code.
uarsResponseStatus :: Lens' UpdateAccountResponse Int
uarsResponseStatus = lens _uarsResponseStatus (\ s a -> s{_uarsResponseStatus = a})

instance NFData UpdateAccountResponse where
