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
-- Module      : Network.AWS.Chime.CreateAccount
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Chime account under the administrator's AWS account. Only @Team@ account types are currently supported for this action. For more information about different account types, see <http://docs.aws.amazon.com/chime/latest/ag/manage-chime-account.html Managing Your Amazon Chime Accounts> in the /Amazon Chime Administration Guide/ .
--
--
module Network.AWS.Chime.CreateAccount
    (
    -- * Creating a Request
      createAccount
    , CreateAccount
    -- * Request Lenses
    , caName

    -- * Destructuring the Response
    , createAccountResponse
    , CreateAccountResponse
    -- * Response Lenses
    , carsAccount
    , carsResponseStatus
    ) where

import Network.AWS.Chime.Types
import Network.AWS.Chime.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createAccount' smart constructor.
newtype CreateAccount = CreateAccount'
  { _caName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caName' - The name of the Amazon Chime account.
createAccount
    :: Text -- ^ 'caName'
    -> CreateAccount
createAccount pName_ = CreateAccount' {_caName = pName_}


-- | The name of the Amazon Chime account.
caName :: Lens' CreateAccount Text
caName = lens _caName (\ s a -> s{_caName = a})

instance AWSRequest CreateAccount where
        type Rs CreateAccount = CreateAccountResponse
        request = postJSON chime
        response
          = receiveJSON
              (\ s h x ->
                 CreateAccountResponse' <$>
                   (x .?> "Account") <*> (pure (fromEnum s)))

instance Hashable CreateAccount where

instance NFData CreateAccount where

instance ToHeaders CreateAccount where
        toHeaders = const mempty

instance ToJSON CreateAccount where
        toJSON CreateAccount'{..}
          = object (catMaybes [Just ("Name" .= _caName)])

instance ToPath CreateAccount where
        toPath = const "/console/accounts"

instance ToQuery CreateAccount where
        toQuery = const mempty

-- | /See:/ 'createAccountResponse' smart constructor.
data CreateAccountResponse = CreateAccountResponse'
  { _carsAccount :: !(Maybe Account)
  , _carsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAccountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carsAccount' - The Amazon Chime account details.
--
-- * 'carsResponseStatus' - -- | The response status code.
createAccountResponse
    :: Int -- ^ 'carsResponseStatus'
    -> CreateAccountResponse
createAccountResponse pResponseStatus_ =
  CreateAccountResponse'
    {_carsAccount = Nothing, _carsResponseStatus = pResponseStatus_}


-- | The Amazon Chime account details.
carsAccount :: Lens' CreateAccountResponse (Maybe Account)
carsAccount = lens _carsAccount (\ s a -> s{_carsAccount = a})

-- | -- | The response status code.
carsResponseStatus :: Lens' CreateAccountResponse Int
carsResponseStatus = lens _carsResponseStatus (\ s a -> s{_carsResponseStatus = a})

instance NFData CreateAccountResponse where
