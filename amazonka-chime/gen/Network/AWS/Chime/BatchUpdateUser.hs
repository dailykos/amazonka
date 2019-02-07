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
-- Module      : Network.AWS.Chime.BatchUpdateUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates user details within the 'UpdateUserRequestItem' object for up to 20 users for the specified Amazon Chime account. Currently, only @LicenseType@ updates are supported for this action.
--
--
module Network.AWS.Chime.BatchUpdateUser
    (
    -- * Creating a Request
      batchUpdateUser
    , BatchUpdateUser
    -- * Request Lenses
    , buuAccountId
    , buuUpdateUserRequestItems

    -- * Destructuring the Response
    , batchUpdateUserResponse
    , BatchUpdateUserResponse
    -- * Response Lenses
    , brsUserErrors
    , brsResponseStatus
    ) where

import Network.AWS.Chime.Types
import Network.AWS.Chime.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchUpdateUser' smart constructor.
data BatchUpdateUser = BatchUpdateUser'
  { _buuAccountId :: !Text
  , _buuUpdateUserRequestItems :: ![UpdateUserRequestItem]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchUpdateUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'buuAccountId' - The Amazon Chime account ID.
--
-- * 'buuUpdateUserRequestItems' - The request containing the user IDs and details to update.
batchUpdateUser
    :: Text -- ^ 'buuAccountId'
    -> BatchUpdateUser
batchUpdateUser pAccountId_ =
  BatchUpdateUser'
    {_buuAccountId = pAccountId_, _buuUpdateUserRequestItems = mempty}


-- | The Amazon Chime account ID.
buuAccountId :: Lens' BatchUpdateUser Text
buuAccountId = lens _buuAccountId (\ s a -> s{_buuAccountId = a})

-- | The request containing the user IDs and details to update.
buuUpdateUserRequestItems :: Lens' BatchUpdateUser [UpdateUserRequestItem]
buuUpdateUserRequestItems = lens _buuUpdateUserRequestItems (\ s a -> s{_buuUpdateUserRequestItems = a}) . _Coerce

instance AWSRequest BatchUpdateUser where
        type Rs BatchUpdateUser = BatchUpdateUserResponse
        request = postJSON chime
        response
          = receiveJSON
              (\ s h x ->
                 BatchUpdateUserResponse' <$>
                   (x .?> "UserErrors" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable BatchUpdateUser where

instance NFData BatchUpdateUser where

instance ToHeaders BatchUpdateUser where
        toHeaders = const mempty

instance ToJSON BatchUpdateUser where
        toJSON BatchUpdateUser'{..}
          = object
              (catMaybes
                 [Just
                    ("UpdateUserRequestItems" .=
                       _buuUpdateUserRequestItems)])

instance ToPath BatchUpdateUser where
        toPath BatchUpdateUser'{..}
          = mconcat
              ["/console/accounts/", toBS _buuAccountId, "/users"]

instance ToQuery BatchUpdateUser where
        toQuery = const mempty

-- | /See:/ 'batchUpdateUserResponse' smart constructor.
data BatchUpdateUserResponse = BatchUpdateUserResponse'
  { _brsUserErrors :: !(Maybe [UserError])
  , _brsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchUpdateUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brsUserErrors' - If the 'BatchUpdateUser' action fails for one or more of the user IDs in the request, a list of the user IDs is returned, along with error codes and error messages.
--
-- * 'brsResponseStatus' - -- | The response status code.
batchUpdateUserResponse
    :: Int -- ^ 'brsResponseStatus'
    -> BatchUpdateUserResponse
batchUpdateUserResponse pResponseStatus_ =
  BatchUpdateUserResponse'
    {_brsUserErrors = Nothing, _brsResponseStatus = pResponseStatus_}


-- | If the 'BatchUpdateUser' action fails for one or more of the user IDs in the request, a list of the user IDs is returned, along with error codes and error messages.
brsUserErrors :: Lens' BatchUpdateUserResponse [UserError]
brsUserErrors = lens _brsUserErrors (\ s a -> s{_brsUserErrors = a}) . _Default . _Coerce

-- | -- | The response status code.
brsResponseStatus :: Lens' BatchUpdateUserResponse Int
brsResponseStatus = lens _brsResponseStatus (\ s a -> s{_brsResponseStatus = a})

instance NFData BatchUpdateUserResponse where
