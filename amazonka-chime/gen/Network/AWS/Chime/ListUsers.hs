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
-- Module      : Network.AWS.Chime.ListUsers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the users that belong to the specified Amazon Chime account. You can specify an email address to list only the user that the email address belongs to.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Chime.ListUsers
    (
    -- * Creating a Request
      listUsers
    , ListUsers
    -- * Request Lenses
    , luNextToken
    , luUserEmail
    , luMaxResults
    , luAccountId

    -- * Destructuring the Response
    , listUsersResponse
    , ListUsersResponse
    -- * Response Lenses
    , lrsUsers
    , lrsNextToken
    , lrsResponseStatus
    ) where

import Network.AWS.Chime.Types
import Network.AWS.Chime.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listUsers' smart constructor.
data ListUsers = ListUsers'
  { _luNextToken :: !(Maybe Text)
  , _luUserEmail :: !(Maybe (Sensitive Text))
  , _luMaxResults :: !(Maybe Nat)
  , _luAccountId :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUsers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luNextToken' - The token to use to retrieve the next page of results.
--
-- * 'luUserEmail' - Optional. The user email address used to filter results. Maximum 1.
--
-- * 'luMaxResults' - The maximum number of results to return in a single call. Defaults to 100.
--
-- * 'luAccountId' - The Amazon Chime account ID.
listUsers
    :: Text -- ^ 'luAccountId'
    -> ListUsers
listUsers pAccountId_ =
  ListUsers'
    { _luNextToken = Nothing
    , _luUserEmail = Nothing
    , _luMaxResults = Nothing
    , _luAccountId = pAccountId_
    }


-- | The token to use to retrieve the next page of results.
luNextToken :: Lens' ListUsers (Maybe Text)
luNextToken = lens _luNextToken (\ s a -> s{_luNextToken = a})

-- | Optional. The user email address used to filter results. Maximum 1.
luUserEmail :: Lens' ListUsers (Maybe Text)
luUserEmail = lens _luUserEmail (\ s a -> s{_luUserEmail = a}) . mapping _Sensitive

-- | The maximum number of results to return in a single call. Defaults to 100.
luMaxResults :: Lens' ListUsers (Maybe Natural)
luMaxResults = lens _luMaxResults (\ s a -> s{_luMaxResults = a}) . mapping _Nat

-- | The Amazon Chime account ID.
luAccountId :: Lens' ListUsers Text
luAccountId = lens _luAccountId (\ s a -> s{_luAccountId = a})

instance AWSPager ListUsers where
        page rq rs
          | stop (rs ^. lrsNextToken) = Nothing
          | stop (rs ^. lrsUsers) = Nothing
          | otherwise =
            Just $ rq & luNextToken .~ rs ^. lrsNextToken

instance AWSRequest ListUsers where
        type Rs ListUsers = ListUsersResponse
        request = get chime
        response
          = receiveJSON
              (\ s h x ->
                 ListUsersResponse' <$>
                   (x .?> "Users" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListUsers where

instance NFData ListUsers where

instance ToHeaders ListUsers where
        toHeaders = const mempty

instance ToPath ListUsers where
        toPath ListUsers'{..}
          = mconcat
              ["/console/accounts/", toBS _luAccountId, "/users"]

instance ToQuery ListUsers where
        toQuery ListUsers'{..}
          = mconcat
              ["next-token" =: _luNextToken,
               "user-email" =: _luUserEmail,
               "max-results" =: _luMaxResults]

-- | /See:/ 'listUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
  { _lrsUsers :: !(Maybe [User])
  , _lrsNextToken :: !(Maybe Text)
  , _lrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUsersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsUsers' - List of users and user details.
--
-- * 'lrsNextToken' - The token to use to retrieve the next page of results.
--
-- * 'lrsResponseStatus' - -- | The response status code.
listUsersResponse
    :: Int -- ^ 'lrsResponseStatus'
    -> ListUsersResponse
listUsersResponse pResponseStatus_ =
  ListUsersResponse'
    { _lrsUsers = Nothing
    , _lrsNextToken = Nothing
    , _lrsResponseStatus = pResponseStatus_
    }


-- | List of users and user details.
lrsUsers :: Lens' ListUsersResponse [User]
lrsUsers = lens _lrsUsers (\ s a -> s{_lrsUsers = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results.
lrsNextToken :: Lens' ListUsersResponse (Maybe Text)
lrsNextToken = lens _lrsNextToken (\ s a -> s{_lrsNextToken = a})

-- | -- | The response status code.
lrsResponseStatus :: Lens' ListUsersResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\ s a -> s{_lrsResponseStatus = a})

instance NFData ListUsersResponse where
