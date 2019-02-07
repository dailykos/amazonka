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
-- Module      : Network.AWS.Chime.ListAccounts
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Amazon Chime accounts under the administrator's AWS account. You can filter accounts by account name prefix. To find out which Amazon Chime account a user belongs to, you can filter by the user's email address, which returns one account result.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Chime.ListAccounts
    (
    -- * Creating a Request
      listAccounts
    , ListAccounts
    -- * Request Lenses
    , laNextToken
    , laName
    , laUserEmail
    , laMaxResults

    -- * Destructuring the Response
    , listAccountsResponse
    , ListAccountsResponse
    -- * Response Lenses
    , larsAccounts
    , larsNextToken
    , larsResponseStatus
    ) where

import Network.AWS.Chime.Types
import Network.AWS.Chime.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAccounts' smart constructor.
data ListAccounts = ListAccounts'
  { _laNextToken :: !(Maybe Text)
  , _laName :: !(Maybe Text)
  , _laUserEmail :: !(Maybe (Sensitive Text))
  , _laMaxResults :: !(Maybe Nat)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAccounts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laNextToken' - The token to use to retrieve the next page of results.
--
-- * 'laName' - Amazon Chime account name prefix with which to filter results.
--
-- * 'laUserEmail' - User email address with which to filter results.
--
-- * 'laMaxResults' - The maximum number of results to return in a single call. Defaults to 100.
listAccounts
    :: ListAccounts
listAccounts =
  ListAccounts'
    { _laNextToken = Nothing
    , _laName = Nothing
    , _laUserEmail = Nothing
    , _laMaxResults = Nothing
    }


-- | The token to use to retrieve the next page of results.
laNextToken :: Lens' ListAccounts (Maybe Text)
laNextToken = lens _laNextToken (\ s a -> s{_laNextToken = a})

-- | Amazon Chime account name prefix with which to filter results.
laName :: Lens' ListAccounts (Maybe Text)
laName = lens _laName (\ s a -> s{_laName = a})

-- | User email address with which to filter results.
laUserEmail :: Lens' ListAccounts (Maybe Text)
laUserEmail = lens _laUserEmail (\ s a -> s{_laUserEmail = a}) . mapping _Sensitive

-- | The maximum number of results to return in a single call. Defaults to 100.
laMaxResults :: Lens' ListAccounts (Maybe Natural)
laMaxResults = lens _laMaxResults (\ s a -> s{_laMaxResults = a}) . mapping _Nat

instance AWSPager ListAccounts where
        page rq rs
          | stop (rs ^. larsNextToken) = Nothing
          | stop (rs ^. larsAccounts) = Nothing
          | otherwise =
            Just $ rq & laNextToken .~ rs ^. larsNextToken

instance AWSRequest ListAccounts where
        type Rs ListAccounts = ListAccountsResponse
        request = get chime
        response
          = receiveJSON
              (\ s h x ->
                 ListAccountsResponse' <$>
                   (x .?> "Accounts" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListAccounts where

instance NFData ListAccounts where

instance ToHeaders ListAccounts where
        toHeaders = const mempty

instance ToPath ListAccounts where
        toPath = const "/console/accounts"

instance ToQuery ListAccounts where
        toQuery ListAccounts'{..}
          = mconcat
              ["next-token" =: _laNextToken, "name" =: _laName,
               "user-email" =: _laUserEmail,
               "max-results" =: _laMaxResults]

-- | /See:/ 'listAccountsResponse' smart constructor.
data ListAccountsResponse = ListAccountsResponse'
  { _larsAccounts :: !(Maybe [Account])
  , _larsNextToken :: !(Maybe Text)
  , _larsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAccountsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larsAccounts' - List of Amazon Chime accounts and account details.
--
-- * 'larsNextToken' - The token to use to retrieve the next page of results.
--
-- * 'larsResponseStatus' - -- | The response status code.
listAccountsResponse
    :: Int -- ^ 'larsResponseStatus'
    -> ListAccountsResponse
listAccountsResponse pResponseStatus_ =
  ListAccountsResponse'
    { _larsAccounts = Nothing
    , _larsNextToken = Nothing
    , _larsResponseStatus = pResponseStatus_
    }


-- | List of Amazon Chime accounts and account details.
larsAccounts :: Lens' ListAccountsResponse [Account]
larsAccounts = lens _larsAccounts (\ s a -> s{_larsAccounts = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results.
larsNextToken :: Lens' ListAccountsResponse (Maybe Text)
larsNextToken = lens _larsNextToken (\ s a -> s{_larsNextToken = a})

-- | -- | The response status code.
larsResponseStatus :: Lens' ListAccountsResponse Int
larsResponseStatus = lens _larsResponseStatus (\ s a -> s{_larsResponseStatus = a})

instance NFData ListAccountsResponse where
