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
-- Module      : Network.AWS.Backup.ListTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of key-value pairs assigned to a target recovery point, backup plan, or backup vault.
--
--
module Network.AWS.Backup.ListTags
    (
    -- * Creating a Request
      listTags
    , ListTags
    -- * Request Lenses
    , ltNextToken
    , ltMaxResults
    , ltResourceARN

    -- * Destructuring the Response
    , listTagsResponse
    , ListTagsResponse
    -- * Response Lenses
    , ltrsNextToken
    , ltrsTags
    , ltrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTags' smart constructor.
data ListTags = ListTags'
  { _ltNextToken :: !(Maybe Text)
  , _ltMaxResults :: !(Maybe Nat)
  , _ltResourceARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltNextToken' - The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
--
-- * 'ltMaxResults' - The maximum number of items to be returned.
--
-- * 'ltResourceARN' - An Amazon Resource Name (ARN) that uniquely identifies a resource. The format of the ARN depends on the type of resource. Valid targets for @ListTags@ are recovery points, backup plans, and backup vaults.
listTags
    :: Text -- ^ 'ltResourceARN'
    -> ListTags
listTags pResourceARN_ =
  ListTags'
    { _ltNextToken = Nothing
    , _ltMaxResults = Nothing
    , _ltResourceARN = pResourceARN_
    }


-- | The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
ltNextToken :: Lens' ListTags (Maybe Text)
ltNextToken = lens _ltNextToken (\ s a -> s{_ltNextToken = a})

-- | The maximum number of items to be returned.
ltMaxResults :: Lens' ListTags (Maybe Natural)
ltMaxResults = lens _ltMaxResults (\ s a -> s{_ltMaxResults = a}) . mapping _Nat

-- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The format of the ARN depends on the type of resource. Valid targets for @ListTags@ are recovery points, backup plans, and backup vaults.
ltResourceARN :: Lens' ListTags Text
ltResourceARN = lens _ltResourceARN (\ s a -> s{_ltResourceARN = a})

instance AWSRequest ListTags where
        type Rs ListTags = ListTagsResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 ListTagsResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Tags" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable ListTags where

instance NFData ListTags where

instance ToHeaders ListTags where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListTags where
        toPath ListTags'{..}
          = mconcat ["/tags/", toBS _ltResourceARN, "/"]

instance ToQuery ListTags where
        toQuery ListTags'{..}
          = mconcat
              ["nextToken" =: _ltNextToken,
               "maxResults" =: _ltMaxResults]

-- | /See:/ 'listTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { _ltrsNextToken :: !(Maybe Text)
  , _ltrsTags :: !(Maybe (Sensitive (Map Text Text)))
  , _ltrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrsNextToken' - The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
--
-- * 'ltrsTags' - To help organize your resources, you can assign your own metadata to the resources you create. Each tag is a key-value pair.
--
-- * 'ltrsResponseStatus' - -- | The response status code.
listTagsResponse
    :: Int -- ^ 'ltrsResponseStatus'
    -> ListTagsResponse
listTagsResponse pResponseStatus_ =
  ListTagsResponse'
    { _ltrsNextToken = Nothing
    , _ltrsTags = Nothing
    , _ltrsResponseStatus = pResponseStatus_
    }


-- | The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
ltrsNextToken :: Lens' ListTagsResponse (Maybe Text)
ltrsNextToken = lens _ltrsNextToken (\ s a -> s{_ltrsNextToken = a})

-- | To help organize your resources, you can assign your own metadata to the resources you create. Each tag is a key-value pair.
ltrsTags :: Lens' ListTagsResponse (Maybe (HashMap Text Text))
ltrsTags = lens _ltrsTags (\ s a -> s{_ltrsTags = a}) . mapping (_Sensitive . _Map)

-- | -- | The response status code.
ltrsResponseStatus :: Lens' ListTagsResponse Int
ltrsResponseStatus = lens _ltrsResponseStatus (\ s a -> s{_ltrsResponseStatus = a})

instance NFData ListTagsResponse where
