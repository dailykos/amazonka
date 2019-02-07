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
-- Module      : Network.AWS.Backup.ListProtectedResources
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of resources successfully backed up by AWS Backup, including the time the resource was saved, an Amazon Resource Name (ARN) of the resource, and a resource type.
--
--
module Network.AWS.Backup.ListProtectedResources
    (
    -- * Creating a Request
      listProtectedResources
    , ListProtectedResources
    -- * Request Lenses
    , lprNextToken
    , lprMaxResults

    -- * Destructuring the Response
    , listProtectedResourcesResponse
    , ListProtectedResourcesResponse
    -- * Response Lenses
    , lprrsResults
    , lprrsNextToken
    , lprrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listProtectedResources' smart constructor.
data ListProtectedResources = ListProtectedResources'
  { _lprNextToken :: !(Maybe Text)
  , _lprMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListProtectedResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprNextToken' - The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
--
-- * 'lprMaxResults' - The maximum number of items to be returned.
listProtectedResources
    :: ListProtectedResources
listProtectedResources =
  ListProtectedResources' {_lprNextToken = Nothing, _lprMaxResults = Nothing}


-- | The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
lprNextToken :: Lens' ListProtectedResources (Maybe Text)
lprNextToken = lens _lprNextToken (\ s a -> s{_lprNextToken = a})

-- | The maximum number of items to be returned.
lprMaxResults :: Lens' ListProtectedResources (Maybe Natural)
lprMaxResults = lens _lprMaxResults (\ s a -> s{_lprMaxResults = a}) . mapping _Nat

instance AWSRequest ListProtectedResources where
        type Rs ListProtectedResources =
             ListProtectedResourcesResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 ListProtectedResourcesResponse' <$>
                   (x .?> "Results" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListProtectedResources where

instance NFData ListProtectedResources where

instance ToHeaders ListProtectedResources where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListProtectedResources where
        toPath = const "/resources/"

instance ToQuery ListProtectedResources where
        toQuery ListProtectedResources'{..}
          = mconcat
              ["nextToken" =: _lprNextToken,
               "maxResults" =: _lprMaxResults]

-- | /See:/ 'listProtectedResourcesResponse' smart constructor.
data ListProtectedResourcesResponse = ListProtectedResourcesResponse'
  { _lprrsResults :: !(Maybe [ProtectedResource])
  , _lprrsNextToken :: !(Maybe Text)
  , _lprrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListProtectedResourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprrsResults' - An array of resources successfully backed up by AWS Backup including the time the resource was saved, an Amazon Resource Name (ARN) of the resource, and a resource type.
--
-- * 'lprrsNextToken' - The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
--
-- * 'lprrsResponseStatus' - -- | The response status code.
listProtectedResourcesResponse
    :: Int -- ^ 'lprrsResponseStatus'
    -> ListProtectedResourcesResponse
listProtectedResourcesResponse pResponseStatus_ =
  ListProtectedResourcesResponse'
    { _lprrsResults = Nothing
    , _lprrsNextToken = Nothing
    , _lprrsResponseStatus = pResponseStatus_
    }


-- | An array of resources successfully backed up by AWS Backup including the time the resource was saved, an Amazon Resource Name (ARN) of the resource, and a resource type.
lprrsResults :: Lens' ListProtectedResourcesResponse [ProtectedResource]
lprrsResults = lens _lprrsResults (\ s a -> s{_lprrsResults = a}) . _Default . _Coerce

-- | The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
lprrsNextToken :: Lens' ListProtectedResourcesResponse (Maybe Text)
lprrsNextToken = lens _lprrsNextToken (\ s a -> s{_lprrsNextToken = a})

-- | -- | The response status code.
lprrsResponseStatus :: Lens' ListProtectedResourcesResponse Int
lprrsResponseStatus = lens _lprrsResponseStatus (\ s a -> s{_lprrsResponseStatus = a})

instance NFData ListProtectedResourcesResponse where
