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
-- Module      : Network.AWS.APIGatewayV2.GetAuthorizers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the Authorizers for an API.
--
--
--
-- This operation returns paginated results.
module Network.AWS.APIGatewayV2.GetAuthorizers
    (
    -- * Creating a Request
      getAuthorizers
    , GetAuthorizers
    -- * Request Lenses
    , gaNextToken
    , gaMaxResults
    , gaAPIId

    -- * Destructuring the Response
    , getAuthorizersResponse
    , GetAuthorizersResponse
    -- * Response Lenses
    , gasrsItems
    , gasrsNextToken
    , gasrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAuthorizers' smart constructor.
data GetAuthorizers = GetAuthorizers'
  { _gaNextToken :: !(Maybe Text)
  , _gaMaxResults :: !(Maybe Text)
  , _gaAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAuthorizers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaNextToken' - The next page of elements from this collection. Not valid for the last element of the collection.
--
-- * 'gaMaxResults' - The maximum number of elements to be returned for this resource.
--
-- * 'gaAPIId' - The API identifier.
getAuthorizers
    :: Text -- ^ 'gaAPIId'
    -> GetAuthorizers
getAuthorizers pAPIId_ =
  GetAuthorizers'
    {_gaNextToken = Nothing, _gaMaxResults = Nothing, _gaAPIId = pAPIId_}


-- | The next page of elements from this collection. Not valid for the last element of the collection.
gaNextToken :: Lens' GetAuthorizers (Maybe Text)
gaNextToken = lens _gaNextToken (\ s a -> s{_gaNextToken = a})

-- | The maximum number of elements to be returned for this resource.
gaMaxResults :: Lens' GetAuthorizers (Maybe Text)
gaMaxResults = lens _gaMaxResults (\ s a -> s{_gaMaxResults = a})

-- | The API identifier.
gaAPIId :: Lens' GetAuthorizers Text
gaAPIId = lens _gaAPIId (\ s a -> s{_gaAPIId = a})

instance AWSPager GetAuthorizers where
        page rq rs
          | stop (rs ^. gasrsNextToken) = Nothing
          | stop (rs ^. gasrsItems) = Nothing
          | otherwise =
            Just $ rq & gaNextToken .~ rs ^. gasrsNextToken

instance AWSRequest GetAuthorizers where
        type Rs GetAuthorizers = GetAuthorizersResponse
        request = get apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 GetAuthorizersResponse' <$>
                   (x .?> "items" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetAuthorizers where

instance NFData GetAuthorizers where

instance ToHeaders GetAuthorizers where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetAuthorizers where
        toPath GetAuthorizers'{..}
          = mconcat
              ["/v2/apis/", toBS _gaAPIId, "/authorizers"]

instance ToQuery GetAuthorizers where
        toQuery GetAuthorizers'{..}
          = mconcat
              ["nextToken" =: _gaNextToken,
               "maxResults" =: _gaMaxResults]

-- | /See:/ 'getAuthorizersResponse' smart constructor.
data GetAuthorizersResponse = GetAuthorizersResponse'
  { _gasrsItems :: !(Maybe [Authorizer])
  , _gasrsNextToken :: !(Maybe Text)
  , _gasrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAuthorizersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gasrsItems' - The elements from this collection.
--
-- * 'gasrsNextToken' - The next page of elements from this collection. Not valid for the last element of the collection.
--
-- * 'gasrsResponseStatus' - -- | The response status code.
getAuthorizersResponse
    :: Int -- ^ 'gasrsResponseStatus'
    -> GetAuthorizersResponse
getAuthorizersResponse pResponseStatus_ =
  GetAuthorizersResponse'
    { _gasrsItems = Nothing
    , _gasrsNextToken = Nothing
    , _gasrsResponseStatus = pResponseStatus_
    }


-- | The elements from this collection.
gasrsItems :: Lens' GetAuthorizersResponse [Authorizer]
gasrsItems = lens _gasrsItems (\ s a -> s{_gasrsItems = a}) . _Default . _Coerce

-- | The next page of elements from this collection. Not valid for the last element of the collection.
gasrsNextToken :: Lens' GetAuthorizersResponse (Maybe Text)
gasrsNextToken = lens _gasrsNextToken (\ s a -> s{_gasrsNextToken = a})

-- | -- | The response status code.
gasrsResponseStatus :: Lens' GetAuthorizersResponse Int
gasrsResponseStatus = lens _gasrsResponseStatus (\ s a -> s{_gasrsResponseStatus = a})

instance NFData GetAuthorizersResponse where
