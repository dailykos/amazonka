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
-- Module      : Network.AWS.APIGatewayV2.GetRoutes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the Routes for an API.
--
--
--
-- This operation returns paginated results.
module Network.AWS.APIGatewayV2.GetRoutes
    (
    -- * Creating a Request
      getRoutes
    , GetRoutes
    -- * Request Lenses
    , ggNextToken
    , ggMaxResults
    , ggAPIId

    -- * Destructuring the Response
    , getRoutesResponse
    , GetRoutesResponse
    -- * Response Lenses
    , grrsItems
    , grrsNextToken
    , grrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRoutes' smart constructor.
data GetRoutes = GetRoutes'
  { _ggNextToken :: !(Maybe Text)
  , _ggMaxResults :: !(Maybe Text)
  , _ggAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRoutes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggNextToken' - The next page of elements from this collection. Not valid for the last element of the collection.
--
-- * 'ggMaxResults' - The maximum number of elements to be returned for this resource.
--
-- * 'ggAPIId' - The API identifier.
getRoutes
    :: Text -- ^ 'ggAPIId'
    -> GetRoutes
getRoutes pAPIId_ =
  GetRoutes'
    {_ggNextToken = Nothing, _ggMaxResults = Nothing, _ggAPIId = pAPIId_}


-- | The next page of elements from this collection. Not valid for the last element of the collection.
ggNextToken :: Lens' GetRoutes (Maybe Text)
ggNextToken = lens _ggNextToken (\ s a -> s{_ggNextToken = a})

-- | The maximum number of elements to be returned for this resource.
ggMaxResults :: Lens' GetRoutes (Maybe Text)
ggMaxResults = lens _ggMaxResults (\ s a -> s{_ggMaxResults = a})

-- | The API identifier.
ggAPIId :: Lens' GetRoutes Text
ggAPIId = lens _ggAPIId (\ s a -> s{_ggAPIId = a})

instance AWSPager GetRoutes where
        page rq rs
          | stop (rs ^. grrsNextToken) = Nothing
          | stop (rs ^. grrsItems) = Nothing
          | otherwise =
            Just $ rq & ggNextToken .~ rs ^. grrsNextToken

instance AWSRequest GetRoutes where
        type Rs GetRoutes = GetRoutesResponse
        request = get apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 GetRoutesResponse' <$>
                   (x .?> "items" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetRoutes where

instance NFData GetRoutes where

instance ToHeaders GetRoutes where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetRoutes where
        toPath GetRoutes'{..}
          = mconcat ["/v2/apis/", toBS _ggAPIId, "/routes"]

instance ToQuery GetRoutes where
        toQuery GetRoutes'{..}
          = mconcat
              ["nextToken" =: _ggNextToken,
               "maxResults" =: _ggMaxResults]

-- | /See:/ 'getRoutesResponse' smart constructor.
data GetRoutesResponse = GetRoutesResponse'
  { _grrsItems :: !(Maybe [Route])
  , _grrsNextToken :: !(Maybe Text)
  , _grrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRoutesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrsItems' - The elements from this collection.
--
-- * 'grrsNextToken' - The next page of elements from this collection. Not valid for the last element of the collection.
--
-- * 'grrsResponseStatus' - -- | The response status code.
getRoutesResponse
    :: Int -- ^ 'grrsResponseStatus'
    -> GetRoutesResponse
getRoutesResponse pResponseStatus_ =
  GetRoutesResponse'
    { _grrsItems = Nothing
    , _grrsNextToken = Nothing
    , _grrsResponseStatus = pResponseStatus_
    }


-- | The elements from this collection.
grrsItems :: Lens' GetRoutesResponse [Route]
grrsItems = lens _grrsItems (\ s a -> s{_grrsItems = a}) . _Default . _Coerce

-- | The next page of elements from this collection. Not valid for the last element of the collection.
grrsNextToken :: Lens' GetRoutesResponse (Maybe Text)
grrsNextToken = lens _grrsNextToken (\ s a -> s{_grrsNextToken = a})

-- | -- | The response status code.
grrsResponseStatus :: Lens' GetRoutesResponse Int
grrsResponseStatus = lens _grrsResponseStatus (\ s a -> s{_grrsResponseStatus = a})

instance NFData GetRoutesResponse where
