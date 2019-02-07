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
-- Module      : Network.AWS.APIGatewayV2.GetRouteResponses
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the RouteResponses for a Route.
--
--
--
-- This operation returns paginated results.
module Network.AWS.APIGatewayV2.GetRouteResponses
    (
    -- * Creating a Request
      getRouteResponses
    , GetRouteResponses
    -- * Request Lenses
    , grrNextToken
    , grrMaxResults
    , grrRouteId
    , grrAPIId

    -- * Destructuring the Response
    , getRouteResponsesResponse
    , GetRouteResponsesResponse
    -- * Response Lenses
    , grrrsItems
    , grrrsNextToken
    , grrrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRouteResponses' smart constructor.
data GetRouteResponses = GetRouteResponses'
  { _grrNextToken :: !(Maybe Text)
  , _grrMaxResults :: !(Maybe Text)
  , _grrRouteId :: !Text
  , _grrAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRouteResponses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrNextToken' - The next page of elements from this collection. Not valid for the last element of the collection.
--
-- * 'grrMaxResults' - The maximum number of elements to be returned for this resource.
--
-- * 'grrRouteId' - The route ID.
--
-- * 'grrAPIId' - The API identifier.
getRouteResponses
    :: Text -- ^ 'grrRouteId'
    -> Text -- ^ 'grrAPIId'
    -> GetRouteResponses
getRouteResponses pRouteId_ pAPIId_ =
  GetRouteResponses'
    { _grrNextToken = Nothing
    , _grrMaxResults = Nothing
    , _grrRouteId = pRouteId_
    , _grrAPIId = pAPIId_
    }


-- | The next page of elements from this collection. Not valid for the last element of the collection.
grrNextToken :: Lens' GetRouteResponses (Maybe Text)
grrNextToken = lens _grrNextToken (\ s a -> s{_grrNextToken = a})

-- | The maximum number of elements to be returned for this resource.
grrMaxResults :: Lens' GetRouteResponses (Maybe Text)
grrMaxResults = lens _grrMaxResults (\ s a -> s{_grrMaxResults = a})

-- | The route ID.
grrRouteId :: Lens' GetRouteResponses Text
grrRouteId = lens _grrRouteId (\ s a -> s{_grrRouteId = a})

-- | The API identifier.
grrAPIId :: Lens' GetRouteResponses Text
grrAPIId = lens _grrAPIId (\ s a -> s{_grrAPIId = a})

instance AWSPager GetRouteResponses where
        page rq rs
          | stop (rs ^. grrrsNextToken) = Nothing
          | stop (rs ^. grrrsItems) = Nothing
          | otherwise =
            Just $ rq & grrNextToken .~ rs ^. grrrsNextToken

instance AWSRequest GetRouteResponses where
        type Rs GetRouteResponses = GetRouteResponsesResponse
        request = get apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 GetRouteResponsesResponse' <$>
                   (x .?> "items" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetRouteResponses where

instance NFData GetRouteResponses where

instance ToHeaders GetRouteResponses where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetRouteResponses where
        toPath GetRouteResponses'{..}
          = mconcat
              ["/v2/apis/", toBS _grrAPIId, "/routes/",
               toBS _grrRouteId, "/routeresponses"]

instance ToQuery GetRouteResponses where
        toQuery GetRouteResponses'{..}
          = mconcat
              ["nextToken" =: _grrNextToken,
               "maxResults" =: _grrMaxResults]

-- | /See:/ 'getRouteResponsesResponse' smart constructor.
data GetRouteResponsesResponse = GetRouteResponsesResponse'
  { _grrrsItems :: !(Maybe [RouteResponse])
  , _grrrsNextToken :: !(Maybe Text)
  , _grrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRouteResponsesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrrsItems' - The elements from this collection.
--
-- * 'grrrsNextToken' - The next page of elements from this collection. Not valid for the last element of the collection.
--
-- * 'grrrsResponseStatus' - -- | The response status code.
getRouteResponsesResponse
    :: Int -- ^ 'grrrsResponseStatus'
    -> GetRouteResponsesResponse
getRouteResponsesResponse pResponseStatus_ =
  GetRouteResponsesResponse'
    { _grrrsItems = Nothing
    , _grrrsNextToken = Nothing
    , _grrrsResponseStatus = pResponseStatus_
    }


-- | The elements from this collection.
grrrsItems :: Lens' GetRouteResponsesResponse [RouteResponse]
grrrsItems = lens _grrrsItems (\ s a -> s{_grrrsItems = a}) . _Default . _Coerce

-- | The next page of elements from this collection. Not valid for the last element of the collection.
grrrsNextToken :: Lens' GetRouteResponsesResponse (Maybe Text)
grrrsNextToken = lens _grrrsNextToken (\ s a -> s{_grrrsNextToken = a})

-- | -- | The response status code.
grrrsResponseStatus :: Lens' GetRouteResponsesResponse Int
grrrsResponseStatus = lens _grrrsResponseStatus (\ s a -> s{_grrrsResponseStatus = a})

instance NFData GetRouteResponsesResponse where
