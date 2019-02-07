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
-- Module      : Network.AWS.APIGatewayV2.GetModels
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the Models for an API.
--
--
--
-- This operation returns paginated results.
module Network.AWS.APIGatewayV2.GetModels
    (
    -- * Creating a Request
      getModels
    , GetModels
    -- * Request Lenses
    , gmNextToken
    , gmMaxResults
    , gmAPIId

    -- * Destructuring the Response
    , getModelsResponse
    , GetModelsResponse
    -- * Response Lenses
    , gmsrsItems
    , gmsrsNextToken
    , gmsrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getModels' smart constructor.
data GetModels = GetModels'
  { _gmNextToken :: !(Maybe Text)
  , _gmMaxResults :: !(Maybe Text)
  , _gmAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetModels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmNextToken' - The next page of elements from this collection. Not valid for the last element of the collection.
--
-- * 'gmMaxResults' - The maximum number of elements to be returned for this resource.
--
-- * 'gmAPIId' - The API identifier.
getModels
    :: Text -- ^ 'gmAPIId'
    -> GetModels
getModels pAPIId_ =
  GetModels'
    {_gmNextToken = Nothing, _gmMaxResults = Nothing, _gmAPIId = pAPIId_}


-- | The next page of elements from this collection. Not valid for the last element of the collection.
gmNextToken :: Lens' GetModels (Maybe Text)
gmNextToken = lens _gmNextToken (\ s a -> s{_gmNextToken = a})

-- | The maximum number of elements to be returned for this resource.
gmMaxResults :: Lens' GetModels (Maybe Text)
gmMaxResults = lens _gmMaxResults (\ s a -> s{_gmMaxResults = a})

-- | The API identifier.
gmAPIId :: Lens' GetModels Text
gmAPIId = lens _gmAPIId (\ s a -> s{_gmAPIId = a})

instance AWSPager GetModels where
        page rq rs
          | stop (rs ^. gmsrsNextToken) = Nothing
          | stop (rs ^. gmsrsItems) = Nothing
          | otherwise =
            Just $ rq & gmNextToken .~ rs ^. gmsrsNextToken

instance AWSRequest GetModels where
        type Rs GetModels = GetModelsResponse
        request = get apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 GetModelsResponse' <$>
                   (x .?> "items" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetModels where

instance NFData GetModels where

instance ToHeaders GetModels where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetModels where
        toPath GetModels'{..}
          = mconcat ["/v2/apis/", toBS _gmAPIId, "/models"]

instance ToQuery GetModels where
        toQuery GetModels'{..}
          = mconcat
              ["nextToken" =: _gmNextToken,
               "maxResults" =: _gmMaxResults]

-- | /See:/ 'getModelsResponse' smart constructor.
data GetModelsResponse = GetModelsResponse'
  { _gmsrsItems :: !(Maybe [Model])
  , _gmsrsNextToken :: !(Maybe Text)
  , _gmsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetModelsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmsrsItems' - The elements from this collection.
--
-- * 'gmsrsNextToken' - The next page of elements from this collection. Not valid for the last element of the collection.
--
-- * 'gmsrsResponseStatus' - -- | The response status code.
getModelsResponse
    :: Int -- ^ 'gmsrsResponseStatus'
    -> GetModelsResponse
getModelsResponse pResponseStatus_ =
  GetModelsResponse'
    { _gmsrsItems = Nothing
    , _gmsrsNextToken = Nothing
    , _gmsrsResponseStatus = pResponseStatus_
    }


-- | The elements from this collection.
gmsrsItems :: Lens' GetModelsResponse [Model]
gmsrsItems = lens _gmsrsItems (\ s a -> s{_gmsrsItems = a}) . _Default . _Coerce

-- | The next page of elements from this collection. Not valid for the last element of the collection.
gmsrsNextToken :: Lens' GetModelsResponse (Maybe Text)
gmsrsNextToken = lens _gmsrsNextToken (\ s a -> s{_gmsrsNextToken = a})

-- | -- | The response status code.
gmsrsResponseStatus :: Lens' GetModelsResponse Int
gmsrsResponseStatus = lens _gmsrsResponseStatus (\ s a -> s{_gmsrsResponseStatus = a})

instance NFData GetModelsResponse where
