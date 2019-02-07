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
-- Module      : Network.AWS.APIGatewayV2.GetAPIs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a collection of Api resources.
--
--
--
-- This operation returns paginated results.
module Network.AWS.APIGatewayV2.GetAPIs
    (
    -- * Creating a Request
      getAPIs
    , GetAPIs
    -- * Request Lenses
    , gapiNextToken
    , gapiMaxResults

    -- * Destructuring the Response
    , getAPIsResponse
    , GetAPIsResponse
    -- * Response Lenses
    , gapirsItems
    , gapirsNextToken
    , gapirsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAPIs' smart constructor.
data GetAPIs = GetAPIs'
  { _gapiNextToken :: !(Maybe Text)
  , _gapiMaxResults :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAPIs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gapiNextToken' - The next page of elements from this collection. Not valid for the last element of the collection.
--
-- * 'gapiMaxResults' - The maximum number of elements to be returned for this resource.
getAPIs
    :: GetAPIs
getAPIs = GetAPIs' {_gapiNextToken = Nothing, _gapiMaxResults = Nothing}


-- | The next page of elements from this collection. Not valid for the last element of the collection.
gapiNextToken :: Lens' GetAPIs (Maybe Text)
gapiNextToken = lens _gapiNextToken (\ s a -> s{_gapiNextToken = a})

-- | The maximum number of elements to be returned for this resource.
gapiMaxResults :: Lens' GetAPIs (Maybe Text)
gapiMaxResults = lens _gapiMaxResults (\ s a -> s{_gapiMaxResults = a})

instance AWSPager GetAPIs where
        page rq rs
          | stop (rs ^. gapirsNextToken) = Nothing
          | stop (rs ^. gapirsItems) = Nothing
          | otherwise =
            Just $ rq & gapiNextToken .~ rs ^. gapirsNextToken

instance AWSRequest GetAPIs where
        type Rs GetAPIs = GetAPIsResponse
        request = get apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 GetAPIsResponse' <$>
                   (x .?> "items" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetAPIs where

instance NFData GetAPIs where

instance ToHeaders GetAPIs where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetAPIs where
        toPath = const "/v2/apis"

instance ToQuery GetAPIs where
        toQuery GetAPIs'{..}
          = mconcat
              ["nextToken" =: _gapiNextToken,
               "maxResults" =: _gapiMaxResults]

-- | /See:/ 'getAPIsResponse' smart constructor.
data GetAPIsResponse = GetAPIsResponse'
  { _gapirsItems :: !(Maybe [API])
  , _gapirsNextToken :: !(Maybe Text)
  , _gapirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAPIsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gapirsItems' - The elements from this collection.
--
-- * 'gapirsNextToken' - The next page of elements from this collection. Not valid for the last element of the collection.
--
-- * 'gapirsResponseStatus' - -- | The response status code.
getAPIsResponse
    :: Int -- ^ 'gapirsResponseStatus'
    -> GetAPIsResponse
getAPIsResponse pResponseStatus_ =
  GetAPIsResponse'
    { _gapirsItems = Nothing
    , _gapirsNextToken = Nothing
    , _gapirsResponseStatus = pResponseStatus_
    }


-- | The elements from this collection.
gapirsItems :: Lens' GetAPIsResponse [API]
gapirsItems = lens _gapirsItems (\ s a -> s{_gapirsItems = a}) . _Default . _Coerce

-- | The next page of elements from this collection. Not valid for the last element of the collection.
gapirsNextToken :: Lens' GetAPIsResponse (Maybe Text)
gapirsNextToken = lens _gapirsNextToken (\ s a -> s{_gapirsNextToken = a})

-- | -- | The response status code.
gapirsResponseStatus :: Lens' GetAPIsResponse Int
gapirsResponseStatus = lens _gapirsResponseStatus (\ s a -> s{_gapirsResponseStatus = a})

instance NFData GetAPIsResponse where
