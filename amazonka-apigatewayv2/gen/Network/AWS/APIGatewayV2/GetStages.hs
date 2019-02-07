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
-- Module      : Network.AWS.APIGatewayV2.GetStages
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the Stages for an API.
--
--
--
-- This operation returns paginated results.
module Network.AWS.APIGatewayV2.GetStages
    (
    -- * Creating a Request
      getStages
    , GetStages
    -- * Request Lenses
    , gsNextToken
    , gsMaxResults
    , gsAPIId

    -- * Destructuring the Response
    , getStagesResponse
    , GetStagesResponse
    -- * Response Lenses
    , gsrsItems
    , gsrsNextToken
    , gsrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getStages' smart constructor.
data GetStages = GetStages'
  { _gsNextToken :: !(Maybe Text)
  , _gsMaxResults :: !(Maybe Text)
  , _gsAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetStages' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsNextToken' - The next page of elements from this collection. Not valid for the last element of the collection.
--
-- * 'gsMaxResults' - The maximum number of elements to be returned for this resource.
--
-- * 'gsAPIId' - The API identifier.
getStages
    :: Text -- ^ 'gsAPIId'
    -> GetStages
getStages pAPIId_ =
  GetStages'
    {_gsNextToken = Nothing, _gsMaxResults = Nothing, _gsAPIId = pAPIId_}


-- | The next page of elements from this collection. Not valid for the last element of the collection.
gsNextToken :: Lens' GetStages (Maybe Text)
gsNextToken = lens _gsNextToken (\ s a -> s{_gsNextToken = a})

-- | The maximum number of elements to be returned for this resource.
gsMaxResults :: Lens' GetStages (Maybe Text)
gsMaxResults = lens _gsMaxResults (\ s a -> s{_gsMaxResults = a})

-- | The API identifier.
gsAPIId :: Lens' GetStages Text
gsAPIId = lens _gsAPIId (\ s a -> s{_gsAPIId = a})

instance AWSPager GetStages where
        page rq rs
          | stop (rs ^. gsrsNextToken) = Nothing
          | stop (rs ^. gsrsItems) = Nothing
          | otherwise =
            Just $ rq & gsNextToken .~ rs ^. gsrsNextToken

instance AWSRequest GetStages where
        type Rs GetStages = GetStagesResponse
        request = get apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 GetStagesResponse' <$>
                   (x .?> "items" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetStages where

instance NFData GetStages where

instance ToHeaders GetStages where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetStages where
        toPath GetStages'{..}
          = mconcat ["/v2/apis/", toBS _gsAPIId, "/stages"]

instance ToQuery GetStages where
        toQuery GetStages'{..}
          = mconcat
              ["nextToken" =: _gsNextToken,
               "maxResults" =: _gsMaxResults]

-- | /See:/ 'getStagesResponse' smart constructor.
data GetStagesResponse = GetStagesResponse'
  { _gsrsItems :: !(Maybe [Stage])
  , _gsrsNextToken :: !(Maybe Text)
  , _gsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetStagesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsrsItems' - The elements from this collection.
--
-- * 'gsrsNextToken' - The next page of elements from this collection. Not valid for the last element of the collection.
--
-- * 'gsrsResponseStatus' - -- | The response status code.
getStagesResponse
    :: Int -- ^ 'gsrsResponseStatus'
    -> GetStagesResponse
getStagesResponse pResponseStatus_ =
  GetStagesResponse'
    { _gsrsItems = Nothing
    , _gsrsNextToken = Nothing
    , _gsrsResponseStatus = pResponseStatus_
    }


-- | The elements from this collection.
gsrsItems :: Lens' GetStagesResponse [Stage]
gsrsItems = lens _gsrsItems (\ s a -> s{_gsrsItems = a}) . _Default . _Coerce

-- | The next page of elements from this collection. Not valid for the last element of the collection.
gsrsNextToken :: Lens' GetStagesResponse (Maybe Text)
gsrsNextToken = lens _gsrsNextToken (\ s a -> s{_gsrsNextToken = a})

-- | -- | The response status code.
gsrsResponseStatus :: Lens' GetStagesResponse Int
gsrsResponseStatus = lens _gsrsResponseStatus (\ s a -> s{_gsrsResponseStatus = a})

instance NFData GetStagesResponse where
