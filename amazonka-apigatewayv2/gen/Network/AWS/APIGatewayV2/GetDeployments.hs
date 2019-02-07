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
-- Module      : Network.AWS.APIGatewayV2.GetDeployments
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the Deployments for an API.
--
--
--
-- This operation returns paginated results.
module Network.AWS.APIGatewayV2.GetDeployments
    (
    -- * Creating a Request
      getDeployments
    , GetDeployments
    -- * Request Lenses
    , gdNextToken
    , gdMaxResults
    , gdAPIId

    -- * Destructuring the Response
    , getDeploymentsResponse
    , GetDeploymentsResponse
    -- * Response Lenses
    , grsItems
    , grsNextToken
    , grsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDeployments' smart constructor.
data GetDeployments = GetDeployments'
  { _gdNextToken :: !(Maybe Text)
  , _gdMaxResults :: !(Maybe Text)
  , _gdAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDeployments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdNextToken' - The next page of elements from this collection. Not valid for the last element of the collection.
--
-- * 'gdMaxResults' - The maximum number of elements to be returned for this resource.
--
-- * 'gdAPIId' - The API identifier.
getDeployments
    :: Text -- ^ 'gdAPIId'
    -> GetDeployments
getDeployments pAPIId_ =
  GetDeployments'
    {_gdNextToken = Nothing, _gdMaxResults = Nothing, _gdAPIId = pAPIId_}


-- | The next page of elements from this collection. Not valid for the last element of the collection.
gdNextToken :: Lens' GetDeployments (Maybe Text)
gdNextToken = lens _gdNextToken (\ s a -> s{_gdNextToken = a})

-- | The maximum number of elements to be returned for this resource.
gdMaxResults :: Lens' GetDeployments (Maybe Text)
gdMaxResults = lens _gdMaxResults (\ s a -> s{_gdMaxResults = a})

-- | The API identifier.
gdAPIId :: Lens' GetDeployments Text
gdAPIId = lens _gdAPIId (\ s a -> s{_gdAPIId = a})

instance AWSPager GetDeployments where
        page rq rs
          | stop (rs ^. grsNextToken) = Nothing
          | stop (rs ^. grsItems) = Nothing
          | otherwise =
            Just $ rq & gdNextToken .~ rs ^. grsNextToken

instance AWSRequest GetDeployments where
        type Rs GetDeployments = GetDeploymentsResponse
        request = get apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 GetDeploymentsResponse' <$>
                   (x .?> "items" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetDeployments where

instance NFData GetDeployments where

instance ToHeaders GetDeployments where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetDeployments where
        toPath GetDeployments'{..}
          = mconcat
              ["/v2/apis/", toBS _gdAPIId, "/deployments"]

instance ToQuery GetDeployments where
        toQuery GetDeployments'{..}
          = mconcat
              ["nextToken" =: _gdNextToken,
               "maxResults" =: _gdMaxResults]

-- | /See:/ 'getDeploymentsResponse' smart constructor.
data GetDeploymentsResponse = GetDeploymentsResponse'
  { _grsItems :: !(Maybe [Deployment])
  , _grsNextToken :: !(Maybe Text)
  , _grsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDeploymentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grsItems' - The elements from this collection.
--
-- * 'grsNextToken' - The next page of elements from this collection. Not valid for the last element of the collection.
--
-- * 'grsResponseStatus' - -- | The response status code.
getDeploymentsResponse
    :: Int -- ^ 'grsResponseStatus'
    -> GetDeploymentsResponse
getDeploymentsResponse pResponseStatus_ =
  GetDeploymentsResponse'
    { _grsItems = Nothing
    , _grsNextToken = Nothing
    , _grsResponseStatus = pResponseStatus_
    }


-- | The elements from this collection.
grsItems :: Lens' GetDeploymentsResponse [Deployment]
grsItems = lens _grsItems (\ s a -> s{_grsItems = a}) . _Default . _Coerce

-- | The next page of elements from this collection. Not valid for the last element of the collection.
grsNextToken :: Lens' GetDeploymentsResponse (Maybe Text)
grsNextToken = lens _grsNextToken (\ s a -> s{_grsNextToken = a})

-- | -- | The response status code.
grsResponseStatus :: Lens' GetDeploymentsResponse Int
grsResponseStatus = lens _grsResponseStatus (\ s a -> s{_grsResponseStatus = a})

instance NFData GetDeploymentsResponse where
