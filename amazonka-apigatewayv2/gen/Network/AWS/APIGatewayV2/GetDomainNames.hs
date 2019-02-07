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
-- Module      : Network.AWS.APIGatewayV2.GetDomainNames
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the domain names for an AWS account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.APIGatewayV2.GetDomainNames
    (
    -- * Creating a Request
      getDomainNames
    , GetDomainNames
    -- * Request Lenses
    , gdnNextToken
    , gdnMaxResults

    -- * Destructuring the Response
    , getDomainNamesResponse
    , GetDomainNamesResponse
    -- * Response Lenses
    , gdnsrsItems
    , gdnsrsNextToken
    , gdnsrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDomainNames' smart constructor.
data GetDomainNames = GetDomainNames'
  { _gdnNextToken :: !(Maybe Text)
  , _gdnMaxResults :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDomainNames' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdnNextToken' - The next page of elements from this collection. Not valid for the last element of the collection.
--
-- * 'gdnMaxResults' - The maximum number of elements to be returned for this resource.
getDomainNames
    :: GetDomainNames
getDomainNames =
  GetDomainNames' {_gdnNextToken = Nothing, _gdnMaxResults = Nothing}


-- | The next page of elements from this collection. Not valid for the last element of the collection.
gdnNextToken :: Lens' GetDomainNames (Maybe Text)
gdnNextToken = lens _gdnNextToken (\ s a -> s{_gdnNextToken = a})

-- | The maximum number of elements to be returned for this resource.
gdnMaxResults :: Lens' GetDomainNames (Maybe Text)
gdnMaxResults = lens _gdnMaxResults (\ s a -> s{_gdnMaxResults = a})

instance AWSPager GetDomainNames where
        page rq rs
          | stop (rs ^. gdnsrsNextToken) = Nothing
          | stop (rs ^. gdnsrsItems) = Nothing
          | otherwise =
            Just $ rq & gdnNextToken .~ rs ^. gdnsrsNextToken

instance AWSRequest GetDomainNames where
        type Rs GetDomainNames = GetDomainNamesResponse
        request = get apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 GetDomainNamesResponse' <$>
                   (x .?> "items" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetDomainNames where

instance NFData GetDomainNames where

instance ToHeaders GetDomainNames where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetDomainNames where
        toPath = const "/v2/domainnames"

instance ToQuery GetDomainNames where
        toQuery GetDomainNames'{..}
          = mconcat
              ["nextToken" =: _gdnNextToken,
               "maxResults" =: _gdnMaxResults]

-- | /See:/ 'getDomainNamesResponse' smart constructor.
data GetDomainNamesResponse = GetDomainNamesResponse'
  { _gdnsrsItems :: !(Maybe [DomainName])
  , _gdnsrsNextToken :: !(Maybe Text)
  , _gdnsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDomainNamesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdnsrsItems' - The elements from this collection.
--
-- * 'gdnsrsNextToken' - The next page of elements from this collection. Not valid for the last element of the collection.
--
-- * 'gdnsrsResponseStatus' - -- | The response status code.
getDomainNamesResponse
    :: Int -- ^ 'gdnsrsResponseStatus'
    -> GetDomainNamesResponse
getDomainNamesResponse pResponseStatus_ =
  GetDomainNamesResponse'
    { _gdnsrsItems = Nothing
    , _gdnsrsNextToken = Nothing
    , _gdnsrsResponseStatus = pResponseStatus_
    }


-- | The elements from this collection.
gdnsrsItems :: Lens' GetDomainNamesResponse [DomainName]
gdnsrsItems = lens _gdnsrsItems (\ s a -> s{_gdnsrsItems = a}) . _Default . _Coerce

-- | The next page of elements from this collection. Not valid for the last element of the collection.
gdnsrsNextToken :: Lens' GetDomainNamesResponse (Maybe Text)
gdnsrsNextToken = lens _gdnsrsNextToken (\ s a -> s{_gdnsrsNextToken = a})

-- | -- | The response status code.
gdnsrsResponseStatus :: Lens' GetDomainNamesResponse Int
gdnsrsResponseStatus = lens _gdnsrsResponseStatus (\ s a -> s{_gdnsrsResponseStatus = a})

instance NFData GetDomainNamesResponse where
