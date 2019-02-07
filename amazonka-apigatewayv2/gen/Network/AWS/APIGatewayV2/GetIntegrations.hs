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
-- Module      : Network.AWS.APIGatewayV2.GetIntegrations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the Integrations for an API.
--
--
--
-- This operation returns paginated results.
module Network.AWS.APIGatewayV2.GetIntegrations
    (
    -- * Creating a Request
      getIntegrations
    , GetIntegrations
    -- * Request Lenses
    , gisNextToken
    , gisMaxResults
    , gisAPIId

    -- * Destructuring the Response
    , getIntegrationsResponse
    , GetIntegrationsResponse
    -- * Response Lenses
    , girsItems
    , girsNextToken
    , girsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getIntegrations' smart constructor.
data GetIntegrations = GetIntegrations'
  { _gisNextToken :: !(Maybe Text)
  , _gisMaxResults :: !(Maybe Text)
  , _gisAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIntegrations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gisNextToken' - The next page of elements from this collection. Not valid for the last element of the collection.
--
-- * 'gisMaxResults' - The maximum number of elements to be returned for this resource.
--
-- * 'gisAPIId' - The API identifier.
getIntegrations
    :: Text -- ^ 'gisAPIId'
    -> GetIntegrations
getIntegrations pAPIId_ =
  GetIntegrations'
    {_gisNextToken = Nothing, _gisMaxResults = Nothing, _gisAPIId = pAPIId_}


-- | The next page of elements from this collection. Not valid for the last element of the collection.
gisNextToken :: Lens' GetIntegrations (Maybe Text)
gisNextToken = lens _gisNextToken (\ s a -> s{_gisNextToken = a})

-- | The maximum number of elements to be returned for this resource.
gisMaxResults :: Lens' GetIntegrations (Maybe Text)
gisMaxResults = lens _gisMaxResults (\ s a -> s{_gisMaxResults = a})

-- | The API identifier.
gisAPIId :: Lens' GetIntegrations Text
gisAPIId = lens _gisAPIId (\ s a -> s{_gisAPIId = a})

instance AWSPager GetIntegrations where
        page rq rs
          | stop (rs ^. girsNextToken) = Nothing
          | stop (rs ^. girsItems) = Nothing
          | otherwise =
            Just $ rq & gisNextToken .~ rs ^. girsNextToken

instance AWSRequest GetIntegrations where
        type Rs GetIntegrations = GetIntegrationsResponse
        request = get apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 GetIntegrationsResponse' <$>
                   (x .?> "items" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetIntegrations where

instance NFData GetIntegrations where

instance ToHeaders GetIntegrations where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetIntegrations where
        toPath GetIntegrations'{..}
          = mconcat
              ["/v2/apis/", toBS _gisAPIId, "/integrations"]

instance ToQuery GetIntegrations where
        toQuery GetIntegrations'{..}
          = mconcat
              ["nextToken" =: _gisNextToken,
               "maxResults" =: _gisMaxResults]

-- | /See:/ 'getIntegrationsResponse' smart constructor.
data GetIntegrationsResponse = GetIntegrationsResponse'
  { _girsItems :: !(Maybe [Integration])
  , _girsNextToken :: !(Maybe Text)
  , _girsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIntegrationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'girsItems' - The elements from this collection.
--
-- * 'girsNextToken' - The next page of elements from this collection. Not valid for the last element of the collection.
--
-- * 'girsResponseStatus' - -- | The response status code.
getIntegrationsResponse
    :: Int -- ^ 'girsResponseStatus'
    -> GetIntegrationsResponse
getIntegrationsResponse pResponseStatus_ =
  GetIntegrationsResponse'
    { _girsItems = Nothing
    , _girsNextToken = Nothing
    , _girsResponseStatus = pResponseStatus_
    }


-- | The elements from this collection.
girsItems :: Lens' GetIntegrationsResponse [Integration]
girsItems = lens _girsItems (\ s a -> s{_girsItems = a}) . _Default . _Coerce

-- | The next page of elements from this collection. Not valid for the last element of the collection.
girsNextToken :: Lens' GetIntegrationsResponse (Maybe Text)
girsNextToken = lens _girsNextToken (\ s a -> s{_girsNextToken = a})

-- | -- | The response status code.
girsResponseStatus :: Lens' GetIntegrationsResponse Int
girsResponseStatus = lens _girsResponseStatus (\ s a -> s{_girsResponseStatus = a})

instance NFData GetIntegrationsResponse where
