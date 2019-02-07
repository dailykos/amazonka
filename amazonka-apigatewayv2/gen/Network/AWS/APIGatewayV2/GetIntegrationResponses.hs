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
-- Module      : Network.AWS.APIGatewayV2.GetIntegrationResponses
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the IntegrationResponses for an Integration.
--
--
--
-- This operation returns paginated results.
module Network.AWS.APIGatewayV2.GetIntegrationResponses
    (
    -- * Creating a Request
      getIntegrationResponses
    , GetIntegrationResponses
    -- * Request Lenses
    , girNextToken
    , girMaxResults
    , girIntegrationId
    , girAPIId

    -- * Destructuring the Response
    , getIntegrationResponsesResponse
    , GetIntegrationResponsesResponse
    -- * Response Lenses
    , girsrsItems
    , girsrsNextToken
    , girsrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getIntegrationResponses' smart constructor.
data GetIntegrationResponses = GetIntegrationResponses'
  { _girNextToken :: !(Maybe Text)
  , _girMaxResults :: !(Maybe Text)
  , _girIntegrationId :: !Text
  , _girAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIntegrationResponses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'girNextToken' - The next page of elements from this collection. Not valid for the last element of the collection.
--
-- * 'girMaxResults' - The maximum number of elements to be returned for this resource.
--
-- * 'girIntegrationId' - The integration ID.
--
-- * 'girAPIId' - The API identifier.
getIntegrationResponses
    :: Text -- ^ 'girIntegrationId'
    -> Text -- ^ 'girAPIId'
    -> GetIntegrationResponses
getIntegrationResponses pIntegrationId_ pAPIId_ =
  GetIntegrationResponses'
    { _girNextToken = Nothing
    , _girMaxResults = Nothing
    , _girIntegrationId = pIntegrationId_
    , _girAPIId = pAPIId_
    }


-- | The next page of elements from this collection. Not valid for the last element of the collection.
girNextToken :: Lens' GetIntegrationResponses (Maybe Text)
girNextToken = lens _girNextToken (\ s a -> s{_girNextToken = a})

-- | The maximum number of elements to be returned for this resource.
girMaxResults :: Lens' GetIntegrationResponses (Maybe Text)
girMaxResults = lens _girMaxResults (\ s a -> s{_girMaxResults = a})

-- | The integration ID.
girIntegrationId :: Lens' GetIntegrationResponses Text
girIntegrationId = lens _girIntegrationId (\ s a -> s{_girIntegrationId = a})

-- | The API identifier.
girAPIId :: Lens' GetIntegrationResponses Text
girAPIId = lens _girAPIId (\ s a -> s{_girAPIId = a})

instance AWSPager GetIntegrationResponses where
        page rq rs
          | stop (rs ^. girsrsNextToken) = Nothing
          | stop (rs ^. girsrsItems) = Nothing
          | otherwise =
            Just $ rq & girNextToken .~ rs ^. girsrsNextToken

instance AWSRequest GetIntegrationResponses where
        type Rs GetIntegrationResponses =
             GetIntegrationResponsesResponse
        request = get apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 GetIntegrationResponsesResponse' <$>
                   (x .?> "items" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetIntegrationResponses where

instance NFData GetIntegrationResponses where

instance ToHeaders GetIntegrationResponses where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetIntegrationResponses where
        toPath GetIntegrationResponses'{..}
          = mconcat
              ["/v2/apis/", toBS _girAPIId, "/integrations/",
               toBS _girIntegrationId, "/integrationresponses"]

instance ToQuery GetIntegrationResponses where
        toQuery GetIntegrationResponses'{..}
          = mconcat
              ["nextToken" =: _girNextToken,
               "maxResults" =: _girMaxResults]

-- | /See:/ 'getIntegrationResponsesResponse' smart constructor.
data GetIntegrationResponsesResponse = GetIntegrationResponsesResponse'
  { _girsrsItems :: !(Maybe [IntegrationResponse])
  , _girsrsNextToken :: !(Maybe Text)
  , _girsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIntegrationResponsesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'girsrsItems' - The elements from this collection.
--
-- * 'girsrsNextToken' - The next page of elements from this collection. Not valid for the last element of the collection.
--
-- * 'girsrsResponseStatus' - -- | The response status code.
getIntegrationResponsesResponse
    :: Int -- ^ 'girsrsResponseStatus'
    -> GetIntegrationResponsesResponse
getIntegrationResponsesResponse pResponseStatus_ =
  GetIntegrationResponsesResponse'
    { _girsrsItems = Nothing
    , _girsrsNextToken = Nothing
    , _girsrsResponseStatus = pResponseStatus_
    }


-- | The elements from this collection.
girsrsItems :: Lens' GetIntegrationResponsesResponse [IntegrationResponse]
girsrsItems = lens _girsrsItems (\ s a -> s{_girsrsItems = a}) . _Default . _Coerce

-- | The next page of elements from this collection. Not valid for the last element of the collection.
girsrsNextToken :: Lens' GetIntegrationResponsesResponse (Maybe Text)
girsrsNextToken = lens _girsrsNextToken (\ s a -> s{_girsrsNextToken = a})

-- | -- | The response status code.
girsrsResponseStatus :: Lens' GetIntegrationResponsesResponse Int
girsrsResponseStatus = lens _girsrsResponseStatus (\ s a -> s{_girsrsResponseStatus = a})

instance NFData GetIntegrationResponsesResponse where
