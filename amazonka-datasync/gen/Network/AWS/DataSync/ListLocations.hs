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
-- Module      : Network.AWS.DataSync.ListLocations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a lists of source and destination locations.
--
--
-- If you have more locations than are returned in a response (that is, the response returns only a truncated list of your agents), the response contains a token that you can specify in your next request to fetch the next page of locations.
--
--
-- This operation returns paginated results.
module Network.AWS.DataSync.ListLocations
    (
    -- * Creating a Request
      listLocations
    , ListLocations
    -- * Request Lenses
    , llNextToken
    , llMaxResults

    -- * Destructuring the Response
    , listLocationsResponse
    , ListLocationsResponse
    -- * Response Lenses
    , llrsNextToken
    , llrsLocations
    , llrsResponseStatus
    ) where

import Network.AWS.DataSync.Types
import Network.AWS.DataSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | ListLocationsRequest
--
--
--
-- /See:/ 'listLocations' smart constructor.
data ListLocations = ListLocations'
  { _llNextToken :: !(Maybe Text)
  , _llMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListLocations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llNextToken' - An opaque string that indicates the position at which to begin the next list of locations.
--
-- * 'llMaxResults' - The maximum number of locations to return.
listLocations
    :: ListLocations
listLocations = ListLocations' {_llNextToken = Nothing, _llMaxResults = Nothing}


-- | An opaque string that indicates the position at which to begin the next list of locations.
llNextToken :: Lens' ListLocations (Maybe Text)
llNextToken = lens _llNextToken (\ s a -> s{_llNextToken = a})

-- | The maximum number of locations to return.
llMaxResults :: Lens' ListLocations (Maybe Natural)
llMaxResults = lens _llMaxResults (\ s a -> s{_llMaxResults = a}) . mapping _Nat

instance AWSPager ListLocations where
        page rq rs
          | stop (rs ^. llrsNextToken) = Nothing
          | stop (rs ^. llrsLocations) = Nothing
          | otherwise =
            Just $ rq & llNextToken .~ rs ^. llrsNextToken

instance AWSRequest ListLocations where
        type Rs ListLocations = ListLocationsResponse
        request = postJSON dataSync
        response
          = receiveJSON
              (\ s h x ->
                 ListLocationsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Locations" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListLocations where

instance NFData ListLocations where

instance ToHeaders ListLocations where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("FmrsService.ListLocations" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListLocations where
        toJSON ListLocations'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _llNextToken,
                  ("MaxResults" .=) <$> _llMaxResults])

instance ToPath ListLocations where
        toPath = const "/"

instance ToQuery ListLocations where
        toQuery = const mempty

-- | ListLocationsResponse
--
--
--
-- /See:/ 'listLocationsResponse' smart constructor.
data ListLocationsResponse = ListLocationsResponse'
  { _llrsNextToken :: !(Maybe Text)
  , _llrsLocations :: !(Maybe [LocationListEntry])
  , _llrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListLocationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llrsNextToken' - An opaque string that indicates the position at which to begin returning the next list of locations.
--
-- * 'llrsLocations' - An array that contains a list of locations.
--
-- * 'llrsResponseStatus' - -- | The response status code.
listLocationsResponse
    :: Int -- ^ 'llrsResponseStatus'
    -> ListLocationsResponse
listLocationsResponse pResponseStatus_ =
  ListLocationsResponse'
    { _llrsNextToken = Nothing
    , _llrsLocations = Nothing
    , _llrsResponseStatus = pResponseStatus_
    }


-- | An opaque string that indicates the position at which to begin returning the next list of locations.
llrsNextToken :: Lens' ListLocationsResponse (Maybe Text)
llrsNextToken = lens _llrsNextToken (\ s a -> s{_llrsNextToken = a})

-- | An array that contains a list of locations.
llrsLocations :: Lens' ListLocationsResponse [LocationListEntry]
llrsLocations = lens _llrsLocations (\ s a -> s{_llrsLocations = a}) . _Default . _Coerce

-- | -- | The response status code.
llrsResponseStatus :: Lens' ListLocationsResponse Int
llrsResponseStatus = lens _llrsResponseStatus (\ s a -> s{_llrsResponseStatus = a})

instance NFData ListLocationsResponse where
