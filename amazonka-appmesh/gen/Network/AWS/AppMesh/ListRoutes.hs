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
-- Module      : Network.AWS.AppMesh.ListRoutes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of existing routes in a service mesh.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AppMesh.ListRoutes
    (
    -- * Creating a Request
      listRoutes
    , ListRoutes
    -- * Request Lenses
    , lrNextToken
    , lrLimit
    , lrMeshName
    , lrVirtualRouterName

    -- * Destructuring the Response
    , listRoutesResponse
    , ListRoutesResponse
    -- * Response Lenses
    , lrrsNextToken
    , lrrsResponseStatus
    , lrrsRoutes
    ) where

import Network.AWS.AppMesh.Types
import Network.AWS.AppMesh.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | 
--
-- /See:/ 'listRoutes' smart constructor.
data ListRoutes = ListRoutes'
  { _lrNextToken :: !(Maybe Text)
  , _lrLimit :: !(Maybe Nat)
  , _lrMeshName :: !Text
  , _lrVirtualRouterName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRoutes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrNextToken' - The @nextToken@ value returned from a previous paginated          @ListRoutes@ request where @limit@ was used and the          results exceeded the value of that parameter. Pagination continues from the end of the          previous results that returned the @nextToken@ value.
--
-- * 'lrLimit' - The maximum number of mesh results returned by @ListRoutes@ in paginated          output. When this parameter is used, @ListRoutes@ only returns             @limit@ results in a single page along with a @nextToken@ response          element. The remaining results of the initial request can be seen by sending another             @ListRoutes@ request with the returned @nextToken@ value. This          value can be between 1 and 100. If this parameter is not          used, then @ListRoutes@ returns up to 100 results and a             @nextToken@ value if applicable.
--
-- * 'lrMeshName' - The name of the service mesh in which to list routes.
--
-- * 'lrVirtualRouterName' - The name of the virtual router in which to list routes.
listRoutes
    :: Text -- ^ 'lrMeshName'
    -> Text -- ^ 'lrVirtualRouterName'
    -> ListRoutes
listRoutes pMeshName_ pVirtualRouterName_ =
  ListRoutes'
    { _lrNextToken = Nothing
    , _lrLimit = Nothing
    , _lrMeshName = pMeshName_
    , _lrVirtualRouterName = pVirtualRouterName_
    }


-- | The @nextToken@ value returned from a previous paginated          @ListRoutes@ request where @limit@ was used and the          results exceeded the value of that parameter. Pagination continues from the end of the          previous results that returned the @nextToken@ value.
lrNextToken :: Lens' ListRoutes (Maybe Text)
lrNextToken = lens _lrNextToken (\ s a -> s{_lrNextToken = a})

-- | The maximum number of mesh results returned by @ListRoutes@ in paginated          output. When this parameter is used, @ListRoutes@ only returns             @limit@ results in a single page along with a @nextToken@ response          element. The remaining results of the initial request can be seen by sending another             @ListRoutes@ request with the returned @nextToken@ value. This          value can be between 1 and 100. If this parameter is not          used, then @ListRoutes@ returns up to 100 results and a             @nextToken@ value if applicable.
lrLimit :: Lens' ListRoutes (Maybe Natural)
lrLimit = lens _lrLimit (\ s a -> s{_lrLimit = a}) . mapping _Nat

-- | The name of the service mesh in which to list routes.
lrMeshName :: Lens' ListRoutes Text
lrMeshName = lens _lrMeshName (\ s a -> s{_lrMeshName = a})

-- | The name of the virtual router in which to list routes.
lrVirtualRouterName :: Lens' ListRoutes Text
lrVirtualRouterName = lens _lrVirtualRouterName (\ s a -> s{_lrVirtualRouterName = a})

instance AWSPager ListRoutes where
        page rq rs
          | stop (rs ^. lrrsNextToken) = Nothing
          | stop (rs ^. lrrsRoutes) = Nothing
          | otherwise =
            Just $ rq & lrNextToken .~ rs ^. lrrsNextToken

instance AWSRequest ListRoutes where
        type Rs ListRoutes = ListRoutesResponse
        request = get appMesh
        response
          = receiveJSON
              (\ s h x ->
                 ListRoutesResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "routes" .!@ mempty))

instance Hashable ListRoutes where

instance NFData ListRoutes where

instance ToHeaders ListRoutes where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListRoutes where
        toPath ListRoutes'{..}
          = mconcat
              ["/meshes/", toBS _lrMeshName, "/virtualRouter/",
               toBS _lrVirtualRouterName, "/routes"]

instance ToQuery ListRoutes where
        toQuery ListRoutes'{..}
          = mconcat
              ["nextToken" =: _lrNextToken, "limit" =: _lrLimit]

-- | 
--
-- /See:/ 'listRoutesResponse' smart constructor.
data ListRoutesResponse = ListRoutesResponse'
  { _lrrsNextToken :: !(Maybe Text)
  , _lrrsResponseStatus :: !Int
  , _lrrsRoutes :: ![RouteRef]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRoutesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrrsNextToken' - The @nextToken@ value to include in a future @ListRoutes@           request. When the results of a @ListRoutes@ request exceed          @limit@ , this value can be used to retrieve the next page of          results. This value is @null@ when there are no more results to          return.
--
-- * 'lrrsResponseStatus' - -- | The response status code.
--
-- * 'lrrsRoutes' - The list of existing routes for the specified service mesh and virtual router.
listRoutesResponse
    :: Int -- ^ 'lrrsResponseStatus'
    -> ListRoutesResponse
listRoutesResponse pResponseStatus_ =
  ListRoutesResponse'
    { _lrrsNextToken = Nothing
    , _lrrsResponseStatus = pResponseStatus_
    , _lrrsRoutes = mempty
    }


-- | The @nextToken@ value to include in a future @ListRoutes@           request. When the results of a @ListRoutes@ request exceed          @limit@ , this value can be used to retrieve the next page of          results. This value is @null@ when there are no more results to          return.
lrrsNextToken :: Lens' ListRoutesResponse (Maybe Text)
lrrsNextToken = lens _lrrsNextToken (\ s a -> s{_lrrsNextToken = a})

-- | -- | The response status code.
lrrsResponseStatus :: Lens' ListRoutesResponse Int
lrrsResponseStatus = lens _lrrsResponseStatus (\ s a -> s{_lrrsResponseStatus = a})

-- | The list of existing routes for the specified service mesh and virtual router.
lrrsRoutes :: Lens' ListRoutesResponse [RouteRef]
lrrsRoutes = lens _lrrsRoutes (\ s a -> s{_lrrsRoutes = a}) . _Coerce

instance NFData ListRoutesResponse where
