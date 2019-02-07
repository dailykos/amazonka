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
-- Module      : Network.AWS.AppMesh.ListVirtualRouters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of existing virtual routers in a service mesh.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AppMesh.ListVirtualRouters
    (
    -- * Creating a Request
      listVirtualRouters
    , ListVirtualRouters
    -- * Request Lenses
    , lvrNextToken
    , lvrLimit
    , lvrMeshName

    -- * Destructuring the Response
    , listVirtualRoutersResponse
    , ListVirtualRoutersResponse
    -- * Response Lenses
    , lvrrsNextToken
    , lvrrsResponseStatus
    , lvrrsVirtualRouters
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
-- /See:/ 'listVirtualRouters' smart constructor.
data ListVirtualRouters = ListVirtualRouters'
  { _lvrNextToken :: !(Maybe Text)
  , _lvrLimit :: !(Maybe Nat)
  , _lvrMeshName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListVirtualRouters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvrNextToken' - The @nextToken@ value returned from a previous paginated          @ListVirtualRouters@ request where @limit@ was used and the          results exceeded the value of that parameter. Pagination continues from the end of the          previous results that returned the @nextToken@ value.
--
-- * 'lvrLimit' - The maximum number of mesh results returned by @ListVirtualRouters@ in          paginated output. When this parameter is used, @ListVirtualRouters@ only returns          @limit@ results in a single page along with a @nextToken@           response element. The remaining results of the initial request can be seen by sending          another @ListVirtualRouters@ request with the returned @nextToken@           value. This value can be between 1 and 100. If this          parameter is not used, then @ListVirtualRouters@ returns up to          100 results and a @nextToken@ value if applicable.
--
-- * 'lvrMeshName' - The name of the service mesh in which to list virtual routers.
listVirtualRouters
    :: Text -- ^ 'lvrMeshName'
    -> ListVirtualRouters
listVirtualRouters pMeshName_ =
  ListVirtualRouters'
    {_lvrNextToken = Nothing, _lvrLimit = Nothing, _lvrMeshName = pMeshName_}


-- | The @nextToken@ value returned from a previous paginated          @ListVirtualRouters@ request where @limit@ was used and the          results exceeded the value of that parameter. Pagination continues from the end of the          previous results that returned the @nextToken@ value.
lvrNextToken :: Lens' ListVirtualRouters (Maybe Text)
lvrNextToken = lens _lvrNextToken (\ s a -> s{_lvrNextToken = a})

-- | The maximum number of mesh results returned by @ListVirtualRouters@ in          paginated output. When this parameter is used, @ListVirtualRouters@ only returns          @limit@ results in a single page along with a @nextToken@           response element. The remaining results of the initial request can be seen by sending          another @ListVirtualRouters@ request with the returned @nextToken@           value. This value can be between 1 and 100. If this          parameter is not used, then @ListVirtualRouters@ returns up to          100 results and a @nextToken@ value if applicable.
lvrLimit :: Lens' ListVirtualRouters (Maybe Natural)
lvrLimit = lens _lvrLimit (\ s a -> s{_lvrLimit = a}) . mapping _Nat

-- | The name of the service mesh in which to list virtual routers.
lvrMeshName :: Lens' ListVirtualRouters Text
lvrMeshName = lens _lvrMeshName (\ s a -> s{_lvrMeshName = a})

instance AWSPager ListVirtualRouters where
        page rq rs
          | stop (rs ^. lvrrsNextToken) = Nothing
          | stop (rs ^. lvrrsVirtualRouters) = Nothing
          | otherwise =
            Just $ rq & lvrNextToken .~ rs ^. lvrrsNextToken

instance AWSRequest ListVirtualRouters where
        type Rs ListVirtualRouters =
             ListVirtualRoutersResponse
        request = get appMesh
        response
          = receiveJSON
              (\ s h x ->
                 ListVirtualRoutersResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "virtualRouters" .!@ mempty))

instance Hashable ListVirtualRouters where

instance NFData ListVirtualRouters where

instance ToHeaders ListVirtualRouters where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListVirtualRouters where
        toPath ListVirtualRouters'{..}
          = mconcat
              ["/meshes/", toBS _lvrMeshName, "/virtualRouters"]

instance ToQuery ListVirtualRouters where
        toQuery ListVirtualRouters'{..}
          = mconcat
              ["nextToken" =: _lvrNextToken, "limit" =: _lvrLimit]

-- | 
--
-- /See:/ 'listVirtualRoutersResponse' smart constructor.
data ListVirtualRoutersResponse = ListVirtualRoutersResponse'
  { _lvrrsNextToken :: !(Maybe Text)
  , _lvrrsResponseStatus :: !Int
  , _lvrrsVirtualRouters :: ![VirtualRouterRef]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListVirtualRoutersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvrrsNextToken' - The @nextToken@ value to include in a future @ListVirtualRouters@           request. When the results of a @ListVirtualRouters@ request exceed          @limit@ , this value can be used to retrieve the next page of          results. This value is @null@ when there are no more results to          return.
--
-- * 'lvrrsResponseStatus' - -- | The response status code.
--
-- * 'lvrrsVirtualRouters' - The list of existing virtual routers for the specified service mesh.
listVirtualRoutersResponse
    :: Int -- ^ 'lvrrsResponseStatus'
    -> ListVirtualRoutersResponse
listVirtualRoutersResponse pResponseStatus_ =
  ListVirtualRoutersResponse'
    { _lvrrsNextToken = Nothing
    , _lvrrsResponseStatus = pResponseStatus_
    , _lvrrsVirtualRouters = mempty
    }


-- | The @nextToken@ value to include in a future @ListVirtualRouters@           request. When the results of a @ListVirtualRouters@ request exceed          @limit@ , this value can be used to retrieve the next page of          results. This value is @null@ when there are no more results to          return.
lvrrsNextToken :: Lens' ListVirtualRoutersResponse (Maybe Text)
lvrrsNextToken = lens _lvrrsNextToken (\ s a -> s{_lvrrsNextToken = a})

-- | -- | The response status code.
lvrrsResponseStatus :: Lens' ListVirtualRoutersResponse Int
lvrrsResponseStatus = lens _lvrrsResponseStatus (\ s a -> s{_lvrrsResponseStatus = a})

-- | The list of existing virtual routers for the specified service mesh.
lvrrsVirtualRouters :: Lens' ListVirtualRoutersResponse [VirtualRouterRef]
lvrrsVirtualRouters = lens _lvrrsVirtualRouters (\ s a -> s{_lvrrsVirtualRouters = a}) . _Coerce

instance NFData ListVirtualRoutersResponse where
