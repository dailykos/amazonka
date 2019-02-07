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
-- Module      : Network.AWS.AppMesh.ListMeshes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of existing service meshes.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AppMesh.ListMeshes
    (
    -- * Creating a Request
      listMeshes
    , ListMeshes
    -- * Request Lenses
    , lmNextToken
    , lmLimit

    -- * Destructuring the Response
    , listMeshesResponse
    , ListMeshesResponse
    -- * Response Lenses
    , lmrsNextToken
    , lmrsResponseStatus
    , lmrsMeshes
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
-- /See:/ 'listMeshes' smart constructor.
data ListMeshes = ListMeshes'
  { _lmNextToken :: !(Maybe Text)
  , _lmLimit :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListMeshes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmNextToken' - The @nextToken@ value returned from a previous paginated          @ListMeshes@ request where @limit@ was used and the          results exceeded the value of that parameter. Pagination continues from the end of the          previous results that returned the @nextToken@ value.
--
-- * 'lmLimit' - The maximum number of mesh results returned by @ListMeshes@ in paginated          output. When this parameter is used, @ListMeshes@ only returns             @limit@ results in a single page along with a @nextToken@ response          element. The remaining results of the initial request can be seen by sending another             @ListMeshes@ request with the returned @nextToken@ value. This          value can be between 1 and 100. If this parameter is not          used, then @ListMeshes@ returns up to 100 results and a             @nextToken@ value if applicable.
listMeshes
    :: ListMeshes
listMeshes = ListMeshes' {_lmNextToken = Nothing, _lmLimit = Nothing}


-- | The @nextToken@ value returned from a previous paginated          @ListMeshes@ request where @limit@ was used and the          results exceeded the value of that parameter. Pagination continues from the end of the          previous results that returned the @nextToken@ value.
lmNextToken :: Lens' ListMeshes (Maybe Text)
lmNextToken = lens _lmNextToken (\ s a -> s{_lmNextToken = a})

-- | The maximum number of mesh results returned by @ListMeshes@ in paginated          output. When this parameter is used, @ListMeshes@ only returns             @limit@ results in a single page along with a @nextToken@ response          element. The remaining results of the initial request can be seen by sending another             @ListMeshes@ request with the returned @nextToken@ value. This          value can be between 1 and 100. If this parameter is not          used, then @ListMeshes@ returns up to 100 results and a             @nextToken@ value if applicable.
lmLimit :: Lens' ListMeshes (Maybe Natural)
lmLimit = lens _lmLimit (\ s a -> s{_lmLimit = a}) . mapping _Nat

instance AWSPager ListMeshes where
        page rq rs
          | stop (rs ^. lmrsNextToken) = Nothing
          | stop (rs ^. lmrsMeshes) = Nothing
          | otherwise =
            Just $ rq & lmNextToken .~ rs ^. lmrsNextToken

instance AWSRequest ListMeshes where
        type Rs ListMeshes = ListMeshesResponse
        request = get appMesh
        response
          = receiveJSON
              (\ s h x ->
                 ListMeshesResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "meshes" .!@ mempty))

instance Hashable ListMeshes where

instance NFData ListMeshes where

instance ToHeaders ListMeshes where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListMeshes where
        toPath = const "/meshes"

instance ToQuery ListMeshes where
        toQuery ListMeshes'{..}
          = mconcat
              ["nextToken" =: _lmNextToken, "limit" =: _lmLimit]

-- | 
--
-- /See:/ 'listMeshesResponse' smart constructor.
data ListMeshesResponse = ListMeshesResponse'
  { _lmrsNextToken :: !(Maybe Text)
  , _lmrsResponseStatus :: !Int
  , _lmrsMeshes :: ![MeshRef]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListMeshesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmrsNextToken' - The @nextToken@ value to include in a future @ListMeshes@           request. When the results of a @ListMeshes@ request exceed          @limit@ , this value can be used to retrieve the next page of          results. This value is @null@ when there are no more results to          return.
--
-- * 'lmrsResponseStatus' - -- | The response status code.
--
-- * 'lmrsMeshes' - The list of existing service meshes.
listMeshesResponse
    :: Int -- ^ 'lmrsResponseStatus'
    -> ListMeshesResponse
listMeshesResponse pResponseStatus_ =
  ListMeshesResponse'
    { _lmrsNextToken = Nothing
    , _lmrsResponseStatus = pResponseStatus_
    , _lmrsMeshes = mempty
    }


-- | The @nextToken@ value to include in a future @ListMeshes@           request. When the results of a @ListMeshes@ request exceed          @limit@ , this value can be used to retrieve the next page of          results. This value is @null@ when there are no more results to          return.
lmrsNextToken :: Lens' ListMeshesResponse (Maybe Text)
lmrsNextToken = lens _lmrsNextToken (\ s a -> s{_lmrsNextToken = a})

-- | -- | The response status code.
lmrsResponseStatus :: Lens' ListMeshesResponse Int
lmrsResponseStatus = lens _lmrsResponseStatus (\ s a -> s{_lmrsResponseStatus = a})

-- | The list of existing service meshes.
lmrsMeshes :: Lens' ListMeshesResponse [MeshRef]
lmrsMeshes = lens _lmrsMeshes (\ s a -> s{_lmrsMeshes = a}) . _Coerce

instance NFData ListMeshesResponse where
