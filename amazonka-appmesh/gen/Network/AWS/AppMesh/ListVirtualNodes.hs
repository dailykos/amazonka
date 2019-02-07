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
-- Module      : Network.AWS.AppMesh.ListVirtualNodes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of existing virtual nodes.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AppMesh.ListVirtualNodes
    (
    -- * Creating a Request
      listVirtualNodes
    , ListVirtualNodes
    -- * Request Lenses
    , lvnNextToken
    , lvnLimit
    , lvnMeshName

    -- * Destructuring the Response
    , listVirtualNodesResponse
    , ListVirtualNodesResponse
    -- * Response Lenses
    , lvnrsNextToken
    , lvnrsResponseStatus
    , lvnrsVirtualNodes
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
-- /See:/ 'listVirtualNodes' smart constructor.
data ListVirtualNodes = ListVirtualNodes'
  { _lvnNextToken :: !(Maybe Text)
  , _lvnLimit :: !(Maybe Nat)
  , _lvnMeshName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListVirtualNodes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvnNextToken' - The @nextToken@ value returned from a previous paginated          @ListVirtualNodes@ request where @limit@ was used and the          results exceeded the value of that parameter. Pagination continues from the end of the          previous results that returned the @nextToken@ value.
--
-- * 'lvnLimit' - The maximum number of mesh results returned by @ListVirtualNodes@ in          paginated output. When this parameter is used, @ListVirtualNodes@ only returns          @limit@ results in a single page along with a @nextToken@           response element. The remaining results of the initial request can be seen by sending          another @ListVirtualNodes@ request with the returned @nextToken@           value. This value can be between 1 and 100. If this          parameter is not used, then @ListVirtualNodes@ returns up to          100 results and a @nextToken@ value if applicable.
--
-- * 'lvnMeshName' - The name of the service mesh in which to list virtual nodes.
listVirtualNodes
    :: Text -- ^ 'lvnMeshName'
    -> ListVirtualNodes
listVirtualNodes pMeshName_ =
  ListVirtualNodes'
    {_lvnNextToken = Nothing, _lvnLimit = Nothing, _lvnMeshName = pMeshName_}


-- | The @nextToken@ value returned from a previous paginated          @ListVirtualNodes@ request where @limit@ was used and the          results exceeded the value of that parameter. Pagination continues from the end of the          previous results that returned the @nextToken@ value.
lvnNextToken :: Lens' ListVirtualNodes (Maybe Text)
lvnNextToken = lens _lvnNextToken (\ s a -> s{_lvnNextToken = a})

-- | The maximum number of mesh results returned by @ListVirtualNodes@ in          paginated output. When this parameter is used, @ListVirtualNodes@ only returns          @limit@ results in a single page along with a @nextToken@           response element. The remaining results of the initial request can be seen by sending          another @ListVirtualNodes@ request with the returned @nextToken@           value. This value can be between 1 and 100. If this          parameter is not used, then @ListVirtualNodes@ returns up to          100 results and a @nextToken@ value if applicable.
lvnLimit :: Lens' ListVirtualNodes (Maybe Natural)
lvnLimit = lens _lvnLimit (\ s a -> s{_lvnLimit = a}) . mapping _Nat

-- | The name of the service mesh in which to list virtual nodes.
lvnMeshName :: Lens' ListVirtualNodes Text
lvnMeshName = lens _lvnMeshName (\ s a -> s{_lvnMeshName = a})

instance AWSPager ListVirtualNodes where
        page rq rs
          | stop (rs ^. lvnrsNextToken) = Nothing
          | stop (rs ^. lvnrsVirtualNodes) = Nothing
          | otherwise =
            Just $ rq & lvnNextToken .~ rs ^. lvnrsNextToken

instance AWSRequest ListVirtualNodes where
        type Rs ListVirtualNodes = ListVirtualNodesResponse
        request = get appMesh
        response
          = receiveJSON
              (\ s h x ->
                 ListVirtualNodesResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "virtualNodes" .!@ mempty))

instance Hashable ListVirtualNodes where

instance NFData ListVirtualNodes where

instance ToHeaders ListVirtualNodes where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListVirtualNodes where
        toPath ListVirtualNodes'{..}
          = mconcat
              ["/meshes/", toBS _lvnMeshName, "/virtualNodes"]

instance ToQuery ListVirtualNodes where
        toQuery ListVirtualNodes'{..}
          = mconcat
              ["nextToken" =: _lvnNextToken, "limit" =: _lvnLimit]

-- | 
--
-- /See:/ 'listVirtualNodesResponse' smart constructor.
data ListVirtualNodesResponse = ListVirtualNodesResponse'
  { _lvnrsNextToken :: !(Maybe Text)
  , _lvnrsResponseStatus :: !Int
  , _lvnrsVirtualNodes :: ![VirtualNodeRef]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListVirtualNodesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvnrsNextToken' - The @nextToken@ value to include in a future @ListVirtualNodes@           request. When the results of a @ListVirtualNodes@ request exceed          @limit@ , this value can be used to retrieve the next page of          results. This value is @null@ when there are no more results to          return.
--
-- * 'lvnrsResponseStatus' - -- | The response status code.
--
-- * 'lvnrsVirtualNodes' - The list of existing virtual nodes for the specified service mesh.
listVirtualNodesResponse
    :: Int -- ^ 'lvnrsResponseStatus'
    -> ListVirtualNodesResponse
listVirtualNodesResponse pResponseStatus_ =
  ListVirtualNodesResponse'
    { _lvnrsNextToken = Nothing
    , _lvnrsResponseStatus = pResponseStatus_
    , _lvnrsVirtualNodes = mempty
    }


-- | The @nextToken@ value to include in a future @ListVirtualNodes@           request. When the results of a @ListVirtualNodes@ request exceed          @limit@ , this value can be used to retrieve the next page of          results. This value is @null@ when there are no more results to          return.
lvnrsNextToken :: Lens' ListVirtualNodesResponse (Maybe Text)
lvnrsNextToken = lens _lvnrsNextToken (\ s a -> s{_lvnrsNextToken = a})

-- | -- | The response status code.
lvnrsResponseStatus :: Lens' ListVirtualNodesResponse Int
lvnrsResponseStatus = lens _lvnrsResponseStatus (\ s a -> s{_lvnrsResponseStatus = a})

-- | The list of existing virtual nodes for the specified service mesh.
lvnrsVirtualNodes :: Lens' ListVirtualNodesResponse [VirtualNodeRef]
lvnrsVirtualNodes = lens _lvnrsVirtualNodes (\ s a -> s{_lvnrsVirtualNodes = a}) . _Coerce

instance NFData ListVirtualNodesResponse where
