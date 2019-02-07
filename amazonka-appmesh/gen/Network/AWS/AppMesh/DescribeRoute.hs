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
-- Module      : Network.AWS.AppMesh.DescribeRoute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing route.
--
--
module Network.AWS.AppMesh.DescribeRoute
    (
    -- * Creating a Request
      describeRoute
    , DescribeRoute
    -- * Request Lenses
    , drMeshName
    , drRouteName
    , drVirtualRouterName

    -- * Destructuring the Response
    , describeRouteResponse
    , DescribeRouteResponse
    -- * Response Lenses
    , drrsRoute
    , drrsResponseStatus
    ) where

import Network.AWS.AppMesh.Types
import Network.AWS.AppMesh.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | 
--
-- /See:/ 'describeRoute' smart constructor.
data DescribeRoute = DescribeRoute'
  { _drMeshName :: !Text
  , _drRouteName :: !Text
  , _drVirtualRouterName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drMeshName' - The name of the service mesh in which the route resides.
--
-- * 'drRouteName' - The name of the route to describe.
--
-- * 'drVirtualRouterName' - The name of the virtual router with which the route is associated.
describeRoute
    :: Text -- ^ 'drMeshName'
    -> Text -- ^ 'drRouteName'
    -> Text -- ^ 'drVirtualRouterName'
    -> DescribeRoute
describeRoute pMeshName_ pRouteName_ pVirtualRouterName_ =
  DescribeRoute'
    { _drMeshName = pMeshName_
    , _drRouteName = pRouteName_
    , _drVirtualRouterName = pVirtualRouterName_
    }


-- | The name of the service mesh in which the route resides.
drMeshName :: Lens' DescribeRoute Text
drMeshName = lens _drMeshName (\ s a -> s{_drMeshName = a})

-- | The name of the route to describe.
drRouteName :: Lens' DescribeRoute Text
drRouteName = lens _drRouteName (\ s a -> s{_drRouteName = a})

-- | The name of the virtual router with which the route is associated.
drVirtualRouterName :: Lens' DescribeRoute Text
drVirtualRouterName = lens _drVirtualRouterName (\ s a -> s{_drVirtualRouterName = a})

instance AWSRequest DescribeRoute where
        type Rs DescribeRoute = DescribeRouteResponse
        request = get appMesh
        response
          = receiveJSON
              (\ s h x ->
                 DescribeRouteResponse' <$>
                   (eitherParseJSON x) <*> (pure (fromEnum s)))

instance Hashable DescribeRoute where

instance NFData DescribeRoute where

instance ToHeaders DescribeRoute where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeRoute where
        toPath DescribeRoute'{..}
          = mconcat
              ["/meshes/", toBS _drMeshName, "/virtualRouter/",
               toBS _drVirtualRouterName, "/routes/",
               toBS _drRouteName]

instance ToQuery DescribeRoute where
        toQuery = const mempty

-- | 
--
-- /See:/ 'describeRouteResponse' smart constructor.
data DescribeRouteResponse = DescribeRouteResponse'
  { _drrsRoute :: !(Maybe RouteData)
  , _drrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRouteResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrsRoute' - The full description of your route.
--
-- * 'drrsResponseStatus' - -- | The response status code.
describeRouteResponse
    :: Int -- ^ 'drrsResponseStatus'
    -> DescribeRouteResponse
describeRouteResponse pResponseStatus_ =
  DescribeRouteResponse'
    {_drrsRoute = Nothing, _drrsResponseStatus = pResponseStatus_}


-- | The full description of your route.
drrsRoute :: Lens' DescribeRouteResponse (Maybe RouteData)
drrsRoute = lens _drrsRoute (\ s a -> s{_drrsRoute = a})

-- | -- | The response status code.
drrsResponseStatus :: Lens' DescribeRouteResponse Int
drrsResponseStatus = lens _drrsResponseStatus (\ s a -> s{_drrsResponseStatus = a})

instance NFData DescribeRouteResponse where
