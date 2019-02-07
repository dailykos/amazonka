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
-- Module      : Network.AWS.AppMesh.CreateRoute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new route that is associated with a virtual router.
--
--
-- You can use the @prefix@ parameter in your route specification for path-based
--          routing of requests. For example, if your virtual router service name is
--             @my-service.local@ , and you want the route to match requests to
--             @my-service.local/metrics@ , then your prefix should be
--          @/metrics@ .
--
-- If your route matches a request, you can distribute traffic to one or more target
--          virtual nodes with relative weighting.
--
module Network.AWS.AppMesh.CreateRoute
    (
    -- * Creating a Request
      createRoute
    , CreateRoute
    -- * Request Lenses
    , crClientToken
    , crMeshName
    , crRouteName
    , crSpec
    , crVirtualRouterName

    -- * Destructuring the Response
    , createRouteResponse
    , CreateRouteResponse
    -- * Response Lenses
    , crrsRoute
    , crrsResponseStatus
    ) where

import Network.AWS.AppMesh.Types
import Network.AWS.AppMesh.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | 
--
-- /See:/ 'createRoute' smart constructor.
data CreateRoute = CreateRoute'
  { _crClientToken :: !(Maybe Text)
  , _crMeshName :: !Text
  , _crRouteName :: !Text
  , _crSpec :: !RouteSpec
  , _crVirtualRouterName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 36 letters, numbers, hyphens, and underscores are allowed.
--
-- * 'crMeshName' - The name of the service mesh in which to create the route.
--
-- * 'crRouteName' - The name to use for the route.
--
-- * 'crSpec' - The route specification to apply.
--
-- * 'crVirtualRouterName' - The name of the virtual router in which to create the route.
createRoute
    :: Text -- ^ 'crMeshName'
    -> Text -- ^ 'crRouteName'
    -> RouteSpec -- ^ 'crSpec'
    -> Text -- ^ 'crVirtualRouterName'
    -> CreateRoute
createRoute pMeshName_ pRouteName_ pSpec_ pVirtualRouterName_ =
  CreateRoute'
    { _crClientToken = Nothing
    , _crMeshName = pMeshName_
    , _crRouteName = pRouteName_
    , _crSpec = pSpec_
    , _crVirtualRouterName = pVirtualRouterName_
    }


-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 36 letters, numbers, hyphens, and underscores are allowed.
crClientToken :: Lens' CreateRoute (Maybe Text)
crClientToken = lens _crClientToken (\ s a -> s{_crClientToken = a})

-- | The name of the service mesh in which to create the route.
crMeshName :: Lens' CreateRoute Text
crMeshName = lens _crMeshName (\ s a -> s{_crMeshName = a})

-- | The name to use for the route.
crRouteName :: Lens' CreateRoute Text
crRouteName = lens _crRouteName (\ s a -> s{_crRouteName = a})

-- | The route specification to apply.
crSpec :: Lens' CreateRoute RouteSpec
crSpec = lens _crSpec (\ s a -> s{_crSpec = a})

-- | The name of the virtual router in which to create the route.
crVirtualRouterName :: Lens' CreateRoute Text
crVirtualRouterName = lens _crVirtualRouterName (\ s a -> s{_crVirtualRouterName = a})

instance AWSRequest CreateRoute where
        type Rs CreateRoute = CreateRouteResponse
        request = putJSON appMesh
        response
          = receiveJSON
              (\ s h x ->
                 CreateRouteResponse' <$>
                   (eitherParseJSON x) <*> (pure (fromEnum s)))

instance Hashable CreateRoute where

instance NFData CreateRoute where

instance ToHeaders CreateRoute where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateRoute where
        toJSON CreateRoute'{..}
          = object
              (catMaybes
                 [("clientToken" .=) <$> _crClientToken,
                  Just ("routeName" .= _crRouteName),
                  Just ("spec" .= _crSpec)])

instance ToPath CreateRoute where
        toPath CreateRoute'{..}
          = mconcat
              ["/meshes/", toBS _crMeshName, "/virtualRouter/",
               toBS _crVirtualRouterName, "/routes"]

instance ToQuery CreateRoute where
        toQuery = const mempty

-- | 
--
-- /See:/ 'createRouteResponse' smart constructor.
data CreateRouteResponse = CreateRouteResponse'
  { _crrsRoute :: !(Maybe RouteData)
  , _crrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRouteResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crrsRoute' - The full description of your mesh following the create call.
--
-- * 'crrsResponseStatus' - -- | The response status code.
createRouteResponse
    :: Int -- ^ 'crrsResponseStatus'
    -> CreateRouteResponse
createRouteResponse pResponseStatus_ =
  CreateRouteResponse'
    {_crrsRoute = Nothing, _crrsResponseStatus = pResponseStatus_}


-- | The full description of your mesh following the create call.
crrsRoute :: Lens' CreateRouteResponse (Maybe RouteData)
crrsRoute = lens _crrsRoute (\ s a -> s{_crrsRoute = a})

-- | -- | The response status code.
crrsResponseStatus :: Lens' CreateRouteResponse Int
crrsResponseStatus = lens _crrsResponseStatus (\ s a -> s{_crrsResponseStatus = a})

instance NFData CreateRouteResponse where
