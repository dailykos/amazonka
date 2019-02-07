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
-- Module      : Network.AWS.AppMesh.UpdateRoute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing route for a specified service mesh and virtual router.
--
--
module Network.AWS.AppMesh.UpdateRoute
    (
    -- * Creating a Request
      updateRoute
    , UpdateRoute
    -- * Request Lenses
    , urClientToken
    , urMeshName
    , urRouteName
    , urSpec
    , urVirtualRouterName

    -- * Destructuring the Response
    , updateRouteResponse
    , UpdateRouteResponse
    -- * Response Lenses
    , urrsRoute
    , urrsResponseStatus
    ) where

import Network.AWS.AppMesh.Types
import Network.AWS.AppMesh.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | 
--
-- /See:/ 'updateRoute' smart constructor.
data UpdateRoute = UpdateRoute'
  { _urClientToken :: !(Maybe Text)
  , _urMeshName :: !Text
  , _urRouteName :: !Text
  , _urSpec :: !RouteSpec
  , _urVirtualRouterName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 36 letters, numbers, hyphens, and underscores are allowed.
--
-- * 'urMeshName' - The name of the service mesh in which the route resides.
--
-- * 'urRouteName' - The name of the route to update.
--
-- * 'urSpec' - The new route specification to apply. This overwrites the existing data.
--
-- * 'urVirtualRouterName' - The name of the virtual router with which the route is associated.
updateRoute
    :: Text -- ^ 'urMeshName'
    -> Text -- ^ 'urRouteName'
    -> RouteSpec -- ^ 'urSpec'
    -> Text -- ^ 'urVirtualRouterName'
    -> UpdateRoute
updateRoute pMeshName_ pRouteName_ pSpec_ pVirtualRouterName_ =
  UpdateRoute'
    { _urClientToken = Nothing
    , _urMeshName = pMeshName_
    , _urRouteName = pRouteName_
    , _urSpec = pSpec_
    , _urVirtualRouterName = pVirtualRouterName_
    }


-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 36 letters, numbers, hyphens, and underscores are allowed.
urClientToken :: Lens' UpdateRoute (Maybe Text)
urClientToken = lens _urClientToken (\ s a -> s{_urClientToken = a})

-- | The name of the service mesh in which the route resides.
urMeshName :: Lens' UpdateRoute Text
urMeshName = lens _urMeshName (\ s a -> s{_urMeshName = a})

-- | The name of the route to update.
urRouteName :: Lens' UpdateRoute Text
urRouteName = lens _urRouteName (\ s a -> s{_urRouteName = a})

-- | The new route specification to apply. This overwrites the existing data.
urSpec :: Lens' UpdateRoute RouteSpec
urSpec = lens _urSpec (\ s a -> s{_urSpec = a})

-- | The name of the virtual router with which the route is associated.
urVirtualRouterName :: Lens' UpdateRoute Text
urVirtualRouterName = lens _urVirtualRouterName (\ s a -> s{_urVirtualRouterName = a})

instance AWSRequest UpdateRoute where
        type Rs UpdateRoute = UpdateRouteResponse
        request = putJSON appMesh
        response
          = receiveJSON
              (\ s h x ->
                 UpdateRouteResponse' <$>
                   (eitherParseJSON x) <*> (pure (fromEnum s)))

instance Hashable UpdateRoute where

instance NFData UpdateRoute where

instance ToHeaders UpdateRoute where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateRoute where
        toJSON UpdateRoute'{..}
          = object
              (catMaybes
                 [("clientToken" .=) <$> _urClientToken,
                  Just ("spec" .= _urSpec)])

instance ToPath UpdateRoute where
        toPath UpdateRoute'{..}
          = mconcat
              ["/meshes/", toBS _urMeshName, "/virtualRouter/",
               toBS _urVirtualRouterName, "/routes/",
               toBS _urRouteName]

instance ToQuery UpdateRoute where
        toQuery = const mempty

-- | 
--
-- /See:/ 'updateRouteResponse' smart constructor.
data UpdateRouteResponse = UpdateRouteResponse'
  { _urrsRoute :: !(Maybe RouteData)
  , _urrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRouteResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urrsRoute' - A full description of the route that was updated.
--
-- * 'urrsResponseStatus' - -- | The response status code.
updateRouteResponse
    :: Int -- ^ 'urrsResponseStatus'
    -> UpdateRouteResponse
updateRouteResponse pResponseStatus_ =
  UpdateRouteResponse'
    {_urrsRoute = Nothing, _urrsResponseStatus = pResponseStatus_}


-- | A full description of the route that was updated.
urrsRoute :: Lens' UpdateRouteResponse (Maybe RouteData)
urrsRoute = lens _urrsRoute (\ s a -> s{_urrsRoute = a})

-- | -- | The response status code.
urrsResponseStatus :: Lens' UpdateRouteResponse Int
urrsResponseStatus = lens _urrsResponseStatus (\ s a -> s{_urrsResponseStatus = a})

instance NFData UpdateRouteResponse where
