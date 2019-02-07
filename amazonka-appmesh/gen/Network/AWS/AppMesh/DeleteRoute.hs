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
-- Module      : Network.AWS.AppMesh.DeleteRoute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing route.
--
--
module Network.AWS.AppMesh.DeleteRoute
    (
    -- * Creating a Request
      deleteRoute
    , DeleteRoute
    -- * Request Lenses
    , dMeshName
    , dRouteName
    , dVirtualRouterName

    -- * Destructuring the Response
    , deleteRouteResponse
    , DeleteRouteResponse
    -- * Response Lenses
    , delrsRoute
    , delrsResponseStatus
    ) where

import Network.AWS.AppMesh.Types
import Network.AWS.AppMesh.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | 
--
-- /See:/ 'deleteRoute' smart constructor.
data DeleteRoute = DeleteRoute'
  { _dMeshName :: !Text
  , _dRouteName :: !Text
  , _dVirtualRouterName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dMeshName' - The name of the service mesh in which to delete the route.
--
-- * 'dRouteName' - The name of the route to delete.
--
-- * 'dVirtualRouterName' - The name of the virtual router in which to delete the route.
deleteRoute
    :: Text -- ^ 'dMeshName'
    -> Text -- ^ 'dRouteName'
    -> Text -- ^ 'dVirtualRouterName'
    -> DeleteRoute
deleteRoute pMeshName_ pRouteName_ pVirtualRouterName_ =
  DeleteRoute'
    { _dMeshName = pMeshName_
    , _dRouteName = pRouteName_
    , _dVirtualRouterName = pVirtualRouterName_
    }


-- | The name of the service mesh in which to delete the route.
dMeshName :: Lens' DeleteRoute Text
dMeshName = lens _dMeshName (\ s a -> s{_dMeshName = a})

-- | The name of the route to delete.
dRouteName :: Lens' DeleteRoute Text
dRouteName = lens _dRouteName (\ s a -> s{_dRouteName = a})

-- | The name of the virtual router in which to delete the route.
dVirtualRouterName :: Lens' DeleteRoute Text
dVirtualRouterName = lens _dVirtualRouterName (\ s a -> s{_dVirtualRouterName = a})

instance AWSRequest DeleteRoute where
        type Rs DeleteRoute = DeleteRouteResponse
        request = delete appMesh
        response
          = receiveJSON
              (\ s h x ->
                 DeleteRouteResponse' <$>
                   (eitherParseJSON x) <*> (pure (fromEnum s)))

instance Hashable DeleteRoute where

instance NFData DeleteRoute where

instance ToHeaders DeleteRoute where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteRoute where
        toPath DeleteRoute'{..}
          = mconcat
              ["/meshes/", toBS _dMeshName, "/virtualRouter/",
               toBS _dVirtualRouterName, "/routes/",
               toBS _dRouteName]

instance ToQuery DeleteRoute where
        toQuery = const mempty

-- | 
--
-- /See:/ 'deleteRouteResponse' smart constructor.
data DeleteRouteResponse = DeleteRouteResponse'
  { _delrsRoute :: !(Maybe RouteData)
  , _delrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRouteResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsRoute' - The route that was deleted.
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteRouteResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeleteRouteResponse
deleteRouteResponse pResponseStatus_ =
  DeleteRouteResponse'
    {_delrsRoute = Nothing, _delrsResponseStatus = pResponseStatus_}


-- | The route that was deleted.
delrsRoute :: Lens' DeleteRouteResponse (Maybe RouteData)
delrsRoute = lens _delrsRoute (\ s a -> s{_delrsRoute = a})

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteRouteResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a})

instance NFData DeleteRouteResponse where
