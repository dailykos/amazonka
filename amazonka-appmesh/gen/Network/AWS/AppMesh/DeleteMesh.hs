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
-- Module      : Network.AWS.AppMesh.DeleteMesh
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing service mesh.
--
--
-- You must delete all resources (routes, virtual routers, virtual nodes) in the service
--          mesh before you can delete the mesh itself.
--
module Network.AWS.AppMesh.DeleteMesh
    (
    -- * Creating a Request
      deleteMesh
    , DeleteMesh
    -- * Request Lenses
    , dmMeshName

    -- * Destructuring the Response
    , deleteMeshResponse
    , DeleteMeshResponse
    -- * Response Lenses
    , dmmrsMesh
    , dmmrsResponseStatus
    ) where

import Network.AWS.AppMesh.Types
import Network.AWS.AppMesh.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | 
--
-- /See:/ 'deleteMesh' smart constructor.
newtype DeleteMesh = DeleteMesh'
  { _dmMeshName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMesh' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmMeshName' - The name of the service mesh to delete.
deleteMesh
    :: Text -- ^ 'dmMeshName'
    -> DeleteMesh
deleteMesh pMeshName_ = DeleteMesh' {_dmMeshName = pMeshName_}


-- | The name of the service mesh to delete.
dmMeshName :: Lens' DeleteMesh Text
dmMeshName = lens _dmMeshName (\ s a -> s{_dmMeshName = a})

instance AWSRequest DeleteMesh where
        type Rs DeleteMesh = DeleteMeshResponse
        request = delete appMesh
        response
          = receiveJSON
              (\ s h x ->
                 DeleteMeshResponse' <$>
                   (eitherParseJSON x) <*> (pure (fromEnum s)))

instance Hashable DeleteMesh where

instance NFData DeleteMesh where

instance ToHeaders DeleteMesh where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteMesh where
        toPath DeleteMesh'{..}
          = mconcat ["/meshes/", toBS _dmMeshName]

instance ToQuery DeleteMesh where
        toQuery = const mempty

-- | 
--
-- /See:/ 'deleteMeshResponse' smart constructor.
data DeleteMeshResponse = DeleteMeshResponse'
  { _dmmrsMesh :: !(Maybe MeshData)
  , _dmmrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMeshResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmmrsMesh' - The service mesh that was deleted.
--
-- * 'dmmrsResponseStatus' - -- | The response status code.
deleteMeshResponse
    :: Int -- ^ 'dmmrsResponseStatus'
    -> DeleteMeshResponse
deleteMeshResponse pResponseStatus_ =
  DeleteMeshResponse'
    {_dmmrsMesh = Nothing, _dmmrsResponseStatus = pResponseStatus_}


-- | The service mesh that was deleted.
dmmrsMesh :: Lens' DeleteMeshResponse (Maybe MeshData)
dmmrsMesh = lens _dmmrsMesh (\ s a -> s{_dmmrsMesh = a})

-- | -- | The response status code.
dmmrsResponseStatus :: Lens' DeleteMeshResponse Int
dmmrsResponseStatus = lens _dmmrsResponseStatus (\ s a -> s{_dmmrsResponseStatus = a})

instance NFData DeleteMeshResponse where
