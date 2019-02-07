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
-- Module      : Network.AWS.AppMesh.DeleteVirtualNode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing virtual node.
--
--
module Network.AWS.AppMesh.DeleteVirtualNode
    (
    -- * Creating a Request
      deleteVirtualNode
    , DeleteVirtualNode
    -- * Request Lenses
    , delMeshName
    , delVirtualNodeName

    -- * Destructuring the Response
    , deleteVirtualNodeResponse
    , DeleteVirtualNodeResponse
    -- * Response Lenses
    , dvnvrsVirtualNode
    , dvnvrsResponseStatus
    ) where

import Network.AWS.AppMesh.Types
import Network.AWS.AppMesh.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | 
--
-- /See:/ 'deleteVirtualNode' smart constructor.
data DeleteVirtualNode = DeleteVirtualNode'
  { _delMeshName :: !Text
  , _delVirtualNodeName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVirtualNode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delMeshName' - The name of the service mesh in which to delete the virtual node.
--
-- * 'delVirtualNodeName' - The name of the virtual node to delete.
deleteVirtualNode
    :: Text -- ^ 'delMeshName'
    -> Text -- ^ 'delVirtualNodeName'
    -> DeleteVirtualNode
deleteVirtualNode pMeshName_ pVirtualNodeName_ =
  DeleteVirtualNode'
    {_delMeshName = pMeshName_, _delVirtualNodeName = pVirtualNodeName_}


-- | The name of the service mesh in which to delete the virtual node.
delMeshName :: Lens' DeleteVirtualNode Text
delMeshName = lens _delMeshName (\ s a -> s{_delMeshName = a})

-- | The name of the virtual node to delete.
delVirtualNodeName :: Lens' DeleteVirtualNode Text
delVirtualNodeName = lens _delVirtualNodeName (\ s a -> s{_delVirtualNodeName = a})

instance AWSRequest DeleteVirtualNode where
        type Rs DeleteVirtualNode = DeleteVirtualNodeResponse
        request = delete appMesh
        response
          = receiveJSON
              (\ s h x ->
                 DeleteVirtualNodeResponse' <$>
                   (eitherParseJSON x) <*> (pure (fromEnum s)))

instance Hashable DeleteVirtualNode where

instance NFData DeleteVirtualNode where

instance ToHeaders DeleteVirtualNode where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteVirtualNode where
        toPath DeleteVirtualNode'{..}
          = mconcat
              ["/meshes/", toBS _delMeshName, "/virtualNodes/",
               toBS _delVirtualNodeName]

instance ToQuery DeleteVirtualNode where
        toQuery = const mempty

-- | 
--
-- /See:/ 'deleteVirtualNodeResponse' smart constructor.
data DeleteVirtualNodeResponse = DeleteVirtualNodeResponse'
  { _dvnvrsVirtualNode :: !(Maybe VirtualNodeData)
  , _dvnvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVirtualNodeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvnvrsVirtualNode' - The virtual node that was deleted.
--
-- * 'dvnvrsResponseStatus' - -- | The response status code.
deleteVirtualNodeResponse
    :: Int -- ^ 'dvnvrsResponseStatus'
    -> DeleteVirtualNodeResponse
deleteVirtualNodeResponse pResponseStatus_ =
  DeleteVirtualNodeResponse'
    {_dvnvrsVirtualNode = Nothing, _dvnvrsResponseStatus = pResponseStatus_}


-- | The virtual node that was deleted.
dvnvrsVirtualNode :: Lens' DeleteVirtualNodeResponse (Maybe VirtualNodeData)
dvnvrsVirtualNode = lens _dvnvrsVirtualNode (\ s a -> s{_dvnvrsVirtualNode = a})

-- | -- | The response status code.
dvnvrsResponseStatus :: Lens' DeleteVirtualNodeResponse Int
dvnvrsResponseStatus = lens _dvnvrsResponseStatus (\ s a -> s{_dvnvrsResponseStatus = a})

instance NFData DeleteVirtualNodeResponse where
