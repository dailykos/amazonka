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
-- Module      : Network.AWS.AppMesh.UpdateVirtualNode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing virtual node in a specified service mesh.
--
--
module Network.AWS.AppMesh.UpdateVirtualNode
    (
    -- * Creating a Request
      updateVirtualNode
    , UpdateVirtualNode
    -- * Request Lenses
    , uvnClientToken
    , uvnMeshName
    , uvnSpec
    , uvnVirtualNodeName

    -- * Destructuring the Response
    , updateVirtualNodeResponse
    , UpdateVirtualNodeResponse
    -- * Response Lenses
    , uvnrsVirtualNode
    , uvnrsResponseStatus
    ) where

import Network.AWS.AppMesh.Types
import Network.AWS.AppMesh.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | 
--
-- /See:/ 'updateVirtualNode' smart constructor.
data UpdateVirtualNode = UpdateVirtualNode'
  { _uvnClientToken :: !(Maybe Text)
  , _uvnMeshName :: !Text
  , _uvnSpec :: !VirtualNodeSpec
  , _uvnVirtualNodeName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateVirtualNode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uvnClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 36 letters, numbers, hyphens, and underscores are allowed.
--
-- * 'uvnMeshName' - The name of the service mesh in which the virtual node resides.
--
-- * 'uvnSpec' - The new virtual node specification to apply. This overwrites the existing data.
--
-- * 'uvnVirtualNodeName' - The name of the virtual node to update.
updateVirtualNode
    :: Text -- ^ 'uvnMeshName'
    -> VirtualNodeSpec -- ^ 'uvnSpec'
    -> Text -- ^ 'uvnVirtualNodeName'
    -> UpdateVirtualNode
updateVirtualNode pMeshName_ pSpec_ pVirtualNodeName_ =
  UpdateVirtualNode'
    { _uvnClientToken = Nothing
    , _uvnMeshName = pMeshName_
    , _uvnSpec = pSpec_
    , _uvnVirtualNodeName = pVirtualNodeName_
    }


-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 36 letters, numbers, hyphens, and underscores are allowed.
uvnClientToken :: Lens' UpdateVirtualNode (Maybe Text)
uvnClientToken = lens _uvnClientToken (\ s a -> s{_uvnClientToken = a})

-- | The name of the service mesh in which the virtual node resides.
uvnMeshName :: Lens' UpdateVirtualNode Text
uvnMeshName = lens _uvnMeshName (\ s a -> s{_uvnMeshName = a})

-- | The new virtual node specification to apply. This overwrites the existing data.
uvnSpec :: Lens' UpdateVirtualNode VirtualNodeSpec
uvnSpec = lens _uvnSpec (\ s a -> s{_uvnSpec = a})

-- | The name of the virtual node to update.
uvnVirtualNodeName :: Lens' UpdateVirtualNode Text
uvnVirtualNodeName = lens _uvnVirtualNodeName (\ s a -> s{_uvnVirtualNodeName = a})

instance AWSRequest UpdateVirtualNode where
        type Rs UpdateVirtualNode = UpdateVirtualNodeResponse
        request = putJSON appMesh
        response
          = receiveJSON
              (\ s h x ->
                 UpdateVirtualNodeResponse' <$>
                   (eitherParseJSON x) <*> (pure (fromEnum s)))

instance Hashable UpdateVirtualNode where

instance NFData UpdateVirtualNode where

instance ToHeaders UpdateVirtualNode where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateVirtualNode where
        toJSON UpdateVirtualNode'{..}
          = object
              (catMaybes
                 [("clientToken" .=) <$> _uvnClientToken,
                  Just ("spec" .= _uvnSpec)])

instance ToPath UpdateVirtualNode where
        toPath UpdateVirtualNode'{..}
          = mconcat
              ["/meshes/", toBS _uvnMeshName, "/virtualNodes/",
               toBS _uvnVirtualNodeName]

instance ToQuery UpdateVirtualNode where
        toQuery = const mempty

-- | 
--
-- /See:/ 'updateVirtualNodeResponse' smart constructor.
data UpdateVirtualNodeResponse = UpdateVirtualNodeResponse'
  { _uvnrsVirtualNode :: !(Maybe VirtualNodeData)
  , _uvnrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateVirtualNodeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uvnrsVirtualNode' - A full description of the virtual node that was updated.
--
-- * 'uvnrsResponseStatus' - -- | The response status code.
updateVirtualNodeResponse
    :: Int -- ^ 'uvnrsResponseStatus'
    -> UpdateVirtualNodeResponse
updateVirtualNodeResponse pResponseStatus_ =
  UpdateVirtualNodeResponse'
    {_uvnrsVirtualNode = Nothing, _uvnrsResponseStatus = pResponseStatus_}


-- | A full description of the virtual node that was updated.
uvnrsVirtualNode :: Lens' UpdateVirtualNodeResponse (Maybe VirtualNodeData)
uvnrsVirtualNode = lens _uvnrsVirtualNode (\ s a -> s{_uvnrsVirtualNode = a})

-- | -- | The response status code.
uvnrsResponseStatus :: Lens' UpdateVirtualNodeResponse Int
uvnrsResponseStatus = lens _uvnrsResponseStatus (\ s a -> s{_uvnrsResponseStatus = a})

instance NFData UpdateVirtualNodeResponse where
