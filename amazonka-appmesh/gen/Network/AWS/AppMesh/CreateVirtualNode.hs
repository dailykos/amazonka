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
-- Module      : Network.AWS.AppMesh.CreateVirtualNode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new virtual node within a service mesh.
--
--
-- A virtual node acts as logical pointer to a particular task group, such as an Amazon ECS
--          service or a Kubernetes deployment. When you create a virtual node, you must specify the
--          DNS service discovery name for your task group.
--
-- Any inbound traffic that your virtual node expects should be specified as a
--             @listener@ . Any outbound traffic that your virtual node expects to reach
--          should be specified as a @backend@ .
--
-- The response metadata for your new virtual node contains the @arn@ that is
--          associated with the virtual node. Set this value (either the full ARN or the truncated
--          resource name, for example, @mesh/default/virtualNode/simpleapp@ , as the
--             @APPMESH_VIRTUAL_NODE_NAME@ environment variable for your task group's Envoy
--          proxy container in your task definition or pod spec. This is then mapped to the
--             @node.id@ and @node.cluster@ Envoy parameters.
--
module Network.AWS.AppMesh.CreateVirtualNode
    (
    -- * Creating a Request
      createVirtualNode
    , CreateVirtualNode
    -- * Request Lenses
    , cvnClientToken
    , cvnMeshName
    , cvnSpec
    , cvnVirtualNodeName

    -- * Destructuring the Response
    , createVirtualNodeResponse
    , CreateVirtualNodeResponse
    -- * Response Lenses
    , cvnrsVirtualNode
    , cvnrsResponseStatus
    ) where

import Network.AWS.AppMesh.Types
import Network.AWS.AppMesh.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | 
--
-- /See:/ 'createVirtualNode' smart constructor.
data CreateVirtualNode = CreateVirtualNode'
  { _cvnClientToken :: !(Maybe Text)
  , _cvnMeshName :: !Text
  , _cvnSpec :: !VirtualNodeSpec
  , _cvnVirtualNodeName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVirtualNode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvnClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 36 letters, numbers, hyphens, and underscores are allowed.
--
-- * 'cvnMeshName' - The name of the service mesh in which to create the virtual node.
--
-- * 'cvnSpec' - The virtual node specification to apply.
--
-- * 'cvnVirtualNodeName' - The name to use for the virtual node.
createVirtualNode
    :: Text -- ^ 'cvnMeshName'
    -> VirtualNodeSpec -- ^ 'cvnSpec'
    -> Text -- ^ 'cvnVirtualNodeName'
    -> CreateVirtualNode
createVirtualNode pMeshName_ pSpec_ pVirtualNodeName_ =
  CreateVirtualNode'
    { _cvnClientToken = Nothing
    , _cvnMeshName = pMeshName_
    , _cvnSpec = pSpec_
    , _cvnVirtualNodeName = pVirtualNodeName_
    }


-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 36 letters, numbers, hyphens, and underscores are allowed.
cvnClientToken :: Lens' CreateVirtualNode (Maybe Text)
cvnClientToken = lens _cvnClientToken (\ s a -> s{_cvnClientToken = a})

-- | The name of the service mesh in which to create the virtual node.
cvnMeshName :: Lens' CreateVirtualNode Text
cvnMeshName = lens _cvnMeshName (\ s a -> s{_cvnMeshName = a})

-- | The virtual node specification to apply.
cvnSpec :: Lens' CreateVirtualNode VirtualNodeSpec
cvnSpec = lens _cvnSpec (\ s a -> s{_cvnSpec = a})

-- | The name to use for the virtual node.
cvnVirtualNodeName :: Lens' CreateVirtualNode Text
cvnVirtualNodeName = lens _cvnVirtualNodeName (\ s a -> s{_cvnVirtualNodeName = a})

instance AWSRequest CreateVirtualNode where
        type Rs CreateVirtualNode = CreateVirtualNodeResponse
        request = putJSON appMesh
        response
          = receiveJSON
              (\ s h x ->
                 CreateVirtualNodeResponse' <$>
                   (eitherParseJSON x) <*> (pure (fromEnum s)))

instance Hashable CreateVirtualNode where

instance NFData CreateVirtualNode where

instance ToHeaders CreateVirtualNode where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateVirtualNode where
        toJSON CreateVirtualNode'{..}
          = object
              (catMaybes
                 [("clientToken" .=) <$> _cvnClientToken,
                  Just ("spec" .= _cvnSpec),
                  Just ("virtualNodeName" .= _cvnVirtualNodeName)])

instance ToPath CreateVirtualNode where
        toPath CreateVirtualNode'{..}
          = mconcat
              ["/meshes/", toBS _cvnMeshName, "/virtualNodes"]

instance ToQuery CreateVirtualNode where
        toQuery = const mempty

-- | 
--
-- /See:/ 'createVirtualNodeResponse' smart constructor.
data CreateVirtualNodeResponse = CreateVirtualNodeResponse'
  { _cvnrsVirtualNode :: !(Maybe VirtualNodeData)
  , _cvnrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVirtualNodeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvnrsVirtualNode' - The full description of your virtual node following the create call.
--
-- * 'cvnrsResponseStatus' - -- | The response status code.
createVirtualNodeResponse
    :: Int -- ^ 'cvnrsResponseStatus'
    -> CreateVirtualNodeResponse
createVirtualNodeResponse pResponseStatus_ =
  CreateVirtualNodeResponse'
    {_cvnrsVirtualNode = Nothing, _cvnrsResponseStatus = pResponseStatus_}


-- | The full description of your virtual node following the create call.
cvnrsVirtualNode :: Lens' CreateVirtualNodeResponse (Maybe VirtualNodeData)
cvnrsVirtualNode = lens _cvnrsVirtualNode (\ s a -> s{_cvnrsVirtualNode = a})

-- | -- | The response status code.
cvnrsResponseStatus :: Lens' CreateVirtualNodeResponse Int
cvnrsResponseStatus = lens _cvnrsResponseStatus (\ s a -> s{_cvnrsResponseStatus = a})

instance NFData CreateVirtualNodeResponse where
