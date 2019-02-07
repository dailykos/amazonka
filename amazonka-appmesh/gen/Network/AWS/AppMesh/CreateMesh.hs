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
-- Module      : Network.AWS.AppMesh.CreateMesh
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new service mesh. A service mesh is a logical boundary for network traffic
--
--          between the services that reside within it.
--
-- After you create your service mesh, you can create virtual nodes, virtual routers, and
--          routes to distribute traffic between the applications in your mesh.
--
module Network.AWS.AppMesh.CreateMesh
    (
    -- * Creating a Request
      createMesh
    , CreateMesh
    -- * Request Lenses
    , cmClientToken
    , cmMeshName

    -- * Destructuring the Response
    , createMeshResponse
    , CreateMeshResponse
    -- * Response Lenses
    , cmrsMesh
    , cmrsResponseStatus
    ) where

import Network.AWS.AppMesh.Types
import Network.AWS.AppMesh.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | 
--
-- /See:/ 'createMesh' smart constructor.
data CreateMesh = CreateMesh'
  { _cmClientToken :: !(Maybe Text)
  , _cmMeshName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateMesh' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 36 letters, numbers, hyphens, and underscores are allowed.
--
-- * 'cmMeshName' - The name to use for the service mesh.
createMesh
    :: Text -- ^ 'cmMeshName'
    -> CreateMesh
createMesh pMeshName_ =
  CreateMesh' {_cmClientToken = Nothing, _cmMeshName = pMeshName_}


-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 36 letters, numbers, hyphens, and underscores are allowed.
cmClientToken :: Lens' CreateMesh (Maybe Text)
cmClientToken = lens _cmClientToken (\ s a -> s{_cmClientToken = a})

-- | The name to use for the service mesh.
cmMeshName :: Lens' CreateMesh Text
cmMeshName = lens _cmMeshName (\ s a -> s{_cmMeshName = a})

instance AWSRequest CreateMesh where
        type Rs CreateMesh = CreateMeshResponse
        request = putJSON appMesh
        response
          = receiveJSON
              (\ s h x ->
                 CreateMeshResponse' <$>
                   (eitherParseJSON x) <*> (pure (fromEnum s)))

instance Hashable CreateMesh where

instance NFData CreateMesh where

instance ToHeaders CreateMesh where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateMesh where
        toJSON CreateMesh'{..}
          = object
              (catMaybes
                 [("clientToken" .=) <$> _cmClientToken,
                  Just ("meshName" .= _cmMeshName)])

instance ToPath CreateMesh where
        toPath = const "/meshes"

instance ToQuery CreateMesh where
        toQuery = const mempty

-- | 
--
-- /See:/ 'createMeshResponse' smart constructor.
data CreateMeshResponse = CreateMeshResponse'
  { _cmrsMesh :: !(Maybe MeshData)
  , _cmrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateMeshResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmrsMesh' - The full description of your service mesh following the create call.
--
-- * 'cmrsResponseStatus' - -- | The response status code.
createMeshResponse
    :: Int -- ^ 'cmrsResponseStatus'
    -> CreateMeshResponse
createMeshResponse pResponseStatus_ =
  CreateMeshResponse'
    {_cmrsMesh = Nothing, _cmrsResponseStatus = pResponseStatus_}


-- | The full description of your service mesh following the create call.
cmrsMesh :: Lens' CreateMeshResponse (Maybe MeshData)
cmrsMesh = lens _cmrsMesh (\ s a -> s{_cmrsMesh = a})

-- | -- | The response status code.
cmrsResponseStatus :: Lens' CreateMeshResponse Int
cmrsResponseStatus = lens _cmrsResponseStatus (\ s a -> s{_cmrsResponseStatus = a})

instance NFData CreateMeshResponse where
