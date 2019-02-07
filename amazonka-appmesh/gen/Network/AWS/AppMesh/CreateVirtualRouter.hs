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
-- Module      : Network.AWS.AppMesh.CreateVirtualRouter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new virtual router within a service mesh.
--
--
-- Virtual routers handle traffic for one or more service names within your mesh. After you
--          create your virtual router, create and associate routes for your virtual router that direct
--          incoming requests to different virtual nodes.
--
module Network.AWS.AppMesh.CreateVirtualRouter
    (
    -- * Creating a Request
      createVirtualRouter
    , CreateVirtualRouter
    -- * Request Lenses
    , cvrClientToken
    , cvrMeshName
    , cvrSpec
    , cvrVirtualRouterName

    -- * Destructuring the Response
    , createVirtualRouterResponse
    , CreateVirtualRouterResponse
    -- * Response Lenses
    , cvrrsVirtualRouter
    , cvrrsResponseStatus
    ) where

import Network.AWS.AppMesh.Types
import Network.AWS.AppMesh.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | 
--
-- /See:/ 'createVirtualRouter' smart constructor.
data CreateVirtualRouter = CreateVirtualRouter'
  { _cvrClientToken :: !(Maybe Text)
  , _cvrMeshName :: !Text
  , _cvrSpec :: !VirtualRouterSpec
  , _cvrVirtualRouterName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVirtualRouter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvrClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 36 letters, numbers, hyphens, and underscores are allowed.
--
-- * 'cvrMeshName' - The name of the service mesh in which to create the virtual router.
--
-- * 'cvrSpec' - The virtual router specification to apply.
--
-- * 'cvrVirtualRouterName' - The name to use for the virtual router.
createVirtualRouter
    :: Text -- ^ 'cvrMeshName'
    -> VirtualRouterSpec -- ^ 'cvrSpec'
    -> Text -- ^ 'cvrVirtualRouterName'
    -> CreateVirtualRouter
createVirtualRouter pMeshName_ pSpec_ pVirtualRouterName_ =
  CreateVirtualRouter'
    { _cvrClientToken = Nothing
    , _cvrMeshName = pMeshName_
    , _cvrSpec = pSpec_
    , _cvrVirtualRouterName = pVirtualRouterName_
    }


-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 36 letters, numbers, hyphens, and underscores are allowed.
cvrClientToken :: Lens' CreateVirtualRouter (Maybe Text)
cvrClientToken = lens _cvrClientToken (\ s a -> s{_cvrClientToken = a})

-- | The name of the service mesh in which to create the virtual router.
cvrMeshName :: Lens' CreateVirtualRouter Text
cvrMeshName = lens _cvrMeshName (\ s a -> s{_cvrMeshName = a})

-- | The virtual router specification to apply.
cvrSpec :: Lens' CreateVirtualRouter VirtualRouterSpec
cvrSpec = lens _cvrSpec (\ s a -> s{_cvrSpec = a})

-- | The name to use for the virtual router.
cvrVirtualRouterName :: Lens' CreateVirtualRouter Text
cvrVirtualRouterName = lens _cvrVirtualRouterName (\ s a -> s{_cvrVirtualRouterName = a})

instance AWSRequest CreateVirtualRouter where
        type Rs CreateVirtualRouter =
             CreateVirtualRouterResponse
        request = putJSON appMesh
        response
          = receiveJSON
              (\ s h x ->
                 CreateVirtualRouterResponse' <$>
                   (eitherParseJSON x) <*> (pure (fromEnum s)))

instance Hashable CreateVirtualRouter where

instance NFData CreateVirtualRouter where

instance ToHeaders CreateVirtualRouter where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateVirtualRouter where
        toJSON CreateVirtualRouter'{..}
          = object
              (catMaybes
                 [("clientToken" .=) <$> _cvrClientToken,
                  Just ("spec" .= _cvrSpec),
                  Just ("virtualRouterName" .= _cvrVirtualRouterName)])

instance ToPath CreateVirtualRouter where
        toPath CreateVirtualRouter'{..}
          = mconcat
              ["/meshes/", toBS _cvrMeshName, "/virtualRouters"]

instance ToQuery CreateVirtualRouter where
        toQuery = const mempty

-- | 
--
-- /See:/ 'createVirtualRouterResponse' smart constructor.
data CreateVirtualRouterResponse = CreateVirtualRouterResponse'
  { _cvrrsVirtualRouter :: !(Maybe VirtualRouterData)
  , _cvrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVirtualRouterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvrrsVirtualRouter' - The full description of your virtual router following the create call.
--
-- * 'cvrrsResponseStatus' - -- | The response status code.
createVirtualRouterResponse
    :: Int -- ^ 'cvrrsResponseStatus'
    -> CreateVirtualRouterResponse
createVirtualRouterResponse pResponseStatus_ =
  CreateVirtualRouterResponse'
    {_cvrrsVirtualRouter = Nothing, _cvrrsResponseStatus = pResponseStatus_}


-- | The full description of your virtual router following the create call.
cvrrsVirtualRouter :: Lens' CreateVirtualRouterResponse (Maybe VirtualRouterData)
cvrrsVirtualRouter = lens _cvrrsVirtualRouter (\ s a -> s{_cvrrsVirtualRouter = a})

-- | -- | The response status code.
cvrrsResponseStatus :: Lens' CreateVirtualRouterResponse Int
cvrrsResponseStatus = lens _cvrrsResponseStatus (\ s a -> s{_cvrrsResponseStatus = a})

instance NFData CreateVirtualRouterResponse where
