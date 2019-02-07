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
-- Module      : Network.AWS.AppMesh.UpdateVirtualRouter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing virtual router in a specified service mesh.
--
--
module Network.AWS.AppMesh.UpdateVirtualRouter
    (
    -- * Creating a Request
      updateVirtualRouter
    , UpdateVirtualRouter
    -- * Request Lenses
    , uvrClientToken
    , uvrMeshName
    , uvrSpec
    , uvrVirtualRouterName

    -- * Destructuring the Response
    , updateVirtualRouterResponse
    , UpdateVirtualRouterResponse
    -- * Response Lenses
    , uvrrsVirtualRouter
    , uvrrsResponseStatus
    ) where

import Network.AWS.AppMesh.Types
import Network.AWS.AppMesh.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | 
--
-- /See:/ 'updateVirtualRouter' smart constructor.
data UpdateVirtualRouter = UpdateVirtualRouter'
  { _uvrClientToken :: !(Maybe Text)
  , _uvrMeshName :: !Text
  , _uvrSpec :: !VirtualRouterSpec
  , _uvrVirtualRouterName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateVirtualRouter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uvrClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 36 letters, numbers, hyphens, and underscores are allowed.
--
-- * 'uvrMeshName' - The name of the service mesh in which the virtual router resides.
--
-- * 'uvrSpec' - The new virtual router specification to apply. This overwrites the existing data.
--
-- * 'uvrVirtualRouterName' - The name of the virtual router to update.
updateVirtualRouter
    :: Text -- ^ 'uvrMeshName'
    -> VirtualRouterSpec -- ^ 'uvrSpec'
    -> Text -- ^ 'uvrVirtualRouterName'
    -> UpdateVirtualRouter
updateVirtualRouter pMeshName_ pSpec_ pVirtualRouterName_ =
  UpdateVirtualRouter'
    { _uvrClientToken = Nothing
    , _uvrMeshName = pMeshName_
    , _uvrSpec = pSpec_
    , _uvrVirtualRouterName = pVirtualRouterName_
    }


-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 36 letters, numbers, hyphens, and underscores are allowed.
uvrClientToken :: Lens' UpdateVirtualRouter (Maybe Text)
uvrClientToken = lens _uvrClientToken (\ s a -> s{_uvrClientToken = a})

-- | The name of the service mesh in which the virtual router resides.
uvrMeshName :: Lens' UpdateVirtualRouter Text
uvrMeshName = lens _uvrMeshName (\ s a -> s{_uvrMeshName = a})

-- | The new virtual router specification to apply. This overwrites the existing data.
uvrSpec :: Lens' UpdateVirtualRouter VirtualRouterSpec
uvrSpec = lens _uvrSpec (\ s a -> s{_uvrSpec = a})

-- | The name of the virtual router to update.
uvrVirtualRouterName :: Lens' UpdateVirtualRouter Text
uvrVirtualRouterName = lens _uvrVirtualRouterName (\ s a -> s{_uvrVirtualRouterName = a})

instance AWSRequest UpdateVirtualRouter where
        type Rs UpdateVirtualRouter =
             UpdateVirtualRouterResponse
        request = putJSON appMesh
        response
          = receiveJSON
              (\ s h x ->
                 UpdateVirtualRouterResponse' <$>
                   (eitherParseJSON x) <*> (pure (fromEnum s)))

instance Hashable UpdateVirtualRouter where

instance NFData UpdateVirtualRouter where

instance ToHeaders UpdateVirtualRouter where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateVirtualRouter where
        toJSON UpdateVirtualRouter'{..}
          = object
              (catMaybes
                 [("clientToken" .=) <$> _uvrClientToken,
                  Just ("spec" .= _uvrSpec)])

instance ToPath UpdateVirtualRouter where
        toPath UpdateVirtualRouter'{..}
          = mconcat
              ["/meshes/", toBS _uvrMeshName, "/virtualRouters/",
               toBS _uvrVirtualRouterName]

instance ToQuery UpdateVirtualRouter where
        toQuery = const mempty

-- | 
--
-- /See:/ 'updateVirtualRouterResponse' smart constructor.
data UpdateVirtualRouterResponse = UpdateVirtualRouterResponse'
  { _uvrrsVirtualRouter :: !(Maybe VirtualRouterData)
  , _uvrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateVirtualRouterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uvrrsVirtualRouter' - A full description of the virtual router that was updated.
--
-- * 'uvrrsResponseStatus' - -- | The response status code.
updateVirtualRouterResponse
    :: Int -- ^ 'uvrrsResponseStatus'
    -> UpdateVirtualRouterResponse
updateVirtualRouterResponse pResponseStatus_ =
  UpdateVirtualRouterResponse'
    {_uvrrsVirtualRouter = Nothing, _uvrrsResponseStatus = pResponseStatus_}


-- | A full description of the virtual router that was updated.
uvrrsVirtualRouter :: Lens' UpdateVirtualRouterResponse (Maybe VirtualRouterData)
uvrrsVirtualRouter = lens _uvrrsVirtualRouter (\ s a -> s{_uvrrsVirtualRouter = a})

-- | -- | The response status code.
uvrrsResponseStatus :: Lens' UpdateVirtualRouterResponse Int
uvrrsResponseStatus = lens _uvrrsResponseStatus (\ s a -> s{_uvrrsResponseStatus = a})

instance NFData UpdateVirtualRouterResponse where
