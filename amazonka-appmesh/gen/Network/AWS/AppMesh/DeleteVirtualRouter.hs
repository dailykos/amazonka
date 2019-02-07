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
-- Module      : Network.AWS.AppMesh.DeleteVirtualRouter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing virtual router.
--
--
-- You must delete any routes associated with the virtual router before you can delete the
--          router itself.
--
module Network.AWS.AppMesh.DeleteVirtualRouter
    (
    -- * Creating a Request
      deleteVirtualRouter
    , DeleteVirtualRouter
    -- * Request Lenses
    , dvrvMeshName
    , dvrvVirtualRouterName

    -- * Destructuring the Response
    , deleteVirtualRouterResponse
    , DeleteVirtualRouterResponse
    -- * Response Lenses
    , drsVirtualRouter
    , drsResponseStatus
    ) where

import Network.AWS.AppMesh.Types
import Network.AWS.AppMesh.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | 
--
-- /See:/ 'deleteVirtualRouter' smart constructor.
data DeleteVirtualRouter = DeleteVirtualRouter'
  { _dvrvMeshName :: !Text
  , _dvrvVirtualRouterName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVirtualRouter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvrvMeshName' - The name of the service mesh in which to delete the virtual router.
--
-- * 'dvrvVirtualRouterName' - The name of the virtual router to delete.
deleteVirtualRouter
    :: Text -- ^ 'dvrvMeshName'
    -> Text -- ^ 'dvrvVirtualRouterName'
    -> DeleteVirtualRouter
deleteVirtualRouter pMeshName_ pVirtualRouterName_ =
  DeleteVirtualRouter'
    {_dvrvMeshName = pMeshName_, _dvrvVirtualRouterName = pVirtualRouterName_}


-- | The name of the service mesh in which to delete the virtual router.
dvrvMeshName :: Lens' DeleteVirtualRouter Text
dvrvMeshName = lens _dvrvMeshName (\ s a -> s{_dvrvMeshName = a})

-- | The name of the virtual router to delete.
dvrvVirtualRouterName :: Lens' DeleteVirtualRouter Text
dvrvVirtualRouterName = lens _dvrvVirtualRouterName (\ s a -> s{_dvrvVirtualRouterName = a})

instance AWSRequest DeleteVirtualRouter where
        type Rs DeleteVirtualRouter =
             DeleteVirtualRouterResponse
        request = delete appMesh
        response
          = receiveJSON
              (\ s h x ->
                 DeleteVirtualRouterResponse' <$>
                   (eitherParseJSON x) <*> (pure (fromEnum s)))

instance Hashable DeleteVirtualRouter where

instance NFData DeleteVirtualRouter where

instance ToHeaders DeleteVirtualRouter where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteVirtualRouter where
        toPath DeleteVirtualRouter'{..}
          = mconcat
              ["/meshes/", toBS _dvrvMeshName, "/virtualRouters/",
               toBS _dvrvVirtualRouterName]

instance ToQuery DeleteVirtualRouter where
        toQuery = const mempty

-- | 
--
-- /See:/ 'deleteVirtualRouterResponse' smart constructor.
data DeleteVirtualRouterResponse = DeleteVirtualRouterResponse'
  { _drsVirtualRouter :: !(Maybe VirtualRouterData)
  , _drsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVirtualRouterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsVirtualRouter' - The virtual router that was deleted.
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteVirtualRouterResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteVirtualRouterResponse
deleteVirtualRouterResponse pResponseStatus_ =
  DeleteVirtualRouterResponse'
    {_drsVirtualRouter = Nothing, _drsResponseStatus = pResponseStatus_}


-- | The virtual router that was deleted.
drsVirtualRouter :: Lens' DeleteVirtualRouterResponse (Maybe VirtualRouterData)
drsVirtualRouter = lens _drsVirtualRouter (\ s a -> s{_drsVirtualRouter = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteVirtualRouterResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteVirtualRouterResponse where
