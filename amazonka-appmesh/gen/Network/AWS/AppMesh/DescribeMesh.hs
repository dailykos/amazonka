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
-- Module      : Network.AWS.AppMesh.DescribeMesh
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing service mesh.
--
--
module Network.AWS.AppMesh.DescribeMesh
    (
    -- * Creating a Request
      describeMesh
    , DescribeMesh
    -- * Request Lenses
    , desMeshName

    -- * Destructuring the Response
    , describeMeshResponse
    , DescribeMeshResponse
    -- * Response Lenses
    , dmrsMesh
    , dmrsResponseStatus
    ) where

import Network.AWS.AppMesh.Types
import Network.AWS.AppMesh.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | 
--
-- /See:/ 'describeMesh' smart constructor.
newtype DescribeMesh = DescribeMesh'
  { _desMeshName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMesh' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desMeshName' - The name of the service mesh to describe.
describeMesh
    :: Text -- ^ 'desMeshName'
    -> DescribeMesh
describeMesh pMeshName_ = DescribeMesh' {_desMeshName = pMeshName_}


-- | The name of the service mesh to describe.
desMeshName :: Lens' DescribeMesh Text
desMeshName = lens _desMeshName (\ s a -> s{_desMeshName = a})

instance AWSRequest DescribeMesh where
        type Rs DescribeMesh = DescribeMeshResponse
        request = get appMesh
        response
          = receiveJSON
              (\ s h x ->
                 DescribeMeshResponse' <$>
                   (eitherParseJSON x) <*> (pure (fromEnum s)))

instance Hashable DescribeMesh where

instance NFData DescribeMesh where

instance ToHeaders DescribeMesh where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeMesh where
        toPath DescribeMesh'{..}
          = mconcat ["/meshes/", toBS _desMeshName]

instance ToQuery DescribeMesh where
        toQuery = const mempty

-- | 
--
-- /See:/ 'describeMeshResponse' smart constructor.
data DescribeMeshResponse = DescribeMeshResponse'
  { _dmrsMesh :: !(Maybe MeshData)
  , _dmrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMeshResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmrsMesh' - The full description of your service mesh.
--
-- * 'dmrsResponseStatus' - -- | The response status code.
describeMeshResponse
    :: Int -- ^ 'dmrsResponseStatus'
    -> DescribeMeshResponse
describeMeshResponse pResponseStatus_ =
  DescribeMeshResponse'
    {_dmrsMesh = Nothing, _dmrsResponseStatus = pResponseStatus_}


-- | The full description of your service mesh.
dmrsMesh :: Lens' DescribeMeshResponse (Maybe MeshData)
dmrsMesh = lens _dmrsMesh (\ s a -> s{_dmrsMesh = a})

-- | -- | The response status code.
dmrsResponseStatus :: Lens' DescribeMeshResponse Int
dmrsResponseStatus = lens _dmrsResponseStatus (\ s a -> s{_dmrsResponseStatus = a})

instance NFData DescribeMeshResponse where
