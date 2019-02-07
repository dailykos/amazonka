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
-- Module      : Network.AWS.AppMesh.DescribeVirtualRouter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing virtual router.
--
--
module Network.AWS.AppMesh.DescribeVirtualRouter
    (
    -- * Creating a Request
      describeVirtualRouter
    , DescribeVirtualRouter
    -- * Request Lenses
    , dvrMeshName
    , dvrVirtualRouterName

    -- * Destructuring the Response
    , describeVirtualRouterResponse
    , DescribeVirtualRouterResponse
    -- * Response Lenses
    , dvrrsVirtualRouter
    , dvrrsResponseStatus
    ) where

import Network.AWS.AppMesh.Types
import Network.AWS.AppMesh.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | 
--
-- /See:/ 'describeVirtualRouter' smart constructor.
data DescribeVirtualRouter = DescribeVirtualRouter'
  { _dvrMeshName :: !Text
  , _dvrVirtualRouterName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVirtualRouter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvrMeshName' - The name of the service mesh in which the virtual router resides.
--
-- * 'dvrVirtualRouterName' - The name of the virtual router to describe.
describeVirtualRouter
    :: Text -- ^ 'dvrMeshName'
    -> Text -- ^ 'dvrVirtualRouterName'
    -> DescribeVirtualRouter
describeVirtualRouter pMeshName_ pVirtualRouterName_ =
  DescribeVirtualRouter'
    {_dvrMeshName = pMeshName_, _dvrVirtualRouterName = pVirtualRouterName_}


-- | The name of the service mesh in which the virtual router resides.
dvrMeshName :: Lens' DescribeVirtualRouter Text
dvrMeshName = lens _dvrMeshName (\ s a -> s{_dvrMeshName = a})

-- | The name of the virtual router to describe.
dvrVirtualRouterName :: Lens' DescribeVirtualRouter Text
dvrVirtualRouterName = lens _dvrVirtualRouterName (\ s a -> s{_dvrVirtualRouterName = a})

instance AWSRequest DescribeVirtualRouter where
        type Rs DescribeVirtualRouter =
             DescribeVirtualRouterResponse
        request = get appMesh
        response
          = receiveJSON
              (\ s h x ->
                 DescribeVirtualRouterResponse' <$>
                   (eitherParseJSON x) <*> (pure (fromEnum s)))

instance Hashable DescribeVirtualRouter where

instance NFData DescribeVirtualRouter where

instance ToHeaders DescribeVirtualRouter where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeVirtualRouter where
        toPath DescribeVirtualRouter'{..}
          = mconcat
              ["/meshes/", toBS _dvrMeshName, "/virtualRouters/",
               toBS _dvrVirtualRouterName]

instance ToQuery DescribeVirtualRouter where
        toQuery = const mempty

-- | 
--
-- /See:/ 'describeVirtualRouterResponse' smart constructor.
data DescribeVirtualRouterResponse = DescribeVirtualRouterResponse'
  { _dvrrsVirtualRouter :: !(Maybe VirtualRouterData)
  , _dvrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVirtualRouterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvrrsVirtualRouter' - The full description of your virtual router.
--
-- * 'dvrrsResponseStatus' - -- | The response status code.
describeVirtualRouterResponse
    :: Int -- ^ 'dvrrsResponseStatus'
    -> DescribeVirtualRouterResponse
describeVirtualRouterResponse pResponseStatus_ =
  DescribeVirtualRouterResponse'
    {_dvrrsVirtualRouter = Nothing, _dvrrsResponseStatus = pResponseStatus_}


-- | The full description of your virtual router.
dvrrsVirtualRouter :: Lens' DescribeVirtualRouterResponse (Maybe VirtualRouterData)
dvrrsVirtualRouter = lens _dvrrsVirtualRouter (\ s a -> s{_dvrrsVirtualRouter = a})

-- | -- | The response status code.
dvrrsResponseStatus :: Lens' DescribeVirtualRouterResponse Int
dvrrsResponseStatus = lens _dvrrsResponseStatus (\ s a -> s{_dvrrsResponseStatus = a})

instance NFData DescribeVirtualRouterResponse where
