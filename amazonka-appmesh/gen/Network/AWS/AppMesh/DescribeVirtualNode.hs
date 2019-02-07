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
-- Module      : Network.AWS.AppMesh.DescribeVirtualNode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing virtual node.
--
--
module Network.AWS.AppMesh.DescribeVirtualNode
    (
    -- * Creating a Request
      describeVirtualNode
    , DescribeVirtualNode
    -- * Request Lenses
    , dvnMeshName
    , dvnVirtualNodeName

    -- * Destructuring the Response
    , describeVirtualNodeResponse
    , DescribeVirtualNodeResponse
    -- * Response Lenses
    , dvnrsVirtualNode
    , dvnrsResponseStatus
    ) where

import Network.AWS.AppMesh.Types
import Network.AWS.AppMesh.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | 
--
-- /See:/ 'describeVirtualNode' smart constructor.
data DescribeVirtualNode = DescribeVirtualNode'
  { _dvnMeshName :: !Text
  , _dvnVirtualNodeName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVirtualNode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvnMeshName' - The name of the service mesh in which the virtual node resides.
--
-- * 'dvnVirtualNodeName' - The name of the virtual node to describe.
describeVirtualNode
    :: Text -- ^ 'dvnMeshName'
    -> Text -- ^ 'dvnVirtualNodeName'
    -> DescribeVirtualNode
describeVirtualNode pMeshName_ pVirtualNodeName_ =
  DescribeVirtualNode'
    {_dvnMeshName = pMeshName_, _dvnVirtualNodeName = pVirtualNodeName_}


-- | The name of the service mesh in which the virtual node resides.
dvnMeshName :: Lens' DescribeVirtualNode Text
dvnMeshName = lens _dvnMeshName (\ s a -> s{_dvnMeshName = a})

-- | The name of the virtual node to describe.
dvnVirtualNodeName :: Lens' DescribeVirtualNode Text
dvnVirtualNodeName = lens _dvnVirtualNodeName (\ s a -> s{_dvnVirtualNodeName = a})

instance AWSRequest DescribeVirtualNode where
        type Rs DescribeVirtualNode =
             DescribeVirtualNodeResponse
        request = get appMesh
        response
          = receiveJSON
              (\ s h x ->
                 DescribeVirtualNodeResponse' <$>
                   (eitherParseJSON x) <*> (pure (fromEnum s)))

instance Hashable DescribeVirtualNode where

instance NFData DescribeVirtualNode where

instance ToHeaders DescribeVirtualNode where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeVirtualNode where
        toPath DescribeVirtualNode'{..}
          = mconcat
              ["/meshes/", toBS _dvnMeshName, "/virtualNodes/",
               toBS _dvnVirtualNodeName]

instance ToQuery DescribeVirtualNode where
        toQuery = const mempty

-- | 
--
-- /See:/ 'describeVirtualNodeResponse' smart constructor.
data DescribeVirtualNodeResponse = DescribeVirtualNodeResponse'
  { _dvnrsVirtualNode :: !(Maybe VirtualNodeData)
  , _dvnrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVirtualNodeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvnrsVirtualNode' - The full description of your virtual node.
--
-- * 'dvnrsResponseStatus' - -- | The response status code.
describeVirtualNodeResponse
    :: Int -- ^ 'dvnrsResponseStatus'
    -> DescribeVirtualNodeResponse
describeVirtualNodeResponse pResponseStatus_ =
  DescribeVirtualNodeResponse'
    {_dvnrsVirtualNode = Nothing, _dvnrsResponseStatus = pResponseStatus_}


-- | The full description of your virtual node.
dvnrsVirtualNode :: Lens' DescribeVirtualNodeResponse (Maybe VirtualNodeData)
dvnrsVirtualNode = lens _dvnrsVirtualNode (\ s a -> s{_dvnrsVirtualNode = a})

-- | -- | The response status code.
dvnrsResponseStatus :: Lens' DescribeVirtualNodeResponse Int
dvnrsResponseStatus = lens _dvnrsResponseStatus (\ s a -> s{_dvnrsResponseStatus = a})

instance NFData DescribeVirtualNodeResponse where
