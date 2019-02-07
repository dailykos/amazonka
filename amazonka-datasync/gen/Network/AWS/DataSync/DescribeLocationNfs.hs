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
-- Module      : Network.AWS.DataSync.DescribeLocationNfs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata, such as the path information, about a NFS location.
--
--
module Network.AWS.DataSync.DescribeLocationNfs
    (
    -- * Creating a Request
      describeLocationNfs
    , DescribeLocationNfs
    -- * Request Lenses
    , dlnLocationARN

    -- * Destructuring the Response
    , describeLocationNfsResponse
    , DescribeLocationNfsResponse
    -- * Response Lenses
    , dlnrsCreationTime
    , dlnrsLocationURI
    , dlnrsOnPremConfig
    , dlnrsLocationARN
    , dlnrsResponseStatus
    ) where

import Network.AWS.DataSync.Types
import Network.AWS.DataSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | DescribeLocationNfsRequest
--
--
--
-- /See:/ 'describeLocationNfs' smart constructor.
newtype DescribeLocationNfs = DescribeLocationNfs'
  { _dlnLocationARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLocationNfs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlnLocationARN' - The Amazon resource Name (ARN) of the NFS location to describe.
describeLocationNfs
    :: Text -- ^ 'dlnLocationARN'
    -> DescribeLocationNfs
describeLocationNfs pLocationARN_ =
  DescribeLocationNfs' {_dlnLocationARN = pLocationARN_}


-- | The Amazon resource Name (ARN) of the NFS location to describe.
dlnLocationARN :: Lens' DescribeLocationNfs Text
dlnLocationARN = lens _dlnLocationARN (\ s a -> s{_dlnLocationARN = a})

instance AWSRequest DescribeLocationNfs where
        type Rs DescribeLocationNfs =
             DescribeLocationNfsResponse
        request = postJSON dataSync
        response
          = receiveJSON
              (\ s h x ->
                 DescribeLocationNfsResponse' <$>
                   (x .?> "CreationTime") <*> (x .?> "LocationUri") <*>
                     (x .?> "OnPremConfig")
                     <*> (x .?> "LocationArn")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeLocationNfs where

instance NFData DescribeLocationNfs where

instance ToHeaders DescribeLocationNfs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("FmrsService.DescribeLocationNfs" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeLocationNfs where
        toJSON DescribeLocationNfs'{..}
          = object
              (catMaybes [Just ("LocationArn" .= _dlnLocationARN)])

instance ToPath DescribeLocationNfs where
        toPath = const "/"

instance ToQuery DescribeLocationNfs where
        toQuery = const mempty

-- | DescribeLocationNfsResponse
--
--
--
-- /See:/ 'describeLocationNfsResponse' smart constructor.
data DescribeLocationNfsResponse = DescribeLocationNfsResponse'
  { _dlnrsCreationTime :: !(Maybe POSIX)
  , _dlnrsLocationURI :: !(Maybe Text)
  , _dlnrsOnPremConfig :: !(Maybe OnPremConfig)
  , _dlnrsLocationARN :: !(Maybe Text)
  , _dlnrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLocationNfsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlnrsCreationTime' - The time that the NFS location was created.
--
-- * 'dlnrsLocationURI' - The URL of the source NFS location that was described.
--
-- * 'dlnrsOnPremConfig' - Undocumented member.
--
-- * 'dlnrsLocationARN' - The Amazon resource Name (ARN) of the NFS location that was described.
--
-- * 'dlnrsResponseStatus' - -- | The response status code.
describeLocationNfsResponse
    :: Int -- ^ 'dlnrsResponseStatus'
    -> DescribeLocationNfsResponse
describeLocationNfsResponse pResponseStatus_ =
  DescribeLocationNfsResponse'
    { _dlnrsCreationTime = Nothing
    , _dlnrsLocationURI = Nothing
    , _dlnrsOnPremConfig = Nothing
    , _dlnrsLocationARN = Nothing
    , _dlnrsResponseStatus = pResponseStatus_
    }


-- | The time that the NFS location was created.
dlnrsCreationTime :: Lens' DescribeLocationNfsResponse (Maybe UTCTime)
dlnrsCreationTime = lens _dlnrsCreationTime (\ s a -> s{_dlnrsCreationTime = a}) . mapping _Time

-- | The URL of the source NFS location that was described.
dlnrsLocationURI :: Lens' DescribeLocationNfsResponse (Maybe Text)
dlnrsLocationURI = lens _dlnrsLocationURI (\ s a -> s{_dlnrsLocationURI = a})

-- | Undocumented member.
dlnrsOnPremConfig :: Lens' DescribeLocationNfsResponse (Maybe OnPremConfig)
dlnrsOnPremConfig = lens _dlnrsOnPremConfig (\ s a -> s{_dlnrsOnPremConfig = a})

-- | The Amazon resource Name (ARN) of the NFS location that was described.
dlnrsLocationARN :: Lens' DescribeLocationNfsResponse (Maybe Text)
dlnrsLocationARN = lens _dlnrsLocationARN (\ s a -> s{_dlnrsLocationARN = a})

-- | -- | The response status code.
dlnrsResponseStatus :: Lens' DescribeLocationNfsResponse Int
dlnrsResponseStatus = lens _dlnrsResponseStatus (\ s a -> s{_dlnrsResponseStatus = a})

instance NFData DescribeLocationNfsResponse where
