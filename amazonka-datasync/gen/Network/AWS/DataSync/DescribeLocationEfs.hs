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
-- Module      : Network.AWS.DataSync.DescribeLocationEfs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata, such as the path information about an Amazon EFS location.
--
--
module Network.AWS.DataSync.DescribeLocationEfs
    (
    -- * Creating a Request
      describeLocationEfs
    , DescribeLocationEfs
    -- * Request Lenses
    , dleLocationARN

    -- * Destructuring the Response
    , describeLocationEfsResponse
    , DescribeLocationEfsResponse
    -- * Response Lenses
    , dlersCreationTime
    , dlersLocationURI
    , dlersLocationARN
    , dlersEC2Config
    , dlersResponseStatus
    ) where

import Network.AWS.DataSync.Types
import Network.AWS.DataSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | DescribeLocationEfsRequest
--
--
--
-- /See:/ 'describeLocationEfs' smart constructor.
newtype DescribeLocationEfs = DescribeLocationEfs'
  { _dleLocationARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLocationEfs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dleLocationARN' - The Amazon Resource Name (ARN) of the EFS location to describe.
describeLocationEfs
    :: Text -- ^ 'dleLocationARN'
    -> DescribeLocationEfs
describeLocationEfs pLocationARN_ =
  DescribeLocationEfs' {_dleLocationARN = pLocationARN_}


-- | The Amazon Resource Name (ARN) of the EFS location to describe.
dleLocationARN :: Lens' DescribeLocationEfs Text
dleLocationARN = lens _dleLocationARN (\ s a -> s{_dleLocationARN = a})

instance AWSRequest DescribeLocationEfs where
        type Rs DescribeLocationEfs =
             DescribeLocationEfsResponse
        request = postJSON dataSync
        response
          = receiveJSON
              (\ s h x ->
                 DescribeLocationEfsResponse' <$>
                   (x .?> "CreationTime") <*> (x .?> "LocationUri") <*>
                     (x .?> "LocationArn")
                     <*> (x .?> "Ec2Config")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeLocationEfs where

instance NFData DescribeLocationEfs where

instance ToHeaders DescribeLocationEfs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("FmrsService.DescribeLocationEfs" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeLocationEfs where
        toJSON DescribeLocationEfs'{..}
          = object
              (catMaybes [Just ("LocationArn" .= _dleLocationARN)])

instance ToPath DescribeLocationEfs where
        toPath = const "/"

instance ToQuery DescribeLocationEfs where
        toQuery = const mempty

-- | DescribeLocationEfsResponse
--
--
--
-- /See:/ 'describeLocationEfsResponse' smart constructor.
data DescribeLocationEfsResponse = DescribeLocationEfsResponse'
  { _dlersCreationTime :: !(Maybe POSIX)
  , _dlersLocationURI :: !(Maybe Text)
  , _dlersLocationARN :: !(Maybe Text)
  , _dlersEC2Config :: !(Maybe EC2Config)
  , _dlersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLocationEfsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlersCreationTime' - The time that the EFS location was created.
--
-- * 'dlersLocationURI' - The URL of the EFS location that was described.
--
-- * 'dlersLocationARN' - The Amazon resource Name (ARN) of the EFS location that was described.
--
-- * 'dlersEC2Config' - Undocumented member.
--
-- * 'dlersResponseStatus' - -- | The response status code.
describeLocationEfsResponse
    :: Int -- ^ 'dlersResponseStatus'
    -> DescribeLocationEfsResponse
describeLocationEfsResponse pResponseStatus_ =
  DescribeLocationEfsResponse'
    { _dlersCreationTime = Nothing
    , _dlersLocationURI = Nothing
    , _dlersLocationARN = Nothing
    , _dlersEC2Config = Nothing
    , _dlersResponseStatus = pResponseStatus_
    }


-- | The time that the EFS location was created.
dlersCreationTime :: Lens' DescribeLocationEfsResponse (Maybe UTCTime)
dlersCreationTime = lens _dlersCreationTime (\ s a -> s{_dlersCreationTime = a}) . mapping _Time

-- | The URL of the EFS location that was described.
dlersLocationURI :: Lens' DescribeLocationEfsResponse (Maybe Text)
dlersLocationURI = lens _dlersLocationURI (\ s a -> s{_dlersLocationURI = a})

-- | The Amazon resource Name (ARN) of the EFS location that was described.
dlersLocationARN :: Lens' DescribeLocationEfsResponse (Maybe Text)
dlersLocationARN = lens _dlersLocationARN (\ s a -> s{_dlersLocationARN = a})

-- | Undocumented member.
dlersEC2Config :: Lens' DescribeLocationEfsResponse (Maybe EC2Config)
dlersEC2Config = lens _dlersEC2Config (\ s a -> s{_dlersEC2Config = a})

-- | -- | The response status code.
dlersResponseStatus :: Lens' DescribeLocationEfsResponse Int
dlersResponseStatus = lens _dlersResponseStatus (\ s a -> s{_dlersResponseStatus = a})

instance NFData DescribeLocationEfsResponse where
