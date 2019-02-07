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
-- Module      : Network.AWS.DataSync.DescribeLocationS3
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata, such as bucket name, about an Amazon S3 bucket location.
--
--
module Network.AWS.DataSync.DescribeLocationS3
    (
    -- * Creating a Request
      describeLocationS3
    , DescribeLocationS3
    -- * Request Lenses
    , dlsLocationARN

    -- * Destructuring the Response
    , describeLocationS3Response
    , DescribeLocationS3Response
    -- * Response Lenses
    , dlsrsCreationTime
    , dlsrsLocationURI
    , dlsrsS3Config
    , dlsrsLocationARN
    , dlsrsResponseStatus
    ) where

import Network.AWS.DataSync.Types
import Network.AWS.DataSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | DescribeLocationS3Request
--
--
--
-- /See:/ 'describeLocationS3' smart constructor.
newtype DescribeLocationS3 = DescribeLocationS3'
  { _dlsLocationARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLocationS3' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlsLocationARN' - The Amazon Resource Name (ARN) of the Amazon S3 bucket location to describe.
describeLocationS3
    :: Text -- ^ 'dlsLocationARN'
    -> DescribeLocationS3
describeLocationS3 pLocationARN_ =
  DescribeLocationS3' {_dlsLocationARN = pLocationARN_}


-- | The Amazon Resource Name (ARN) of the Amazon S3 bucket location to describe.
dlsLocationARN :: Lens' DescribeLocationS3 Text
dlsLocationARN = lens _dlsLocationARN (\ s a -> s{_dlsLocationARN = a})

instance AWSRequest DescribeLocationS3 where
        type Rs DescribeLocationS3 =
             DescribeLocationS3Response
        request = postJSON dataSync
        response
          = receiveJSON
              (\ s h x ->
                 DescribeLocationS3Response' <$>
                   (x .?> "CreationTime") <*> (x .?> "LocationUri") <*>
                     (x .?> "S3Config")
                     <*> (x .?> "LocationArn")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeLocationS3 where

instance NFData DescribeLocationS3 where

instance ToHeaders DescribeLocationS3 where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("FmrsService.DescribeLocationS3" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeLocationS3 where
        toJSON DescribeLocationS3'{..}
          = object
              (catMaybes [Just ("LocationArn" .= _dlsLocationARN)])

instance ToPath DescribeLocationS3 where
        toPath = const "/"

instance ToQuery DescribeLocationS3 where
        toQuery = const mempty

-- | DescribeLocationS3Response
--
--
--
-- /See:/ 'describeLocationS3Response' smart constructor.
data DescribeLocationS3Response = DescribeLocationS3Response'
  { _dlsrsCreationTime :: !(Maybe POSIX)
  , _dlsrsLocationURI :: !(Maybe Text)
  , _dlsrsS3Config :: !(Maybe S3Config)
  , _dlsrsLocationARN :: !(Maybe Text)
  , _dlsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLocationS3Response' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlsrsCreationTime' - The time that the Amazon S3 bucket location was created.
--
-- * 'dlsrsLocationURI' - The URL of the Amazon S3 location that was described.
--
-- * 'dlsrsS3Config' - Undocumented member.
--
-- * 'dlsrsLocationARN' - The Amazon Resource Name (ARN) of the Amazon S3 bucket location.
--
-- * 'dlsrsResponseStatus' - -- | The response status code.
describeLocationS3Response
    :: Int -- ^ 'dlsrsResponseStatus'
    -> DescribeLocationS3Response
describeLocationS3Response pResponseStatus_ =
  DescribeLocationS3Response'
    { _dlsrsCreationTime = Nothing
    , _dlsrsLocationURI = Nothing
    , _dlsrsS3Config = Nothing
    , _dlsrsLocationARN = Nothing
    , _dlsrsResponseStatus = pResponseStatus_
    }


-- | The time that the Amazon S3 bucket location was created.
dlsrsCreationTime :: Lens' DescribeLocationS3Response (Maybe UTCTime)
dlsrsCreationTime = lens _dlsrsCreationTime (\ s a -> s{_dlsrsCreationTime = a}) . mapping _Time

-- | The URL of the Amazon S3 location that was described.
dlsrsLocationURI :: Lens' DescribeLocationS3Response (Maybe Text)
dlsrsLocationURI = lens _dlsrsLocationURI (\ s a -> s{_dlsrsLocationURI = a})

-- | Undocumented member.
dlsrsS3Config :: Lens' DescribeLocationS3Response (Maybe S3Config)
dlsrsS3Config = lens _dlsrsS3Config (\ s a -> s{_dlsrsS3Config = a})

-- | The Amazon Resource Name (ARN) of the Amazon S3 bucket location.
dlsrsLocationARN :: Lens' DescribeLocationS3Response (Maybe Text)
dlsrsLocationARN = lens _dlsrsLocationARN (\ s a -> s{_dlsrsLocationARN = a})

-- | -- | The response status code.
dlsrsResponseStatus :: Lens' DescribeLocationS3Response Int
dlsrsResponseStatus = lens _dlsrsResponseStatus (\ s a -> s{_dlsrsResponseStatus = a})

instance NFData DescribeLocationS3Response where
