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
-- Module      : Network.AWS.DataSync.CreateLocationS3
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint for an Amazon S3 bucket.
--
--
-- For AWS DataSync to access a destination S3 bucket, it needs an AWS Identity and Access Management (IAM) role that has the required permissions. You can set up the required permissions by creating an IAM policy that grants the required permissions and attaching the policy to the role. An example of such a policy is shown in the examples section. For more information, see <https://docs.aws.amazon.com/sync-service/latest/userguide/configuring-s3-locations.html Configuring Amazon S3 Location Settings> in the /AWS DataSync User Guide/ .
--
module Network.AWS.DataSync.CreateLocationS3
    (
    -- * Creating a Request
      createLocationS3
    , CreateLocationS3
    -- * Request Lenses
    , clsTags
    , clsSubdirectory
    , clsS3BucketARN
    , clsS3Config

    -- * Destructuring the Response
    , createLocationS3Response
    , CreateLocationS3Response
    -- * Response Lenses
    , clsrsLocationARN
    , clsrsResponseStatus
    ) where

import Network.AWS.DataSync.Types
import Network.AWS.DataSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | CreateLocationS3Request
--
--
--
-- /See:/ 'createLocationS3' smart constructor.
data CreateLocationS3 = CreateLocationS3'
  { _clsTags :: !(Maybe [TagListEntry])
  , _clsSubdirectory :: !Text
  , _clsS3BucketARN :: !Text
  , _clsS3Config :: !S3Config
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLocationS3' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clsTags' - The key-value pair that represents the tag that you want to add to the location. The value can be an empty string. We recommend using tags to name your resources.
--
-- * 'clsSubdirectory' - A subdirectory in the Amazon S3 bucket. This subdirectory in Amazon S3 is used to read data from the S3 source location or write data to the S3 destination.
--
-- * 'clsS3BucketARN' - The Amazon Resource Name (ARN) of the Amazon S3 bucket.
--
-- * 'clsS3Config' - Undocumented member.
createLocationS3
    :: Text -- ^ 'clsSubdirectory'
    -> Text -- ^ 'clsS3BucketARN'
    -> S3Config -- ^ 'clsS3Config'
    -> CreateLocationS3
createLocationS3 pSubdirectory_ pS3BucketARN_ pS3Config_ =
  CreateLocationS3'
    { _clsTags = Nothing
    , _clsSubdirectory = pSubdirectory_
    , _clsS3BucketARN = pS3BucketARN_
    , _clsS3Config = pS3Config_
    }


-- | The key-value pair that represents the tag that you want to add to the location. The value can be an empty string. We recommend using tags to name your resources.
clsTags :: Lens' CreateLocationS3 [TagListEntry]
clsTags = lens _clsTags (\ s a -> s{_clsTags = a}) . _Default . _Coerce

-- | A subdirectory in the Amazon S3 bucket. This subdirectory in Amazon S3 is used to read data from the S3 source location or write data to the S3 destination.
clsSubdirectory :: Lens' CreateLocationS3 Text
clsSubdirectory = lens _clsSubdirectory (\ s a -> s{_clsSubdirectory = a})

-- | The Amazon Resource Name (ARN) of the Amazon S3 bucket.
clsS3BucketARN :: Lens' CreateLocationS3 Text
clsS3BucketARN = lens _clsS3BucketARN (\ s a -> s{_clsS3BucketARN = a})

-- | Undocumented member.
clsS3Config :: Lens' CreateLocationS3 S3Config
clsS3Config = lens _clsS3Config (\ s a -> s{_clsS3Config = a})

instance AWSRequest CreateLocationS3 where
        type Rs CreateLocationS3 = CreateLocationS3Response
        request = postJSON dataSync
        response
          = receiveJSON
              (\ s h x ->
                 CreateLocationS3Response' <$>
                   (x .?> "LocationArn") <*> (pure (fromEnum s)))

instance Hashable CreateLocationS3 where

instance NFData CreateLocationS3 where

instance ToHeaders CreateLocationS3 where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("FmrsService.CreateLocationS3" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateLocationS3 where
        toJSON CreateLocationS3'{..}
          = object
              (catMaybes
                 [("Tags" .=) <$> _clsTags,
                  Just ("Subdirectory" .= _clsSubdirectory),
                  Just ("S3BucketArn" .= _clsS3BucketARN),
                  Just ("S3Config" .= _clsS3Config)])

instance ToPath CreateLocationS3 where
        toPath = const "/"

instance ToQuery CreateLocationS3 where
        toQuery = const mempty

-- | CreateLocationS3Response
--
--
--
-- /See:/ 'createLocationS3Response' smart constructor.
data CreateLocationS3Response = CreateLocationS3Response'
  { _clsrsLocationARN :: !(Maybe Text)
  , _clsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLocationS3Response' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clsrsLocationARN' - The Amazon Resource Name (ARN) of the source Amazon S3 bucket location that is created.
--
-- * 'clsrsResponseStatus' - -- | The response status code.
createLocationS3Response
    :: Int -- ^ 'clsrsResponseStatus'
    -> CreateLocationS3Response
createLocationS3Response pResponseStatus_ =
  CreateLocationS3Response'
    {_clsrsLocationARN = Nothing, _clsrsResponseStatus = pResponseStatus_}


-- | The Amazon Resource Name (ARN) of the source Amazon S3 bucket location that is created.
clsrsLocationARN :: Lens' CreateLocationS3Response (Maybe Text)
clsrsLocationARN = lens _clsrsLocationARN (\ s a -> s{_clsrsLocationARN = a})

-- | -- | The response status code.
clsrsResponseStatus :: Lens' CreateLocationS3Response Int
clsrsResponseStatus = lens _clsrsResponseStatus (\ s a -> s{_clsrsResponseStatus = a})

instance NFData CreateLocationS3Response where
