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
-- Module      : Network.AWS.DataSync.CreateLocationEfs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint for an Amazon EFS file system.
--
--
module Network.AWS.DataSync.CreateLocationEfs
    (
    -- * Creating a Request
      createLocationEfs
    , CreateLocationEfs
    -- * Request Lenses
    , cleTags
    , cleSubdirectory
    , cleEfsFilesystemARN
    , cleEC2Config

    -- * Destructuring the Response
    , createLocationEfsResponse
    , CreateLocationEfsResponse
    -- * Response Lenses
    , clersLocationARN
    , clersResponseStatus
    ) where

import Network.AWS.DataSync.Types
import Network.AWS.DataSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | CreateLocationEfsRequest
--
--
--
-- /See:/ 'createLocationEfs' smart constructor.
data CreateLocationEfs = CreateLocationEfs'
  { _cleTags :: !(Maybe [TagListEntry])
  , _cleSubdirectory :: !Text
  , _cleEfsFilesystemARN :: !Text
  , _cleEC2Config :: !EC2Config
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLocationEfs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cleTags' - The key-value pair that represents a tag that you want to add to the resource. The value can be an empty string. This value helps you manage, filter, and search for your resources. We recommend that you create a name tag for your location.
--
-- * 'cleSubdirectory' - A subdirectory in the location’s path. This subdirectory in the EFS file system is used to read data from the EFS source location or write data to the EFS destination. By default, AWS DataSync uses the root directory.
--
-- * 'cleEfsFilesystemARN' - The Amazon Resource Name (ARN) for the Amazon EFS file system.
--
-- * 'cleEC2Config' - The subnet and security group that the Amazon EFS file system uses.
createLocationEfs
    :: Text -- ^ 'cleSubdirectory'
    -> Text -- ^ 'cleEfsFilesystemARN'
    -> EC2Config -- ^ 'cleEC2Config'
    -> CreateLocationEfs
createLocationEfs pSubdirectory_ pEfsFilesystemARN_ pEC2Config_ =
  CreateLocationEfs'
    { _cleTags = Nothing
    , _cleSubdirectory = pSubdirectory_
    , _cleEfsFilesystemARN = pEfsFilesystemARN_
    , _cleEC2Config = pEC2Config_
    }


-- | The key-value pair that represents a tag that you want to add to the resource. The value can be an empty string. This value helps you manage, filter, and search for your resources. We recommend that you create a name tag for your location.
cleTags :: Lens' CreateLocationEfs [TagListEntry]
cleTags = lens _cleTags (\ s a -> s{_cleTags = a}) . _Default . _Coerce

-- | A subdirectory in the location’s path. This subdirectory in the EFS file system is used to read data from the EFS source location or write data to the EFS destination. By default, AWS DataSync uses the root directory.
cleSubdirectory :: Lens' CreateLocationEfs Text
cleSubdirectory = lens _cleSubdirectory (\ s a -> s{_cleSubdirectory = a})

-- | The Amazon Resource Name (ARN) for the Amazon EFS file system.
cleEfsFilesystemARN :: Lens' CreateLocationEfs Text
cleEfsFilesystemARN = lens _cleEfsFilesystemARN (\ s a -> s{_cleEfsFilesystemARN = a})

-- | The subnet and security group that the Amazon EFS file system uses.
cleEC2Config :: Lens' CreateLocationEfs EC2Config
cleEC2Config = lens _cleEC2Config (\ s a -> s{_cleEC2Config = a})

instance AWSRequest CreateLocationEfs where
        type Rs CreateLocationEfs = CreateLocationEfsResponse
        request = postJSON dataSync
        response
          = receiveJSON
              (\ s h x ->
                 CreateLocationEfsResponse' <$>
                   (x .?> "LocationArn") <*> (pure (fromEnum s)))

instance Hashable CreateLocationEfs where

instance NFData CreateLocationEfs where

instance ToHeaders CreateLocationEfs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("FmrsService.CreateLocationEfs" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateLocationEfs where
        toJSON CreateLocationEfs'{..}
          = object
              (catMaybes
                 [("Tags" .=) <$> _cleTags,
                  Just ("Subdirectory" .= _cleSubdirectory),
                  Just ("EfsFilesystemArn" .= _cleEfsFilesystemARN),
                  Just ("Ec2Config" .= _cleEC2Config)])

instance ToPath CreateLocationEfs where
        toPath = const "/"

instance ToQuery CreateLocationEfs where
        toQuery = const mempty

-- | CreateLocationEfs
--
--
--
-- /See:/ 'createLocationEfsResponse' smart constructor.
data CreateLocationEfsResponse = CreateLocationEfsResponse'
  { _clersLocationARN :: !(Maybe Text)
  , _clersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLocationEfsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clersLocationARN' - The Amazon Resource Name (ARN) of the Amazon EFS file system location that is created.
--
-- * 'clersResponseStatus' - -- | The response status code.
createLocationEfsResponse
    :: Int -- ^ 'clersResponseStatus'
    -> CreateLocationEfsResponse
createLocationEfsResponse pResponseStatus_ =
  CreateLocationEfsResponse'
    {_clersLocationARN = Nothing, _clersResponseStatus = pResponseStatus_}


-- | The Amazon Resource Name (ARN) of the Amazon EFS file system location that is created.
clersLocationARN :: Lens' CreateLocationEfsResponse (Maybe Text)
clersLocationARN = lens _clersLocationARN (\ s a -> s{_clersLocationARN = a})

-- | -- | The response status code.
clersResponseStatus :: Lens' CreateLocationEfsResponse Int
clersResponseStatus = lens _clersResponseStatus (\ s a -> s{_clersResponseStatus = a})

instance NFData CreateLocationEfsResponse where
