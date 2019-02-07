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
-- Module      : Network.AWS.Backup.DescribeProtectedResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a saved resource, including the last time it was backed-up, its Amazon Resource Name (ARN), and the AWS service type of the saved resource.
--
--
module Network.AWS.Backup.DescribeProtectedResource
    (
    -- * Creating a Request
      describeProtectedResource
    , DescribeProtectedResource
    -- * Request Lenses
    , dprResourceARN

    -- * Destructuring the Response
    , describeProtectedResourceResponse
    , DescribeProtectedResourceResponse
    -- * Response Lenses
    , dprrsResourceType
    , dprrsLastBackupTime
    , dprrsResourceARN
    , dprrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeProtectedResource' smart constructor.
newtype DescribeProtectedResource = DescribeProtectedResource'
  { _dprResourceARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeProtectedResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dprResourceARN' - An Amazon Resource Name (ARN) that uniquely identifies a resource. The format of the ARN depends on the resource type.
describeProtectedResource
    :: Text -- ^ 'dprResourceARN'
    -> DescribeProtectedResource
describeProtectedResource pResourceARN_ =
  DescribeProtectedResource' {_dprResourceARN = pResourceARN_}


-- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The format of the ARN depends on the resource type.
dprResourceARN :: Lens' DescribeProtectedResource Text
dprResourceARN = lens _dprResourceARN (\ s a -> s{_dprResourceARN = a})

instance AWSRequest DescribeProtectedResource where
        type Rs DescribeProtectedResource =
             DescribeProtectedResourceResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 DescribeProtectedResourceResponse' <$>
                   (x .?> "ResourceType") <*> (x .?> "LastBackupTime")
                     <*> (x .?> "ResourceArn")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeProtectedResource where

instance NFData DescribeProtectedResource where

instance ToHeaders DescribeProtectedResource where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeProtectedResource where
        toPath DescribeProtectedResource'{..}
          = mconcat ["/resources/", toBS _dprResourceARN]

instance ToQuery DescribeProtectedResource where
        toQuery = const mempty

-- | /See:/ 'describeProtectedResourceResponse' smart constructor.
data DescribeProtectedResourceResponse = DescribeProtectedResourceResponse'
  { _dprrsResourceType :: !(Maybe Text)
  , _dprrsLastBackupTime :: !(Maybe POSIX)
  , _dprrsResourceARN :: !(Maybe Text)
  , _dprrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeProtectedResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dprrsResourceType' - The type of AWS resource saved as a recovery point; for example, an EBS volume or an Amazon RDS database.
--
-- * 'dprrsLastBackupTime' - The date and time that a resource was last backed up, in Unix format and Coordinated Universal Time (UTC). The value of @LastBackupTime@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'dprrsResourceARN' - An ARN that uniquely identifies a resource. The format of the ARN depends on the resource type.
--
-- * 'dprrsResponseStatus' - -- | The response status code.
describeProtectedResourceResponse
    :: Int -- ^ 'dprrsResponseStatus'
    -> DescribeProtectedResourceResponse
describeProtectedResourceResponse pResponseStatus_ =
  DescribeProtectedResourceResponse'
    { _dprrsResourceType = Nothing
    , _dprrsLastBackupTime = Nothing
    , _dprrsResourceARN = Nothing
    , _dprrsResponseStatus = pResponseStatus_
    }


-- | The type of AWS resource saved as a recovery point; for example, an EBS volume or an Amazon RDS database.
dprrsResourceType :: Lens' DescribeProtectedResourceResponse (Maybe Text)
dprrsResourceType = lens _dprrsResourceType (\ s a -> s{_dprrsResourceType = a})

-- | The date and time that a resource was last backed up, in Unix format and Coordinated Universal Time (UTC). The value of @LastBackupTime@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
dprrsLastBackupTime :: Lens' DescribeProtectedResourceResponse (Maybe UTCTime)
dprrsLastBackupTime = lens _dprrsLastBackupTime (\ s a -> s{_dprrsLastBackupTime = a}) . mapping _Time

-- | An ARN that uniquely identifies a resource. The format of the ARN depends on the resource type.
dprrsResourceARN :: Lens' DescribeProtectedResourceResponse (Maybe Text)
dprrsResourceARN = lens _dprrsResourceARN (\ s a -> s{_dprrsResourceARN = a})

-- | -- | The response status code.
dprrsResponseStatus :: Lens' DescribeProtectedResourceResponse Int
dprrsResponseStatus = lens _dprrsResponseStatus (\ s a -> s{_dprrsResponseStatus = a})

instance NFData DescribeProtectedResourceResponse
         where
