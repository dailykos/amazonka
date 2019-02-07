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
-- Module      : Network.AWS.Backup.GetSupportedResourceTypes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the AWS resource types supported by AWS Backup.
--
--
module Network.AWS.Backup.GetSupportedResourceTypes
    (
    -- * Creating a Request
      getSupportedResourceTypes
    , GetSupportedResourceTypes

    -- * Destructuring the Response
    , getSupportedResourceTypesResponse
    , GetSupportedResourceTypesResponse
    -- * Response Lenses
    , gsrtrsResourceTypes
    , gsrtrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSupportedResourceTypes' smart constructor.
data GetSupportedResourceTypes =
  GetSupportedResourceTypes'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSupportedResourceTypes' with the minimum fields required to make a request.
--
getSupportedResourceTypes
    :: GetSupportedResourceTypes
getSupportedResourceTypes = GetSupportedResourceTypes'


instance AWSRequest GetSupportedResourceTypes where
        type Rs GetSupportedResourceTypes =
             GetSupportedResourceTypesResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 GetSupportedResourceTypesResponse' <$>
                   (x .?> "ResourceTypes" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable GetSupportedResourceTypes where

instance NFData GetSupportedResourceTypes where

instance ToHeaders GetSupportedResourceTypes where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetSupportedResourceTypes where
        toPath = const "/supported-resource-types"

instance ToQuery GetSupportedResourceTypes where
        toQuery = const mempty

-- | /See:/ 'getSupportedResourceTypesResponse' smart constructor.
data GetSupportedResourceTypesResponse = GetSupportedResourceTypesResponse'
  { _gsrtrsResourceTypes :: !(Maybe [Text])
  , _gsrtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSupportedResourceTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsrtrsResourceTypes' - Contains a string with the supported AWS resource types:     * @EBS@ for Amazon Elastic Block Store     * @SGW@ for AWS Storage Gateway     * @RDS@ for Amazon Relational Database Service     * @DDB@ for Amazon DynamoDB     * @EFS@ for Amazon Elastic File System
--
-- * 'gsrtrsResponseStatus' - -- | The response status code.
getSupportedResourceTypesResponse
    :: Int -- ^ 'gsrtrsResponseStatus'
    -> GetSupportedResourceTypesResponse
getSupportedResourceTypesResponse pResponseStatus_ =
  GetSupportedResourceTypesResponse'
    {_gsrtrsResourceTypes = Nothing, _gsrtrsResponseStatus = pResponseStatus_}


-- | Contains a string with the supported AWS resource types:     * @EBS@ for Amazon Elastic Block Store     * @SGW@ for AWS Storage Gateway     * @RDS@ for Amazon Relational Database Service     * @DDB@ for Amazon DynamoDB     * @EFS@ for Amazon Elastic File System
gsrtrsResourceTypes :: Lens' GetSupportedResourceTypesResponse [Text]
gsrtrsResourceTypes = lens _gsrtrsResourceTypes (\ s a -> s{_gsrtrsResourceTypes = a}) . _Default . _Coerce

-- | -- | The response status code.
gsrtrsResponseStatus :: Lens' GetSupportedResourceTypesResponse Int
gsrtrsResponseStatus = lens _gsrtrsResponseStatus (\ s a -> s{_gsrtrsResponseStatus = a})

instance NFData GetSupportedResourceTypesResponse
         where
