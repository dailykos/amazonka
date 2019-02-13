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
-- Module      : Network.AWS.Config.DescribeRetentionConfigurations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of one or more retention configurations. If the retention configuration name is not specified, this action returns the details for all the retention configurations for that account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeRetentionConfigurations
    (
    -- * Creating a Request
      describeRetentionConfigurations
    , DescribeRetentionConfigurations
    -- * Request Lenses
    , drcRetentionConfigurationNames
    , drcNextToken

    -- * Destructuring the Response
    , describeRetentionConfigurationsResponse
    , DescribeRetentionConfigurationsResponse
    -- * Response Lenses
    , drcrsRetentionConfigurations
    , drcrsNextToken
    , drcrsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeRetentionConfigurations' smart constructor.
data DescribeRetentionConfigurations = DescribeRetentionConfigurations'
  { _drcRetentionConfigurationNames :: !(Maybe [Text])
  , _drcNextToken :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRetentionConfigurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drcRetentionConfigurationNames' - A list of names of retention configurations for which you want details. If you do not specify a name, AWS Config returns details for all the retention configurations for that account.
--
-- * 'drcNextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response. 
describeRetentionConfigurations
    :: DescribeRetentionConfigurations
describeRetentionConfigurations =
  DescribeRetentionConfigurations'
    {_drcRetentionConfigurationNames = Nothing, _drcNextToken = Nothing}


-- | A list of names of retention configurations for which you want details. If you do not specify a name, AWS Config returns details for all the retention configurations for that account.
drcRetentionConfigurationNames :: Lens' DescribeRetentionConfigurations [Text]
drcRetentionConfigurationNames = lens _drcRetentionConfigurationNames (\ s a -> s{_drcRetentionConfigurationNames = a}) . _Default . _Coerce

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response. 
drcNextToken :: Lens' DescribeRetentionConfigurations (Maybe Text)
drcNextToken = lens _drcNextToken (\ s a -> s{_drcNextToken = a})

instance AWSPager DescribeRetentionConfigurations
         where
        page rq rs
          | stop (rs ^. drcrsNextToken) = Nothing
          | stop (rs ^. drcrsRetentionConfigurations) = Nothing
          | otherwise =
            Just $ rq & drcNextToken .~ rs ^. drcrsNextToken

instance AWSRequest DescribeRetentionConfigurations
         where
        type Rs DescribeRetentionConfigurations =
             DescribeRetentionConfigurationsResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 DescribeRetentionConfigurationsResponse' <$>
                   (x .?> "RetentionConfigurations" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeRetentionConfigurations
         where

instance NFData DescribeRetentionConfigurations where

instance ToHeaders DescribeRetentionConfigurations
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DescribeRetentionConfigurations"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeRetentionConfigurations where
        toJSON DescribeRetentionConfigurations'{..}
          = object
              (catMaybes
                 [("RetentionConfigurationNames" .=) <$>
                    _drcRetentionConfigurationNames,
                  ("NextToken" .=) <$> _drcNextToken])

instance ToPath DescribeRetentionConfigurations where
        toPath = const "/"

instance ToQuery DescribeRetentionConfigurations
         where
        toQuery = const mempty

-- | /See:/ 'describeRetentionConfigurationsResponse' smart constructor.
data DescribeRetentionConfigurationsResponse = DescribeRetentionConfigurationsResponse'
  { _drcrsRetentionConfigurations :: !(Maybe [RetentionConfiguration])
  , _drcrsNextToken :: !(Maybe Text)
  , _drcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRetentionConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drcrsRetentionConfigurations' - Returns a retention configuration object.
--
-- * 'drcrsNextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response. 
--
-- * 'drcrsResponseStatus' - -- | The response status code.
describeRetentionConfigurationsResponse
    :: Int -- ^ 'drcrsResponseStatus'
    -> DescribeRetentionConfigurationsResponse
describeRetentionConfigurationsResponse pResponseStatus_ =
  DescribeRetentionConfigurationsResponse'
    { _drcrsRetentionConfigurations = Nothing
    , _drcrsNextToken = Nothing
    , _drcrsResponseStatus = pResponseStatus_
    }


-- | Returns a retention configuration object.
drcrsRetentionConfigurations :: Lens' DescribeRetentionConfigurationsResponse [RetentionConfiguration]
drcrsRetentionConfigurations = lens _drcrsRetentionConfigurations (\ s a -> s{_drcrsRetentionConfigurations = a}) . _Default . _Coerce

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response. 
drcrsNextToken :: Lens' DescribeRetentionConfigurationsResponse (Maybe Text)
drcrsNextToken = lens _drcrsNextToken (\ s a -> s{_drcrsNextToken = a})

-- | -- | The response status code.
drcrsResponseStatus :: Lens' DescribeRetentionConfigurationsResponse Int
drcrsResponseStatus = lens _drcrsResponseStatus (\ s a -> s{_drcrsResponseStatus = a})

instance NFData
           DescribeRetentionConfigurationsResponse
         where
