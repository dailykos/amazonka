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
-- Module      : Network.AWS.DocDB.DescribeDBClusters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about provisioned Amazon DocumentDB DB clusters. This API operation supports pagination.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DocDB.DescribeDBClusters
    (
    -- * Creating a Request
      describeDBClusters
    , DescribeDBClusters
    -- * Request Lenses
    , ddcDBClusterIdentifier
    , ddcFilters
    , ddcMarker
    , ddcMaxRecords

    -- * Destructuring the Response
    , describeDBClustersResponse
    , DescribeDBClustersResponse
    -- * Response Lenses
    , ddcrsDBClusters
    , ddcrsMarker
    , ddcrsResponseStatus
    ) where

import Network.AWS.DocDB.Types
import Network.AWS.DocDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input to 'DescribeDBClusters' .
--
--
--
-- /See:/ 'describeDBClusters' smart constructor.
data DescribeDBClusters = DescribeDBClusters'
  { _ddcDBClusterIdentifier :: !(Maybe Text)
  , _ddcFilters :: !(Maybe [Filter])
  , _ddcMarker :: !(Maybe Text)
  , _ddcMaxRecords :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBClusters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcDBClusterIdentifier' - The user-provided DB cluster identifier. If this parameter is specified, information from only the specific DB cluster is returned. This parameter isn't case sensitive. Constraints:     * If provided, must match an existing @DBClusterIdentifier@ .
--
-- * 'ddcFilters' - A filter that specifies one or more DB clusters to describe. Supported filters:     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list only includes information about the DB clusters identified by these ARNs.
--
-- * 'ddcMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddcMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token (marker) is included in the response so that the remaining results can be retrieved. Default: 100 Constraints: Minimum 20, maximum 100.
describeDBClusters
    :: DescribeDBClusters
describeDBClusters =
  DescribeDBClusters'
    { _ddcDBClusterIdentifier = Nothing
    , _ddcFilters = Nothing
    , _ddcMarker = Nothing
    , _ddcMaxRecords = Nothing
    }


-- | The user-provided DB cluster identifier. If this parameter is specified, information from only the specific DB cluster is returned. This parameter isn't case sensitive. Constraints:     * If provided, must match an existing @DBClusterIdentifier@ .
ddcDBClusterIdentifier :: Lens' DescribeDBClusters (Maybe Text)
ddcDBClusterIdentifier = lens _ddcDBClusterIdentifier (\ s a -> s{_ddcDBClusterIdentifier = a})

-- | A filter that specifies one or more DB clusters to describe. Supported filters:     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list only includes information about the DB clusters identified by these ARNs.
ddcFilters :: Lens' DescribeDBClusters [Filter]
ddcFilters = lens _ddcFilters (\ s a -> s{_ddcFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddcMarker :: Lens' DescribeDBClusters (Maybe Text)
ddcMarker = lens _ddcMarker (\ s a -> s{_ddcMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token (marker) is included in the response so that the remaining results can be retrieved. Default: 100 Constraints: Minimum 20, maximum 100.
ddcMaxRecords :: Lens' DescribeDBClusters (Maybe Int)
ddcMaxRecords = lens _ddcMaxRecords (\ s a -> s{_ddcMaxRecords = a})

instance AWSPager DescribeDBClusters where
        page rq rs
          | stop (rs ^. ddcrsMarker) = Nothing
          | stop (rs ^. ddcrsDBClusters) = Nothing
          | otherwise =
            Just $ rq & ddcMarker .~ rs ^. ddcrsMarker

instance AWSRequest DescribeDBClusters where
        type Rs DescribeDBClusters =
             DescribeDBClustersResponse
        request = postQuery docDB
        response
          = receiveXMLWrapper "DescribeDBClustersResult"
              (\ s h x ->
                 DescribeDBClustersResponse' <$>
                   (x .@? "DBClusters" .!@ mempty >>=
                      may (parseXMLList "DBCluster"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeDBClusters where

instance NFData DescribeDBClusters where

instance ToHeaders DescribeDBClusters where
        toHeaders = const mempty

instance ToPath DescribeDBClusters where
        toPath = const "/"

instance ToQuery DescribeDBClusters where
        toQuery DescribeDBClusters'{..}
          = mconcat
              ["Action" =: ("DescribeDBClusters" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBClusterIdentifier" =: _ddcDBClusterIdentifier,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _ddcFilters),
               "Marker" =: _ddcMarker,
               "MaxRecords" =: _ddcMaxRecords]

-- | Represents the output of 'DescribeDBClusters' .
--
--
--
-- /See:/ 'describeDBClustersResponse' smart constructor.
data DescribeDBClustersResponse = DescribeDBClustersResponse'
  { _ddcrsDBClusters :: !(Maybe [DBCluster])
  , _ddcrsMarker :: !(Maybe Text)
  , _ddcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBClustersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcrsDBClusters' - A list of DB clusters.
--
-- * 'ddcrsMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddcrsResponseStatus' - -- | The response status code.
describeDBClustersResponse
    :: Int -- ^ 'ddcrsResponseStatus'
    -> DescribeDBClustersResponse
describeDBClustersResponse pResponseStatus_ =
  DescribeDBClustersResponse'
    { _ddcrsDBClusters = Nothing
    , _ddcrsMarker = Nothing
    , _ddcrsResponseStatus = pResponseStatus_
    }


-- | A list of DB clusters.
ddcrsDBClusters :: Lens' DescribeDBClustersResponse [DBCluster]
ddcrsDBClusters = lens _ddcrsDBClusters (\ s a -> s{_ddcrsDBClusters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddcrsMarker :: Lens' DescribeDBClustersResponse (Maybe Text)
ddcrsMarker = lens _ddcrsMarker (\ s a -> s{_ddcrsMarker = a})

-- | -- | The response status code.
ddcrsResponseStatus :: Lens' DescribeDBClustersResponse Int
ddcrsResponseStatus = lens _ddcrsResponseStatus (\ s a -> s{_ddcrsResponseStatus = a})

instance NFData DescribeDBClustersResponse where
