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
-- Module      : Network.AWS.DocDB.RestoreDBClusterFromSnapshot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB cluster from a DB snapshot or DB cluster snapshot.
--
--
-- If a DB snapshot is specified, the target DB cluster is created from the source DB snapshot with a default configuration and default security group.
--
-- If a DB cluster snapshot is specified, the target DB cluster is created from the source DB cluster restore point with the same configuration as the original source DB cluster, except that the new DB cluster is created with the default security group.
--
module Network.AWS.DocDB.RestoreDBClusterFromSnapshot
    (
    -- * Creating a Request
      restoreDBClusterFromSnapshot
    , RestoreDBClusterFromSnapshot
    -- * Request Lenses
    , rdcfsEngineVersion
    , rdcfsDBSubnetGroupName
    , rdcfsAvailabilityZones
    , rdcfsKMSKeyId
    , rdcfsVPCSecurityGroupIds
    , rdcfsTags
    , rdcfsPort
    , rdcfsEnableCloudwatchLogsExports
    , rdcfsDBClusterIdentifier
    , rdcfsSnapshotIdentifier
    , rdcfsEngine

    -- * Destructuring the Response
    , restoreDBClusterFromSnapshotResponse
    , RestoreDBClusterFromSnapshotResponse
    -- * Response Lenses
    , rdcfsrsDBCluster
    , rdcfsrsResponseStatus
    ) where

import Network.AWS.DocDB.Types
import Network.AWS.DocDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input to 'RestoreDBClusterFromSnapshot' .
--
--
--
-- /See:/ 'restoreDBClusterFromSnapshot' smart constructor.
data RestoreDBClusterFromSnapshot = RestoreDBClusterFromSnapshot'
  { _rdcfsEngineVersion :: !(Maybe Text)
  , _rdcfsDBSubnetGroupName :: !(Maybe Text)
  , _rdcfsAvailabilityZones :: !(Maybe [Text])
  , _rdcfsKMSKeyId :: !(Maybe Text)
  , _rdcfsVPCSecurityGroupIds :: !(Maybe [Text])
  , _rdcfsTags :: !(Maybe [Tag])
  , _rdcfsPort :: !(Maybe Int)
  , _rdcfsEnableCloudwatchLogsExports :: !(Maybe [Text])
  , _rdcfsDBClusterIdentifier :: !Text
  , _rdcfsSnapshotIdentifier :: !Text
  , _rdcfsEngine :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreDBClusterFromSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdcfsEngineVersion' - The version of the database engine to use for the new DB cluster.
--
-- * 'rdcfsDBSubnetGroupName' - The name of the DB subnet group to use for the new DB cluster. Constraints: If provided, must match the name of an existing @DBSubnetGroup@ . Example: @mySubnetgroup@ 
--
-- * 'rdcfsAvailabilityZones' - Provides the list of Amazon EC2 Availability Zones that instances in the restored DB cluster can be created in.
--
-- * 'rdcfsKMSKeyId' - The AWS KMS key identifier to use when restoring an encrypted DB cluster from a DB snapshot or DB cluster snapshot. The AWS KMS key identifier is the Amazon Resource Name (ARN) for the AWS KMS encryption key. If you are restoring a DB cluster with the same AWS account that owns the AWS KMS encryption key used to encrypt the new DB cluster, then you can use the AWS KMS key alias instead of the ARN for the AWS KMS encryption key. If you do not specify a value for the @KmsKeyId@ parameter, then the following occurs:     * If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@ is encrypted, then the restored DB cluster is encrypted using the AWS KMS key that was used to encrypt the DB snapshot or the DB cluster snapshot.     * If the DB snapshot or the DB cluster snapshot in @SnapshotIdentifier@ is not encrypted, then the restored DB cluster is not encrypted.
--
-- * 'rdcfsVPCSecurityGroupIds' - A list of virtual private cloud (VPC) security groups that the new DB cluster will belong to.
--
-- * 'rdcfsTags' - The tags to be assigned to the restored DB cluster.
--
-- * 'rdcfsPort' - The port number on which the new DB cluster accepts connections. Constraints: Must be a value from @1150@ to @65535@ . Default: The same port as the original DB cluster.
--
-- * 'rdcfsEnableCloudwatchLogsExports' - A list of log types that must be enabled for exporting to Amazon CloudWatch Logs.
--
-- * 'rdcfsDBClusterIdentifier' - The name of the DB cluster to create from the DB snapshot or DB cluster snapshot. This parameter isn't case sensitive. Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens.     * The first character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @my-snapshot-id@ 
--
-- * 'rdcfsSnapshotIdentifier' - The identifier for the DB snapshot or DB cluster snapshot to restore from. You can use either the name or the Amazon Resource Name (ARN) to specify a DB cluster snapshot. However, you can use only the ARN to specify a DB snapshot. Constraints:     * Must match the identifier of an existing snapshot.
--
-- * 'rdcfsEngine' - The database engine to use for the new DB cluster. Default: The same as source. Constraint: Must be compatible with the engine of the source.
restoreDBClusterFromSnapshot
    :: Text -- ^ 'rdcfsDBClusterIdentifier'
    -> Text -- ^ 'rdcfsSnapshotIdentifier'
    -> Text -- ^ 'rdcfsEngine'
    -> RestoreDBClusterFromSnapshot
restoreDBClusterFromSnapshot pDBClusterIdentifier_ pSnapshotIdentifier_ pEngine_ =
  RestoreDBClusterFromSnapshot'
    { _rdcfsEngineVersion = Nothing
    , _rdcfsDBSubnetGroupName = Nothing
    , _rdcfsAvailabilityZones = Nothing
    , _rdcfsKMSKeyId = Nothing
    , _rdcfsVPCSecurityGroupIds = Nothing
    , _rdcfsTags = Nothing
    , _rdcfsPort = Nothing
    , _rdcfsEnableCloudwatchLogsExports = Nothing
    , _rdcfsDBClusterIdentifier = pDBClusterIdentifier_
    , _rdcfsSnapshotIdentifier = pSnapshotIdentifier_
    , _rdcfsEngine = pEngine_
    }


-- | The version of the database engine to use for the new DB cluster.
rdcfsEngineVersion :: Lens' RestoreDBClusterFromSnapshot (Maybe Text)
rdcfsEngineVersion = lens _rdcfsEngineVersion (\ s a -> s{_rdcfsEngineVersion = a})

-- | The name of the DB subnet group to use for the new DB cluster. Constraints: If provided, must match the name of an existing @DBSubnetGroup@ . Example: @mySubnetgroup@ 
rdcfsDBSubnetGroupName :: Lens' RestoreDBClusterFromSnapshot (Maybe Text)
rdcfsDBSubnetGroupName = lens _rdcfsDBSubnetGroupName (\ s a -> s{_rdcfsDBSubnetGroupName = a})

-- | Provides the list of Amazon EC2 Availability Zones that instances in the restored DB cluster can be created in.
rdcfsAvailabilityZones :: Lens' RestoreDBClusterFromSnapshot [Text]
rdcfsAvailabilityZones = lens _rdcfsAvailabilityZones (\ s a -> s{_rdcfsAvailabilityZones = a}) . _Default . _Coerce

-- | The AWS KMS key identifier to use when restoring an encrypted DB cluster from a DB snapshot or DB cluster snapshot. The AWS KMS key identifier is the Amazon Resource Name (ARN) for the AWS KMS encryption key. If you are restoring a DB cluster with the same AWS account that owns the AWS KMS encryption key used to encrypt the new DB cluster, then you can use the AWS KMS key alias instead of the ARN for the AWS KMS encryption key. If you do not specify a value for the @KmsKeyId@ parameter, then the following occurs:     * If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@ is encrypted, then the restored DB cluster is encrypted using the AWS KMS key that was used to encrypt the DB snapshot or the DB cluster snapshot.     * If the DB snapshot or the DB cluster snapshot in @SnapshotIdentifier@ is not encrypted, then the restored DB cluster is not encrypted.
rdcfsKMSKeyId :: Lens' RestoreDBClusterFromSnapshot (Maybe Text)
rdcfsKMSKeyId = lens _rdcfsKMSKeyId (\ s a -> s{_rdcfsKMSKeyId = a})

-- | A list of virtual private cloud (VPC) security groups that the new DB cluster will belong to.
rdcfsVPCSecurityGroupIds :: Lens' RestoreDBClusterFromSnapshot [Text]
rdcfsVPCSecurityGroupIds = lens _rdcfsVPCSecurityGroupIds (\ s a -> s{_rdcfsVPCSecurityGroupIds = a}) . _Default . _Coerce

-- | The tags to be assigned to the restored DB cluster.
rdcfsTags :: Lens' RestoreDBClusterFromSnapshot [Tag]
rdcfsTags = lens _rdcfsTags (\ s a -> s{_rdcfsTags = a}) . _Default . _Coerce

-- | The port number on which the new DB cluster accepts connections. Constraints: Must be a value from @1150@ to @65535@ . Default: The same port as the original DB cluster.
rdcfsPort :: Lens' RestoreDBClusterFromSnapshot (Maybe Int)
rdcfsPort = lens _rdcfsPort (\ s a -> s{_rdcfsPort = a})

-- | A list of log types that must be enabled for exporting to Amazon CloudWatch Logs.
rdcfsEnableCloudwatchLogsExports :: Lens' RestoreDBClusterFromSnapshot [Text]
rdcfsEnableCloudwatchLogsExports = lens _rdcfsEnableCloudwatchLogsExports (\ s a -> s{_rdcfsEnableCloudwatchLogsExports = a}) . _Default . _Coerce

-- | The name of the DB cluster to create from the DB snapshot or DB cluster snapshot. This parameter isn't case sensitive. Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens.     * The first character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @my-snapshot-id@ 
rdcfsDBClusterIdentifier :: Lens' RestoreDBClusterFromSnapshot Text
rdcfsDBClusterIdentifier = lens _rdcfsDBClusterIdentifier (\ s a -> s{_rdcfsDBClusterIdentifier = a})

-- | The identifier for the DB snapshot or DB cluster snapshot to restore from. You can use either the name or the Amazon Resource Name (ARN) to specify a DB cluster snapshot. However, you can use only the ARN to specify a DB snapshot. Constraints:     * Must match the identifier of an existing snapshot.
rdcfsSnapshotIdentifier :: Lens' RestoreDBClusterFromSnapshot Text
rdcfsSnapshotIdentifier = lens _rdcfsSnapshotIdentifier (\ s a -> s{_rdcfsSnapshotIdentifier = a})

-- | The database engine to use for the new DB cluster. Default: The same as source. Constraint: Must be compatible with the engine of the source.
rdcfsEngine :: Lens' RestoreDBClusterFromSnapshot Text
rdcfsEngine = lens _rdcfsEngine (\ s a -> s{_rdcfsEngine = a})

instance AWSRequest RestoreDBClusterFromSnapshot
         where
        type Rs RestoreDBClusterFromSnapshot =
             RestoreDBClusterFromSnapshotResponse
        request = postQuery docDB
        response
          = receiveXMLWrapper
              "RestoreDBClusterFromSnapshotResult"
              (\ s h x ->
                 RestoreDBClusterFromSnapshotResponse' <$>
                   (x .@? "DBCluster") <*> (pure (fromEnum s)))

instance Hashable RestoreDBClusterFromSnapshot where

instance NFData RestoreDBClusterFromSnapshot where

instance ToHeaders RestoreDBClusterFromSnapshot where
        toHeaders = const mempty

instance ToPath RestoreDBClusterFromSnapshot where
        toPath = const "/"

instance ToQuery RestoreDBClusterFromSnapshot where
        toQuery RestoreDBClusterFromSnapshot'{..}
          = mconcat
              ["Action" =:
                 ("RestoreDBClusterFromSnapshot" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "EngineVersion" =: _rdcfsEngineVersion,
               "DBSubnetGroupName" =: _rdcfsDBSubnetGroupName,
               "AvailabilityZones" =:
                 toQuery
                   (toQueryList "AvailabilityZone" <$>
                      _rdcfsAvailabilityZones),
               "KmsKeyId" =: _rdcfsKMSKeyId,
               "VpcSecurityGroupIds" =:
                 toQuery
                   (toQueryList "VpcSecurityGroupId" <$>
                      _rdcfsVPCSecurityGroupIds),
               "Tags" =: toQuery (toQueryList "Tag" <$> _rdcfsTags),
               "Port" =: _rdcfsPort,
               "EnableCloudwatchLogsExports" =:
                 toQuery
                   (toQueryList "member" <$>
                      _rdcfsEnableCloudwatchLogsExports),
               "DBClusterIdentifier" =: _rdcfsDBClusterIdentifier,
               "SnapshotIdentifier" =: _rdcfsSnapshotIdentifier,
               "Engine" =: _rdcfsEngine]

-- | /See:/ 'restoreDBClusterFromSnapshotResponse' smart constructor.
data RestoreDBClusterFromSnapshotResponse = RestoreDBClusterFromSnapshotResponse'
  { _rdcfsrsDBCluster :: !(Maybe DBCluster)
  , _rdcfsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreDBClusterFromSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdcfsrsDBCluster' - Undocumented member.
--
-- * 'rdcfsrsResponseStatus' - -- | The response status code.
restoreDBClusterFromSnapshotResponse
    :: Int -- ^ 'rdcfsrsResponseStatus'
    -> RestoreDBClusterFromSnapshotResponse
restoreDBClusterFromSnapshotResponse pResponseStatus_ =
  RestoreDBClusterFromSnapshotResponse'
    {_rdcfsrsDBCluster = Nothing, _rdcfsrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
rdcfsrsDBCluster :: Lens' RestoreDBClusterFromSnapshotResponse (Maybe DBCluster)
rdcfsrsDBCluster = lens _rdcfsrsDBCluster (\ s a -> s{_rdcfsrsDBCluster = a})

-- | -- | The response status code.
rdcfsrsResponseStatus :: Lens' RestoreDBClusterFromSnapshotResponse Int
rdcfsrsResponseStatus = lens _rdcfsrsResponseStatus (\ s a -> s{_rdcfsrsResponseStatus = a})

instance NFData RestoreDBClusterFromSnapshotResponse
         where
