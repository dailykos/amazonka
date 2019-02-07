{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DocDB.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DocDB.Types.Product where

import Network.AWS.DocDB.Internal
import Network.AWS.DocDB.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an Availability Zone.
--
--
--
-- /See:/ 'availabilityZone' smart constructor.
newtype AvailabilityZone = AvailabilityZone'
  { _azName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azName' - The name of the Availability Zone.
availabilityZone
    :: AvailabilityZone
availabilityZone = AvailabilityZone' {_azName = Nothing}


-- | The name of the Availability Zone.
azName :: Lens' AvailabilityZone (Maybe Text)
azName = lens _azName (\ s a -> s{_azName = a})

instance FromXML AvailabilityZone where
        parseXML x = AvailabilityZone' <$> (x .@? "Name")

instance Hashable AvailabilityZone where

instance NFData AvailabilityZone where

-- | The configuration setting for the log types to be enabled for export to Amazon CloudWatch Logs for a specific DB instance or DB cluster.
--
--
-- The @EnableLogTypes@ and @DisableLogTypes@ arrays determine which logs are exported (or not exported) to CloudWatch Logs. The values within these arrays depend on the DB engine that is being used.
--
--
-- /See:/ 'cloudwatchLogsExportConfiguration' smart constructor.
data CloudwatchLogsExportConfiguration = CloudwatchLogsExportConfiguration'
  { _clecDisableLogTypes :: !(Maybe [Text])
  , _clecEnableLogTypes :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CloudwatchLogsExportConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clecDisableLogTypes' - The list of log types to disable.
--
-- * 'clecEnableLogTypes' - The list of log types to enable.
cloudwatchLogsExportConfiguration
    :: CloudwatchLogsExportConfiguration
cloudwatchLogsExportConfiguration =
  CloudwatchLogsExportConfiguration'
    {_clecDisableLogTypes = Nothing, _clecEnableLogTypes = Nothing}


-- | The list of log types to disable.
clecDisableLogTypes :: Lens' CloudwatchLogsExportConfiguration [Text]
clecDisableLogTypes = lens _clecDisableLogTypes (\ s a -> s{_clecDisableLogTypes = a}) . _Default . _Coerce

-- | The list of log types to enable.
clecEnableLogTypes :: Lens' CloudwatchLogsExportConfiguration [Text]
clecEnableLogTypes = lens _clecEnableLogTypes (\ s a -> s{_clecEnableLogTypes = a}) . _Default . _Coerce

instance Hashable CloudwatchLogsExportConfiguration
         where

instance NFData CloudwatchLogsExportConfiguration
         where

instance ToQuery CloudwatchLogsExportConfiguration
         where
        toQuery CloudwatchLogsExportConfiguration'{..}
          = mconcat
              ["DisableLogTypes" =:
                 toQuery
                   (toQueryList "member" <$> _clecDisableLogTypes),
               "EnableLogTypes" =:
                 toQuery
                   (toQueryList "member" <$> _clecEnableLogTypes)]

-- | Detailed information about a DB cluster. 
--
--
--
-- /See:/ 'dbCluster' smart constructor.
data DBCluster = DBCluster'
  { _dcEngineVersion :: !(Maybe Text)
  , _dcStatus :: !(Maybe Text)
  , _dcStorageEncrypted :: !(Maybe Bool)
  , _dcDBClusterIdentifier :: !(Maybe Text)
  , _dcDBClusterMembers :: !(Maybe [DBClusterMember])
  , _dcHostedZoneId :: !(Maybe Text)
  , _dcDBClusterParameterGroup :: !(Maybe Text)
  , _dcMasterUsername :: !(Maybe Text)
  , _dcDBClusterResourceId :: !(Maybe Text)
  , _dcEarliestRestorableTime :: !(Maybe ISO8601)
  , _dcEngine :: !(Maybe Text)
  , _dcDBClusterARN :: !(Maybe Text)
  , _dcLatestRestorableTime :: !(Maybe ISO8601)
  , _dcPreferredMaintenanceWindow :: !(Maybe Text)
  , _dcAvailabilityZones :: !(Maybe [Text])
  , _dcKMSKeyId :: !(Maybe Text)
  , _dcPreferredBackupWindow :: !(Maybe Text)
  , _dcAssociatedRoles :: !(Maybe [DBClusterRole])
  , _dcVPCSecurityGroups :: !(Maybe [VPCSecurityGroupMembership])
  , _dcBackupRetentionPeriod :: !(Maybe Int)
  , _dcDBSubnetGroup :: !(Maybe Text)
  , _dcMultiAZ :: !(Maybe Bool)
  , _dcEnabledCloudwatchLogsExports :: !(Maybe [Text])
  , _dcClusterCreateTime :: !(Maybe ISO8601)
  , _dcEndpoint :: !(Maybe Text)
  , _dcPercentProgress :: !(Maybe Text)
  , _dcReaderEndpoint :: !(Maybe Text)
  , _dcPort :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcEngineVersion' - Indicates the database engine version.
--
-- * 'dcStatus' - Specifies the current state of this DB cluster.
--
-- * 'dcStorageEncrypted' - Specifies whether the DB cluster is encrypted.
--
-- * 'dcDBClusterIdentifier' - Contains a user-supplied DB cluster identifier. This identifier is the unique key that identifies a DB cluster.
--
-- * 'dcDBClusterMembers' - Provides the list of instances that make up the DB cluster.
--
-- * 'dcHostedZoneId' - Specifies the ID that Amazon Route 53 assigns when you create a hosted zone.
--
-- * 'dcDBClusterParameterGroup' - Specifies the name of the DB cluster parameter group for the DB cluster.
--
-- * 'dcMasterUsername' - Contains the master user name for the DB cluster.
--
-- * 'dcDBClusterResourceId' - The AWS Region-unique, immutable identifier for the DB cluster. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB cluster is accessed.
--
-- * 'dcEarliestRestorableTime' - The earliest time to which a database can be restored with point-in-time restore.
--
-- * 'dcEngine' - Provides the name of the database engine to be used for this DB cluster.
--
-- * 'dcDBClusterARN' - The Amazon Resource Name (ARN) for the DB cluster.
--
-- * 'dcLatestRestorableTime' - Specifies the latest time to which a database can be restored with point-in-time restore.
--
-- * 'dcPreferredMaintenanceWindow' - Specifies the weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
--
-- * 'dcAvailabilityZones' - Provides the list of Amazon EC2 Availability Zones that instances in the DB cluster can be created in.
--
-- * 'dcKMSKeyId' - If @StorageEncrypted@ is @true@ , the AWS KMS key identifier for the encrypted DB cluster.
--
-- * 'dcPreferredBackupWindow' - Specifies the daily time range during which automated backups are created if automated backups are enabled, as determined by the @BackupRetentionPeriod@ . 
--
-- * 'dcAssociatedRoles' - Provides a list of the AWS Identity and Access Management (IAM) roles that are associated with the DB cluster. IAM roles that are associated with a DB cluster grant permission for the DB cluster to access other AWS services on your behalf.
--
-- * 'dcVPCSecurityGroups' - Provides a list of virtual private cloud (VPC) security groups that the DB cluster belongs to.
--
-- * 'dcBackupRetentionPeriod' - Specifies the number of days for which automatic DB snapshots are retained.
--
-- * 'dcDBSubnetGroup' - Specifies information on the subnet group that is associated with the DB cluster, including the name, description, and subnets in the subnet group.
--
-- * 'dcMultiAZ' - Specifies whether the DB cluster has instances in multiple Availability Zones.
--
-- * 'dcEnabledCloudwatchLogsExports' - A list of log types that this DB cluster is configured to export to Amazon CloudWatch Logs.
--
-- * 'dcClusterCreateTime' - Specifies the time when the DB cluster was created, in Universal Coordinated Time (UTC).
--
-- * 'dcEndpoint' - Specifies the connection endpoint for the primary instance of the DB cluster.
--
-- * 'dcPercentProgress' - Specifies the progress of the operation as a percentage.
--
-- * 'dcReaderEndpoint' - The reader endpoint for the DB cluster. The reader endpoint for a DB cluster load balances connections across the Amazon DocumentDB replicas that are available in a DB cluster. As clients request new connections to the reader endpoint, Amazon DocumentDB distributes the connection requests among the Amazon DocumentDB replicas in the DB cluster. This functionality can help balance your read workload across multiple Amazon DocumentDB replicas in your DB cluster.  If a failover occurs, and the Amazon DocumentDB replica that you are connected to is promoted to be the primary instance, your connection is dropped. To continue sending your read workload to other Amazon DocumentDB replicas in the cluster, you can then reconnect to the reader endpoint.
--
-- * 'dcPort' - Specifies the port that the database engine is listening on.
dbCluster
    :: DBCluster
dbCluster =
  DBCluster'
    { _dcEngineVersion = Nothing
    , _dcStatus = Nothing
    , _dcStorageEncrypted = Nothing
    , _dcDBClusterIdentifier = Nothing
    , _dcDBClusterMembers = Nothing
    , _dcHostedZoneId = Nothing
    , _dcDBClusterParameterGroup = Nothing
    , _dcMasterUsername = Nothing
    , _dcDBClusterResourceId = Nothing
    , _dcEarliestRestorableTime = Nothing
    , _dcEngine = Nothing
    , _dcDBClusterARN = Nothing
    , _dcLatestRestorableTime = Nothing
    , _dcPreferredMaintenanceWindow = Nothing
    , _dcAvailabilityZones = Nothing
    , _dcKMSKeyId = Nothing
    , _dcPreferredBackupWindow = Nothing
    , _dcAssociatedRoles = Nothing
    , _dcVPCSecurityGroups = Nothing
    , _dcBackupRetentionPeriod = Nothing
    , _dcDBSubnetGroup = Nothing
    , _dcMultiAZ = Nothing
    , _dcEnabledCloudwatchLogsExports = Nothing
    , _dcClusterCreateTime = Nothing
    , _dcEndpoint = Nothing
    , _dcPercentProgress = Nothing
    , _dcReaderEndpoint = Nothing
    , _dcPort = Nothing
    }


-- | Indicates the database engine version.
dcEngineVersion :: Lens' DBCluster (Maybe Text)
dcEngineVersion = lens _dcEngineVersion (\ s a -> s{_dcEngineVersion = a})

-- | Specifies the current state of this DB cluster.
dcStatus :: Lens' DBCluster (Maybe Text)
dcStatus = lens _dcStatus (\ s a -> s{_dcStatus = a})

-- | Specifies whether the DB cluster is encrypted.
dcStorageEncrypted :: Lens' DBCluster (Maybe Bool)
dcStorageEncrypted = lens _dcStorageEncrypted (\ s a -> s{_dcStorageEncrypted = a})

-- | Contains a user-supplied DB cluster identifier. This identifier is the unique key that identifies a DB cluster.
dcDBClusterIdentifier :: Lens' DBCluster (Maybe Text)
dcDBClusterIdentifier = lens _dcDBClusterIdentifier (\ s a -> s{_dcDBClusterIdentifier = a})

-- | Provides the list of instances that make up the DB cluster.
dcDBClusterMembers :: Lens' DBCluster [DBClusterMember]
dcDBClusterMembers = lens _dcDBClusterMembers (\ s a -> s{_dcDBClusterMembers = a}) . _Default . _Coerce

-- | Specifies the ID that Amazon Route 53 assigns when you create a hosted zone.
dcHostedZoneId :: Lens' DBCluster (Maybe Text)
dcHostedZoneId = lens _dcHostedZoneId (\ s a -> s{_dcHostedZoneId = a})

-- | Specifies the name of the DB cluster parameter group for the DB cluster.
dcDBClusterParameterGroup :: Lens' DBCluster (Maybe Text)
dcDBClusterParameterGroup = lens _dcDBClusterParameterGroup (\ s a -> s{_dcDBClusterParameterGroup = a})

-- | Contains the master user name for the DB cluster.
dcMasterUsername :: Lens' DBCluster (Maybe Text)
dcMasterUsername = lens _dcMasterUsername (\ s a -> s{_dcMasterUsername = a})

-- | The AWS Region-unique, immutable identifier for the DB cluster. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB cluster is accessed.
dcDBClusterResourceId :: Lens' DBCluster (Maybe Text)
dcDBClusterResourceId = lens _dcDBClusterResourceId (\ s a -> s{_dcDBClusterResourceId = a})

-- | The earliest time to which a database can be restored with point-in-time restore.
dcEarliestRestorableTime :: Lens' DBCluster (Maybe UTCTime)
dcEarliestRestorableTime = lens _dcEarliestRestorableTime (\ s a -> s{_dcEarliestRestorableTime = a}) . mapping _Time

-- | Provides the name of the database engine to be used for this DB cluster.
dcEngine :: Lens' DBCluster (Maybe Text)
dcEngine = lens _dcEngine (\ s a -> s{_dcEngine = a})

-- | The Amazon Resource Name (ARN) for the DB cluster.
dcDBClusterARN :: Lens' DBCluster (Maybe Text)
dcDBClusterARN = lens _dcDBClusterARN (\ s a -> s{_dcDBClusterARN = a})

-- | Specifies the latest time to which a database can be restored with point-in-time restore.
dcLatestRestorableTime :: Lens' DBCluster (Maybe UTCTime)
dcLatestRestorableTime = lens _dcLatestRestorableTime (\ s a -> s{_dcLatestRestorableTime = a}) . mapping _Time

-- | Specifies the weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
dcPreferredMaintenanceWindow :: Lens' DBCluster (Maybe Text)
dcPreferredMaintenanceWindow = lens _dcPreferredMaintenanceWindow (\ s a -> s{_dcPreferredMaintenanceWindow = a})

-- | Provides the list of Amazon EC2 Availability Zones that instances in the DB cluster can be created in.
dcAvailabilityZones :: Lens' DBCluster [Text]
dcAvailabilityZones = lens _dcAvailabilityZones (\ s a -> s{_dcAvailabilityZones = a}) . _Default . _Coerce

-- | If @StorageEncrypted@ is @true@ , the AWS KMS key identifier for the encrypted DB cluster.
dcKMSKeyId :: Lens' DBCluster (Maybe Text)
dcKMSKeyId = lens _dcKMSKeyId (\ s a -> s{_dcKMSKeyId = a})

-- | Specifies the daily time range during which automated backups are created if automated backups are enabled, as determined by the @BackupRetentionPeriod@ . 
dcPreferredBackupWindow :: Lens' DBCluster (Maybe Text)
dcPreferredBackupWindow = lens _dcPreferredBackupWindow (\ s a -> s{_dcPreferredBackupWindow = a})

-- | Provides a list of the AWS Identity and Access Management (IAM) roles that are associated with the DB cluster. IAM roles that are associated with a DB cluster grant permission for the DB cluster to access other AWS services on your behalf.
dcAssociatedRoles :: Lens' DBCluster [DBClusterRole]
dcAssociatedRoles = lens _dcAssociatedRoles (\ s a -> s{_dcAssociatedRoles = a}) . _Default . _Coerce

-- | Provides a list of virtual private cloud (VPC) security groups that the DB cluster belongs to.
dcVPCSecurityGroups :: Lens' DBCluster [VPCSecurityGroupMembership]
dcVPCSecurityGroups = lens _dcVPCSecurityGroups (\ s a -> s{_dcVPCSecurityGroups = a}) . _Default . _Coerce

-- | Specifies the number of days for which automatic DB snapshots are retained.
dcBackupRetentionPeriod :: Lens' DBCluster (Maybe Int)
dcBackupRetentionPeriod = lens _dcBackupRetentionPeriod (\ s a -> s{_dcBackupRetentionPeriod = a})

-- | Specifies information on the subnet group that is associated with the DB cluster, including the name, description, and subnets in the subnet group.
dcDBSubnetGroup :: Lens' DBCluster (Maybe Text)
dcDBSubnetGroup = lens _dcDBSubnetGroup (\ s a -> s{_dcDBSubnetGroup = a})

-- | Specifies whether the DB cluster has instances in multiple Availability Zones.
dcMultiAZ :: Lens' DBCluster (Maybe Bool)
dcMultiAZ = lens _dcMultiAZ (\ s a -> s{_dcMultiAZ = a})

-- | A list of log types that this DB cluster is configured to export to Amazon CloudWatch Logs.
dcEnabledCloudwatchLogsExports :: Lens' DBCluster [Text]
dcEnabledCloudwatchLogsExports = lens _dcEnabledCloudwatchLogsExports (\ s a -> s{_dcEnabledCloudwatchLogsExports = a}) . _Default . _Coerce

-- | Specifies the time when the DB cluster was created, in Universal Coordinated Time (UTC).
dcClusterCreateTime :: Lens' DBCluster (Maybe UTCTime)
dcClusterCreateTime = lens _dcClusterCreateTime (\ s a -> s{_dcClusterCreateTime = a}) . mapping _Time

-- | Specifies the connection endpoint for the primary instance of the DB cluster.
dcEndpoint :: Lens' DBCluster (Maybe Text)
dcEndpoint = lens _dcEndpoint (\ s a -> s{_dcEndpoint = a})

-- | Specifies the progress of the operation as a percentage.
dcPercentProgress :: Lens' DBCluster (Maybe Text)
dcPercentProgress = lens _dcPercentProgress (\ s a -> s{_dcPercentProgress = a})

-- | The reader endpoint for the DB cluster. The reader endpoint for a DB cluster load balances connections across the Amazon DocumentDB replicas that are available in a DB cluster. As clients request new connections to the reader endpoint, Amazon DocumentDB distributes the connection requests among the Amazon DocumentDB replicas in the DB cluster. This functionality can help balance your read workload across multiple Amazon DocumentDB replicas in your DB cluster.  If a failover occurs, and the Amazon DocumentDB replica that you are connected to is promoted to be the primary instance, your connection is dropped. To continue sending your read workload to other Amazon DocumentDB replicas in the cluster, you can then reconnect to the reader endpoint.
dcReaderEndpoint :: Lens' DBCluster (Maybe Text)
dcReaderEndpoint = lens _dcReaderEndpoint (\ s a -> s{_dcReaderEndpoint = a})

-- | Specifies the port that the database engine is listening on.
dcPort :: Lens' DBCluster (Maybe Int)
dcPort = lens _dcPort (\ s a -> s{_dcPort = a})

instance FromXML DBCluster where
        parseXML x
          = DBCluster' <$>
              (x .@? "EngineVersion") <*> (x .@? "Status") <*>
                (x .@? "StorageEncrypted")
                <*> (x .@? "DBClusterIdentifier")
                <*>
                (x .@? "DBClusterMembers" .!@ mempty >>=
                   may (parseXMLList "DBClusterMember"))
                <*> (x .@? "HostedZoneId")
                <*> (x .@? "DBClusterParameterGroup")
                <*> (x .@? "MasterUsername")
                <*> (x .@? "DbClusterResourceId")
                <*> (x .@? "EarliestRestorableTime")
                <*> (x .@? "Engine")
                <*> (x .@? "DBClusterArn")
                <*> (x .@? "LatestRestorableTime")
                <*> (x .@? "PreferredMaintenanceWindow")
                <*>
                (x .@? "AvailabilityZones" .!@ mempty >>=
                   may (parseXMLList "AvailabilityZone"))
                <*> (x .@? "KmsKeyId")
                <*> (x .@? "PreferredBackupWindow")
                <*>
                (x .@? "AssociatedRoles" .!@ mempty >>=
                   may (parseXMLList "DBClusterRole"))
                <*>
                (x .@? "VpcSecurityGroups" .!@ mempty >>=
                   may (parseXMLList "VpcSecurityGroupMembership"))
                <*> (x .@? "BackupRetentionPeriod")
                <*> (x .@? "DBSubnetGroup")
                <*> (x .@? "MultiAZ")
                <*>
                (x .@? "EnabledCloudwatchLogsExports" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "ClusterCreateTime")
                <*> (x .@? "Endpoint")
                <*> (x .@? "PercentProgress")
                <*> (x .@? "ReaderEndpoint")
                <*> (x .@? "Port")

instance Hashable DBCluster where

instance NFData DBCluster where

-- | Contains information about an instance that is part of a DB cluster.
--
--
--
-- /See:/ 'dbClusterMember' smart constructor.
data DBClusterMember = DBClusterMember'
  { _dcmPromotionTier :: !(Maybe Int)
  , _dcmDBInstanceIdentifier :: !(Maybe Text)
  , _dcmIsClusterWriter :: !(Maybe Bool)
  , _dcmDBClusterParameterGroupStatus :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBClusterMember' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcmPromotionTier' - A value that specifies the order in which an Amazon DocumentDB replica is promoted to the primary instance after a failure of the existing primary instance. 
--
-- * 'dcmDBInstanceIdentifier' - Specifies the instance identifier for this member of the DB cluster.
--
-- * 'dcmIsClusterWriter' - A value that is @true@ if the cluster member is the primary instance for the DB cluster and @false@ otherwise.
--
-- * 'dcmDBClusterParameterGroupStatus' - Specifies the status of the DB cluster parameter group for this member of the DB cluster.
dbClusterMember
    :: DBClusterMember
dbClusterMember =
  DBClusterMember'
    { _dcmPromotionTier = Nothing
    , _dcmDBInstanceIdentifier = Nothing
    , _dcmIsClusterWriter = Nothing
    , _dcmDBClusterParameterGroupStatus = Nothing
    }


-- | A value that specifies the order in which an Amazon DocumentDB replica is promoted to the primary instance after a failure of the existing primary instance. 
dcmPromotionTier :: Lens' DBClusterMember (Maybe Int)
dcmPromotionTier = lens _dcmPromotionTier (\ s a -> s{_dcmPromotionTier = a})

-- | Specifies the instance identifier for this member of the DB cluster.
dcmDBInstanceIdentifier :: Lens' DBClusterMember (Maybe Text)
dcmDBInstanceIdentifier = lens _dcmDBInstanceIdentifier (\ s a -> s{_dcmDBInstanceIdentifier = a})

-- | A value that is @true@ if the cluster member is the primary instance for the DB cluster and @false@ otherwise.
dcmIsClusterWriter :: Lens' DBClusterMember (Maybe Bool)
dcmIsClusterWriter = lens _dcmIsClusterWriter (\ s a -> s{_dcmIsClusterWriter = a})

-- | Specifies the status of the DB cluster parameter group for this member of the DB cluster.
dcmDBClusterParameterGroupStatus :: Lens' DBClusterMember (Maybe Text)
dcmDBClusterParameterGroupStatus = lens _dcmDBClusterParameterGroupStatus (\ s a -> s{_dcmDBClusterParameterGroupStatus = a})

instance FromXML DBClusterMember where
        parseXML x
          = DBClusterMember' <$>
              (x .@? "PromotionTier") <*>
                (x .@? "DBInstanceIdentifier")
                <*> (x .@? "IsClusterWriter")
                <*> (x .@? "DBClusterParameterGroupStatus")

instance Hashable DBClusterMember where

instance NFData DBClusterMember where

-- | Detailed information about a DB cluster parameter group. 
--
--
--
-- /See:/ 'dbClusterParameterGroup' smart constructor.
data DBClusterParameterGroup = DBClusterParameterGroup'
  { _dcpgDBClusterParameterGroupARN :: !(Maybe Text)
  , _dcpgDBParameterGroupFamily :: !(Maybe Text)
  , _dcpgDBClusterParameterGroupName :: !(Maybe Text)
  , _dcpgDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBClusterParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpgDBClusterParameterGroupARN' - The Amazon Resource Name (ARN) for the DB cluster parameter group.
--
-- * 'dcpgDBParameterGroupFamily' - Provides the name of the DB parameter group family that this DB cluster parameter group is compatible with.
--
-- * 'dcpgDBClusterParameterGroupName' - Provides the name of the DB cluster parameter group.
--
-- * 'dcpgDescription' - Provides the customer-specified description for this DB cluster parameter group.
dbClusterParameterGroup
    :: DBClusterParameterGroup
dbClusterParameterGroup =
  DBClusterParameterGroup'
    { _dcpgDBClusterParameterGroupARN = Nothing
    , _dcpgDBParameterGroupFamily = Nothing
    , _dcpgDBClusterParameterGroupName = Nothing
    , _dcpgDescription = Nothing
    }


-- | The Amazon Resource Name (ARN) for the DB cluster parameter group.
dcpgDBClusterParameterGroupARN :: Lens' DBClusterParameterGroup (Maybe Text)
dcpgDBClusterParameterGroupARN = lens _dcpgDBClusterParameterGroupARN (\ s a -> s{_dcpgDBClusterParameterGroupARN = a})

-- | Provides the name of the DB parameter group family that this DB cluster parameter group is compatible with.
dcpgDBParameterGroupFamily :: Lens' DBClusterParameterGroup (Maybe Text)
dcpgDBParameterGroupFamily = lens _dcpgDBParameterGroupFamily (\ s a -> s{_dcpgDBParameterGroupFamily = a})

-- | Provides the name of the DB cluster parameter group.
dcpgDBClusterParameterGroupName :: Lens' DBClusterParameterGroup (Maybe Text)
dcpgDBClusterParameterGroupName = lens _dcpgDBClusterParameterGroupName (\ s a -> s{_dcpgDBClusterParameterGroupName = a})

-- | Provides the customer-specified description for this DB cluster parameter group.
dcpgDescription :: Lens' DBClusterParameterGroup (Maybe Text)
dcpgDescription = lens _dcpgDescription (\ s a -> s{_dcpgDescription = a})

instance FromXML DBClusterParameterGroup where
        parseXML x
          = DBClusterParameterGroup' <$>
              (x .@? "DBClusterParameterGroupArn") <*>
                (x .@? "DBParameterGroupFamily")
                <*> (x .@? "DBClusterParameterGroupName")
                <*> (x .@? "Description")

instance Hashable DBClusterParameterGroup where

instance NFData DBClusterParameterGroup where

-- | Contains the name of a DB cluster parameter group.
--
--
--
-- /See:/ 'dbClusterParameterGroupNameMessage' smart constructor.
newtype DBClusterParameterGroupNameMessage = DBClusterParameterGroupNameMessage'
  { _dcpgnmDBClusterParameterGroupName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBClusterParameterGroupNameMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpgnmDBClusterParameterGroupName' - The name of a DB cluster parameter group. Constraints:     * Must be from 1 to 255 letters or numbers.     * The first character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
dbClusterParameterGroupNameMessage
    :: DBClusterParameterGroupNameMessage
dbClusterParameterGroupNameMessage =
  DBClusterParameterGroupNameMessage'
    {_dcpgnmDBClusterParameterGroupName = Nothing}


-- | The name of a DB cluster parameter group. Constraints:     * Must be from 1 to 255 letters or numbers.     * The first character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
dcpgnmDBClusterParameterGroupName :: Lens' DBClusterParameterGroupNameMessage (Maybe Text)
dcpgnmDBClusterParameterGroupName = lens _dcpgnmDBClusterParameterGroupName (\ s a -> s{_dcpgnmDBClusterParameterGroupName = a})

instance FromXML DBClusterParameterGroupNameMessage
         where
        parseXML x
          = DBClusterParameterGroupNameMessage' <$>
              (x .@? "DBClusterParameterGroupName")

instance Hashable DBClusterParameterGroupNameMessage
         where

instance NFData DBClusterParameterGroupNameMessage
         where

-- | Describes an AWS Identity and Access Management (IAM) role that is associated with a DB cluster.
--
--
--
-- /See:/ 'dbClusterRole' smart constructor.
data DBClusterRole = DBClusterRole'
  { _dcrStatus :: !(Maybe Text)
  , _dcrRoleARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBClusterRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrStatus' - Describes the state of association between the IAM role and the DB cluster. The @Status@ property returns one of the following values:     * @ACTIVE@ - The IAM role ARN is associated with the DB cluster and can be used to access other AWS services on your behalf.     * @PENDING@ - The IAM role ARN is being associated with the DB cluster.     * @INVALID@ - The IAM role ARN is associated with the DB cluster, but the DB cluster cannot assume the IAM role to access other AWS services on your behalf.
--
-- * 'dcrRoleARN' - The Amazon Resource Name (ARN) of the IAM role that is associated with the DB cluster.
dbClusterRole
    :: DBClusterRole
dbClusterRole = DBClusterRole' {_dcrStatus = Nothing, _dcrRoleARN = Nothing}


-- | Describes the state of association between the IAM role and the DB cluster. The @Status@ property returns one of the following values:     * @ACTIVE@ - The IAM role ARN is associated with the DB cluster and can be used to access other AWS services on your behalf.     * @PENDING@ - The IAM role ARN is being associated with the DB cluster.     * @INVALID@ - The IAM role ARN is associated with the DB cluster, but the DB cluster cannot assume the IAM role to access other AWS services on your behalf.
dcrStatus :: Lens' DBClusterRole (Maybe Text)
dcrStatus = lens _dcrStatus (\ s a -> s{_dcrStatus = a})

-- | The Amazon Resource Name (ARN) of the IAM role that is associated with the DB cluster.
dcrRoleARN :: Lens' DBClusterRole (Maybe Text)
dcrRoleARN = lens _dcrRoleARN (\ s a -> s{_dcrRoleARN = a})

instance FromXML DBClusterRole where
        parseXML x
          = DBClusterRole' <$>
              (x .@? "Status") <*> (x .@? "RoleArn")

instance Hashable DBClusterRole where

instance NFData DBClusterRole where

-- | Detailed information about a DB cluster snapshot. 
--
--
--
-- /See:/ 'dbClusterSnapshot' smart constructor.
data DBClusterSnapshot = DBClusterSnapshot'
  { _dcsEngineVersion :: !(Maybe Text)
  , _dcsStatus :: !(Maybe Text)
  , _dcsStorageEncrypted :: !(Maybe Bool)
  , _dcsDBClusterIdentifier :: !(Maybe Text)
  , _dcsMasterUsername :: !(Maybe Text)
  , _dcsDBClusterSnapshotARN :: !(Maybe Text)
  , _dcsVPCId :: !(Maybe Text)
  , _dcsDBClusterSnapshotIdentifier :: !(Maybe Text)
  , _dcsEngine :: !(Maybe Text)
  , _dcsAvailabilityZones :: !(Maybe [Text])
  , _dcsSnapshotType :: !(Maybe Text)
  , _dcsKMSKeyId :: !(Maybe Text)
  , _dcsSnapshotCreateTime :: !(Maybe ISO8601)
  , _dcsSourceDBClusterSnapshotARN :: !(Maybe Text)
  , _dcsClusterCreateTime :: !(Maybe ISO8601)
  , _dcsPercentProgress :: !(Maybe Int)
  , _dcsPort :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBClusterSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsEngineVersion' - Provides the version of the database engine for this DB cluster snapshot.
--
-- * 'dcsStatus' - Specifies the status of this DB cluster snapshot.
--
-- * 'dcsStorageEncrypted' - Specifies whether the DB cluster snapshot is encrypted.
--
-- * 'dcsDBClusterIdentifier' - Specifies the DB cluster identifier of the DB cluster that this DB cluster snapshot was created from.
--
-- * 'dcsMasterUsername' - Provides the master user name for the DB cluster snapshot.
--
-- * 'dcsDBClusterSnapshotARN' - The Amazon Resource Name (ARN) for the DB cluster snapshot.
--
-- * 'dcsVPCId' - Provides the virtual private cloud (VPC) ID that is associated with the DB cluster snapshot.
--
-- * 'dcsDBClusterSnapshotIdentifier' - Specifies the identifier for the DB cluster snapshot.
--
-- * 'dcsEngine' - Specifies the name of the database engine.
--
-- * 'dcsAvailabilityZones' - Provides the list of Amazon EC2 Availability Zones that instances in the DB cluster snapshot can be restored in.
--
-- * 'dcsSnapshotType' - Provides the type of the DB cluster snapshot.
--
-- * 'dcsKMSKeyId' - If @StorageEncrypted@ is @true@ , the AWS KMS key identifier for the encrypted DB cluster snapshot.
--
-- * 'dcsSnapshotCreateTime' - Provides the time when the snapshot was taken, in UTC.
--
-- * 'dcsSourceDBClusterSnapshotARN' - If the DB cluster snapshot was copied from a source DB cluster snapshot, the ARN for the source DB cluster snapshot; otherwise, a null value.
--
-- * 'dcsClusterCreateTime' - Specifies the time when the DB cluster was created, in Universal Coordinated Time (UTC).
--
-- * 'dcsPercentProgress' - Specifies the percentage of the estimated data that has been transferred.
--
-- * 'dcsPort' - Specifies the port that the DB cluster was listening on at the time of the snapshot.
dbClusterSnapshot
    :: DBClusterSnapshot
dbClusterSnapshot =
  DBClusterSnapshot'
    { _dcsEngineVersion = Nothing
    , _dcsStatus = Nothing
    , _dcsStorageEncrypted = Nothing
    , _dcsDBClusterIdentifier = Nothing
    , _dcsMasterUsername = Nothing
    , _dcsDBClusterSnapshotARN = Nothing
    , _dcsVPCId = Nothing
    , _dcsDBClusterSnapshotIdentifier = Nothing
    , _dcsEngine = Nothing
    , _dcsAvailabilityZones = Nothing
    , _dcsSnapshotType = Nothing
    , _dcsKMSKeyId = Nothing
    , _dcsSnapshotCreateTime = Nothing
    , _dcsSourceDBClusterSnapshotARN = Nothing
    , _dcsClusterCreateTime = Nothing
    , _dcsPercentProgress = Nothing
    , _dcsPort = Nothing
    }


-- | Provides the version of the database engine for this DB cluster snapshot.
dcsEngineVersion :: Lens' DBClusterSnapshot (Maybe Text)
dcsEngineVersion = lens _dcsEngineVersion (\ s a -> s{_dcsEngineVersion = a})

-- | Specifies the status of this DB cluster snapshot.
dcsStatus :: Lens' DBClusterSnapshot (Maybe Text)
dcsStatus = lens _dcsStatus (\ s a -> s{_dcsStatus = a})

-- | Specifies whether the DB cluster snapshot is encrypted.
dcsStorageEncrypted :: Lens' DBClusterSnapshot (Maybe Bool)
dcsStorageEncrypted = lens _dcsStorageEncrypted (\ s a -> s{_dcsStorageEncrypted = a})

-- | Specifies the DB cluster identifier of the DB cluster that this DB cluster snapshot was created from.
dcsDBClusterIdentifier :: Lens' DBClusterSnapshot (Maybe Text)
dcsDBClusterIdentifier = lens _dcsDBClusterIdentifier (\ s a -> s{_dcsDBClusterIdentifier = a})

-- | Provides the master user name for the DB cluster snapshot.
dcsMasterUsername :: Lens' DBClusterSnapshot (Maybe Text)
dcsMasterUsername = lens _dcsMasterUsername (\ s a -> s{_dcsMasterUsername = a})

-- | The Amazon Resource Name (ARN) for the DB cluster snapshot.
dcsDBClusterSnapshotARN :: Lens' DBClusterSnapshot (Maybe Text)
dcsDBClusterSnapshotARN = lens _dcsDBClusterSnapshotARN (\ s a -> s{_dcsDBClusterSnapshotARN = a})

-- | Provides the virtual private cloud (VPC) ID that is associated with the DB cluster snapshot.
dcsVPCId :: Lens' DBClusterSnapshot (Maybe Text)
dcsVPCId = lens _dcsVPCId (\ s a -> s{_dcsVPCId = a})

-- | Specifies the identifier for the DB cluster snapshot.
dcsDBClusterSnapshotIdentifier :: Lens' DBClusterSnapshot (Maybe Text)
dcsDBClusterSnapshotIdentifier = lens _dcsDBClusterSnapshotIdentifier (\ s a -> s{_dcsDBClusterSnapshotIdentifier = a})

-- | Specifies the name of the database engine.
dcsEngine :: Lens' DBClusterSnapshot (Maybe Text)
dcsEngine = lens _dcsEngine (\ s a -> s{_dcsEngine = a})

-- | Provides the list of Amazon EC2 Availability Zones that instances in the DB cluster snapshot can be restored in.
dcsAvailabilityZones :: Lens' DBClusterSnapshot [Text]
dcsAvailabilityZones = lens _dcsAvailabilityZones (\ s a -> s{_dcsAvailabilityZones = a}) . _Default . _Coerce

-- | Provides the type of the DB cluster snapshot.
dcsSnapshotType :: Lens' DBClusterSnapshot (Maybe Text)
dcsSnapshotType = lens _dcsSnapshotType (\ s a -> s{_dcsSnapshotType = a})

-- | If @StorageEncrypted@ is @true@ , the AWS KMS key identifier for the encrypted DB cluster snapshot.
dcsKMSKeyId :: Lens' DBClusterSnapshot (Maybe Text)
dcsKMSKeyId = lens _dcsKMSKeyId (\ s a -> s{_dcsKMSKeyId = a})

-- | Provides the time when the snapshot was taken, in UTC.
dcsSnapshotCreateTime :: Lens' DBClusterSnapshot (Maybe UTCTime)
dcsSnapshotCreateTime = lens _dcsSnapshotCreateTime (\ s a -> s{_dcsSnapshotCreateTime = a}) . mapping _Time

-- | If the DB cluster snapshot was copied from a source DB cluster snapshot, the ARN for the source DB cluster snapshot; otherwise, a null value.
dcsSourceDBClusterSnapshotARN :: Lens' DBClusterSnapshot (Maybe Text)
dcsSourceDBClusterSnapshotARN = lens _dcsSourceDBClusterSnapshotARN (\ s a -> s{_dcsSourceDBClusterSnapshotARN = a})

-- | Specifies the time when the DB cluster was created, in Universal Coordinated Time (UTC).
dcsClusterCreateTime :: Lens' DBClusterSnapshot (Maybe UTCTime)
dcsClusterCreateTime = lens _dcsClusterCreateTime (\ s a -> s{_dcsClusterCreateTime = a}) . mapping _Time

-- | Specifies the percentage of the estimated data that has been transferred.
dcsPercentProgress :: Lens' DBClusterSnapshot (Maybe Int)
dcsPercentProgress = lens _dcsPercentProgress (\ s a -> s{_dcsPercentProgress = a})

-- | Specifies the port that the DB cluster was listening on at the time of the snapshot.
dcsPort :: Lens' DBClusterSnapshot (Maybe Int)
dcsPort = lens _dcsPort (\ s a -> s{_dcsPort = a})

instance FromXML DBClusterSnapshot where
        parseXML x
          = DBClusterSnapshot' <$>
              (x .@? "EngineVersion") <*> (x .@? "Status") <*>
                (x .@? "StorageEncrypted")
                <*> (x .@? "DBClusterIdentifier")
                <*> (x .@? "MasterUsername")
                <*> (x .@? "DBClusterSnapshotArn")
                <*> (x .@? "VpcId")
                <*> (x .@? "DBClusterSnapshotIdentifier")
                <*> (x .@? "Engine")
                <*>
                (x .@? "AvailabilityZones" .!@ mempty >>=
                   may (parseXMLList "AvailabilityZone"))
                <*> (x .@? "SnapshotType")
                <*> (x .@? "KmsKeyId")
                <*> (x .@? "SnapshotCreateTime")
                <*> (x .@? "SourceDBClusterSnapshotArn")
                <*> (x .@? "ClusterCreateTime")
                <*> (x .@? "PercentProgress")
                <*> (x .@? "Port")

instance Hashable DBClusterSnapshot where

instance NFData DBClusterSnapshot where

-- | Contains the name and values of a manual DB cluster snapshot attribute.
--
--
-- Manual DB cluster snapshot attributes are used to authorize other AWS accounts to restore a manual DB cluster snapshot.
--
--
-- /See:/ 'dbClusterSnapshotAttribute' smart constructor.
data DBClusterSnapshotAttribute = DBClusterSnapshotAttribute'
  { _dcsaAttributeValues :: !(Maybe [Text])
  , _dcsaAttributeName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBClusterSnapshotAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsaAttributeValues' - The values for the manual DB cluster snapshot attribute. If the @AttributeName@ field is set to @restore@ , then this element returns a list of IDs of the AWS accounts that are authorized to copy or restore the manual DB cluster snapshot. If a value of @all@ is in the list, then the manual DB cluster snapshot is public and available for any AWS account to copy or restore.
--
-- * 'dcsaAttributeName' - The name of the manual DB cluster snapshot attribute. The attribute named @restore@ refers to the list of AWS accounts that have permission to copy or restore the manual DB cluster snapshot.
dbClusterSnapshotAttribute
    :: DBClusterSnapshotAttribute
dbClusterSnapshotAttribute =
  DBClusterSnapshotAttribute'
    {_dcsaAttributeValues = Nothing, _dcsaAttributeName = Nothing}


-- | The values for the manual DB cluster snapshot attribute. If the @AttributeName@ field is set to @restore@ , then this element returns a list of IDs of the AWS accounts that are authorized to copy or restore the manual DB cluster snapshot. If a value of @all@ is in the list, then the manual DB cluster snapshot is public and available for any AWS account to copy or restore.
dcsaAttributeValues :: Lens' DBClusterSnapshotAttribute [Text]
dcsaAttributeValues = lens _dcsaAttributeValues (\ s a -> s{_dcsaAttributeValues = a}) . _Default . _Coerce

-- | The name of the manual DB cluster snapshot attribute. The attribute named @restore@ refers to the list of AWS accounts that have permission to copy or restore the manual DB cluster snapshot.
dcsaAttributeName :: Lens' DBClusterSnapshotAttribute (Maybe Text)
dcsaAttributeName = lens _dcsaAttributeName (\ s a -> s{_dcsaAttributeName = a})

instance FromXML DBClusterSnapshotAttribute where
        parseXML x
          = DBClusterSnapshotAttribute' <$>
              (x .@? "AttributeValues" .!@ mempty >>=
                 may (parseXMLList "AttributeValue"))
                <*> (x .@? "AttributeName")

instance Hashable DBClusterSnapshotAttribute where

instance NFData DBClusterSnapshotAttribute where

-- | Detailed information about the attributes that are associated with a DB cluster snapshot.
--
--
--
-- /See:/ 'dbClusterSnapshotAttributesResult' smart constructor.
data DBClusterSnapshotAttributesResult = DBClusterSnapshotAttributesResult'
  { _dcsarDBClusterSnapshotIdentifier :: !(Maybe Text)
  , _dcsarDBClusterSnapshotAttributes :: !(Maybe [DBClusterSnapshotAttribute])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBClusterSnapshotAttributesResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsarDBClusterSnapshotIdentifier' - The identifier of the DB cluster snapshot that the attributes apply to.
--
-- * 'dcsarDBClusterSnapshotAttributes' - The list of attributes and values for the DB cluster snapshot.
dbClusterSnapshotAttributesResult
    :: DBClusterSnapshotAttributesResult
dbClusterSnapshotAttributesResult =
  DBClusterSnapshotAttributesResult'
    { _dcsarDBClusterSnapshotIdentifier = Nothing
    , _dcsarDBClusterSnapshotAttributes = Nothing
    }


-- | The identifier of the DB cluster snapshot that the attributes apply to.
dcsarDBClusterSnapshotIdentifier :: Lens' DBClusterSnapshotAttributesResult (Maybe Text)
dcsarDBClusterSnapshotIdentifier = lens _dcsarDBClusterSnapshotIdentifier (\ s a -> s{_dcsarDBClusterSnapshotIdentifier = a})

-- | The list of attributes and values for the DB cluster snapshot.
dcsarDBClusterSnapshotAttributes :: Lens' DBClusterSnapshotAttributesResult [DBClusterSnapshotAttribute]
dcsarDBClusterSnapshotAttributes = lens _dcsarDBClusterSnapshotAttributes (\ s a -> s{_dcsarDBClusterSnapshotAttributes = a}) . _Default . _Coerce

instance FromXML DBClusterSnapshotAttributesResult
         where
        parseXML x
          = DBClusterSnapshotAttributesResult' <$>
              (x .@? "DBClusterSnapshotIdentifier") <*>
                (x .@? "DBClusterSnapshotAttributes" .!@ mempty >>=
                   may (parseXMLList "DBClusterSnapshotAttribute"))

instance Hashable DBClusterSnapshotAttributesResult
         where

instance NFData DBClusterSnapshotAttributesResult
         where

-- | Detailed information about a DB engine version. 
--
--
--
-- /See:/ 'dbEngineVersion' smart constructor.
data DBEngineVersion = DBEngineVersion'
  { _devEngineVersion :: !(Maybe Text)
  , _devDBEngineVersionDescription :: !(Maybe Text)
  , _devEngine :: !(Maybe Text)
  , _devDBParameterGroupFamily :: !(Maybe Text)
  , _devDBEngineDescription :: !(Maybe Text)
  , _devValidUpgradeTarget :: !(Maybe [UpgradeTarget])
  , _devSupportsLogExportsToCloudwatchLogs :: !(Maybe Bool)
  , _devExportableLogTypes :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBEngineVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'devEngineVersion' - The version number of the database engine.
--
-- * 'devDBEngineVersionDescription' - The description of the database engine version.
--
-- * 'devEngine' - The name of the database engine.
--
-- * 'devDBParameterGroupFamily' - The name of the DB parameter group family for the database engine.
--
-- * 'devDBEngineDescription' - The description of the database engine.
--
-- * 'devValidUpgradeTarget' - A list of engine versions that this database engine version can be upgraded to.
--
-- * 'devSupportsLogExportsToCloudwatchLogs' - A value that indicates whether the engine version supports exporting the log types specified by @ExportableLogTypes@ to CloudWatch Logs.
--
-- * 'devExportableLogTypes' - The types of logs that the database engine has available for export to Amazon CloudWatch Logs.
dbEngineVersion
    :: DBEngineVersion
dbEngineVersion =
  DBEngineVersion'
    { _devEngineVersion = Nothing
    , _devDBEngineVersionDescription = Nothing
    , _devEngine = Nothing
    , _devDBParameterGroupFamily = Nothing
    , _devDBEngineDescription = Nothing
    , _devValidUpgradeTarget = Nothing
    , _devSupportsLogExportsToCloudwatchLogs = Nothing
    , _devExportableLogTypes = Nothing
    }


-- | The version number of the database engine.
devEngineVersion :: Lens' DBEngineVersion (Maybe Text)
devEngineVersion = lens _devEngineVersion (\ s a -> s{_devEngineVersion = a})

-- | The description of the database engine version.
devDBEngineVersionDescription :: Lens' DBEngineVersion (Maybe Text)
devDBEngineVersionDescription = lens _devDBEngineVersionDescription (\ s a -> s{_devDBEngineVersionDescription = a})

-- | The name of the database engine.
devEngine :: Lens' DBEngineVersion (Maybe Text)
devEngine = lens _devEngine (\ s a -> s{_devEngine = a})

-- | The name of the DB parameter group family for the database engine.
devDBParameterGroupFamily :: Lens' DBEngineVersion (Maybe Text)
devDBParameterGroupFamily = lens _devDBParameterGroupFamily (\ s a -> s{_devDBParameterGroupFamily = a})

-- | The description of the database engine.
devDBEngineDescription :: Lens' DBEngineVersion (Maybe Text)
devDBEngineDescription = lens _devDBEngineDescription (\ s a -> s{_devDBEngineDescription = a})

-- | A list of engine versions that this database engine version can be upgraded to.
devValidUpgradeTarget :: Lens' DBEngineVersion [UpgradeTarget]
devValidUpgradeTarget = lens _devValidUpgradeTarget (\ s a -> s{_devValidUpgradeTarget = a}) . _Default . _Coerce

-- | A value that indicates whether the engine version supports exporting the log types specified by @ExportableLogTypes@ to CloudWatch Logs.
devSupportsLogExportsToCloudwatchLogs :: Lens' DBEngineVersion (Maybe Bool)
devSupportsLogExportsToCloudwatchLogs = lens _devSupportsLogExportsToCloudwatchLogs (\ s a -> s{_devSupportsLogExportsToCloudwatchLogs = a})

-- | The types of logs that the database engine has available for export to Amazon CloudWatch Logs.
devExportableLogTypes :: Lens' DBEngineVersion [Text]
devExportableLogTypes = lens _devExportableLogTypes (\ s a -> s{_devExportableLogTypes = a}) . _Default . _Coerce

instance FromXML DBEngineVersion where
        parseXML x
          = DBEngineVersion' <$>
              (x .@? "EngineVersion") <*>
                (x .@? "DBEngineVersionDescription")
                <*> (x .@? "Engine")
                <*> (x .@? "DBParameterGroupFamily")
                <*> (x .@? "DBEngineDescription")
                <*>
                (x .@? "ValidUpgradeTarget" .!@ mempty >>=
                   may (parseXMLList "UpgradeTarget"))
                <*> (x .@? "SupportsLogExportsToCloudwatchLogs")
                <*>
                (x .@? "ExportableLogTypes" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable DBEngineVersion where

instance NFData DBEngineVersion where

-- | Detailed information about a DB instance. 
--
--
--
-- /See:/ 'dbInstance' smart constructor.
data DBInstance = DBInstance'
  { _diEngineVersion :: !(Maybe Text)
  , _diStorageEncrypted :: !(Maybe Bool)
  , _diDBClusterIdentifier :: !(Maybe Text)
  , _diPubliclyAccessible :: !(Maybe Bool)
  , _diAutoMinorVersionUpgrade :: !(Maybe Bool)
  , _diDBInstanceARN :: !(Maybe Text)
  , _diInstanceCreateTime :: !(Maybe ISO8601)
  , _diEngine :: !(Maybe Text)
  , _diLatestRestorableTime :: !(Maybe ISO8601)
  , _diDBInstanceClass :: !(Maybe Text)
  , _diPromotionTier :: !(Maybe Int)
  , _diPreferredMaintenanceWindow :: !(Maybe Text)
  , _diDBInstanceIdentifier :: !(Maybe Text)
  , _diKMSKeyId :: !(Maybe Text)
  , _diPreferredBackupWindow :: !(Maybe Text)
  , _diAvailabilityZone :: !(Maybe Text)
  , _diVPCSecurityGroups :: !(Maybe [VPCSecurityGroupMembership])
  , _diBackupRetentionPeriod :: !(Maybe Int)
  , _diDBSubnetGroup :: !(Maybe DBSubnetGroup)
  , _diEnabledCloudwatchLogsExports :: !(Maybe [Text])
  , _diDBiResourceId :: !(Maybe Text)
  , _diEndpoint :: !(Maybe Endpoint)
  , _diDBInstanceStatus :: !(Maybe Text)
  , _diPendingModifiedValues :: !(Maybe PendingModifiedValues)
  , _diStatusInfos :: !(Maybe [DBInstanceStatusInfo])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diEngineVersion' - Indicates the database engine version.
--
-- * 'diStorageEncrypted' - Specifies whether the DB instance is encrypted.
--
-- * 'diDBClusterIdentifier' - Contains the name of the DB cluster that the DB instance is a member of if the DB instance is a member of a DB cluster.
--
-- * 'diPubliclyAccessible' - Specifies the availability options for the DB instance. A value of @true@ specifies an internet-facing instance with a publicly resolvable DNS name, which resolves to a public IP address. A value of @false@ specifies an internal instance with a DNS name that resolves to a private IP address.
--
-- * 'diAutoMinorVersionUpgrade' - Indicates that minor version patches are applied automatically.
--
-- * 'diDBInstanceARN' - The Amazon Resource Name (ARN) for the DB instance.
--
-- * 'diInstanceCreateTime' - Provides the date and time that the DB instance was created.
--
-- * 'diEngine' - Provides the name of the database engine to be used for this DB instance.
--
-- * 'diLatestRestorableTime' - Specifies the latest time to which a database can be restored with point-in-time restore.
--
-- * 'diDBInstanceClass' - Contains the name of the compute and memory capacity class of the DB instance.
--
-- * 'diPromotionTier' - A value that specifies the order in which an Amazon DocumentDB replica is promoted to the primary instance after a failure of the existing primary instance.
--
-- * 'diPreferredMaintenanceWindow' - Specifies the weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
--
-- * 'diDBInstanceIdentifier' - Contains a user-provided database identifier. This identifier is the unique key that identifies a DB instance.
--
-- * 'diKMSKeyId' - If @StorageEncrypted@ is @true@ , the AWS KMS key identifier for the encrypted DB instance. 
--
-- * 'diPreferredBackupWindow' - Specifies the daily time range during which automated backups are created if automated backups are enabled, as determined by the @BackupRetentionPeriod@ . 
--
-- * 'diAvailabilityZone' - Specifies the name of the Availability Zone that the DB instance is located in.
--
-- * 'diVPCSecurityGroups' - Provides a list of VPC security group elements that the DB instance belongs to.
--
-- * 'diBackupRetentionPeriod' - Specifies the number of days for which automatic DB snapshots are retained.
--
-- * 'diDBSubnetGroup' - Specifies information on the subnet group that is associated with the DB instance, including the name, description, and subnets in the subnet group.
--
-- * 'diEnabledCloudwatchLogsExports' - A list of log types that this DB instance is configured to export to Amazon CloudWatch Logs.
--
-- * 'diDBiResourceId' - The AWS Region-unique, immutable identifier for the DB instance. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB instance is accessed.
--
-- * 'diEndpoint' - Specifies the connection endpoint.
--
-- * 'diDBInstanceStatus' - Specifies the current state of this database.
--
-- * 'diPendingModifiedValues' - Specifies that changes to the DB instance are pending. This element is included only when changes are pending. Specific changes are identified by subelements.
--
-- * 'diStatusInfos' - The status of a read replica. If the instance is not a read replica, this is blank.
dbInstance
    :: DBInstance
dbInstance =
  DBInstance'
    { _diEngineVersion = Nothing
    , _diStorageEncrypted = Nothing
    , _diDBClusterIdentifier = Nothing
    , _diPubliclyAccessible = Nothing
    , _diAutoMinorVersionUpgrade = Nothing
    , _diDBInstanceARN = Nothing
    , _diInstanceCreateTime = Nothing
    , _diEngine = Nothing
    , _diLatestRestorableTime = Nothing
    , _diDBInstanceClass = Nothing
    , _diPromotionTier = Nothing
    , _diPreferredMaintenanceWindow = Nothing
    , _diDBInstanceIdentifier = Nothing
    , _diKMSKeyId = Nothing
    , _diPreferredBackupWindow = Nothing
    , _diAvailabilityZone = Nothing
    , _diVPCSecurityGroups = Nothing
    , _diBackupRetentionPeriod = Nothing
    , _diDBSubnetGroup = Nothing
    , _diEnabledCloudwatchLogsExports = Nothing
    , _diDBiResourceId = Nothing
    , _diEndpoint = Nothing
    , _diDBInstanceStatus = Nothing
    , _diPendingModifiedValues = Nothing
    , _diStatusInfos = Nothing
    }


-- | Indicates the database engine version.
diEngineVersion :: Lens' DBInstance (Maybe Text)
diEngineVersion = lens _diEngineVersion (\ s a -> s{_diEngineVersion = a})

-- | Specifies whether the DB instance is encrypted.
diStorageEncrypted :: Lens' DBInstance (Maybe Bool)
diStorageEncrypted = lens _diStorageEncrypted (\ s a -> s{_diStorageEncrypted = a})

-- | Contains the name of the DB cluster that the DB instance is a member of if the DB instance is a member of a DB cluster.
diDBClusterIdentifier :: Lens' DBInstance (Maybe Text)
diDBClusterIdentifier = lens _diDBClusterIdentifier (\ s a -> s{_diDBClusterIdentifier = a})

-- | Specifies the availability options for the DB instance. A value of @true@ specifies an internet-facing instance with a publicly resolvable DNS name, which resolves to a public IP address. A value of @false@ specifies an internal instance with a DNS name that resolves to a private IP address.
diPubliclyAccessible :: Lens' DBInstance (Maybe Bool)
diPubliclyAccessible = lens _diPubliclyAccessible (\ s a -> s{_diPubliclyAccessible = a})

-- | Indicates that minor version patches are applied automatically.
diAutoMinorVersionUpgrade :: Lens' DBInstance (Maybe Bool)
diAutoMinorVersionUpgrade = lens _diAutoMinorVersionUpgrade (\ s a -> s{_diAutoMinorVersionUpgrade = a})

-- | The Amazon Resource Name (ARN) for the DB instance.
diDBInstanceARN :: Lens' DBInstance (Maybe Text)
diDBInstanceARN = lens _diDBInstanceARN (\ s a -> s{_diDBInstanceARN = a})

-- | Provides the date and time that the DB instance was created.
diInstanceCreateTime :: Lens' DBInstance (Maybe UTCTime)
diInstanceCreateTime = lens _diInstanceCreateTime (\ s a -> s{_diInstanceCreateTime = a}) . mapping _Time

-- | Provides the name of the database engine to be used for this DB instance.
diEngine :: Lens' DBInstance (Maybe Text)
diEngine = lens _diEngine (\ s a -> s{_diEngine = a})

-- | Specifies the latest time to which a database can be restored with point-in-time restore.
diLatestRestorableTime :: Lens' DBInstance (Maybe UTCTime)
diLatestRestorableTime = lens _diLatestRestorableTime (\ s a -> s{_diLatestRestorableTime = a}) . mapping _Time

-- | Contains the name of the compute and memory capacity class of the DB instance.
diDBInstanceClass :: Lens' DBInstance (Maybe Text)
diDBInstanceClass = lens _diDBInstanceClass (\ s a -> s{_diDBInstanceClass = a})

-- | A value that specifies the order in which an Amazon DocumentDB replica is promoted to the primary instance after a failure of the existing primary instance.
diPromotionTier :: Lens' DBInstance (Maybe Int)
diPromotionTier = lens _diPromotionTier (\ s a -> s{_diPromotionTier = a})

-- | Specifies the weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
diPreferredMaintenanceWindow :: Lens' DBInstance (Maybe Text)
diPreferredMaintenanceWindow = lens _diPreferredMaintenanceWindow (\ s a -> s{_diPreferredMaintenanceWindow = a})

-- | Contains a user-provided database identifier. This identifier is the unique key that identifies a DB instance.
diDBInstanceIdentifier :: Lens' DBInstance (Maybe Text)
diDBInstanceIdentifier = lens _diDBInstanceIdentifier (\ s a -> s{_diDBInstanceIdentifier = a})

-- | If @StorageEncrypted@ is @true@ , the AWS KMS key identifier for the encrypted DB instance. 
diKMSKeyId :: Lens' DBInstance (Maybe Text)
diKMSKeyId = lens _diKMSKeyId (\ s a -> s{_diKMSKeyId = a})

-- | Specifies the daily time range during which automated backups are created if automated backups are enabled, as determined by the @BackupRetentionPeriod@ . 
diPreferredBackupWindow :: Lens' DBInstance (Maybe Text)
diPreferredBackupWindow = lens _diPreferredBackupWindow (\ s a -> s{_diPreferredBackupWindow = a})

-- | Specifies the name of the Availability Zone that the DB instance is located in.
diAvailabilityZone :: Lens' DBInstance (Maybe Text)
diAvailabilityZone = lens _diAvailabilityZone (\ s a -> s{_diAvailabilityZone = a})

-- | Provides a list of VPC security group elements that the DB instance belongs to.
diVPCSecurityGroups :: Lens' DBInstance [VPCSecurityGroupMembership]
diVPCSecurityGroups = lens _diVPCSecurityGroups (\ s a -> s{_diVPCSecurityGroups = a}) . _Default . _Coerce

-- | Specifies the number of days for which automatic DB snapshots are retained.
diBackupRetentionPeriod :: Lens' DBInstance (Maybe Int)
diBackupRetentionPeriod = lens _diBackupRetentionPeriod (\ s a -> s{_diBackupRetentionPeriod = a})

-- | Specifies information on the subnet group that is associated with the DB instance, including the name, description, and subnets in the subnet group.
diDBSubnetGroup :: Lens' DBInstance (Maybe DBSubnetGroup)
diDBSubnetGroup = lens _diDBSubnetGroup (\ s a -> s{_diDBSubnetGroup = a})

-- | A list of log types that this DB instance is configured to export to Amazon CloudWatch Logs.
diEnabledCloudwatchLogsExports :: Lens' DBInstance [Text]
diEnabledCloudwatchLogsExports = lens _diEnabledCloudwatchLogsExports (\ s a -> s{_diEnabledCloudwatchLogsExports = a}) . _Default . _Coerce

-- | The AWS Region-unique, immutable identifier for the DB instance. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB instance is accessed.
diDBiResourceId :: Lens' DBInstance (Maybe Text)
diDBiResourceId = lens _diDBiResourceId (\ s a -> s{_diDBiResourceId = a})

-- | Specifies the connection endpoint.
diEndpoint :: Lens' DBInstance (Maybe Endpoint)
diEndpoint = lens _diEndpoint (\ s a -> s{_diEndpoint = a})

-- | Specifies the current state of this database.
diDBInstanceStatus :: Lens' DBInstance (Maybe Text)
diDBInstanceStatus = lens _diDBInstanceStatus (\ s a -> s{_diDBInstanceStatus = a})

-- | Specifies that changes to the DB instance are pending. This element is included only when changes are pending. Specific changes are identified by subelements.
diPendingModifiedValues :: Lens' DBInstance (Maybe PendingModifiedValues)
diPendingModifiedValues = lens _diPendingModifiedValues (\ s a -> s{_diPendingModifiedValues = a})

-- | The status of a read replica. If the instance is not a read replica, this is blank.
diStatusInfos :: Lens' DBInstance [DBInstanceStatusInfo]
diStatusInfos = lens _diStatusInfos (\ s a -> s{_diStatusInfos = a}) . _Default . _Coerce

instance FromXML DBInstance where
        parseXML x
          = DBInstance' <$>
              (x .@? "EngineVersion") <*>
                (x .@? "StorageEncrypted")
                <*> (x .@? "DBClusterIdentifier")
                <*> (x .@? "PubliclyAccessible")
                <*> (x .@? "AutoMinorVersionUpgrade")
                <*> (x .@? "DBInstanceArn")
                <*> (x .@? "InstanceCreateTime")
                <*> (x .@? "Engine")
                <*> (x .@? "LatestRestorableTime")
                <*> (x .@? "DBInstanceClass")
                <*> (x .@? "PromotionTier")
                <*> (x .@? "PreferredMaintenanceWindow")
                <*> (x .@? "DBInstanceIdentifier")
                <*> (x .@? "KmsKeyId")
                <*> (x .@? "PreferredBackupWindow")
                <*> (x .@? "AvailabilityZone")
                <*>
                (x .@? "VpcSecurityGroups" .!@ mempty >>=
                   may (parseXMLList "VpcSecurityGroupMembership"))
                <*> (x .@? "BackupRetentionPeriod")
                <*> (x .@? "DBSubnetGroup")
                <*>
                (x .@? "EnabledCloudwatchLogsExports" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "DbiResourceId")
                <*> (x .@? "Endpoint")
                <*> (x .@? "DBInstanceStatus")
                <*> (x .@? "PendingModifiedValues")
                <*>
                (x .@? "StatusInfos" .!@ mempty >>=
                   may (parseXMLList "DBInstanceStatusInfo"))

instance Hashable DBInstance where

instance NFData DBInstance where

-- | Provides a list of status information for a DB instance.
--
--
--
-- /See:/ 'dbInstanceStatusInfo' smart constructor.
data DBInstanceStatusInfo = DBInstanceStatusInfo'
  { _disiStatus :: !(Maybe Text)
  , _disiNormal :: !(Maybe Bool)
  , _disiStatusType :: !(Maybe Text)
  , _disiMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBInstanceStatusInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disiStatus' - Status of the DB instance. For a @StatusType@ of read replica, the values can be @replicating@ , error, @stopped@ , or @terminated@ .
--
-- * 'disiNormal' - A Boolean value that is @true@ if the instance is operating normally, or @false@ if the instance is in an error state.
--
-- * 'disiStatusType' - This value is currently "@read replication@ ."
--
-- * 'disiMessage' - Details of the error if there is an error for the instance. If the instance is not in an error state, this value is blank.
dbInstanceStatusInfo
    :: DBInstanceStatusInfo
dbInstanceStatusInfo =
  DBInstanceStatusInfo'
    { _disiStatus = Nothing
    , _disiNormal = Nothing
    , _disiStatusType = Nothing
    , _disiMessage = Nothing
    }


-- | Status of the DB instance. For a @StatusType@ of read replica, the values can be @replicating@ , error, @stopped@ , or @terminated@ .
disiStatus :: Lens' DBInstanceStatusInfo (Maybe Text)
disiStatus = lens _disiStatus (\ s a -> s{_disiStatus = a})

-- | A Boolean value that is @true@ if the instance is operating normally, or @false@ if the instance is in an error state.
disiNormal :: Lens' DBInstanceStatusInfo (Maybe Bool)
disiNormal = lens _disiNormal (\ s a -> s{_disiNormal = a})

-- | This value is currently "@read replication@ ."
disiStatusType :: Lens' DBInstanceStatusInfo (Maybe Text)
disiStatusType = lens _disiStatusType (\ s a -> s{_disiStatusType = a})

-- | Details of the error if there is an error for the instance. If the instance is not in an error state, this value is blank.
disiMessage :: Lens' DBInstanceStatusInfo (Maybe Text)
disiMessage = lens _disiMessage (\ s a -> s{_disiMessage = a})

instance FromXML DBInstanceStatusInfo where
        parseXML x
          = DBInstanceStatusInfo' <$>
              (x .@? "Status") <*> (x .@? "Normal") <*>
                (x .@? "StatusType")
                <*> (x .@? "Message")

instance Hashable DBInstanceStatusInfo where

instance NFData DBInstanceStatusInfo where

-- | Detailed information about a DB subnet group. 
--
--
--
-- /See:/ 'dbSubnetGroup' smart constructor.
data DBSubnetGroup = DBSubnetGroup'
  { _dsgDBSubnetGroupName :: !(Maybe Text)
  , _dsgVPCId :: !(Maybe Text)
  , _dsgSubnets :: !(Maybe [Subnet])
  , _dsgDBSubnetGroupDescription :: !(Maybe Text)
  , _dsgDBSubnetGroupARN :: !(Maybe Text)
  , _dsgSubnetGroupStatus :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsgDBSubnetGroupName' - The name of the DB subnet group.
--
-- * 'dsgVPCId' - Provides the virtual private cloud (VPC) ID of the DB subnet group.
--
-- * 'dsgSubnets' - Detailed information about one or more subnets within a DB subnet group.
--
-- * 'dsgDBSubnetGroupDescription' - Provides the description of the DB subnet group.
--
-- * 'dsgDBSubnetGroupARN' - The Amazon Resource Identifier (ARN) for the DB subnet group.
--
-- * 'dsgSubnetGroupStatus' - Provides the status of the DB subnet group.
dbSubnetGroup
    :: DBSubnetGroup
dbSubnetGroup =
  DBSubnetGroup'
    { _dsgDBSubnetGroupName = Nothing
    , _dsgVPCId = Nothing
    , _dsgSubnets = Nothing
    , _dsgDBSubnetGroupDescription = Nothing
    , _dsgDBSubnetGroupARN = Nothing
    , _dsgSubnetGroupStatus = Nothing
    }


-- | The name of the DB subnet group.
dsgDBSubnetGroupName :: Lens' DBSubnetGroup (Maybe Text)
dsgDBSubnetGroupName = lens _dsgDBSubnetGroupName (\ s a -> s{_dsgDBSubnetGroupName = a})

-- | Provides the virtual private cloud (VPC) ID of the DB subnet group.
dsgVPCId :: Lens' DBSubnetGroup (Maybe Text)
dsgVPCId = lens _dsgVPCId (\ s a -> s{_dsgVPCId = a})

-- | Detailed information about one or more subnets within a DB subnet group.
dsgSubnets :: Lens' DBSubnetGroup [Subnet]
dsgSubnets = lens _dsgSubnets (\ s a -> s{_dsgSubnets = a}) . _Default . _Coerce

-- | Provides the description of the DB subnet group.
dsgDBSubnetGroupDescription :: Lens' DBSubnetGroup (Maybe Text)
dsgDBSubnetGroupDescription = lens _dsgDBSubnetGroupDescription (\ s a -> s{_dsgDBSubnetGroupDescription = a})

-- | The Amazon Resource Identifier (ARN) for the DB subnet group.
dsgDBSubnetGroupARN :: Lens' DBSubnetGroup (Maybe Text)
dsgDBSubnetGroupARN = lens _dsgDBSubnetGroupARN (\ s a -> s{_dsgDBSubnetGroupARN = a})

-- | Provides the status of the DB subnet group.
dsgSubnetGroupStatus :: Lens' DBSubnetGroup (Maybe Text)
dsgSubnetGroupStatus = lens _dsgSubnetGroupStatus (\ s a -> s{_dsgSubnetGroupStatus = a})

instance FromXML DBSubnetGroup where
        parseXML x
          = DBSubnetGroup' <$>
              (x .@? "DBSubnetGroupName") <*> (x .@? "VpcId") <*>
                (x .@? "Subnets" .!@ mempty >>=
                   may (parseXMLList "Subnet"))
                <*> (x .@? "DBSubnetGroupDescription")
                <*> (x .@? "DBSubnetGroupArn")
                <*> (x .@? "SubnetGroupStatus")

instance Hashable DBSubnetGroup where

instance NFData DBSubnetGroup where

-- | Network information for accessing a DB cluster or DB instance. Client programs must specify a valid endpoint to access these Amazon DocumentDB resources.
--
--
--
-- /See:/ 'endpoint' smart constructor.
data Endpoint = Endpoint'
  { _eHostedZoneId :: !(Maybe Text)
  , _eAddress :: !(Maybe Text)
  , _ePort :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Endpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eHostedZoneId' - Specifies the ID that Amazon Route 53 assigns when you create a hosted zone.
--
-- * 'eAddress' - Specifies the DNS address of the DB instance.
--
-- * 'ePort' - Specifies the port that the database engine is listening on.
endpoint
    :: Endpoint
endpoint =
  Endpoint' {_eHostedZoneId = Nothing, _eAddress = Nothing, _ePort = Nothing}


-- | Specifies the ID that Amazon Route 53 assigns when you create a hosted zone.
eHostedZoneId :: Lens' Endpoint (Maybe Text)
eHostedZoneId = lens _eHostedZoneId (\ s a -> s{_eHostedZoneId = a})

-- | Specifies the DNS address of the DB instance.
eAddress :: Lens' Endpoint (Maybe Text)
eAddress = lens _eAddress (\ s a -> s{_eAddress = a})

-- | Specifies the port that the database engine is listening on.
ePort :: Lens' Endpoint (Maybe Int)
ePort = lens _ePort (\ s a -> s{_ePort = a})

instance FromXML Endpoint where
        parseXML x
          = Endpoint' <$>
              (x .@? "HostedZoneId") <*> (x .@? "Address") <*>
                (x .@? "Port")

instance Hashable Endpoint where

instance NFData Endpoint where

-- | Contains the result of a successful invocation of the @DescribeEngineDefaultClusterParameters@ operation. 
--
--
--
-- /See:/ 'engineDefaults' smart constructor.
data EngineDefaults = EngineDefaults'
  { _edDBParameterGroupFamily :: !(Maybe Text)
  , _edMarker :: !(Maybe Text)
  , _edParameters :: !(Maybe [Parameter])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EngineDefaults' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edDBParameterGroupFamily' - The name of the DB cluster parameter group family to return the engine parameter information for.
--
-- * 'edMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'edParameters' - The parameters of a particular DB cluster parameter group family.
engineDefaults
    :: EngineDefaults
engineDefaults =
  EngineDefaults'
    { _edDBParameterGroupFamily = Nothing
    , _edMarker = Nothing
    , _edParameters = Nothing
    }


-- | The name of the DB cluster parameter group family to return the engine parameter information for.
edDBParameterGroupFamily :: Lens' EngineDefaults (Maybe Text)
edDBParameterGroupFamily = lens _edDBParameterGroupFamily (\ s a -> s{_edDBParameterGroupFamily = a})

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
edMarker :: Lens' EngineDefaults (Maybe Text)
edMarker = lens _edMarker (\ s a -> s{_edMarker = a})

-- | The parameters of a particular DB cluster parameter group family.
edParameters :: Lens' EngineDefaults [Parameter]
edParameters = lens _edParameters (\ s a -> s{_edParameters = a}) . _Default . _Coerce

instance FromXML EngineDefaults where
        parseXML x
          = EngineDefaults' <$>
              (x .@? "DBParameterGroupFamily") <*> (x .@? "Marker")
                <*>
                (x .@? "Parameters" .!@ mempty >>=
                   may (parseXMLList "Parameter"))

instance Hashable EngineDefaults where

instance NFData EngineDefaults where

-- | Detailed information about an event.
--
--
--
-- /See:/ 'event' smart constructor.
data Event = Event'
  { _eSourceType :: !(Maybe SourceType)
  , _eSourceARN :: !(Maybe Text)
  , _eSourceIdentifier :: !(Maybe Text)
  , _eDate :: !(Maybe ISO8601)
  , _eEventCategories :: !(Maybe [Text])
  , _eMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eSourceType' - Specifies the source type for this event.
--
-- * 'eSourceARN' - The Amazon Resource Name (ARN) for the event.
--
-- * 'eSourceIdentifier' - Provides the identifier for the source of the event.
--
-- * 'eDate' - Specifies the date and time of the event.
--
-- * 'eEventCategories' - Specifies the category for the event.
--
-- * 'eMessage' - Provides the text of this event.
event
    :: Event
event =
  Event'
    { _eSourceType = Nothing
    , _eSourceARN = Nothing
    , _eSourceIdentifier = Nothing
    , _eDate = Nothing
    , _eEventCategories = Nothing
    , _eMessage = Nothing
    }


-- | Specifies the source type for this event.
eSourceType :: Lens' Event (Maybe SourceType)
eSourceType = lens _eSourceType (\ s a -> s{_eSourceType = a})

-- | The Amazon Resource Name (ARN) for the event.
eSourceARN :: Lens' Event (Maybe Text)
eSourceARN = lens _eSourceARN (\ s a -> s{_eSourceARN = a})

-- | Provides the identifier for the source of the event.
eSourceIdentifier :: Lens' Event (Maybe Text)
eSourceIdentifier = lens _eSourceIdentifier (\ s a -> s{_eSourceIdentifier = a})

-- | Specifies the date and time of the event.
eDate :: Lens' Event (Maybe UTCTime)
eDate = lens _eDate (\ s a -> s{_eDate = a}) . mapping _Time

-- | Specifies the category for the event.
eEventCategories :: Lens' Event [Text]
eEventCategories = lens _eEventCategories (\ s a -> s{_eEventCategories = a}) . _Default . _Coerce

-- | Provides the text of this event.
eMessage :: Lens' Event (Maybe Text)
eMessage = lens _eMessage (\ s a -> s{_eMessage = a})

instance FromXML Event where
        parseXML x
          = Event' <$>
              (x .@? "SourceType") <*> (x .@? "SourceArn") <*>
                (x .@? "SourceIdentifier")
                <*> (x .@? "Date")
                <*>
                (x .@? "EventCategories" .!@ mempty >>=
                   may (parseXMLList "EventCategory"))
                <*> (x .@? "Message")

instance Hashable Event where

instance NFData Event where

-- | An event source type, accompanied by one or more event category names.
--
--
--
-- /See:/ 'eventCategoriesMap' smart constructor.
data EventCategoriesMap = EventCategoriesMap'
  { _ecmSourceType :: !(Maybe Text)
  , _ecmEventCategories :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventCategoriesMap' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecmSourceType' - The source type that the returned categories belong to.
--
-- * 'ecmEventCategories' - The event categories for the specified source type.
eventCategoriesMap
    :: EventCategoriesMap
eventCategoriesMap =
  EventCategoriesMap' {_ecmSourceType = Nothing, _ecmEventCategories = Nothing}


-- | The source type that the returned categories belong to.
ecmSourceType :: Lens' EventCategoriesMap (Maybe Text)
ecmSourceType = lens _ecmSourceType (\ s a -> s{_ecmSourceType = a})

-- | The event categories for the specified source type.
ecmEventCategories :: Lens' EventCategoriesMap [Text]
ecmEventCategories = lens _ecmEventCategories (\ s a -> s{_ecmEventCategories = a}) . _Default . _Coerce

instance FromXML EventCategoriesMap where
        parseXML x
          = EventCategoriesMap' <$>
              (x .@? "SourceType") <*>
                (x .@? "EventCategories" .!@ mempty >>=
                   may (parseXMLList "EventCategory"))

instance Hashable EventCategoriesMap where

instance NFData EventCategoriesMap where

-- | A named set of filter values, used to return a more specific list of results. You can use a filter to match a set of resources by specific criteria, such as IDs.
--
--
-- Wildcards are not supported in filters.
--
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter'
  { _fName :: !Text
  , _fValues :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fName' - The name of the filter. Filter names are case sensitive.
--
-- * 'fValues' - One or more filter values. Filter values are case sensitive.
filter'
    :: Text -- ^ 'fName'
    -> Filter
filter' pName_ = Filter' {_fName = pName_, _fValues = mempty}


-- | The name of the filter. Filter names are case sensitive.
fName :: Lens' Filter Text
fName = lens _fName (\ s a -> s{_fName = a})

-- | One or more filter values. Filter values are case sensitive.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\ s a -> s{_fValues = a}) . _Coerce

instance Hashable Filter where

instance NFData Filter where

instance ToQuery Filter where
        toQuery Filter'{..}
          = mconcat
              ["Name" =: _fName,
               "Values" =: toQueryList "Value" _fValues]

-- | The options that are available for a DB instance.
--
--
--
-- /See:/ 'orderableDBInstanceOption' smart constructor.
data OrderableDBInstanceOption = OrderableDBInstanceOption'
  { _odioEngineVersion :: !(Maybe Text)
  , _odioEngine :: !(Maybe Text)
  , _odioDBInstanceClass :: !(Maybe Text)
  , _odioLicenseModel :: !(Maybe Text)
  , _odioAvailabilityZones :: !(Maybe [AvailabilityZone])
  , _odioVPC :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OrderableDBInstanceOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'odioEngineVersion' - The engine version of a DB instance.
--
-- * 'odioEngine' - The engine type of a DB instance.
--
-- * 'odioDBInstanceClass' - The DB instance class for a DB instance.
--
-- * 'odioLicenseModel' - The license model for a DB instance.
--
-- * 'odioAvailabilityZones' - A list of Availability Zones for a DB instance.
--
-- * 'odioVPC' - Indicates whether a DB instance is in a virtual private cloud (VPC).
orderableDBInstanceOption
    :: OrderableDBInstanceOption
orderableDBInstanceOption =
  OrderableDBInstanceOption'
    { _odioEngineVersion = Nothing
    , _odioEngine = Nothing
    , _odioDBInstanceClass = Nothing
    , _odioLicenseModel = Nothing
    , _odioAvailabilityZones = Nothing
    , _odioVPC = Nothing
    }


-- | The engine version of a DB instance.
odioEngineVersion :: Lens' OrderableDBInstanceOption (Maybe Text)
odioEngineVersion = lens _odioEngineVersion (\ s a -> s{_odioEngineVersion = a})

-- | The engine type of a DB instance.
odioEngine :: Lens' OrderableDBInstanceOption (Maybe Text)
odioEngine = lens _odioEngine (\ s a -> s{_odioEngine = a})

-- | The DB instance class for a DB instance.
odioDBInstanceClass :: Lens' OrderableDBInstanceOption (Maybe Text)
odioDBInstanceClass = lens _odioDBInstanceClass (\ s a -> s{_odioDBInstanceClass = a})

-- | The license model for a DB instance.
odioLicenseModel :: Lens' OrderableDBInstanceOption (Maybe Text)
odioLicenseModel = lens _odioLicenseModel (\ s a -> s{_odioLicenseModel = a})

-- | A list of Availability Zones for a DB instance.
odioAvailabilityZones :: Lens' OrderableDBInstanceOption [AvailabilityZone]
odioAvailabilityZones = lens _odioAvailabilityZones (\ s a -> s{_odioAvailabilityZones = a}) . _Default . _Coerce

-- | Indicates whether a DB instance is in a virtual private cloud (VPC).
odioVPC :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioVPC = lens _odioVPC (\ s a -> s{_odioVPC = a})

instance FromXML OrderableDBInstanceOption where
        parseXML x
          = OrderableDBInstanceOption' <$>
              (x .@? "EngineVersion") <*> (x .@? "Engine") <*>
                (x .@? "DBInstanceClass")
                <*> (x .@? "LicenseModel")
                <*>
                (x .@? "AvailabilityZones" .!@ mempty >>=
                   may (parseXMLList "AvailabilityZone"))
                <*> (x .@? "Vpc")

instance Hashable OrderableDBInstanceOption where

instance NFData OrderableDBInstanceOption where

-- | Detailed information about an individual parameter.
--
--
--
-- /See:/ 'parameter' smart constructor.
data Parameter = Parameter'
  { _pApplyType :: !(Maybe Text)
  , _pParameterValue :: !(Maybe Text)
  , _pApplyMethod :: !(Maybe ApplyMethod)
  , _pMinimumEngineVersion :: !(Maybe Text)
  , _pSource :: !(Maybe Text)
  , _pIsModifiable :: !(Maybe Bool)
  , _pDataType :: !(Maybe Text)
  , _pAllowedValues :: !(Maybe Text)
  , _pParameterName :: !(Maybe Text)
  , _pDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Parameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pApplyType' - Specifies the engine-specific parameters type.
--
-- * 'pParameterValue' - Specifies the value of the parameter.
--
-- * 'pApplyMethod' - Indicates when to apply parameter updates.
--
-- * 'pMinimumEngineVersion' - The earliest engine version to which the parameter can apply.
--
-- * 'pSource' - Indicates the source of the parameter value.
--
-- * 'pIsModifiable' - Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed. 
--
-- * 'pDataType' - Specifies the valid data type for the parameter.
--
-- * 'pAllowedValues' - Specifies the valid range of values for the parameter.
--
-- * 'pParameterName' - Specifies the name of the parameter.
--
-- * 'pDescription' - Provides a description of the parameter.
parameter
    :: Parameter
parameter =
  Parameter'
    { _pApplyType = Nothing
    , _pParameterValue = Nothing
    , _pApplyMethod = Nothing
    , _pMinimumEngineVersion = Nothing
    , _pSource = Nothing
    , _pIsModifiable = Nothing
    , _pDataType = Nothing
    , _pAllowedValues = Nothing
    , _pParameterName = Nothing
    , _pDescription = Nothing
    }


-- | Specifies the engine-specific parameters type.
pApplyType :: Lens' Parameter (Maybe Text)
pApplyType = lens _pApplyType (\ s a -> s{_pApplyType = a})

-- | Specifies the value of the parameter.
pParameterValue :: Lens' Parameter (Maybe Text)
pParameterValue = lens _pParameterValue (\ s a -> s{_pParameterValue = a})

-- | Indicates when to apply parameter updates.
pApplyMethod :: Lens' Parameter (Maybe ApplyMethod)
pApplyMethod = lens _pApplyMethod (\ s a -> s{_pApplyMethod = a})

-- | The earliest engine version to which the parameter can apply.
pMinimumEngineVersion :: Lens' Parameter (Maybe Text)
pMinimumEngineVersion = lens _pMinimumEngineVersion (\ s a -> s{_pMinimumEngineVersion = a})

-- | Indicates the source of the parameter value.
pSource :: Lens' Parameter (Maybe Text)
pSource = lens _pSource (\ s a -> s{_pSource = a})

-- | Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed. 
pIsModifiable :: Lens' Parameter (Maybe Bool)
pIsModifiable = lens _pIsModifiable (\ s a -> s{_pIsModifiable = a})

-- | Specifies the valid data type for the parameter.
pDataType :: Lens' Parameter (Maybe Text)
pDataType = lens _pDataType (\ s a -> s{_pDataType = a})

-- | Specifies the valid range of values for the parameter.
pAllowedValues :: Lens' Parameter (Maybe Text)
pAllowedValues = lens _pAllowedValues (\ s a -> s{_pAllowedValues = a})

-- | Specifies the name of the parameter.
pParameterName :: Lens' Parameter (Maybe Text)
pParameterName = lens _pParameterName (\ s a -> s{_pParameterName = a})

-- | Provides a description of the parameter.
pDescription :: Lens' Parameter (Maybe Text)
pDescription = lens _pDescription (\ s a -> s{_pDescription = a})

instance FromXML Parameter where
        parseXML x
          = Parameter' <$>
              (x .@? "ApplyType") <*> (x .@? "ParameterValue") <*>
                (x .@? "ApplyMethod")
                <*> (x .@? "MinimumEngineVersion")
                <*> (x .@? "Source")
                <*> (x .@? "IsModifiable")
                <*> (x .@? "DataType")
                <*> (x .@? "AllowedValues")
                <*> (x .@? "ParameterName")
                <*> (x .@? "Description")

instance Hashable Parameter where

instance NFData Parameter where

instance ToQuery Parameter where
        toQuery Parameter'{..}
          = mconcat
              ["ApplyType" =: _pApplyType,
               "ParameterValue" =: _pParameterValue,
               "ApplyMethod" =: _pApplyMethod,
               "MinimumEngineVersion" =: _pMinimumEngineVersion,
               "Source" =: _pSource,
               "IsModifiable" =: _pIsModifiable,
               "DataType" =: _pDataType,
               "AllowedValues" =: _pAllowedValues,
               "ParameterName" =: _pParameterName,
               "Description" =: _pDescription]

-- | A list of the log types whose configuration is still pending. These log types are in the process of being activated or deactivated.
--
--
--
-- /See:/ 'pendingCloudwatchLogsExports' smart constructor.
data PendingCloudwatchLogsExports = PendingCloudwatchLogsExports'
  { _pcleLogTypesToEnable :: !(Maybe [Text])
  , _pcleLogTypesToDisable :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PendingCloudwatchLogsExports' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcleLogTypesToEnable' - Log types that are in the process of being deactivated. After they are deactivated, these log types aren't exported to CloudWatch Logs.
--
-- * 'pcleLogTypesToDisable' - Log types that are in the process of being enabled. After they are enabled, these log types are exported to Amazon CloudWatch Logs.
pendingCloudwatchLogsExports
    :: PendingCloudwatchLogsExports
pendingCloudwatchLogsExports =
  PendingCloudwatchLogsExports'
    {_pcleLogTypesToEnable = Nothing, _pcleLogTypesToDisable = Nothing}


-- | Log types that are in the process of being deactivated. After they are deactivated, these log types aren't exported to CloudWatch Logs.
pcleLogTypesToEnable :: Lens' PendingCloudwatchLogsExports [Text]
pcleLogTypesToEnable = lens _pcleLogTypesToEnable (\ s a -> s{_pcleLogTypesToEnable = a}) . _Default . _Coerce

-- | Log types that are in the process of being enabled. After they are enabled, these log types are exported to Amazon CloudWatch Logs.
pcleLogTypesToDisable :: Lens' PendingCloudwatchLogsExports [Text]
pcleLogTypesToDisable = lens _pcleLogTypesToDisable (\ s a -> s{_pcleLogTypesToDisable = a}) . _Default . _Coerce

instance FromXML PendingCloudwatchLogsExports where
        parseXML x
          = PendingCloudwatchLogsExports' <$>
              (x .@? "LogTypesToEnable" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*>
                (x .@? "LogTypesToDisable" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable PendingCloudwatchLogsExports where

instance NFData PendingCloudwatchLogsExports where

-- | Provides information about a pending maintenance action for a resource.
--
--
--
-- /See:/ 'pendingMaintenanceAction' smart constructor.
data PendingMaintenanceAction = PendingMaintenanceAction'
  { _pmaAutoAppliedAfterDate :: !(Maybe ISO8601)
  , _pmaAction :: !(Maybe Text)
  , _pmaOptInStatus :: !(Maybe Text)
  , _pmaDescription :: !(Maybe Text)
  , _pmaForcedApplyDate :: !(Maybe ISO8601)
  , _pmaCurrentApplyDate :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PendingMaintenanceAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmaAutoAppliedAfterDate' - The date of the maintenance window when the action is applied. The maintenance action is applied to the resource during its first maintenance window after this date. If this date is specified, any @next-maintenance@ opt-in requests are ignored.
--
-- * 'pmaAction' - The type of pending maintenance action that is available for the resource.
--
-- * 'pmaOptInStatus' - Indicates the type of opt-in request that has been received for the resource.
--
-- * 'pmaDescription' - A description providing more detail about the maintenance action.
--
-- * 'pmaForcedApplyDate' - The date when the maintenance action is automatically applied. The maintenance action is applied to the resource on this date regardless of the maintenance window for the resource. If this date is specified, any @immediate@ opt-in requests are ignored.
--
-- * 'pmaCurrentApplyDate' - The effective date when the pending maintenance action is applied to the resource.
pendingMaintenanceAction
    :: PendingMaintenanceAction
pendingMaintenanceAction =
  PendingMaintenanceAction'
    { _pmaAutoAppliedAfterDate = Nothing
    , _pmaAction = Nothing
    , _pmaOptInStatus = Nothing
    , _pmaDescription = Nothing
    , _pmaForcedApplyDate = Nothing
    , _pmaCurrentApplyDate = Nothing
    }


-- | The date of the maintenance window when the action is applied. The maintenance action is applied to the resource during its first maintenance window after this date. If this date is specified, any @next-maintenance@ opt-in requests are ignored.
pmaAutoAppliedAfterDate :: Lens' PendingMaintenanceAction (Maybe UTCTime)
pmaAutoAppliedAfterDate = lens _pmaAutoAppliedAfterDate (\ s a -> s{_pmaAutoAppliedAfterDate = a}) . mapping _Time

-- | The type of pending maintenance action that is available for the resource.
pmaAction :: Lens' PendingMaintenanceAction (Maybe Text)
pmaAction = lens _pmaAction (\ s a -> s{_pmaAction = a})

-- | Indicates the type of opt-in request that has been received for the resource.
pmaOptInStatus :: Lens' PendingMaintenanceAction (Maybe Text)
pmaOptInStatus = lens _pmaOptInStatus (\ s a -> s{_pmaOptInStatus = a})

-- | A description providing more detail about the maintenance action.
pmaDescription :: Lens' PendingMaintenanceAction (Maybe Text)
pmaDescription = lens _pmaDescription (\ s a -> s{_pmaDescription = a})

-- | The date when the maintenance action is automatically applied. The maintenance action is applied to the resource on this date regardless of the maintenance window for the resource. If this date is specified, any @immediate@ opt-in requests are ignored.
pmaForcedApplyDate :: Lens' PendingMaintenanceAction (Maybe UTCTime)
pmaForcedApplyDate = lens _pmaForcedApplyDate (\ s a -> s{_pmaForcedApplyDate = a}) . mapping _Time

-- | The effective date when the pending maintenance action is applied to the resource.
pmaCurrentApplyDate :: Lens' PendingMaintenanceAction (Maybe UTCTime)
pmaCurrentApplyDate = lens _pmaCurrentApplyDate (\ s a -> s{_pmaCurrentApplyDate = a}) . mapping _Time

instance FromXML PendingMaintenanceAction where
        parseXML x
          = PendingMaintenanceAction' <$>
              (x .@? "AutoAppliedAfterDate") <*> (x .@? "Action")
                <*> (x .@? "OptInStatus")
                <*> (x .@? "Description")
                <*> (x .@? "ForcedApplyDate")
                <*> (x .@? "CurrentApplyDate")

instance Hashable PendingMaintenanceAction where

instance NFData PendingMaintenanceAction where

-- | One or more modified settings for a DB instance. These modified settings have been requested, but haven't been applied yet.
--
--
--
-- /See:/ 'pendingModifiedValues' smart constructor.
data PendingModifiedValues = PendingModifiedValues'
  { _pmvEngineVersion :: !(Maybe Text)
  , _pmvMasterUserPassword :: !(Maybe Text)
  , _pmvDBSubnetGroupName :: !(Maybe Text)
  , _pmvIOPS :: !(Maybe Int)
  , _pmvDBInstanceClass :: !(Maybe Text)
  , _pmvLicenseModel :: !(Maybe Text)
  , _pmvCACertificateIdentifier :: !(Maybe Text)
  , _pmvDBInstanceIdentifier :: !(Maybe Text)
  , _pmvPendingCloudwatchLogsExports :: !(Maybe PendingCloudwatchLogsExports)
  , _pmvBackupRetentionPeriod :: !(Maybe Int)
  , _pmvMultiAZ :: !(Maybe Bool)
  , _pmvAllocatedStorage :: !(Maybe Int)
  , _pmvPort :: !(Maybe Int)
  , _pmvStorageType :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PendingModifiedValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmvEngineVersion' - Indicates the database engine version.
--
-- * 'pmvMasterUserPassword' - Contains the pending or currently in-progress change of the master credentials for the DB instance.
--
-- * 'pmvDBSubnetGroupName' - The new DB subnet group for the DB instance. 
--
-- * 'pmvIOPS' - Specifies the new Provisioned IOPS value for the DB instance that will be applied or is currently being applied.
--
-- * 'pmvDBInstanceClass' - Contains the new @DBInstanceClass@ for the DB instance that will be applied or is currently being applied. 
--
-- * 'pmvLicenseModel' - The license model for the DB instance. Valid values: @license-included@ , @bring-your-own-license@ , @general-public-license@ 
--
-- * 'pmvCACertificateIdentifier' - Specifies the identifier of the certificate authority (CA) certificate for the DB instance.
--
-- * 'pmvDBInstanceIdentifier' - Contains the new @DBInstanceIdentifier@ for the DB instance that will be applied or is currently being applied. 
--
-- * 'pmvPendingCloudwatchLogsExports' - A list of the log types whose configuration is still pending. These log types are in the process of being activated or deactivated.
--
-- * 'pmvBackupRetentionPeriod' - Specifies the pending number of days for which automated backups are retained.
--
-- * 'pmvMultiAZ' - Indicates that the Single-AZ DB instance is to change to a Multi-AZ deployment.
--
-- * 'pmvAllocatedStorage' - Contains the new @AllocatedStorage@ size for the DB instance that will be applied or is currently being applied. 
--
-- * 'pmvPort' - Specifies the pending port for the DB instance.
--
-- * 'pmvStorageType' - Specifies the storage type to be associated with the DB instance.
pendingModifiedValues
    :: PendingModifiedValues
pendingModifiedValues =
  PendingModifiedValues'
    { _pmvEngineVersion = Nothing
    , _pmvMasterUserPassword = Nothing
    , _pmvDBSubnetGroupName = Nothing
    , _pmvIOPS = Nothing
    , _pmvDBInstanceClass = Nothing
    , _pmvLicenseModel = Nothing
    , _pmvCACertificateIdentifier = Nothing
    , _pmvDBInstanceIdentifier = Nothing
    , _pmvPendingCloudwatchLogsExports = Nothing
    , _pmvBackupRetentionPeriod = Nothing
    , _pmvMultiAZ = Nothing
    , _pmvAllocatedStorage = Nothing
    , _pmvPort = Nothing
    , _pmvStorageType = Nothing
    }


-- | Indicates the database engine version.
pmvEngineVersion :: Lens' PendingModifiedValues (Maybe Text)
pmvEngineVersion = lens _pmvEngineVersion (\ s a -> s{_pmvEngineVersion = a})

-- | Contains the pending or currently in-progress change of the master credentials for the DB instance.
pmvMasterUserPassword :: Lens' PendingModifiedValues (Maybe Text)
pmvMasterUserPassword = lens _pmvMasterUserPassword (\ s a -> s{_pmvMasterUserPassword = a})

-- | The new DB subnet group for the DB instance. 
pmvDBSubnetGroupName :: Lens' PendingModifiedValues (Maybe Text)
pmvDBSubnetGroupName = lens _pmvDBSubnetGroupName (\ s a -> s{_pmvDBSubnetGroupName = a})

-- | Specifies the new Provisioned IOPS value for the DB instance that will be applied or is currently being applied.
pmvIOPS :: Lens' PendingModifiedValues (Maybe Int)
pmvIOPS = lens _pmvIOPS (\ s a -> s{_pmvIOPS = a})

-- | Contains the new @DBInstanceClass@ for the DB instance that will be applied or is currently being applied. 
pmvDBInstanceClass :: Lens' PendingModifiedValues (Maybe Text)
pmvDBInstanceClass = lens _pmvDBInstanceClass (\ s a -> s{_pmvDBInstanceClass = a})

-- | The license model for the DB instance. Valid values: @license-included@ , @bring-your-own-license@ , @general-public-license@ 
pmvLicenseModel :: Lens' PendingModifiedValues (Maybe Text)
pmvLicenseModel = lens _pmvLicenseModel (\ s a -> s{_pmvLicenseModel = a})

-- | Specifies the identifier of the certificate authority (CA) certificate for the DB instance.
pmvCACertificateIdentifier :: Lens' PendingModifiedValues (Maybe Text)
pmvCACertificateIdentifier = lens _pmvCACertificateIdentifier (\ s a -> s{_pmvCACertificateIdentifier = a})

-- | Contains the new @DBInstanceIdentifier@ for the DB instance that will be applied or is currently being applied. 
pmvDBInstanceIdentifier :: Lens' PendingModifiedValues (Maybe Text)
pmvDBInstanceIdentifier = lens _pmvDBInstanceIdentifier (\ s a -> s{_pmvDBInstanceIdentifier = a})

-- | A list of the log types whose configuration is still pending. These log types are in the process of being activated or deactivated.
pmvPendingCloudwatchLogsExports :: Lens' PendingModifiedValues (Maybe PendingCloudwatchLogsExports)
pmvPendingCloudwatchLogsExports = lens _pmvPendingCloudwatchLogsExports (\ s a -> s{_pmvPendingCloudwatchLogsExports = a})

-- | Specifies the pending number of days for which automated backups are retained.
pmvBackupRetentionPeriod :: Lens' PendingModifiedValues (Maybe Int)
pmvBackupRetentionPeriod = lens _pmvBackupRetentionPeriod (\ s a -> s{_pmvBackupRetentionPeriod = a})

-- | Indicates that the Single-AZ DB instance is to change to a Multi-AZ deployment.
pmvMultiAZ :: Lens' PendingModifiedValues (Maybe Bool)
pmvMultiAZ = lens _pmvMultiAZ (\ s a -> s{_pmvMultiAZ = a})

-- | Contains the new @AllocatedStorage@ size for the DB instance that will be applied or is currently being applied. 
pmvAllocatedStorage :: Lens' PendingModifiedValues (Maybe Int)
pmvAllocatedStorage = lens _pmvAllocatedStorage (\ s a -> s{_pmvAllocatedStorage = a})

-- | Specifies the pending port for the DB instance.
pmvPort :: Lens' PendingModifiedValues (Maybe Int)
pmvPort = lens _pmvPort (\ s a -> s{_pmvPort = a})

-- | Specifies the storage type to be associated with the DB instance.
pmvStorageType :: Lens' PendingModifiedValues (Maybe Text)
pmvStorageType = lens _pmvStorageType (\ s a -> s{_pmvStorageType = a})

instance FromXML PendingModifiedValues where
        parseXML x
          = PendingModifiedValues' <$>
              (x .@? "EngineVersion") <*>
                (x .@? "MasterUserPassword")
                <*> (x .@? "DBSubnetGroupName")
                <*> (x .@? "Iops")
                <*> (x .@? "DBInstanceClass")
                <*> (x .@? "LicenseModel")
                <*> (x .@? "CACertificateIdentifier")
                <*> (x .@? "DBInstanceIdentifier")
                <*> (x .@? "PendingCloudwatchLogsExports")
                <*> (x .@? "BackupRetentionPeriod")
                <*> (x .@? "MultiAZ")
                <*> (x .@? "AllocatedStorage")
                <*> (x .@? "Port")
                <*> (x .@? "StorageType")

instance Hashable PendingModifiedValues where

instance NFData PendingModifiedValues where

-- | Represents the output of 'ApplyPendingMaintenanceAction' .
--
--
--
-- /See:/ 'resourcePendingMaintenanceActions' smart constructor.
data ResourcePendingMaintenanceActions = ResourcePendingMaintenanceActions'
  { _rpmaPendingMaintenanceActionDetails :: !(Maybe [PendingMaintenanceAction])
  , _rpmaResourceIdentifier :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourcePendingMaintenanceActions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpmaPendingMaintenanceActionDetails' - A list that provides details about the pending maintenance actions for the resource.
--
-- * 'rpmaResourceIdentifier' - The Amazon Resource Name (ARN) of the resource that has pending maintenance actions.
resourcePendingMaintenanceActions
    :: ResourcePendingMaintenanceActions
resourcePendingMaintenanceActions =
  ResourcePendingMaintenanceActions'
    { _rpmaPendingMaintenanceActionDetails = Nothing
    , _rpmaResourceIdentifier = Nothing
    }


-- | A list that provides details about the pending maintenance actions for the resource.
rpmaPendingMaintenanceActionDetails :: Lens' ResourcePendingMaintenanceActions [PendingMaintenanceAction]
rpmaPendingMaintenanceActionDetails = lens _rpmaPendingMaintenanceActionDetails (\ s a -> s{_rpmaPendingMaintenanceActionDetails = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the resource that has pending maintenance actions.
rpmaResourceIdentifier :: Lens' ResourcePendingMaintenanceActions (Maybe Text)
rpmaResourceIdentifier = lens _rpmaResourceIdentifier (\ s a -> s{_rpmaResourceIdentifier = a})

instance FromXML ResourcePendingMaintenanceActions
         where
        parseXML x
          = ResourcePendingMaintenanceActions' <$>
              (x .@? "PendingMaintenanceActionDetails" .!@ mempty
                 >>= may (parseXMLList "PendingMaintenanceAction"))
                <*> (x .@? "ResourceIdentifier")

instance Hashable ResourcePendingMaintenanceActions
         where

instance NFData ResourcePendingMaintenanceActions
         where

-- | Detailed information about a subnet. 
--
--
--
-- /See:/ 'subnet' smart constructor.
data Subnet = Subnet'
  { _sSubnetStatus :: !(Maybe Text)
  , _sSubnetIdentifier :: !(Maybe Text)
  , _sSubnetAvailabilityZone :: !(Maybe AvailabilityZone)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Subnet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSubnetStatus' - Specifies the status of the subnet.
--
-- * 'sSubnetIdentifier' - Specifies the identifier of the subnet.
--
-- * 'sSubnetAvailabilityZone' - Specifies the Availability Zone for the subnet.
subnet
    :: Subnet
subnet =
  Subnet'
    { _sSubnetStatus = Nothing
    , _sSubnetIdentifier = Nothing
    , _sSubnetAvailabilityZone = Nothing
    }


-- | Specifies the status of the subnet.
sSubnetStatus :: Lens' Subnet (Maybe Text)
sSubnetStatus = lens _sSubnetStatus (\ s a -> s{_sSubnetStatus = a})

-- | Specifies the identifier of the subnet.
sSubnetIdentifier :: Lens' Subnet (Maybe Text)
sSubnetIdentifier = lens _sSubnetIdentifier (\ s a -> s{_sSubnetIdentifier = a})

-- | Specifies the Availability Zone for the subnet.
sSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
sSubnetAvailabilityZone = lens _sSubnetAvailabilityZone (\ s a -> s{_sSubnetAvailabilityZone = a})

instance FromXML Subnet where
        parseXML x
          = Subnet' <$>
              (x .@? "SubnetStatus") <*> (x .@? "SubnetIdentifier")
                <*> (x .@? "SubnetAvailabilityZone")

instance Hashable Subnet where

instance NFData Subnet where

-- | Metadata assigned to an Amazon DocumentDB resource consisting of a key-value pair.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - The optional value of the tag. The string value can be from 1 to 256 Unicode characters in length and can't be prefixed with "aws:" or "rds:". The string can contain only the set of Unicode letters, digits, white space, '_', '.', '/', '=', '+', '-' (Java regex: "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
--
-- * 'tagKey' - The required name of the tag. The string value can be from 1 to 128 Unicode characters in length and can't be prefixed with "aws:" or "rds:". The string can contain only the set of Unicode letters, digits, white space, '_', '.', '/', '=', '+', '-' (Java regex: "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
tag
    :: Tag
tag = Tag' {_tagValue = Nothing, _tagKey = Nothing}


-- | The optional value of the tag. The string value can be from 1 to 256 Unicode characters in length and can't be prefixed with "aws:" or "rds:". The string can contain only the set of Unicode letters, digits, white space, '_', '.', '/', '=', '+', '-' (Java regex: "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | The required name of the tag. The string value can be from 1 to 128 Unicode characters in length and can't be prefixed with "aws:" or "rds:". The string can contain only the set of Unicode letters, digits, white space, '_', '.', '/', '=', '+', '-' (Java regex: "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromXML Tag where
        parseXML x
          = Tag' <$> (x .@? "Value") <*> (x .@? "Key")

instance Hashable Tag where

instance NFData Tag where

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat ["Value" =: _tagValue, "Key" =: _tagKey]

-- | The version of the database engine that a DB instance can be upgraded to.
--
--
--
-- /See:/ 'upgradeTarget' smart constructor.
data UpgradeTarget = UpgradeTarget'
  { _utEngineVersion :: !(Maybe Text)
  , _utIsMajorVersionUpgrade :: !(Maybe Bool)
  , _utEngine :: !(Maybe Text)
  , _utAutoUpgrade :: !(Maybe Bool)
  , _utDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpgradeTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utEngineVersion' - The version number of the upgrade target database engine.
--
-- * 'utIsMajorVersionUpgrade' - A value that indicates whether a database engine is upgraded to a major version.
--
-- * 'utEngine' - The name of the upgrade target database engine.
--
-- * 'utAutoUpgrade' - A value that indicates whether the target version is applied to any source DB instances that have @AutoMinorVersionUpgrade@ set to @true@ .
--
-- * 'utDescription' - The version of the database engine that a DB instance can be upgraded to.
upgradeTarget
    :: UpgradeTarget
upgradeTarget =
  UpgradeTarget'
    { _utEngineVersion = Nothing
    , _utIsMajorVersionUpgrade = Nothing
    , _utEngine = Nothing
    , _utAutoUpgrade = Nothing
    , _utDescription = Nothing
    }


-- | The version number of the upgrade target database engine.
utEngineVersion :: Lens' UpgradeTarget (Maybe Text)
utEngineVersion = lens _utEngineVersion (\ s a -> s{_utEngineVersion = a})

-- | A value that indicates whether a database engine is upgraded to a major version.
utIsMajorVersionUpgrade :: Lens' UpgradeTarget (Maybe Bool)
utIsMajorVersionUpgrade = lens _utIsMajorVersionUpgrade (\ s a -> s{_utIsMajorVersionUpgrade = a})

-- | The name of the upgrade target database engine.
utEngine :: Lens' UpgradeTarget (Maybe Text)
utEngine = lens _utEngine (\ s a -> s{_utEngine = a})

-- | A value that indicates whether the target version is applied to any source DB instances that have @AutoMinorVersionUpgrade@ set to @true@ .
utAutoUpgrade :: Lens' UpgradeTarget (Maybe Bool)
utAutoUpgrade = lens _utAutoUpgrade (\ s a -> s{_utAutoUpgrade = a})

-- | The version of the database engine that a DB instance can be upgraded to.
utDescription :: Lens' UpgradeTarget (Maybe Text)
utDescription = lens _utDescription (\ s a -> s{_utDescription = a})

instance FromXML UpgradeTarget where
        parseXML x
          = UpgradeTarget' <$>
              (x .@? "EngineVersion") <*>
                (x .@? "IsMajorVersionUpgrade")
                <*> (x .@? "Engine")
                <*> (x .@? "AutoUpgrade")
                <*> (x .@? "Description")

instance Hashable UpgradeTarget where

instance NFData UpgradeTarget where

-- | Used as a response element for queries on virtual private cloud (VPC) security group membership.
--
--
--
-- /See:/ 'vpcSecurityGroupMembership' smart constructor.
data VPCSecurityGroupMembership = VPCSecurityGroupMembership'
  { _vsgmStatus :: !(Maybe Text)
  , _vsgmVPCSecurityGroupId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCSecurityGroupMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsgmStatus' - The status of the VPC security group.
--
-- * 'vsgmVPCSecurityGroupId' - The name of the VPC security group.
vpcSecurityGroupMembership
    :: VPCSecurityGroupMembership
vpcSecurityGroupMembership =
  VPCSecurityGroupMembership'
    {_vsgmStatus = Nothing, _vsgmVPCSecurityGroupId = Nothing}


-- | The status of the VPC security group.
vsgmStatus :: Lens' VPCSecurityGroupMembership (Maybe Text)
vsgmStatus = lens _vsgmStatus (\ s a -> s{_vsgmStatus = a})

-- | The name of the VPC security group.
vsgmVPCSecurityGroupId :: Lens' VPCSecurityGroupMembership (Maybe Text)
vsgmVPCSecurityGroupId = lens _vsgmVPCSecurityGroupId (\ s a -> s{_vsgmVPCSecurityGroupId = a})

instance FromXML VPCSecurityGroupMembership where
        parseXML x
          = VPCSecurityGroupMembership' <$>
              (x .@? "Status") <*> (x .@? "VpcSecurityGroupId")

instance Hashable VPCSecurityGroupMembership where

instance NFData VPCSecurityGroupMembership where
