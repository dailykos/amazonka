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
-- Module      : Network.AWS.DocDB.CreateDBCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon DocumentDB DB cluster.
--
--
module Network.AWS.DocDB.CreateDBCluster
    (
    -- * Creating a Request
      createDBCluster
    , CreateDBCluster
    -- * Request Lenses
    , cdcEngineVersion
    , cdcStorageEncrypted
    , cdcMasterUserPassword
    , cdcMasterUsername
    , cdcDBSubnetGroupName
    , cdcPreferredMaintenanceWindow
    , cdcAvailabilityZones
    , cdcKMSKeyId
    , cdcPreferredBackupWindow
    , cdcBackupRetentionPeriod
    , cdcVPCSecurityGroupIds
    , cdcDBClusterParameterGroupName
    , cdcTags
    , cdcPort
    , cdcEnableCloudwatchLogsExports
    , cdcDBClusterIdentifier
    , cdcEngine

    -- * Destructuring the Response
    , createDBClusterResponse
    , CreateDBClusterResponse
    -- * Response Lenses
    , cdcrsDBCluster
    , cdcrsResponseStatus
    ) where

import Network.AWS.DocDB.Types
import Network.AWS.DocDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input to 'CreateDBCluster' .
--
--
--
-- /See:/ 'createDBCluster' smart constructor.
data CreateDBCluster = CreateDBCluster'
  { _cdcEngineVersion :: !(Maybe Text)
  , _cdcStorageEncrypted :: !(Maybe Bool)
  , _cdcMasterUserPassword :: !(Maybe Text)
  , _cdcMasterUsername :: !(Maybe Text)
  , _cdcDBSubnetGroupName :: !(Maybe Text)
  , _cdcPreferredMaintenanceWindow :: !(Maybe Text)
  , _cdcAvailabilityZones :: !(Maybe [Text])
  , _cdcKMSKeyId :: !(Maybe Text)
  , _cdcPreferredBackupWindow :: !(Maybe Text)
  , _cdcBackupRetentionPeriod :: !(Maybe Int)
  , _cdcVPCSecurityGroupIds :: !(Maybe [Text])
  , _cdcDBClusterParameterGroupName :: !(Maybe Text)
  , _cdcTags :: !(Maybe [Tag])
  , _cdcPort :: !(Maybe Int)
  , _cdcEnableCloudwatchLogsExports :: !(Maybe [Text])
  , _cdcDBClusterIdentifier :: !Text
  , _cdcEngine :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDBCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcEngineVersion' - The version number of the database engine to use.
--
-- * 'cdcStorageEncrypted' - Specifies whether the DB cluster is encrypted.
--
-- * 'cdcMasterUserPassword' - The password for the master database user. This password can contain any printable ASCII character except "/", """, or "@". Constraints: Must contain from 8 to 41 characters.
--
-- * 'cdcMasterUsername' - The name of the master user for the DB cluster. Constraints:     * Must be from 1 to 16 letters or numbers.     * The first character must be a letter.     * Cannot be a reserved word for the chosen database engine.
--
-- * 'cdcDBSubnetGroupName' - A DB subnet group to associate with this DB cluster. Constraints: Must match the name of an existing @DBSubnetGroup@ . Must not be default. Example: @mySubnetgroup@ 
--
-- * 'cdcPreferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC). Format: @ddd:hh24:mi-ddd:hh24:mi@  The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. Valid days: Mon, Tue, Wed, Thu, Fri, Sat, Sun Constraints: Minimum 30-minute window.
--
-- * 'cdcAvailabilityZones' - A list of Amazon EC2 Availability Zones that instances in the DB cluster can be created in.
--
-- * 'cdcKMSKeyId' - The AWS KMS key identifier for an encrypted DB cluster. The AWS KMS key identifier is the Amazon Resource Name (ARN) for the AWS KMS encryption key. If you are creating a DB cluster using the same AWS account that owns the AWS KMS encryption key that is used to encrypt the new DB cluster, you can use the AWS KMS key alias instead of the ARN for the AWS KMS encryption key. If an encryption key is not specified in @KmsKeyId@ :     * If @ReplicationSourceIdentifier@ identifies an encrypted source, then Amazon DocumentDB uses the encryption key that is used to encrypt the source. Otherwise, Amazon DocumentDB uses your default encryption key.      * If the @StorageEncrypted@ parameter is @true@ and @ReplicationSourceIdentifier@ is not specified, Amazon DocumentDB uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region. If you create a replica of an encrypted DB cluster in another AWS Region, you must set @KmsKeyId@ to a KMS key ID that is valid in the destination AWS Region. This key is used to encrypt the replica in that AWS Region.
--
-- * 'cdcPreferredBackupWindow' - The daily time range during which automated backups are created if automated backups are enabled using the @BackupRetentionPeriod@ parameter.  The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region.  Constraints:     * Must be in the format @hh24:mi-hh24:mi@ .     * Must be in Universal Coordinated Time (UTC).     * Must not conflict with the preferred maintenance window.     * Must be at least 30 minutes.
--
-- * 'cdcBackupRetentionPeriod' - The number of days for which automated backups are retained. You must specify a minimum value of 1. Default: 1 Constraints:     * Must be a value from 1 to 35.
--
-- * 'cdcVPCSecurityGroupIds' - A list of EC2 VPC security groups to associate with this DB cluster.
--
-- * 'cdcDBClusterParameterGroupName' - The name of the DB cluster parameter group to associate with this DB cluster.
--
-- * 'cdcTags' - The tags to be assigned to the DB cluster.
--
-- * 'cdcPort' - The port number on which the instances in the DB cluster accept connections.
--
-- * 'cdcEnableCloudwatchLogsExports' - A list of log types that need to be enabled for exporting to Amazon CloudWatch Logs.
--
-- * 'cdcDBClusterIdentifier' - The DB cluster identifier. This parameter is stored as a lowercase string. Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens.     * The first character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @my-cluster@ 
--
-- * 'cdcEngine' - The name of the database engine to be used for this DB cluster. Valid values: @docdb@ 
createDBCluster
    :: Text -- ^ 'cdcDBClusterIdentifier'
    -> Text -- ^ 'cdcEngine'
    -> CreateDBCluster
createDBCluster pDBClusterIdentifier_ pEngine_ =
  CreateDBCluster'
    { _cdcEngineVersion = Nothing
    , _cdcStorageEncrypted = Nothing
    , _cdcMasterUserPassword = Nothing
    , _cdcMasterUsername = Nothing
    , _cdcDBSubnetGroupName = Nothing
    , _cdcPreferredMaintenanceWindow = Nothing
    , _cdcAvailabilityZones = Nothing
    , _cdcKMSKeyId = Nothing
    , _cdcPreferredBackupWindow = Nothing
    , _cdcBackupRetentionPeriod = Nothing
    , _cdcVPCSecurityGroupIds = Nothing
    , _cdcDBClusterParameterGroupName = Nothing
    , _cdcTags = Nothing
    , _cdcPort = Nothing
    , _cdcEnableCloudwatchLogsExports = Nothing
    , _cdcDBClusterIdentifier = pDBClusterIdentifier_
    , _cdcEngine = pEngine_
    }


-- | The version number of the database engine to use.
cdcEngineVersion :: Lens' CreateDBCluster (Maybe Text)
cdcEngineVersion = lens _cdcEngineVersion (\ s a -> s{_cdcEngineVersion = a})

-- | Specifies whether the DB cluster is encrypted.
cdcStorageEncrypted :: Lens' CreateDBCluster (Maybe Bool)
cdcStorageEncrypted = lens _cdcStorageEncrypted (\ s a -> s{_cdcStorageEncrypted = a})

-- | The password for the master database user. This password can contain any printable ASCII character except "/", """, or "@". Constraints: Must contain from 8 to 41 characters.
cdcMasterUserPassword :: Lens' CreateDBCluster (Maybe Text)
cdcMasterUserPassword = lens _cdcMasterUserPassword (\ s a -> s{_cdcMasterUserPassword = a})

-- | The name of the master user for the DB cluster. Constraints:     * Must be from 1 to 16 letters or numbers.     * The first character must be a letter.     * Cannot be a reserved word for the chosen database engine.
cdcMasterUsername :: Lens' CreateDBCluster (Maybe Text)
cdcMasterUsername = lens _cdcMasterUsername (\ s a -> s{_cdcMasterUsername = a})

-- | A DB subnet group to associate with this DB cluster. Constraints: Must match the name of an existing @DBSubnetGroup@ . Must not be default. Example: @mySubnetgroup@ 
cdcDBSubnetGroupName :: Lens' CreateDBCluster (Maybe Text)
cdcDBSubnetGroupName = lens _cdcDBSubnetGroupName (\ s a -> s{_cdcDBSubnetGroupName = a})

-- | The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC). Format: @ddd:hh24:mi-ddd:hh24:mi@  The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. Valid days: Mon, Tue, Wed, Thu, Fri, Sat, Sun Constraints: Minimum 30-minute window.
cdcPreferredMaintenanceWindow :: Lens' CreateDBCluster (Maybe Text)
cdcPreferredMaintenanceWindow = lens _cdcPreferredMaintenanceWindow (\ s a -> s{_cdcPreferredMaintenanceWindow = a})

-- | A list of Amazon EC2 Availability Zones that instances in the DB cluster can be created in.
cdcAvailabilityZones :: Lens' CreateDBCluster [Text]
cdcAvailabilityZones = lens _cdcAvailabilityZones (\ s a -> s{_cdcAvailabilityZones = a}) . _Default . _Coerce

-- | The AWS KMS key identifier for an encrypted DB cluster. The AWS KMS key identifier is the Amazon Resource Name (ARN) for the AWS KMS encryption key. If you are creating a DB cluster using the same AWS account that owns the AWS KMS encryption key that is used to encrypt the new DB cluster, you can use the AWS KMS key alias instead of the ARN for the AWS KMS encryption key. If an encryption key is not specified in @KmsKeyId@ :     * If @ReplicationSourceIdentifier@ identifies an encrypted source, then Amazon DocumentDB uses the encryption key that is used to encrypt the source. Otherwise, Amazon DocumentDB uses your default encryption key.      * If the @StorageEncrypted@ parameter is @true@ and @ReplicationSourceIdentifier@ is not specified, Amazon DocumentDB uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region. If you create a replica of an encrypted DB cluster in another AWS Region, you must set @KmsKeyId@ to a KMS key ID that is valid in the destination AWS Region. This key is used to encrypt the replica in that AWS Region.
cdcKMSKeyId :: Lens' CreateDBCluster (Maybe Text)
cdcKMSKeyId = lens _cdcKMSKeyId (\ s a -> s{_cdcKMSKeyId = a})

-- | The daily time range during which automated backups are created if automated backups are enabled using the @BackupRetentionPeriod@ parameter.  The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region.  Constraints:     * Must be in the format @hh24:mi-hh24:mi@ .     * Must be in Universal Coordinated Time (UTC).     * Must not conflict with the preferred maintenance window.     * Must be at least 30 minutes.
cdcPreferredBackupWindow :: Lens' CreateDBCluster (Maybe Text)
cdcPreferredBackupWindow = lens _cdcPreferredBackupWindow (\ s a -> s{_cdcPreferredBackupWindow = a})

-- | The number of days for which automated backups are retained. You must specify a minimum value of 1. Default: 1 Constraints:     * Must be a value from 1 to 35.
cdcBackupRetentionPeriod :: Lens' CreateDBCluster (Maybe Int)
cdcBackupRetentionPeriod = lens _cdcBackupRetentionPeriod (\ s a -> s{_cdcBackupRetentionPeriod = a})

-- | A list of EC2 VPC security groups to associate with this DB cluster.
cdcVPCSecurityGroupIds :: Lens' CreateDBCluster [Text]
cdcVPCSecurityGroupIds = lens _cdcVPCSecurityGroupIds (\ s a -> s{_cdcVPCSecurityGroupIds = a}) . _Default . _Coerce

-- | The name of the DB cluster parameter group to associate with this DB cluster.
cdcDBClusterParameterGroupName :: Lens' CreateDBCluster (Maybe Text)
cdcDBClusterParameterGroupName = lens _cdcDBClusterParameterGroupName (\ s a -> s{_cdcDBClusterParameterGroupName = a})

-- | The tags to be assigned to the DB cluster.
cdcTags :: Lens' CreateDBCluster [Tag]
cdcTags = lens _cdcTags (\ s a -> s{_cdcTags = a}) . _Default . _Coerce

-- | The port number on which the instances in the DB cluster accept connections.
cdcPort :: Lens' CreateDBCluster (Maybe Int)
cdcPort = lens _cdcPort (\ s a -> s{_cdcPort = a})

-- | A list of log types that need to be enabled for exporting to Amazon CloudWatch Logs.
cdcEnableCloudwatchLogsExports :: Lens' CreateDBCluster [Text]
cdcEnableCloudwatchLogsExports = lens _cdcEnableCloudwatchLogsExports (\ s a -> s{_cdcEnableCloudwatchLogsExports = a}) . _Default . _Coerce

-- | The DB cluster identifier. This parameter is stored as a lowercase string. Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens.     * The first character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @my-cluster@ 
cdcDBClusterIdentifier :: Lens' CreateDBCluster Text
cdcDBClusterIdentifier = lens _cdcDBClusterIdentifier (\ s a -> s{_cdcDBClusterIdentifier = a})

-- | The name of the database engine to be used for this DB cluster. Valid values: @docdb@ 
cdcEngine :: Lens' CreateDBCluster Text
cdcEngine = lens _cdcEngine (\ s a -> s{_cdcEngine = a})

instance AWSRequest CreateDBCluster where
        type Rs CreateDBCluster = CreateDBClusterResponse
        request = postQuery docDB
        response
          = receiveXMLWrapper "CreateDBClusterResult"
              (\ s h x ->
                 CreateDBClusterResponse' <$>
                   (x .@? "DBCluster") <*> (pure (fromEnum s)))

instance Hashable CreateDBCluster where

instance NFData CreateDBCluster where

instance ToHeaders CreateDBCluster where
        toHeaders = const mempty

instance ToPath CreateDBCluster where
        toPath = const "/"

instance ToQuery CreateDBCluster where
        toQuery CreateDBCluster'{..}
          = mconcat
              ["Action" =: ("CreateDBCluster" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "EngineVersion" =: _cdcEngineVersion,
               "StorageEncrypted" =: _cdcStorageEncrypted,
               "MasterUserPassword" =: _cdcMasterUserPassword,
               "MasterUsername" =: _cdcMasterUsername,
               "DBSubnetGroupName" =: _cdcDBSubnetGroupName,
               "PreferredMaintenanceWindow" =:
                 _cdcPreferredMaintenanceWindow,
               "AvailabilityZones" =:
                 toQuery
                   (toQueryList "AvailabilityZone" <$>
                      _cdcAvailabilityZones),
               "KmsKeyId" =: _cdcKMSKeyId,
               "PreferredBackupWindow" =: _cdcPreferredBackupWindow,
               "BackupRetentionPeriod" =: _cdcBackupRetentionPeriod,
               "VpcSecurityGroupIds" =:
                 toQuery
                   (toQueryList "VpcSecurityGroupId" <$>
                      _cdcVPCSecurityGroupIds),
               "DBClusterParameterGroupName" =:
                 _cdcDBClusterParameterGroupName,
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdcTags),
               "Port" =: _cdcPort,
               "EnableCloudwatchLogsExports" =:
                 toQuery
                   (toQueryList "member" <$>
                      _cdcEnableCloudwatchLogsExports),
               "DBClusterIdentifier" =: _cdcDBClusterIdentifier,
               "Engine" =: _cdcEngine]

-- | /See:/ 'createDBClusterResponse' smart constructor.
data CreateDBClusterResponse = CreateDBClusterResponse'
  { _cdcrsDBCluster :: !(Maybe DBCluster)
  , _cdcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDBClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcrsDBCluster' - Undocumented member.
--
-- * 'cdcrsResponseStatus' - -- | The response status code.
createDBClusterResponse
    :: Int -- ^ 'cdcrsResponseStatus'
    -> CreateDBClusterResponse
createDBClusterResponse pResponseStatus_ =
  CreateDBClusterResponse'
    {_cdcrsDBCluster = Nothing, _cdcrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
cdcrsDBCluster :: Lens' CreateDBClusterResponse (Maybe DBCluster)
cdcrsDBCluster = lens _cdcrsDBCluster (\ s a -> s{_cdcrsDBCluster = a})

-- | -- | The response status code.
cdcrsResponseStatus :: Lens' CreateDBClusterResponse Int
cdcrsResponseStatus = lens _cdcrsResponseStatus (\ s a -> s{_cdcrsResponseStatus = a})

instance NFData CreateDBClusterResponse where
