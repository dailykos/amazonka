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
-- Module      : Network.AWS.DocDB.RestoreDBClusterToPointInTime
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a DB cluster to an arbitrary point in time. Users can restore to any point in time before @LatestRestorableTime@ for up to @BackupRetentionPeriod@ days. The target DB cluster is created from the source DB cluster with the same configuration as the original DB cluster, except that the new DB cluster is created with the default DB security group. 
--
--
module Network.AWS.DocDB.RestoreDBClusterToPointInTime
    (
    -- * Creating a Request
      restoreDBClusterToPointInTime
    , RestoreDBClusterToPointInTime
    -- * Request Lenses
    , rdctpitUseLatestRestorableTime
    , rdctpitDBSubnetGroupName
    , rdctpitKMSKeyId
    , rdctpitVPCSecurityGroupIds
    , rdctpitRestoreToTime
    , rdctpitTags
    , rdctpitPort
    , rdctpitEnableCloudwatchLogsExports
    , rdctpitDBClusterIdentifier
    , rdctpitSourceDBClusterIdentifier

    -- * Destructuring the Response
    , restoreDBClusterToPointInTimeResponse
    , RestoreDBClusterToPointInTimeResponse
    -- * Response Lenses
    , rdctpitrsDBCluster
    , rdctpitrsResponseStatus
    ) where

import Network.AWS.DocDB.Types
import Network.AWS.DocDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input to 'RestoreDBClusterToPointInTime' .
--
--
--
-- /See:/ 'restoreDBClusterToPointInTime' smart constructor.
data RestoreDBClusterToPointInTime = RestoreDBClusterToPointInTime'
  { _rdctpitUseLatestRestorableTime :: !(Maybe Bool)
  , _rdctpitDBSubnetGroupName :: !(Maybe Text)
  , _rdctpitKMSKeyId :: !(Maybe Text)
  , _rdctpitVPCSecurityGroupIds :: !(Maybe [Text])
  , _rdctpitRestoreToTime :: !(Maybe ISO8601)
  , _rdctpitTags :: !(Maybe [Tag])
  , _rdctpitPort :: !(Maybe Int)
  , _rdctpitEnableCloudwatchLogsExports :: !(Maybe [Text])
  , _rdctpitDBClusterIdentifier :: !Text
  , _rdctpitSourceDBClusterIdentifier :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreDBClusterToPointInTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdctpitUseLatestRestorableTime' - A value that is set to @true@ to restore the DB cluster to the latest restorable backup time, and @false@ otherwise.  Default: @false@  Constraints: Cannot be specified if the @RestoreToTime@ parameter is provided.
--
-- * 'rdctpitDBSubnetGroupName' - The DB subnet group name to use for the new DB cluster. Constraints: If provided, must match the name of an existing @DBSubnetGroup@ . Example: @mySubnetgroup@ 
--
-- * 'rdctpitKMSKeyId' - The AWS KMS key identifier to use when restoring an encrypted DB cluster from an encrypted DB cluster. The AWS KMS key identifier is the Amazon Resource Name (ARN) for the AWS KMS encryption key. If you are restoring a DB cluster with the same AWS account that owns the AWS KMS encryption key used to encrypt the new DB cluster, then you can use the AWS KMS key alias instead of the ARN for the AWS KMS encryption key. You can restore to a new DB cluster and encrypt the new DB cluster with an AWS KMS key that is different from the AWS KMS key used to encrypt the source DB cluster. The new DB cluster is encrypted with the AWS KMS key identified by the @KmsKeyId@ parameter. If you do not specify a value for the @KmsKeyId@ parameter, then the following occurs:     * If the DB cluster is encrypted, then the restored DB cluster is encrypted using the AWS KMS key that was used to encrypt the source DB cluster.     * If the DB cluster is not encrypted, then the restored DB cluster is not encrypted. If @DBClusterIdentifier@ refers to a DB cluster that is not encrypted, then the restore request is rejected.
--
-- * 'rdctpitVPCSecurityGroupIds' - A list of VPC security groups that the new DB cluster belongs to.
--
-- * 'rdctpitRestoreToTime' - The date and time to restore the DB cluster to. Valid values: A time in Universal Coordinated Time (UTC) format. Constraints:     * Must be before the latest restorable time for the DB instance.     * Must be specified if the @UseLatestRestorableTime@ parameter is not provided.     * Cannot be specified if the @UseLatestRestorableTime@ parameter is @true@ .     * Cannot be specified if the @RestoreType@ parameter is @copy-on-write@ . Example: @2015-03-07T23:45:00Z@ 
--
-- * 'rdctpitTags' - The tags to be assigned to the restored DB cluster.
--
-- * 'rdctpitPort' - The port number on which the new DB cluster accepts connections. Constraints: Must be a value from @1150@ to @65535@ .  Default: The default port for the engine.
--
-- * 'rdctpitEnableCloudwatchLogsExports' - A list of log types that must be enabled for exporting to Amazon CloudWatch Logs.
--
-- * 'rdctpitDBClusterIdentifier' - The name of the new DB cluster to be created. Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens.     * The first character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
--
-- * 'rdctpitSourceDBClusterIdentifier' - The identifier of the source DB cluster from which to restore. Constraints:     * Must match the identifier of an existing @DBCluster@ .
restoreDBClusterToPointInTime
    :: Text -- ^ 'rdctpitDBClusterIdentifier'
    -> Text -- ^ 'rdctpitSourceDBClusterIdentifier'
    -> RestoreDBClusterToPointInTime
restoreDBClusterToPointInTime pDBClusterIdentifier_ pSourceDBClusterIdentifier_ =
  RestoreDBClusterToPointInTime'
    { _rdctpitUseLatestRestorableTime = Nothing
    , _rdctpitDBSubnetGroupName = Nothing
    , _rdctpitKMSKeyId = Nothing
    , _rdctpitVPCSecurityGroupIds = Nothing
    , _rdctpitRestoreToTime = Nothing
    , _rdctpitTags = Nothing
    , _rdctpitPort = Nothing
    , _rdctpitEnableCloudwatchLogsExports = Nothing
    , _rdctpitDBClusterIdentifier = pDBClusterIdentifier_
    , _rdctpitSourceDBClusterIdentifier = pSourceDBClusterIdentifier_
    }


-- | A value that is set to @true@ to restore the DB cluster to the latest restorable backup time, and @false@ otherwise.  Default: @false@  Constraints: Cannot be specified if the @RestoreToTime@ parameter is provided.
rdctpitUseLatestRestorableTime :: Lens' RestoreDBClusterToPointInTime (Maybe Bool)
rdctpitUseLatestRestorableTime = lens _rdctpitUseLatestRestorableTime (\ s a -> s{_rdctpitUseLatestRestorableTime = a})

-- | The DB subnet group name to use for the new DB cluster. Constraints: If provided, must match the name of an existing @DBSubnetGroup@ . Example: @mySubnetgroup@ 
rdctpitDBSubnetGroupName :: Lens' RestoreDBClusterToPointInTime (Maybe Text)
rdctpitDBSubnetGroupName = lens _rdctpitDBSubnetGroupName (\ s a -> s{_rdctpitDBSubnetGroupName = a})

-- | The AWS KMS key identifier to use when restoring an encrypted DB cluster from an encrypted DB cluster. The AWS KMS key identifier is the Amazon Resource Name (ARN) for the AWS KMS encryption key. If you are restoring a DB cluster with the same AWS account that owns the AWS KMS encryption key used to encrypt the new DB cluster, then you can use the AWS KMS key alias instead of the ARN for the AWS KMS encryption key. You can restore to a new DB cluster and encrypt the new DB cluster with an AWS KMS key that is different from the AWS KMS key used to encrypt the source DB cluster. The new DB cluster is encrypted with the AWS KMS key identified by the @KmsKeyId@ parameter. If you do not specify a value for the @KmsKeyId@ parameter, then the following occurs:     * If the DB cluster is encrypted, then the restored DB cluster is encrypted using the AWS KMS key that was used to encrypt the source DB cluster.     * If the DB cluster is not encrypted, then the restored DB cluster is not encrypted. If @DBClusterIdentifier@ refers to a DB cluster that is not encrypted, then the restore request is rejected.
rdctpitKMSKeyId :: Lens' RestoreDBClusterToPointInTime (Maybe Text)
rdctpitKMSKeyId = lens _rdctpitKMSKeyId (\ s a -> s{_rdctpitKMSKeyId = a})

-- | A list of VPC security groups that the new DB cluster belongs to.
rdctpitVPCSecurityGroupIds :: Lens' RestoreDBClusterToPointInTime [Text]
rdctpitVPCSecurityGroupIds = lens _rdctpitVPCSecurityGroupIds (\ s a -> s{_rdctpitVPCSecurityGroupIds = a}) . _Default . _Coerce

-- | The date and time to restore the DB cluster to. Valid values: A time in Universal Coordinated Time (UTC) format. Constraints:     * Must be before the latest restorable time for the DB instance.     * Must be specified if the @UseLatestRestorableTime@ parameter is not provided.     * Cannot be specified if the @UseLatestRestorableTime@ parameter is @true@ .     * Cannot be specified if the @RestoreType@ parameter is @copy-on-write@ . Example: @2015-03-07T23:45:00Z@ 
rdctpitRestoreToTime :: Lens' RestoreDBClusterToPointInTime (Maybe UTCTime)
rdctpitRestoreToTime = lens _rdctpitRestoreToTime (\ s a -> s{_rdctpitRestoreToTime = a}) . mapping _Time

-- | The tags to be assigned to the restored DB cluster.
rdctpitTags :: Lens' RestoreDBClusterToPointInTime [Tag]
rdctpitTags = lens _rdctpitTags (\ s a -> s{_rdctpitTags = a}) . _Default . _Coerce

-- | The port number on which the new DB cluster accepts connections. Constraints: Must be a value from @1150@ to @65535@ .  Default: The default port for the engine.
rdctpitPort :: Lens' RestoreDBClusterToPointInTime (Maybe Int)
rdctpitPort = lens _rdctpitPort (\ s a -> s{_rdctpitPort = a})

-- | A list of log types that must be enabled for exporting to Amazon CloudWatch Logs.
rdctpitEnableCloudwatchLogsExports :: Lens' RestoreDBClusterToPointInTime [Text]
rdctpitEnableCloudwatchLogsExports = lens _rdctpitEnableCloudwatchLogsExports (\ s a -> s{_rdctpitEnableCloudwatchLogsExports = a}) . _Default . _Coerce

-- | The name of the new DB cluster to be created. Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens.     * The first character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
rdctpitDBClusterIdentifier :: Lens' RestoreDBClusterToPointInTime Text
rdctpitDBClusterIdentifier = lens _rdctpitDBClusterIdentifier (\ s a -> s{_rdctpitDBClusterIdentifier = a})

-- | The identifier of the source DB cluster from which to restore. Constraints:     * Must match the identifier of an existing @DBCluster@ .
rdctpitSourceDBClusterIdentifier :: Lens' RestoreDBClusterToPointInTime Text
rdctpitSourceDBClusterIdentifier = lens _rdctpitSourceDBClusterIdentifier (\ s a -> s{_rdctpitSourceDBClusterIdentifier = a})

instance AWSRequest RestoreDBClusterToPointInTime
         where
        type Rs RestoreDBClusterToPointInTime =
             RestoreDBClusterToPointInTimeResponse
        request = postQuery docDB
        response
          = receiveXMLWrapper
              "RestoreDBClusterToPointInTimeResult"
              (\ s h x ->
                 RestoreDBClusterToPointInTimeResponse' <$>
                   (x .@? "DBCluster") <*> (pure (fromEnum s)))

instance Hashable RestoreDBClusterToPointInTime where

instance NFData RestoreDBClusterToPointInTime where

instance ToHeaders RestoreDBClusterToPointInTime
         where
        toHeaders = const mempty

instance ToPath RestoreDBClusterToPointInTime where
        toPath = const "/"

instance ToQuery RestoreDBClusterToPointInTime where
        toQuery RestoreDBClusterToPointInTime'{..}
          = mconcat
              ["Action" =:
                 ("RestoreDBClusterToPointInTime" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "UseLatestRestorableTime" =:
                 _rdctpitUseLatestRestorableTime,
               "DBSubnetGroupName" =: _rdctpitDBSubnetGroupName,
               "KmsKeyId" =: _rdctpitKMSKeyId,
               "VpcSecurityGroupIds" =:
                 toQuery
                   (toQueryList "VpcSecurityGroupId" <$>
                      _rdctpitVPCSecurityGroupIds),
               "RestoreToTime" =: _rdctpitRestoreToTime,
               "Tags" =:
                 toQuery (toQueryList "Tag" <$> _rdctpitTags),
               "Port" =: _rdctpitPort,
               "EnableCloudwatchLogsExports" =:
                 toQuery
                   (toQueryList "member" <$>
                      _rdctpitEnableCloudwatchLogsExports),
               "DBClusterIdentifier" =: _rdctpitDBClusterIdentifier,
               "SourceDBClusterIdentifier" =:
                 _rdctpitSourceDBClusterIdentifier]

-- | /See:/ 'restoreDBClusterToPointInTimeResponse' smart constructor.
data RestoreDBClusterToPointInTimeResponse = RestoreDBClusterToPointInTimeResponse'
  { _rdctpitrsDBCluster :: !(Maybe DBCluster)
  , _rdctpitrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreDBClusterToPointInTimeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdctpitrsDBCluster' - Undocumented member.
--
-- * 'rdctpitrsResponseStatus' - -- | The response status code.
restoreDBClusterToPointInTimeResponse
    :: Int -- ^ 'rdctpitrsResponseStatus'
    -> RestoreDBClusterToPointInTimeResponse
restoreDBClusterToPointInTimeResponse pResponseStatus_ =
  RestoreDBClusterToPointInTimeResponse'
    {_rdctpitrsDBCluster = Nothing, _rdctpitrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
rdctpitrsDBCluster :: Lens' RestoreDBClusterToPointInTimeResponse (Maybe DBCluster)
rdctpitrsDBCluster = lens _rdctpitrsDBCluster (\ s a -> s{_rdctpitrsDBCluster = a})

-- | -- | The response status code.
rdctpitrsResponseStatus :: Lens' RestoreDBClusterToPointInTimeResponse Int
rdctpitrsResponseStatus = lens _rdctpitrsResponseStatus (\ s a -> s{_rdctpitrsResponseStatus = a})

instance NFData RestoreDBClusterToPointInTimeResponse
         where
