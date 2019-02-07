{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DocDB.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DocDB.Types
    (
    -- * Service Configuration
      docDB

    -- * Errors
    , _InvalidDBParameterGroupStateFault
    , _CertificateNotFoundFault
    , _DBClusterSnapshotAlreadyExistsFault
    , _DBParameterGroupAlreadyExistsFault
    , _DBParameterGroupQuotaExceededFault
    , _InsufficientDBClusterCapacityFault
    , _InvalidSubnet
    , _SharedSnapshotQuotaExceededFault
    , _DBSubnetQuotaExceededFault
    , _DBClusterNotFoundFault
    , _DBClusterAlreadyExistsFault
    , _StorageTypeNotSupportedFault
    , _KMSKeyNotAccessibleFault
    , _DBSnapshotNotFoundFault
    , _DBClusterParameterGroupNotFoundFault
    , _DBClusterQuotaExceededFault
    , _SnapshotQuotaExceededFault
    , _DBSubnetGroupAlreadyExistsFault
    , _DBSecurityGroupNotFoundFault
    , _InstanceQuotaExceededFault
    , _DBParameterGroupNotFoundFault
    , _InvalidDBSubnetStateFault
    , _DBClusterSnapshotNotFoundFault
    , _InsufficientDBInstanceCapacityFault
    , _InvalidDBClusterSnapshotStateFault
    , _InvalidVPCNetworkStateFault
    , _AuthorizationNotFoundFault
    , _DBSubnetGroupQuotaExceededFault
    , _InsufficientStorageClusterCapacityFault
    , _InvalidDBClusterStateFault
    , _DBInstanceAlreadyExistsFault
    , _InvalidRestoreFault
    , _InvalidDBSecurityGroupStateFault
    , _ResourceNotFoundFault
    , _DBSubnetGroupNotFoundFault
    , _DBUpgradeDependencyFailureFault
    , _InvalidDBInstanceStateFault
    , _DBSnapshotAlreadyExistsFault
    , _DBInstanceNotFoundFault
    , _StorageQuotaExceededFault
    , _InvalidDBSnapshotStateFault
    , _InvalidDBSubnetGroupStateFault
    , _DBSubnetGroupDoesNotCoverEnoughAZs
    , _SubnetAlreadyInUse

    -- * Re-exported Types
    , module Network.AWS.DocDB.Internal

    -- * ApplyMethod
    , ApplyMethod (..)

    -- * SourceType
    , SourceType (..)

    -- * AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azName

    -- * CloudwatchLogsExportConfiguration
    , CloudwatchLogsExportConfiguration
    , cloudwatchLogsExportConfiguration
    , clecDisableLogTypes
    , clecEnableLogTypes

    -- * DBCluster
    , DBCluster
    , dbCluster
    , dcEngineVersion
    , dcStatus
    , dcStorageEncrypted
    , dcDBClusterIdentifier
    , dcDBClusterMembers
    , dcHostedZoneId
    , dcDBClusterParameterGroup
    , dcMasterUsername
    , dcDBClusterResourceId
    , dcEarliestRestorableTime
    , dcEngine
    , dcDBClusterARN
    , dcLatestRestorableTime
    , dcPreferredMaintenanceWindow
    , dcAvailabilityZones
    , dcKMSKeyId
    , dcPreferredBackupWindow
    , dcAssociatedRoles
    , dcVPCSecurityGroups
    , dcBackupRetentionPeriod
    , dcDBSubnetGroup
    , dcMultiAZ
    , dcEnabledCloudwatchLogsExports
    , dcClusterCreateTime
    , dcEndpoint
    , dcPercentProgress
    , dcReaderEndpoint
    , dcPort

    -- * DBClusterMember
    , DBClusterMember
    , dbClusterMember
    , dcmPromotionTier
    , dcmDBInstanceIdentifier
    , dcmIsClusterWriter
    , dcmDBClusterParameterGroupStatus

    -- * DBClusterParameterGroup
    , DBClusterParameterGroup
    , dbClusterParameterGroup
    , dcpgDBClusterParameterGroupARN
    , dcpgDBParameterGroupFamily
    , dcpgDBClusterParameterGroupName
    , dcpgDescription

    -- * DBClusterParameterGroupNameMessage
    , DBClusterParameterGroupNameMessage
    , dbClusterParameterGroupNameMessage
    , dcpgnmDBClusterParameterGroupName

    -- * DBClusterRole
    , DBClusterRole
    , dbClusterRole
    , dcrStatus
    , dcrRoleARN

    -- * DBClusterSnapshot
    , DBClusterSnapshot
    , dbClusterSnapshot
    , dcsEngineVersion
    , dcsStatus
    , dcsStorageEncrypted
    , dcsDBClusterIdentifier
    , dcsMasterUsername
    , dcsDBClusterSnapshotARN
    , dcsVPCId
    , dcsDBClusterSnapshotIdentifier
    , dcsEngine
    , dcsAvailabilityZones
    , dcsSnapshotType
    , dcsKMSKeyId
    , dcsSnapshotCreateTime
    , dcsSourceDBClusterSnapshotARN
    , dcsClusterCreateTime
    , dcsPercentProgress
    , dcsPort

    -- * DBClusterSnapshotAttribute
    , DBClusterSnapshotAttribute
    , dbClusterSnapshotAttribute
    , dcsaAttributeValues
    , dcsaAttributeName

    -- * DBClusterSnapshotAttributesResult
    , DBClusterSnapshotAttributesResult
    , dbClusterSnapshotAttributesResult
    , dcsarDBClusterSnapshotIdentifier
    , dcsarDBClusterSnapshotAttributes

    -- * DBEngineVersion
    , DBEngineVersion
    , dbEngineVersion
    , devEngineVersion
    , devDBEngineVersionDescription
    , devEngine
    , devDBParameterGroupFamily
    , devDBEngineDescription
    , devValidUpgradeTarget
    , devSupportsLogExportsToCloudwatchLogs
    , devExportableLogTypes

    -- * DBInstance
    , DBInstance
    , dbInstance
    , diEngineVersion
    , diStorageEncrypted
    , diDBClusterIdentifier
    , diPubliclyAccessible
    , diAutoMinorVersionUpgrade
    , diDBInstanceARN
    , diInstanceCreateTime
    , diEngine
    , diLatestRestorableTime
    , diDBInstanceClass
    , diPromotionTier
    , diPreferredMaintenanceWindow
    , diDBInstanceIdentifier
    , diKMSKeyId
    , diPreferredBackupWindow
    , diAvailabilityZone
    , diVPCSecurityGroups
    , diBackupRetentionPeriod
    , diDBSubnetGroup
    , diEnabledCloudwatchLogsExports
    , diDBiResourceId
    , diEndpoint
    , diDBInstanceStatus
    , diPendingModifiedValues
    , diStatusInfos

    -- * DBInstanceStatusInfo
    , DBInstanceStatusInfo
    , dbInstanceStatusInfo
    , disiStatus
    , disiNormal
    , disiStatusType
    , disiMessage

    -- * DBSubnetGroup
    , DBSubnetGroup
    , dbSubnetGroup
    , dsgDBSubnetGroupName
    , dsgVPCId
    , dsgSubnets
    , dsgDBSubnetGroupDescription
    , dsgDBSubnetGroupARN
    , dsgSubnetGroupStatus

    -- * Endpoint
    , Endpoint
    , endpoint
    , eHostedZoneId
    , eAddress
    , ePort

    -- * EngineDefaults
    , EngineDefaults
    , engineDefaults
    , edDBParameterGroupFamily
    , edMarker
    , edParameters

    -- * Event
    , Event
    , event
    , eSourceType
    , eSourceARN
    , eSourceIdentifier
    , eDate
    , eEventCategories
    , eMessage

    -- * EventCategoriesMap
    , EventCategoriesMap
    , eventCategoriesMap
    , ecmSourceType
    , ecmEventCategories

    -- * Filter
    , Filter
    , filter'
    , fName
    , fValues

    -- * OrderableDBInstanceOption
    , OrderableDBInstanceOption
    , orderableDBInstanceOption
    , odioEngineVersion
    , odioEngine
    , odioDBInstanceClass
    , odioLicenseModel
    , odioAvailabilityZones
    , odioVPC

    -- * Parameter
    , Parameter
    , parameter
    , pApplyType
    , pParameterValue
    , pApplyMethod
    , pMinimumEngineVersion
    , pSource
    , pIsModifiable
    , pDataType
    , pAllowedValues
    , pParameterName
    , pDescription

    -- * PendingCloudwatchLogsExports
    , PendingCloudwatchLogsExports
    , pendingCloudwatchLogsExports
    , pcleLogTypesToEnable
    , pcleLogTypesToDisable

    -- * PendingMaintenanceAction
    , PendingMaintenanceAction
    , pendingMaintenanceAction
    , pmaAutoAppliedAfterDate
    , pmaAction
    , pmaOptInStatus
    , pmaDescription
    , pmaForcedApplyDate
    , pmaCurrentApplyDate

    -- * PendingModifiedValues
    , PendingModifiedValues
    , pendingModifiedValues
    , pmvEngineVersion
    , pmvMasterUserPassword
    , pmvDBSubnetGroupName
    , pmvIOPS
    , pmvDBInstanceClass
    , pmvLicenseModel
    , pmvCACertificateIdentifier
    , pmvDBInstanceIdentifier
    , pmvPendingCloudwatchLogsExports
    , pmvBackupRetentionPeriod
    , pmvMultiAZ
    , pmvAllocatedStorage
    , pmvPort
    , pmvStorageType

    -- * ResourcePendingMaintenanceActions
    , ResourcePendingMaintenanceActions
    , resourcePendingMaintenanceActions
    , rpmaPendingMaintenanceActionDetails
    , rpmaResourceIdentifier

    -- * Subnet
    , Subnet
    , subnet
    , sSubnetStatus
    , sSubnetIdentifier
    , sSubnetAvailabilityZone

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * UpgradeTarget
    , UpgradeTarget
    , upgradeTarget
    , utEngineVersion
    , utIsMajorVersionUpgrade
    , utEngine
    , utAutoUpgrade
    , utDescription

    -- * VPCSecurityGroupMembership
    , VPCSecurityGroupMembership
    , vpcSecurityGroupMembership
    , vsgmStatus
    , vsgmVPCSecurityGroupId
    ) where

import Network.AWS.DocDB.Internal
import Network.AWS.DocDB.Types.Product
import Network.AWS.DocDB.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2014-10-31@ of the Amazon DocumentDB with MongoDB compatibility SDK configuration.
docDB :: Service
docDB =
  Service
    { _svcAbbrev = "DocDB"
    , _svcSigner = v4
    , _svcPrefix = "rds"
    , _svcVersion = "2014-10-31"
    , _svcEndpoint = defaultEndpoint docDB
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError "DocDB"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The DB parameter group is in use, or it is in a state that is not valid. If you are trying to delete the parameter group, you can't delete it when the parameter group is in this state.
--
--
_InvalidDBParameterGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBParameterGroupStateFault =
  _MatchServiceError docDB "InvalidDBParameterGroupState" . hasStatus 400


-- | @CertificateIdentifier@ doesn't refer to an existing certificate. 
--
--
_CertificateNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_CertificateNotFoundFault =
  _MatchServiceError docDB "CertificateNotFound" . hasStatus 404


-- | You already have a DB cluster snapshot with the given identifier.
--
--
_DBClusterSnapshotAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterSnapshotAlreadyExistsFault =
  _MatchServiceError docDB "DBClusterSnapshotAlreadyExistsFault" . hasStatus 400


-- | A DB parameter group with the same name already exists.
--
--
_DBParameterGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBParameterGroupAlreadyExistsFault =
  _MatchServiceError docDB "DBParameterGroupAlreadyExists" . hasStatus 400


-- | This request would cause you to exceed the allowed number of DB parameter groups.
--
--
_DBParameterGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBParameterGroupQuotaExceededFault =
  _MatchServiceError docDB "DBParameterGroupQuotaExceeded" . hasStatus 400


-- | The DB cluster doesn't have enough capacity for the current operation.
--
--
_InsufficientDBClusterCapacityFault :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientDBClusterCapacityFault =
  _MatchServiceError docDB "InsufficientDBClusterCapacityFault" . hasStatus 403


-- | The requested subnet is not valid, or multiple subnets were requested that are not all in a common virtual private cloud (VPC).
--
--
_InvalidSubnet :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSubnet = _MatchServiceError docDB "InvalidSubnet" . hasStatus 400


-- | You have exceeded the maximum number of accounts that you can share a manual DB snapshot with. 
--
--
_SharedSnapshotQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_SharedSnapshotQuotaExceededFault =
  _MatchServiceError docDB "SharedSnapshotQuotaExceeded" . hasStatus 400


-- | The request would cause you to exceed the allowed number of subnets in a DB subnet group.
--
--
_DBSubnetQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSubnetQuotaExceededFault =
  _MatchServiceError docDB "DBSubnetQuotaExceededFault" . hasStatus 400


-- | @DBClusterIdentifier@ doesn't refer to an existing DB cluster. 
--
--
_DBClusterNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterNotFoundFault =
  _MatchServiceError docDB "DBClusterNotFoundFault" . hasStatus 404


-- | You already have a DB cluster with the given identifier.
--
--
_DBClusterAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterAlreadyExistsFault =
  _MatchServiceError docDB "DBClusterAlreadyExistsFault" . hasStatus 400


-- | Storage of the specified @StorageType@ can't be associated with the DB instance. 
--
--
_StorageTypeNotSupportedFault :: AsError a => Getting (First ServiceError) a ServiceError
_StorageTypeNotSupportedFault =
  _MatchServiceError docDB "StorageTypeNotSupported" . hasStatus 400


-- | An error occurred when accessing an AWS KMS key.
--
--
_KMSKeyNotAccessibleFault :: AsError a => Getting (First ServiceError) a ServiceError
_KMSKeyNotAccessibleFault =
  _MatchServiceError docDB "KMSKeyNotAccessibleFault" . hasStatus 400


-- | @DBSnapshotIdentifier@ doesn't refer to an existing DB snapshot. 
--
--
_DBSnapshotNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSnapshotNotFoundFault =
  _MatchServiceError docDB "DBSnapshotNotFound" . hasStatus 404


-- | @DBClusterParameterGroupName@ doesn't refer to an existing DB cluster parameter group. 
--
--
_DBClusterParameterGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterParameterGroupNotFoundFault =
  _MatchServiceError docDB "DBClusterParameterGroupNotFound" . hasStatus 404


-- | The DB cluster can't be created because you have reached the maximum allowed quota of DB clusters.
--
--
_DBClusterQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterQuotaExceededFault =
  _MatchServiceError docDB "DBClusterQuotaExceededFault" . hasStatus 403


-- | The request would cause you to exceed the allowed number of DB snapshots.
--
--
_SnapshotQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotQuotaExceededFault =
  _MatchServiceError docDB "SnapshotQuotaExceeded" . hasStatus 400


-- | @DBSubnetGroupName@ is already being used by an existing DB subnet group. 
--
--
_DBSubnetGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupAlreadyExistsFault =
  _MatchServiceError docDB "DBSubnetGroupAlreadyExists" . hasStatus 400


-- | @DBSecurityGroupName@ doesn't refer to an existing DB security group. 
--
--
_DBSecurityGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSecurityGroupNotFoundFault =
  _MatchServiceError docDB "DBSecurityGroupNotFound" . hasStatus 404


-- | The request would cause you to exceed the allowed number of DB instances.
--
--
_InstanceQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_InstanceQuotaExceededFault =
  _MatchServiceError docDB "InstanceQuotaExceeded" . hasStatus 400


-- | @DBParameterGroupName@ doesn't refer to an existing DB parameter group. 
--
--
_DBParameterGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBParameterGroupNotFoundFault =
  _MatchServiceError docDB "DBParameterGroupNotFound" . hasStatus 404


-- | The DB subnet isn't in the /available/ state. 
--
--
_InvalidDBSubnetStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBSubnetStateFault =
  _MatchServiceError docDB "InvalidDBSubnetStateFault" . hasStatus 400


-- | @DBClusterSnapshotIdentifier@ doesn't refer to an existing DB cluster snapshot. 
--
--
_DBClusterSnapshotNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterSnapshotNotFoundFault =
  _MatchServiceError docDB "DBClusterSnapshotNotFoundFault" . hasStatus 404


-- | The specified DB instance class isn't available in the specified Availability Zone.
--
--
_InsufficientDBInstanceCapacityFault :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientDBInstanceCapacityFault =
  _MatchServiceError docDB "InsufficientDBInstanceCapacity" . hasStatus 400


-- | The provided value isn't a valid DB cluster snapshot state.
--
--
_InvalidDBClusterSnapshotStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBClusterSnapshotStateFault =
  _MatchServiceError docDB "InvalidDBClusterSnapshotStateFault" . hasStatus 400


-- | The DB subnet group doesn't cover all Availability Zones after it is created because of changes that were made.
--
--
_InvalidVPCNetworkStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidVPCNetworkStateFault =
  _MatchServiceError docDB "InvalidVPCNetworkStateFault" . hasStatus 400


-- | The specified CIDR IP or Amazon EC2 security group isn't authorized for the specified DB security group.
--
--
-- Amazon DocumentDB also might not be authorized to perform necessary actions on your behalf using IAM.
--
_AuthorizationNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorizationNotFoundFault =
  _MatchServiceError docDB "AuthorizationNotFound" . hasStatus 404


-- | The request would cause you to exceed the allowed number of DB subnet groups.
--
--
_DBSubnetGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupQuotaExceededFault =
  _MatchServiceError docDB "DBSubnetGroupQuotaExceeded" . hasStatus 400


-- | There is not enough storage available for the current action. You might be able to resolve this error by updating your subnet group to use different Availability Zones that have more storage available. 
--
--
_InsufficientStorageClusterCapacityFault :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientStorageClusterCapacityFault =
  _MatchServiceError docDB "InsufficientStorageClusterCapacity" . hasStatus 400


-- | The DB cluster isn't in a valid state.
--
--
_InvalidDBClusterStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBClusterStateFault =
  _MatchServiceError docDB "InvalidDBClusterStateFault" . hasStatus 400


-- | You already have a DB instance with the given identifier.
--
--
_DBInstanceAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBInstanceAlreadyExistsFault =
  _MatchServiceError docDB "DBInstanceAlreadyExists" . hasStatus 400


-- | You cannot restore from a virtual private cloud (VPC) backup to a non-VPC DB instance.
--
--
_InvalidRestoreFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRestoreFault =
  _MatchServiceError docDB "InvalidRestoreFault" . hasStatus 400


-- | The state of the DB security group doesn't allow deletion.
--
--
_InvalidDBSecurityGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBSecurityGroupStateFault =
  _MatchServiceError docDB "InvalidDBSecurityGroupState" . hasStatus 400


-- | The specified resource ID was not found.
--
--
_ResourceNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundFault =
  _MatchServiceError docDB "ResourceNotFoundFault" . hasStatus 404


-- | @DBSubnetGroupName@ doesn't refer to an existing DB subnet group. 
--
--
_DBSubnetGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupNotFoundFault =
  _MatchServiceError docDB "DBSubnetGroupNotFoundFault" . hasStatus 404


-- | The DB upgrade failed because a resource that the DB depends on can't be modified.
--
--
_DBUpgradeDependencyFailureFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBUpgradeDependencyFailureFault =
  _MatchServiceError docDB "DBUpgradeDependencyFailure" . hasStatus 400


-- | The specified DB instance isn't in the /available/ state. 
--
--
_InvalidDBInstanceStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBInstanceStateFault =
  _MatchServiceError docDB "InvalidDBInstanceState" . hasStatus 400


-- | @DBSnapshotIdentifier@ is already being used by an existing snapshot. 
--
--
_DBSnapshotAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSnapshotAlreadyExistsFault =
  _MatchServiceError docDB "DBSnapshotAlreadyExists" . hasStatus 400


-- | @DBInstanceIdentifier@ doesn't refer to an existing DB instance. 
--
--
_DBInstanceNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBInstanceNotFoundFault =
  _MatchServiceError docDB "DBInstanceNotFound" . hasStatus 404


-- | The request would cause you to exceed the allowed amount of storage available across all DB instances.
--
--
_StorageQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_StorageQuotaExceededFault =
  _MatchServiceError docDB "StorageQuotaExceeded" . hasStatus 400


-- | The state of the DB snapshot doesn't allow deletion.
--
--
_InvalidDBSnapshotStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBSnapshotStateFault =
  _MatchServiceError docDB "InvalidDBSnapshotState" . hasStatus 400


-- | The DB subnet group can't be deleted because it's in use.
--
--
_InvalidDBSubnetGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBSubnetGroupStateFault =
  _MatchServiceError docDB "InvalidDBSubnetGroupStateFault" . hasStatus 400


-- | Subnets in the DB subnet group should cover at least two Availability Zones unless there is only one Availability Zone.
--
--
_DBSubnetGroupDoesNotCoverEnoughAZs :: AsError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupDoesNotCoverEnoughAZs =
  _MatchServiceError docDB "DBSubnetGroupDoesNotCoverEnoughAZs" . hasStatus 400


-- | The DB subnet is already in use in the Availability Zone.
--
--
_SubnetAlreadyInUse :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetAlreadyInUse =
  _MatchServiceError docDB "SubnetAlreadyInUse" . hasStatus 400

