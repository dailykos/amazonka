{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DocDB
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon DocumentDB API documentation
--
--
module Network.AWS.DocDB
    (
    -- * Service Configuration
      docDB

    -- * Errors
    -- $errors

    -- ** InvalidDBParameterGroupStateFault
    , _InvalidDBParameterGroupStateFault

    -- ** CertificateNotFoundFault
    , _CertificateNotFoundFault

    -- ** DBClusterSnapshotAlreadyExistsFault
    , _DBClusterSnapshotAlreadyExistsFault

    -- ** DBParameterGroupAlreadyExistsFault
    , _DBParameterGroupAlreadyExistsFault

    -- ** DBParameterGroupQuotaExceededFault
    , _DBParameterGroupQuotaExceededFault

    -- ** InsufficientDBClusterCapacityFault
    , _InsufficientDBClusterCapacityFault

    -- ** InvalidSubnet
    , _InvalidSubnet

    -- ** SharedSnapshotQuotaExceededFault
    , _SharedSnapshotQuotaExceededFault

    -- ** DBSubnetQuotaExceededFault
    , _DBSubnetQuotaExceededFault

    -- ** DBClusterNotFoundFault
    , _DBClusterNotFoundFault

    -- ** DBClusterAlreadyExistsFault
    , _DBClusterAlreadyExistsFault

    -- ** StorageTypeNotSupportedFault
    , _StorageTypeNotSupportedFault

    -- ** KMSKeyNotAccessibleFault
    , _KMSKeyNotAccessibleFault

    -- ** DBSnapshotNotFoundFault
    , _DBSnapshotNotFoundFault

    -- ** DBClusterParameterGroupNotFoundFault
    , _DBClusterParameterGroupNotFoundFault

    -- ** DBClusterQuotaExceededFault
    , _DBClusterQuotaExceededFault

    -- ** SnapshotQuotaExceededFault
    , _SnapshotQuotaExceededFault

    -- ** DBSubnetGroupAlreadyExistsFault
    , _DBSubnetGroupAlreadyExistsFault

    -- ** DBSecurityGroupNotFoundFault
    , _DBSecurityGroupNotFoundFault

    -- ** InstanceQuotaExceededFault
    , _InstanceQuotaExceededFault

    -- ** DBParameterGroupNotFoundFault
    , _DBParameterGroupNotFoundFault

    -- ** InvalidDBSubnetStateFault
    , _InvalidDBSubnetStateFault

    -- ** DBClusterSnapshotNotFoundFault
    , _DBClusterSnapshotNotFoundFault

    -- ** InsufficientDBInstanceCapacityFault
    , _InsufficientDBInstanceCapacityFault

    -- ** InvalidDBClusterSnapshotStateFault
    , _InvalidDBClusterSnapshotStateFault

    -- ** InvalidVPCNetworkStateFault
    , _InvalidVPCNetworkStateFault

    -- ** AuthorizationNotFoundFault
    , _AuthorizationNotFoundFault

    -- ** DBSubnetGroupQuotaExceededFault
    , _DBSubnetGroupQuotaExceededFault

    -- ** InsufficientStorageClusterCapacityFault
    , _InsufficientStorageClusterCapacityFault

    -- ** InvalidDBClusterStateFault
    , _InvalidDBClusterStateFault

    -- ** DBInstanceAlreadyExistsFault
    , _DBInstanceAlreadyExistsFault

    -- ** InvalidRestoreFault
    , _InvalidRestoreFault

    -- ** InvalidDBSecurityGroupStateFault
    , _InvalidDBSecurityGroupStateFault

    -- ** ResourceNotFoundFault
    , _ResourceNotFoundFault

    -- ** DBSubnetGroupNotFoundFault
    , _DBSubnetGroupNotFoundFault

    -- ** DBUpgradeDependencyFailureFault
    , _DBUpgradeDependencyFailureFault

    -- ** InvalidDBInstanceStateFault
    , _InvalidDBInstanceStateFault

    -- ** DBSnapshotAlreadyExistsFault
    , _DBSnapshotAlreadyExistsFault

    -- ** DBInstanceNotFoundFault
    , _DBInstanceNotFoundFault

    -- ** StorageQuotaExceededFault
    , _StorageQuotaExceededFault

    -- ** InvalidDBSnapshotStateFault
    , _InvalidDBSnapshotStateFault

    -- ** InvalidDBSubnetGroupStateFault
    , _InvalidDBSubnetGroupStateFault

    -- ** DBSubnetGroupDoesNotCoverEnoughAZs
    , _DBSubnetGroupDoesNotCoverEnoughAZs

    -- ** SubnetAlreadyInUse
    , _SubnetAlreadyInUse

    -- * Waiters
    -- $waiters

    -- ** DBInstanceAvailable
    , dbInstanceAvailable

    -- ** DBInstanceDeleted
    , dbInstanceDeleted

    -- * Operations
    -- $operations

    -- ** DescribeDBClusterParameterGroups 
    , module Network.AWS.DocDB.DescribeDBClusterParameterGroups

    -- ** DescribeDBEngineVersions (Paginated)
    , module Network.AWS.DocDB.DescribeDBEngineVersions

    -- ** ModifyDBInstance 
    , module Network.AWS.DocDB.ModifyDBInstance

    -- ** ResetDBClusterParameterGroup 
    , module Network.AWS.DocDB.ResetDBClusterParameterGroup

    -- ** DescribeEvents (Paginated)
    , module Network.AWS.DocDB.DescribeEvents

    -- ** DescribeDBClusters (Paginated)
    , module Network.AWS.DocDB.DescribeDBClusters

    -- ** ModifyDBSubnetGroup 
    , module Network.AWS.DocDB.ModifyDBSubnetGroup

    -- ** ListTagsForResource 
    , module Network.AWS.DocDB.ListTagsForResource

    -- ** DeleteDBCluster 
    , module Network.AWS.DocDB.DeleteDBCluster

    -- ** DescribeEngineDefaultClusterParameters 
    , module Network.AWS.DocDB.DescribeEngineDefaultClusterParameters

    -- ** RemoveTagsFromResource 
    , module Network.AWS.DocDB.RemoveTagsFromResource

    -- ** CreateDBInstance 
    , module Network.AWS.DocDB.CreateDBInstance

    -- ** DeleteDBClusterParameterGroup 
    , module Network.AWS.DocDB.DeleteDBClusterParameterGroup

    -- ** RestoreDBClusterFromSnapshot 
    , module Network.AWS.DocDB.RestoreDBClusterFromSnapshot

    -- ** DescribeOrderableDBInstanceOptions (Paginated)
    , module Network.AWS.DocDB.DescribeOrderableDBInstanceOptions

    -- ** CreateDBClusterParameterGroup 
    , module Network.AWS.DocDB.CreateDBClusterParameterGroup

    -- ** DeleteDBClusterSnapshot 
    , module Network.AWS.DocDB.DeleteDBClusterSnapshot

    -- ** AddTagsToResource 
    , module Network.AWS.DocDB.AddTagsToResource

    -- ** CreateDBClusterSnapshot 
    , module Network.AWS.DocDB.CreateDBClusterSnapshot

    -- ** DescribeDBSubnetGroups (Paginated)
    , module Network.AWS.DocDB.DescribeDBSubnetGroups

    -- ** ModifyDBClusterSnapshotAttribute 
    , module Network.AWS.DocDB.ModifyDBClusterSnapshotAttribute

    -- ** ModifyDBCluster 
    , module Network.AWS.DocDB.ModifyDBCluster

    -- ** CopyDBClusterParameterGroup 
    , module Network.AWS.DocDB.CopyDBClusterParameterGroup

    -- ** DescribeEventCategories 
    , module Network.AWS.DocDB.DescribeEventCategories

    -- ** ModifyDBClusterParameterGroup 
    , module Network.AWS.DocDB.ModifyDBClusterParameterGroup

    -- ** DescribeDBClusterSnapshotAttributes 
    , module Network.AWS.DocDB.DescribeDBClusterSnapshotAttributes

    -- ** DescribePendingMaintenanceActions 
    , module Network.AWS.DocDB.DescribePendingMaintenanceActions

    -- ** CopyDBClusterSnapshot 
    , module Network.AWS.DocDB.CopyDBClusterSnapshot

    -- ** CreateDBCluster 
    , module Network.AWS.DocDB.CreateDBCluster

    -- ** FailoverDBCluster 
    , module Network.AWS.DocDB.FailoverDBCluster

    -- ** ApplyPendingMaintenanceAction 
    , module Network.AWS.DocDB.ApplyPendingMaintenanceAction

    -- ** DescribeDBClusterParameters 
    , module Network.AWS.DocDB.DescribeDBClusterParameters

    -- ** DeleteDBSubnetGroup 
    , module Network.AWS.DocDB.DeleteDBSubnetGroup

    -- ** DescribeDBClusterSnapshots 
    , module Network.AWS.DocDB.DescribeDBClusterSnapshots

    -- ** RebootDBInstance 
    , module Network.AWS.DocDB.RebootDBInstance

    -- ** CreateDBSubnetGroup 
    , module Network.AWS.DocDB.CreateDBSubnetGroup

    -- ** DeleteDBInstance 
    , module Network.AWS.DocDB.DeleteDBInstance

    -- ** RestoreDBClusterToPointInTime 
    , module Network.AWS.DocDB.RestoreDBClusterToPointInTime

    -- ** DescribeDBInstances (Paginated)
    , module Network.AWS.DocDB.DescribeDBInstances

    -- * Types

    -- ** Common
    , module Network.AWS.DocDB.Internal

    -- ** ApplyMethod
    , ApplyMethod (..)

    -- ** SourceType
    , SourceType (..)

    -- ** AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azName

    -- ** CloudwatchLogsExportConfiguration
    , CloudwatchLogsExportConfiguration
    , cloudwatchLogsExportConfiguration
    , clecDisableLogTypes
    , clecEnableLogTypes

    -- ** DBCluster
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

    -- ** DBClusterMember
    , DBClusterMember
    , dbClusterMember
    , dcmPromotionTier
    , dcmDBInstanceIdentifier
    , dcmIsClusterWriter
    , dcmDBClusterParameterGroupStatus

    -- ** DBClusterParameterGroup
    , DBClusterParameterGroup
    , dbClusterParameterGroup
    , dcpgDBClusterParameterGroupARN
    , dcpgDBParameterGroupFamily
    , dcpgDBClusterParameterGroupName
    , dcpgDescription

    -- ** DBClusterParameterGroupNameMessage
    , DBClusterParameterGroupNameMessage
    , dbClusterParameterGroupNameMessage
    , dcpgnmDBClusterParameterGroupName

    -- ** DBClusterRole
    , DBClusterRole
    , dbClusterRole
    , dcrStatus
    , dcrRoleARN

    -- ** DBClusterSnapshot
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

    -- ** DBClusterSnapshotAttribute
    , DBClusterSnapshotAttribute
    , dbClusterSnapshotAttribute
    , dcsaAttributeValues
    , dcsaAttributeName

    -- ** DBClusterSnapshotAttributesResult
    , DBClusterSnapshotAttributesResult
    , dbClusterSnapshotAttributesResult
    , dcsarDBClusterSnapshotIdentifier
    , dcsarDBClusterSnapshotAttributes

    -- ** DBEngineVersion
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

    -- ** DBInstance
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

    -- ** DBInstanceStatusInfo
    , DBInstanceStatusInfo
    , dbInstanceStatusInfo
    , disiStatus
    , disiNormal
    , disiStatusType
    , disiMessage

    -- ** DBSubnetGroup
    , DBSubnetGroup
    , dbSubnetGroup
    , dsgDBSubnetGroupName
    , dsgVPCId
    , dsgSubnets
    , dsgDBSubnetGroupDescription
    , dsgDBSubnetGroupARN
    , dsgSubnetGroupStatus

    -- ** Endpoint
    , Endpoint
    , endpoint
    , eHostedZoneId
    , eAddress
    , ePort

    -- ** EngineDefaults
    , EngineDefaults
    , engineDefaults
    , edDBParameterGroupFamily
    , edMarker
    , edParameters

    -- ** Event
    , Event
    , event
    , eSourceType
    , eSourceARN
    , eSourceIdentifier
    , eDate
    , eEventCategories
    , eMessage

    -- ** EventCategoriesMap
    , EventCategoriesMap
    , eventCategoriesMap
    , ecmSourceType
    , ecmEventCategories

    -- ** Filter
    , Filter
    , filter'
    , fName
    , fValues

    -- ** OrderableDBInstanceOption
    , OrderableDBInstanceOption
    , orderableDBInstanceOption
    , odioEngineVersion
    , odioEngine
    , odioDBInstanceClass
    , odioLicenseModel
    , odioAvailabilityZones
    , odioVPC

    -- ** Parameter
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

    -- ** PendingCloudwatchLogsExports
    , PendingCloudwatchLogsExports
    , pendingCloudwatchLogsExports
    , pcleLogTypesToEnable
    , pcleLogTypesToDisable

    -- ** PendingMaintenanceAction
    , PendingMaintenanceAction
    , pendingMaintenanceAction
    , pmaAutoAppliedAfterDate
    , pmaAction
    , pmaOptInStatus
    , pmaDescription
    , pmaForcedApplyDate
    , pmaCurrentApplyDate

    -- ** PendingModifiedValues
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

    -- ** ResourcePendingMaintenanceActions
    , ResourcePendingMaintenanceActions
    , resourcePendingMaintenanceActions
    , rpmaPendingMaintenanceActionDetails
    , rpmaResourceIdentifier

    -- ** Subnet
    , Subnet
    , subnet
    , sSubnetStatus
    , sSubnetIdentifier
    , sSubnetAvailabilityZone

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** UpgradeTarget
    , UpgradeTarget
    , upgradeTarget
    , utEngineVersion
    , utIsMajorVersionUpgrade
    , utEngine
    , utAutoUpgrade
    , utDescription

    -- ** VPCSecurityGroupMembership
    , VPCSecurityGroupMembership
    , vpcSecurityGroupMembership
    , vsgmStatus
    , vsgmVPCSecurityGroupId
    ) where

import Network.AWS.DocDB.AddTagsToResource
import Network.AWS.DocDB.ApplyPendingMaintenanceAction
import Network.AWS.DocDB.CopyDBClusterParameterGroup
import Network.AWS.DocDB.CopyDBClusterSnapshot
import Network.AWS.DocDB.CreateDBCluster
import Network.AWS.DocDB.CreateDBClusterParameterGroup
import Network.AWS.DocDB.CreateDBClusterSnapshot
import Network.AWS.DocDB.CreateDBInstance
import Network.AWS.DocDB.CreateDBSubnetGroup
import Network.AWS.DocDB.DeleteDBCluster
import Network.AWS.DocDB.DeleteDBClusterParameterGroup
import Network.AWS.DocDB.DeleteDBClusterSnapshot
import Network.AWS.DocDB.DeleteDBInstance
import Network.AWS.DocDB.DeleteDBSubnetGroup
import Network.AWS.DocDB.DescribeDBClusterParameterGroups
import Network.AWS.DocDB.DescribeDBClusterParameters
import Network.AWS.DocDB.DescribeDBClusterSnapshotAttributes
import Network.AWS.DocDB.DescribeDBClusterSnapshots
import Network.AWS.DocDB.DescribeDBClusters
import Network.AWS.DocDB.DescribeDBEngineVersions
import Network.AWS.DocDB.DescribeDBInstances
import Network.AWS.DocDB.DescribeDBSubnetGroups
import Network.AWS.DocDB.DescribeEngineDefaultClusterParameters
import Network.AWS.DocDB.DescribeEventCategories
import Network.AWS.DocDB.DescribeEvents
import Network.AWS.DocDB.DescribeOrderableDBInstanceOptions
import Network.AWS.DocDB.DescribePendingMaintenanceActions
import Network.AWS.DocDB.FailoverDBCluster
import Network.AWS.DocDB.ListTagsForResource
import Network.AWS.DocDB.ModifyDBCluster
import Network.AWS.DocDB.ModifyDBClusterParameterGroup
import Network.AWS.DocDB.ModifyDBClusterSnapshotAttribute
import Network.AWS.DocDB.ModifyDBInstance
import Network.AWS.DocDB.ModifyDBSubnetGroup
import Network.AWS.DocDB.RebootDBInstance
import Network.AWS.DocDB.RemoveTagsFromResource
import Network.AWS.DocDB.ResetDBClusterParameterGroup
import Network.AWS.DocDB.RestoreDBClusterFromSnapshot
import Network.AWS.DocDB.RestoreDBClusterToPointInTime
import Network.AWS.DocDB.Types
import Network.AWS.DocDB.Waiters
import Network.AWS.DocDB.Internal

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'DocDB'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
