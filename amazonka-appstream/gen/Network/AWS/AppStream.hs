{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon AppStream 2.0__ 
--
-- This is the /Amazon AppStream 2.0 API Reference/ . It provides descriptions and syntax for each of the actions and data types in AppStream 2.0. AppStream 2.0 is a fully managed application streaming service. You centrally manage your desktop applications on AppStream 2.0 and securely deliver them to any computer. AppStream 2.0 manages the AWS resources required to host and run your applications, scales automatically, and provides access to your users on demand.
--
-- To learn more about AppStream 2.0, see the following resources:
--
--     * <http://aws.amazon.com/appstream2 Amazon AppStream 2.0 product page> 
--
--     * <http://aws.amazon.com/documentation/appstream2 Amazon AppStream 2.0 documentation> 
--
--
--
module Network.AWS.AppStream
    (
    -- * Service Configuration
      appStream

    -- * Errors
    -- $errors

    -- ** InvalidRoleException
    , _InvalidRoleException

    -- ** ResourceAlreadyExistsException
    , _ResourceAlreadyExistsException

    -- ** IncompatibleImageException
    , _IncompatibleImageException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** OperationNotPermittedException
    , _OperationNotPermittedException

    -- ** InvalidAccountStatusException
    , _InvalidAccountStatusException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** InvalidParameterCombinationException
    , _InvalidParameterCombinationException

    -- ** ResourceNotAvailableException
    , _ResourceNotAvailableException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** ResourceInUseException
    , _ResourceInUseException

    -- * Waiters
    -- $waiters

    -- ** FleetStopped
    , fleetStopped

    -- ** FleetStarted
    , fleetStarted

    -- * Operations
    -- $operations

    -- ** DisassociateFleet 
    , module Network.AWS.AppStream.DisassociateFleet

    -- ** ListAssociatedFleets (Paginated)
    , module Network.AWS.AppStream.ListAssociatedFleets

    -- ** DeleteStack 
    , module Network.AWS.AppStream.DeleteStack

    -- ** UpdateStack 
    , module Network.AWS.AppStream.UpdateStack

    -- ** CreateDirectoryConfig 
    , module Network.AWS.AppStream.CreateDirectoryConfig

    -- ** DescribeUsers (Paginated)
    , module Network.AWS.AppStream.DescribeUsers

    -- ** ListAssociatedStacks (Paginated)
    , module Network.AWS.AppStream.ListAssociatedStacks

    -- ** DeleteFleet 
    , module Network.AWS.AppStream.DeleteFleet

    -- ** UpdateFleet 
    , module Network.AWS.AppStream.UpdateFleet

    -- ** DeleteImageBuilder 
    , module Network.AWS.AppStream.DeleteImageBuilder

    -- ** AssociateFleet 
    , module Network.AWS.AppStream.AssociateFleet

    -- ** CreateImageBuilder 
    , module Network.AWS.AppStream.CreateImageBuilder

    -- ** ListTagsForResource 
    , module Network.AWS.AppStream.ListTagsForResource

    -- ** DescribeDirectoryConfigs (Paginated)
    , module Network.AWS.AppStream.DescribeDirectoryConfigs

    -- ** CreateImageBuilderStreamingURL 
    , module Network.AWS.AppStream.CreateImageBuilderStreamingURL

    -- ** DescribeSessions (Paginated)
    , module Network.AWS.AppStream.DescribeSessions

    -- ** DescribeStacks (Paginated)
    , module Network.AWS.AppStream.DescribeStacks

    -- ** DescribeFleets (Paginated)
    , module Network.AWS.AppStream.DescribeFleets

    -- ** DescribeImageBuilders (Paginated)
    , module Network.AWS.AppStream.DescribeImageBuilders

    -- ** EnableUser 
    , module Network.AWS.AppStream.EnableUser

    -- ** DescribeUserStackAssociations (Paginated)
    , module Network.AWS.AppStream.DescribeUserStackAssociations

    -- ** UpdateImagePermissions 
    , module Network.AWS.AppStream.UpdateImagePermissions

    -- ** DeleteImagePermissions 
    , module Network.AWS.AppStream.DeleteImagePermissions

    -- ** StopFleet 
    , module Network.AWS.AppStream.StopFleet

    -- ** StartImageBuilder 
    , module Network.AWS.AppStream.StartImageBuilder

    -- ** BatchAssociateUserStack 
    , module Network.AWS.AppStream.BatchAssociateUserStack

    -- ** DescribeImagePermissions 
    , module Network.AWS.AppStream.DescribeImagePermissions

    -- ** DeleteDirectoryConfig 
    , module Network.AWS.AppStream.DeleteDirectoryConfig

    -- ** UpdateDirectoryConfig 
    , module Network.AWS.AppStream.UpdateDirectoryConfig

    -- ** CreateFleet 
    , module Network.AWS.AppStream.CreateFleet

    -- ** CreateStack 
    , module Network.AWS.AppStream.CreateStack

    -- ** CopyImage 
    , module Network.AWS.AppStream.CopyImage

    -- ** ExpireSession 
    , module Network.AWS.AppStream.ExpireSession

    -- ** CreateUser 
    , module Network.AWS.AppStream.CreateUser

    -- ** DisableUser 
    , module Network.AWS.AppStream.DisableUser

    -- ** DeleteUser 
    , module Network.AWS.AppStream.DeleteUser

    -- ** TagResource 
    , module Network.AWS.AppStream.TagResource

    -- ** CreateStreamingURL 
    , module Network.AWS.AppStream.CreateStreamingURL

    -- ** UntagResource 
    , module Network.AWS.AppStream.UntagResource

    -- ** StartFleet 
    , module Network.AWS.AppStream.StartFleet

    -- ** StopImageBuilder 
    , module Network.AWS.AppStream.StopImageBuilder

    -- ** DeleteImage 
    , module Network.AWS.AppStream.DeleteImage

    -- ** BatchDisassociateUserStack 
    , module Network.AWS.AppStream.BatchDisassociateUserStack

    -- ** DescribeImages (Paginated)
    , module Network.AWS.AppStream.DescribeImages

    -- * Types

    -- ** Action
    , Action (..)

    -- ** AuthenticationType
    , AuthenticationType (..)

    -- ** FleetAttribute
    , FleetAttribute (..)

    -- ** FleetErrorCode
    , FleetErrorCode (..)

    -- ** FleetState
    , FleetState (..)

    -- ** FleetType
    , FleetType (..)

    -- ** ImageBuilderState
    , ImageBuilderState (..)

    -- ** ImageBuilderStateChangeReasonCode
    , ImageBuilderStateChangeReasonCode (..)

    -- ** ImageState
    , ImageState (..)

    -- ** ImageStateChangeReasonCode
    , ImageStateChangeReasonCode (..)

    -- ** MessageAction
    , MessageAction (..)

    -- ** Permission
    , Permission (..)

    -- ** PlatformType
    , PlatformType (..)

    -- ** SessionState
    , SessionState (..)

    -- ** StackAttribute
    , StackAttribute (..)

    -- ** StackErrorCode
    , StackErrorCode (..)

    -- ** StorageConnectorType
    , StorageConnectorType (..)

    -- ** UserStackAssociationErrorCode
    , UserStackAssociationErrorCode (..)

    -- ** VisibilityType
    , VisibilityType (..)

    -- ** Application
    , Application
    , application
    , appEnabled
    , appLaunchPath
    , appLaunchParameters
    , appName
    , appDisplayName
    , appMetadata
    , appIconURL

    -- ** ApplicationSettings
    , ApplicationSettings
    , applicationSettings
    , aSettingsGroup
    , aEnabled

    -- ** ApplicationSettingsResponse
    , ApplicationSettingsResponse
    , applicationSettingsResponse
    , asEnabled
    , asSettingsGroup
    , asS3BucketName

    -- ** ComputeCapacity
    , ComputeCapacity
    , computeCapacity
    , ccDesiredInstances

    -- ** ComputeCapacityStatus
    , ComputeCapacityStatus
    , computeCapacityStatus
    , ccsInUse
    , ccsRunning
    , ccsAvailable
    , ccsDesired

    -- ** DirectoryConfig
    , DirectoryConfig
    , directoryConfig
    , dcCreatedTime
    , dcServiceAccountCredentials
    , dcOrganizationalUnitDistinguishedNames
    , dcDirectoryName

    -- ** DomainJoinInfo
    , DomainJoinInfo
    , domainJoinInfo
    , djiOrganizationalUnitDistinguishedName
    , djiDirectoryName

    -- ** Fleet
    , Fleet
    , fleet
    , fDomainJoinInfo
    , fDisconnectTimeoutInSeconds
    , fMaxUserDurationInSeconds
    , fCreatedTime
    , fFleetType
    , fVPCConfig
    , fImageARN
    , fFleetErrors
    , fDisplayName
    , fEnableDefaultInternetAccess
    , fImageName
    , fDescription
    , fARN
    , fName
    , fInstanceType
    , fComputeCapacityStatus
    , fState

    -- ** FleetError
    , FleetError
    , fleetError
    , feErrorCode
    , feErrorMessage

    -- ** Image
    , Image
    , image
    , iState
    , iImagePermissions
    , iPlatform
    , iPublicBaseImageReleasedDate
    , iStateChangeReason
    , iARN
    , iCreatedTime
    , iImageBuilderSupported
    , iVisibility
    , iBaseImageARN
    , iDisplayName
    , iDescription
    , iAppstreamAgentVersion
    , iApplications
    , iName

    -- ** ImageBuilder
    , ImageBuilder
    , imageBuilder
    , ibDomainJoinInfo
    , ibState
    , ibPlatform
    , ibStateChangeReason
    , ibARN
    , ibCreatedTime
    , ibImageBuilderErrors
    , ibInstanceType
    , ibVPCConfig
    , ibImageARN
    , ibDisplayName
    , ibEnableDefaultInternetAccess
    , ibDescription
    , ibAppstreamAgentVersion
    , ibName

    -- ** ImageBuilderStateChangeReason
    , ImageBuilderStateChangeReason
    , imageBuilderStateChangeReason
    , ibscrCode
    , ibscrMessage

    -- ** ImagePermissions
    , ImagePermissions
    , imagePermissions
    , ipAllowFleet
    , ipAllowImageBuilder

    -- ** ImageStateChangeReason
    , ImageStateChangeReason
    , imageStateChangeReason
    , iscrCode
    , iscrMessage

    -- ** NetworkAccessConfiguration
    , NetworkAccessConfiguration
    , networkAccessConfiguration
    , nacEniId
    , nacEniPrivateIPAddress

    -- ** ResourceError
    , ResourceError
    , resourceError
    , reErrorCode
    , reErrorMessage
    , reErrorTimestamp

    -- ** ServiceAccountCredentials
    , ServiceAccountCredentials
    , serviceAccountCredentials
    , sacAccountName
    , sacAccountPassword

    -- ** Session
    , Session
    , session
    , sNetworkAccessConfiguration
    , sAuthenticationType
    , sId
    , sUserId
    , sStackName
    , sFleetName
    , sState

    -- ** SharedImagePermissions
    , SharedImagePermissions
    , sharedImagePermissions
    , sipSharedAccountId
    , sipImagePermissions

    -- ** Stack
    , Stack
    , stack
    , sUserSettings
    , sApplicationSettings
    , sFeedbackURL
    , sARN
    , sCreatedTime
    , sStorageConnectors
    , sDisplayName
    , sStackErrors
    , sDescription
    , sRedirectURL
    , sName

    -- ** StackError
    , StackError
    , stackError
    , seErrorCode
    , seErrorMessage

    -- ** StorageConnector
    , StorageConnector
    , storageConnector
    , scDomains
    , scResourceIdentifier
    , scConnectorType

    -- ** User
    , User
    , user
    , uStatus
    , uEnabled
    , uLastName
    , uARN
    , uCreatedTime
    , uUserName
    , uFirstName
    , uAuthenticationType

    -- ** UserSetting
    , UserSetting
    , userSetting
    , usAction
    , usPermission

    -- ** UserStackAssociation
    , UserStackAssociation
    , userStackAssociation
    , usaSendEmailNotification
    , usaStackName
    , usaUserName
    , usaAuthenticationType

    -- ** UserStackAssociationError
    , UserStackAssociationError
    , userStackAssociationError
    , usaeUserStackAssociation
    , usaeErrorCode
    , usaeErrorMessage

    -- ** VPCConfig
    , VPCConfig
    , vpcConfig
    , vcSecurityGroupIds
    , vcSubnetIds
    ) where

import Network.AWS.AppStream.AssociateFleet
import Network.AWS.AppStream.BatchAssociateUserStack
import Network.AWS.AppStream.BatchDisassociateUserStack
import Network.AWS.AppStream.CopyImage
import Network.AWS.AppStream.CreateDirectoryConfig
import Network.AWS.AppStream.CreateFleet
import Network.AWS.AppStream.CreateImageBuilder
import Network.AWS.AppStream.CreateImageBuilderStreamingURL
import Network.AWS.AppStream.CreateStack
import Network.AWS.AppStream.CreateStreamingURL
import Network.AWS.AppStream.CreateUser
import Network.AWS.AppStream.DeleteDirectoryConfig
import Network.AWS.AppStream.DeleteFleet
import Network.AWS.AppStream.DeleteImage
import Network.AWS.AppStream.DeleteImageBuilder
import Network.AWS.AppStream.DeleteImagePermissions
import Network.AWS.AppStream.DeleteStack
import Network.AWS.AppStream.DeleteUser
import Network.AWS.AppStream.DescribeDirectoryConfigs
import Network.AWS.AppStream.DescribeFleets
import Network.AWS.AppStream.DescribeImageBuilders
import Network.AWS.AppStream.DescribeImagePermissions
import Network.AWS.AppStream.DescribeImages
import Network.AWS.AppStream.DescribeSessions
import Network.AWS.AppStream.DescribeStacks
import Network.AWS.AppStream.DescribeUserStackAssociations
import Network.AWS.AppStream.DescribeUsers
import Network.AWS.AppStream.DisableUser
import Network.AWS.AppStream.DisassociateFleet
import Network.AWS.AppStream.EnableUser
import Network.AWS.AppStream.ExpireSession
import Network.AWS.AppStream.ListAssociatedFleets
import Network.AWS.AppStream.ListAssociatedStacks
import Network.AWS.AppStream.ListTagsForResource
import Network.AWS.AppStream.StartFleet
import Network.AWS.AppStream.StartImageBuilder
import Network.AWS.AppStream.StopFleet
import Network.AWS.AppStream.StopImageBuilder
import Network.AWS.AppStream.TagResource
import Network.AWS.AppStream.Types
import Network.AWS.AppStream.UntagResource
import Network.AWS.AppStream.UpdateDirectoryConfig
import Network.AWS.AppStream.UpdateFleet
import Network.AWS.AppStream.UpdateImagePermissions
import Network.AWS.AppStream.UpdateStack
import Network.AWS.AppStream.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'AppStream'.
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
