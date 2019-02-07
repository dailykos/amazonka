{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Amplify
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amplify is a fully managed continuous deployment and hosting service for modern web apps. 
--
--
module Network.AWS.Amplify
    (
    -- * Service Configuration
      amplify

    -- * Errors
    -- $errors

    -- ** DependentServiceFailureException
    , _DependentServiceFailureException

    -- ** NotFoundException
    , _NotFoundException

    -- ** InternalFailureException
    , _InternalFailureException

    -- ** UnauthorizedException
    , _UnauthorizedException

    -- ** BadRequestException
    , _BadRequestException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetDomainAssociation 
    , module Network.AWS.Amplify.GetDomainAssociation

    -- ** StopJob 
    , module Network.AWS.Amplify.StopJob

    -- ** GetBranch 
    , module Network.AWS.Amplify.GetBranch

    -- ** CreateDomainAssociation 
    , module Network.AWS.Amplify.CreateDomainAssociation

    -- ** DeleteBranch 
    , module Network.AWS.Amplify.DeleteBranch

    -- ** UpdateBranch 
    , module Network.AWS.Amplify.UpdateBranch

    -- ** CreateBranch 
    , module Network.AWS.Amplify.CreateBranch

    -- ** ListApps (Paginated)
    , module Network.AWS.Amplify.ListApps

    -- ** ListBranches (Paginated)
    , module Network.AWS.Amplify.ListBranches

    -- ** DeleteApp 
    , module Network.AWS.Amplify.DeleteApp

    -- ** UpdateApp 
    , module Network.AWS.Amplify.UpdateApp

    -- ** ListJobs (Paginated)
    , module Network.AWS.Amplify.ListJobs

    -- ** DeleteJob 
    , module Network.AWS.Amplify.DeleteJob

    -- ** GetJob 
    , module Network.AWS.Amplify.GetJob

    -- ** StartJob 
    , module Network.AWS.Amplify.StartJob

    -- ** GetApp 
    , module Network.AWS.Amplify.GetApp

    -- ** CreateApp 
    , module Network.AWS.Amplify.CreateApp

    -- ** DeleteDomainAssociation 
    , module Network.AWS.Amplify.DeleteDomainAssociation

    -- ** UpdateDomainAssociation 
    , module Network.AWS.Amplify.UpdateDomainAssociation

    -- ** ListDomainAssociations (Paginated)
    , module Network.AWS.Amplify.ListDomainAssociations

    -- * Types

    -- ** Common
    , module Network.AWS.Amplify.Internal

    -- ** DomainStatus
    , DomainStatus (..)

    -- ** JobStatus
    , JobStatus (..)

    -- ** JobType
    , JobType (..)

    -- ** Platform
    , Platform (..)

    -- ** Stage
    , Stage (..)

    -- ** App
    , App
    , app
    , appBasicAuthCredentials
    , appBuildSpec
    , appCustomRules
    , appIamServiceRoleARN
    , appProductionBranch
    , appTags
    , appAppId
    , appAppARN
    , appName
    , appDescription
    , appRepository
    , appPlatform
    , appCreateTime
    , appUpdateTime
    , appEnvironmentVariables
    , appDefaultDomain
    , appEnableBranchAutoBuild
    , appEnableBasicAuth

    -- ** Branch
    , Branch
    , branch
    , bThumbnailURL
    , bBasicAuthCredentials
    , bBuildSpec
    , bDisplayName
    , bTags
    , bBranchARN
    , bBranchName
    , bDescription
    , bStage
    , bEnableNotification
    , bCreateTime
    , bUpdateTime
    , bEnvironmentVariables
    , bEnableAutoBuild
    , bCustomDomains
    , bFramework
    , bActiveJobId
    , bTotalNumberOfJobs
    , bEnableBasicAuth
    , bTtl

    -- ** CustomRule
    , CustomRule
    , customRule
    , crStatus
    , crCondition
    , crSource
    , crTarget

    -- ** DomainAssociation
    , DomainAssociation
    , domainAssociation
    , daDomainAssociationARN
    , daDomainName
    , daEnableAutoSubDomain
    , daDomainStatus
    , daStatusReason
    , daCertificateVerificationDNSRecord
    , daSubDomains

    -- ** Job
    , Job
    , job
    , jSummary
    , jSteps

    -- ** JobSummary
    , JobSummary
    , jobSummary
    , jsEndTime
    , jsJobARN
    , jsJobId
    , jsCommitId
    , jsCommitMessage
    , jsCommitTime
    , jsStartTime
    , jsStatus
    , jsJobType

    -- ** ProductionBranch
    , ProductionBranch
    , productionBranch
    , pbLastDeployTime
    , pbStatus
    , pbThumbnailURL
    , pbBranchName

    -- ** Step
    , Step
    , step
    , sLogURL
    , sArtifactsURL
    , sScreenshots
    , sStepName
    , sStartTime
    , sStatus
    , sEndTime

    -- ** SubDomain
    , SubDomain
    , subDomain
    , sdSubDomainSetting
    , sdVerified
    , sdDnsRecord

    -- ** SubDomainSetting
    , SubDomainSetting
    , subDomainSetting
    , sdsPrefix
    , sdsBranchName
    ) where

import Network.AWS.Amplify.CreateApp
import Network.AWS.Amplify.CreateBranch
import Network.AWS.Amplify.CreateDomainAssociation
import Network.AWS.Amplify.DeleteApp
import Network.AWS.Amplify.DeleteBranch
import Network.AWS.Amplify.DeleteDomainAssociation
import Network.AWS.Amplify.DeleteJob
import Network.AWS.Amplify.GetApp
import Network.AWS.Amplify.GetBranch
import Network.AWS.Amplify.GetDomainAssociation
import Network.AWS.Amplify.GetJob
import Network.AWS.Amplify.ListApps
import Network.AWS.Amplify.ListBranches
import Network.AWS.Amplify.ListDomainAssociations
import Network.AWS.Amplify.ListJobs
import Network.AWS.Amplify.StartJob
import Network.AWS.Amplify.StopJob
import Network.AWS.Amplify.Types
import Network.AWS.Amplify.UpdateApp
import Network.AWS.Amplify.UpdateBranch
import Network.AWS.Amplify.UpdateDomainAssociation
import Network.AWS.Amplify.Waiters
import Network.AWS.Amplify.Internal

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Amplify'.
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
