{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DLM
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Data Lifecycle Manager__ 
--
-- With Amazon Data Lifecycle Manager, you can manage the lifecycle of your AWS resources. You create lifecycle policies, which are used to automate operations on the specified resources.
--
-- Amazon DLM supports Amazon EBS volumes and snapshots. For information about using Amazon DLM with Amazon EBS, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshot-lifecycle.html Automating the Amazon EBS Snapshot Lifecycle> in the /Amazon EC2 User Guide/ .
--
module Network.AWS.DLM
    (
    -- * Service Configuration
      dlm

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** InternalServerException
    , _InternalServerException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteLifecyclePolicy 
    , module Network.AWS.DLM.DeleteLifecyclePolicy

    -- ** UpdateLifecyclePolicy 
    , module Network.AWS.DLM.UpdateLifecyclePolicy

    -- ** CreateLifecyclePolicy 
    , module Network.AWS.DLM.CreateLifecyclePolicy

    -- ** GetLifecyclePolicy 
    , module Network.AWS.DLM.GetLifecyclePolicy

    -- ** GetLifecyclePolicies 
    , module Network.AWS.DLM.GetLifecyclePolicies

    -- * Types

    -- ** Common
    , module Network.AWS.DLM.Internal

    -- ** GettablePolicyStateValues
    , GettablePolicyStateValues (..)

    -- ** IntervalUnitValues
    , IntervalUnitValues (..)

    -- ** ResourceTypeValues
    , ResourceTypeValues (..)

    -- ** SettablePolicyStateValues
    , SettablePolicyStateValues (..)

    -- ** CreateRule
    , CreateRule
    , createRule
    , crTimes
    , crInterval
    , crIntervalUnit

    -- ** LifecyclePolicy
    , LifecyclePolicy
    , lifecyclePolicy
    , lpState
    , lpPolicyDetails
    , lpPolicyId
    , lpExecutionRoleARN
    , lpDateCreated
    , lpDateModified
    , lpDescription

    -- ** LifecyclePolicySummary
    , LifecyclePolicySummary
    , lifecyclePolicySummary
    , lpsState
    , lpsPolicyId
    , lpsDescription

    -- ** PolicyDetails
    , PolicyDetails
    , policyDetails
    , pdTargetTags
    , pdSchedules
    , pdResourceTypes

    -- ** RetainRule
    , RetainRule
    , retainRule
    , rrCount

    -- ** Schedule
    , Schedule
    , schedule
    , sCreateRule
    , sCopyTags
    , sName
    , sTagsToAdd
    , sRetainRule

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue
    ) where

import Network.AWS.DLM.CreateLifecyclePolicy
import Network.AWS.DLM.DeleteLifecyclePolicy
import Network.AWS.DLM.GetLifecyclePolicies
import Network.AWS.DLM.GetLifecyclePolicy
import Network.AWS.DLM.Types
import Network.AWS.DLM.UpdateLifecyclePolicy
import Network.AWS.DLM.Waiters
import Network.AWS.DLM.Internal

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'DLM'.
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
