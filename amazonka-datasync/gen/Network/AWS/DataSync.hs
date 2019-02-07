{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataSync
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS DataSync__ 
--
-- AWS DataSync is a managed data transfer service that makes it simpler for you to automate moving data between on-premises storage and Amazon Simple Storage Service (Amazon S3) or Amazon Elastic File System (Amazon EFS). 
--
-- This API interface reference for AWS DataSync contains documentation for a programming interface that you can use to manage AWS DataSync.
--
module Network.AWS.DataSync
    (
    -- * Service Configuration
      dataSync

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** UpdateTask 
    , module Network.AWS.DataSync.UpdateTask

    -- ** DescribeAgent 
    , module Network.AWS.DataSync.DescribeAgent

    -- ** DeleteTask 
    , module Network.AWS.DataSync.DeleteTask

    -- ** ListLocations (Paginated)
    , module Network.AWS.DataSync.ListLocations

    -- ** CreateLocationNfs 
    , module Network.AWS.DataSync.CreateLocationNfs

    -- ** ListTagsForResource (Paginated)
    , module Network.AWS.DataSync.ListTagsForResource

    -- ** DescribeTask 
    , module Network.AWS.DataSync.DescribeTask

    -- ** DescribeLocationS3 
    , module Network.AWS.DataSync.DescribeLocationS3

    -- ** ListAgents (Paginated)
    , module Network.AWS.DataSync.ListAgents

    -- ** DeleteAgent 
    , module Network.AWS.DataSync.DeleteAgent

    -- ** UpdateAgent 
    , module Network.AWS.DataSync.UpdateAgent

    -- ** ListTaskExecutions (Paginated)
    , module Network.AWS.DataSync.ListTaskExecutions

    -- ** CreateLocationS3 
    , module Network.AWS.DataSync.CreateLocationS3

    -- ** CreateTask 
    , module Network.AWS.DataSync.CreateTask

    -- ** CreateLocationEfs 
    , module Network.AWS.DataSync.CreateLocationEfs

    -- ** DeleteLocation 
    , module Network.AWS.DataSync.DeleteLocation

    -- ** ListTasks (Paginated)
    , module Network.AWS.DataSync.ListTasks

    -- ** StartTaskExecution 
    , module Network.AWS.DataSync.StartTaskExecution

    -- ** DescribeTaskExecution 
    , module Network.AWS.DataSync.DescribeTaskExecution

    -- ** CreateAgent 
    , module Network.AWS.DataSync.CreateAgent

    -- ** DescribeLocationEfs 
    , module Network.AWS.DataSync.DescribeLocationEfs

    -- ** TagResource 
    , module Network.AWS.DataSync.TagResource

    -- ** UntagResource 
    , module Network.AWS.DataSync.UntagResource

    -- ** DescribeLocationNfs 
    , module Network.AWS.DataSync.DescribeLocationNfs

    -- ** CancelTaskExecution 
    , module Network.AWS.DataSync.CancelTaskExecution

    -- * Types

    -- ** Common
    , module Network.AWS.DataSync.Internal

    -- ** AgentStatus
    , AgentStatus (..)

    -- ** Atime
    , Atime (..)

    -- ** Gid
    , Gid (..)

    -- ** Mtime
    , Mtime (..)

    -- ** PhaseStatus
    , PhaseStatus (..)

    -- ** PosixPermissions
    , PosixPermissions (..)

    -- ** PreserveDeletedFiles
    , PreserveDeletedFiles (..)

    -- ** PreserveDevices
    , PreserveDevices (..)

    -- ** TaskExecutionStatus
    , TaskExecutionStatus (..)

    -- ** TaskStatus
    , TaskStatus (..)

    -- ** Uid
    , Uid (..)

    -- ** VerifyMode
    , VerifyMode (..)

    -- ** AgentListEntry
    , AgentListEntry
    , agentListEntry
    , aleStatus
    , aleAgentARN
    , aleName

    -- ** EC2Config
    , EC2Config
    , ec2Config
    , ecSubnetARN
    , ecSecurityGroupARNs

    -- ** LocationListEntry
    , LocationListEntry
    , locationListEntry
    , lleLocationURI
    , lleLocationARN

    -- ** OnPremConfig
    , OnPremConfig
    , onPremConfig
    , opcAgentARNs

    -- ** Options
    , Options
    , options
    , oAtime
    , oVerifyMode
    , oPosixPermissions
    , oMtime
    , oUid
    , oBytesPerSecond
    , oGid
    , oPreserveDeletedFiles
    , oPreserveDevices

    -- ** S3Config
    , S3Config
    , s3Config
    , scBucketAccessRoleARN

    -- ** TagListEntry
    , TagListEntry
    , tagListEntry
    , tleValue
    , tleKey

    -- ** TaskExecutionListEntry
    , TaskExecutionListEntry
    , taskExecutionListEntry
    , teleStatus
    , teleTaskExecutionARN

    -- ** TaskExecutionResultDetail
    , TaskExecutionResultDetail
    , taskExecutionResultDetail
    , terdPrepareDuration
    , terdPrepareStatus
    , terdVerifyStatus
    , terdVerifyDuration
    , terdTransferStatus
    , terdErrorCode
    , terdTransferDuration
    , terdErrorDetail

    -- ** TaskListEntry
    , TaskListEntry
    , taskListEntry
    , tleStatus
    , tleTaskARN
    , tleName
    ) where

import Network.AWS.DataSync.CancelTaskExecution
import Network.AWS.DataSync.CreateAgent
import Network.AWS.DataSync.CreateLocationEfs
import Network.AWS.DataSync.CreateLocationNfs
import Network.AWS.DataSync.CreateLocationS3
import Network.AWS.DataSync.CreateTask
import Network.AWS.DataSync.DeleteAgent
import Network.AWS.DataSync.DeleteLocation
import Network.AWS.DataSync.DeleteTask
import Network.AWS.DataSync.DescribeAgent
import Network.AWS.DataSync.DescribeLocationEfs
import Network.AWS.DataSync.DescribeLocationNfs
import Network.AWS.DataSync.DescribeLocationS3
import Network.AWS.DataSync.DescribeTask
import Network.AWS.DataSync.DescribeTaskExecution
import Network.AWS.DataSync.ListAgents
import Network.AWS.DataSync.ListLocations
import Network.AWS.DataSync.ListTagsForResource
import Network.AWS.DataSync.ListTaskExecutions
import Network.AWS.DataSync.ListTasks
import Network.AWS.DataSync.StartTaskExecution
import Network.AWS.DataSync.TagResource
import Network.AWS.DataSync.Types
import Network.AWS.DataSync.UntagResource
import Network.AWS.DataSync.UpdateAgent
import Network.AWS.DataSync.UpdateTask
import Network.AWS.DataSync.Waiters
import Network.AWS.DataSync.Internal

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'DataSync'.
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
