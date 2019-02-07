{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Chime
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The Amazon Chime API (application programming interface) is designed for administrators to use to perform key tasks, such as creating and managing Amazon Chime accounts and users. This guide provides detailed information about the Amazon Chime API, including operations, types, inputs and outputs, and error codes.
--
--
-- You can use an AWS SDK, the AWS Command Line Interface (AWS CLI), or the REST API to make API calls. We recommend using an AWS SDK or the AWS CLI. Each API operation includes links to information about using it with a language-specific AWS SDK or the AWS CLI.
--
--     * Using an AWS SDK    * You don't need to write code to calculate a signature for request authentication. The SDK clients authenticate your requests by using access keys that you provide. For more information about AWS SDKs, see the <http://aws.amazon.com/developer/ AWS Developer Center> .
--
--     * Using the AWS CLI    * Use your access keys with the AWS CLI to make API calls. For information about setting up the AWS CLI, see <http://docs.aws.amazon.com/cli/latest/userguide/installing.html Installing the AWS Command Line Interface> in the /AWS Command Line Interface User Guide/ . For a list of available Amazon Chime commands, see the <http://docs.aws.amazon.com/cli/latest/reference/chime/index.html Amazon Chime commands> in the /AWS CLI Command Reference/ .
--
--     * Using REST API    * If you use REST to make API calls, you must authenticate your request by providing a signature. Amazon Chime supports signature version 4. For more information, see <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> in the /Amazon Web Services General Reference/ .
--
-- When making REST API calls, use the service name @chime@ and REST endpoint @https://service.chime.aws.amazon.com@ .
--
--
--
-- Administrative permissions are controlled using AWS Identity and Access Management (IAM). For more information, see <http://docs.aws.amazon.com/chime/latest/ag/control-access.html Control Access to the Amazon Chime Console> in the /Amazon Chime Administration Guide/ .
--
module Network.AWS.Chime
    (
    -- * Service Configuration
      chime

    -- * Errors
    -- $errors

    -- ** ThrottledClientException
    , _ThrottledClientException

    -- ** UnprocessableEntityException
    , _UnprocessableEntityException

    -- ** ConflictException
    , _ConflictException

    -- ** ForbiddenException
    , _ForbiddenException

    -- ** NotFoundException
    , _NotFoundException

    -- ** ServiceFailureException
    , _ServiceFailureException

    -- ** UnauthorizedClientException
    , _UnauthorizedClientException

    -- ** ServiceUnavailableException
    , _ServiceUnavailableException

    -- ** BadRequestException
    , _BadRequestException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ResetPersonalPIN 
    , module Network.AWS.Chime.ResetPersonalPIN

    -- ** UpdateAccountSettings 
    , module Network.AWS.Chime.UpdateAccountSettings

    -- ** ListUsers (Paginated)
    , module Network.AWS.Chime.ListUsers

    -- ** DeleteAccount 
    , module Network.AWS.Chime.DeleteAccount

    -- ** UpdateAccount 
    , module Network.AWS.Chime.UpdateAccount

    -- ** ListAccounts (Paginated)
    , module Network.AWS.Chime.ListAccounts

    -- ** LogoutUser 
    , module Network.AWS.Chime.LogoutUser

    -- ** BatchUpdateUser 
    , module Network.AWS.Chime.BatchUpdateUser

    -- ** BatchUnsuspendUser 
    , module Network.AWS.Chime.BatchUnsuspendUser

    -- ** GetUser 
    , module Network.AWS.Chime.GetUser

    -- ** BatchSuspendUser 
    , module Network.AWS.Chime.BatchSuspendUser

    -- ** GetAccount 
    , module Network.AWS.Chime.GetAccount

    -- ** GetAccountSettings 
    , module Network.AWS.Chime.GetAccountSettings

    -- ** CreateAccount 
    , module Network.AWS.Chime.CreateAccount

    -- ** UpdateUser 
    , module Network.AWS.Chime.UpdateUser

    -- ** InviteUsers 
    , module Network.AWS.Chime.InviteUsers

    -- * Types

    -- ** Common
    , module Network.AWS.Chime.Internal

    -- ** AccountType
    , AccountType (..)

    -- ** EmailStatus
    , EmailStatus (..)

    -- ** ErrorCode
    , ErrorCode (..)

    -- ** InviteStatus
    , InviteStatus (..)

    -- ** License
    , License (..)

    -- ** RegistrationStatus
    , RegistrationStatus (..)

    -- ** Account
    , Account
    , account
    , aDefaultLicense
    , aSupportedLicenses
    , aCreatedTimestamp
    , aAccountType
    , aAWSAccountId
    , aAccountId
    , aName

    -- ** AccountSettings
    , AccountSettings
    , accountSettings
    , asEnableDialOut
    , asDisableRemoteControl

    -- ** Invite
    , Invite
    , invite
    , iStatus
    , iEmailStatus
    , iInviteId
    , iEmailAddress

    -- ** UpdateUserRequestItem
    , UpdateUserRequestItem
    , updateUserRequestItem
    , uuriLicenseType
    , uuriUserId

    -- ** User
    , User
    , user
    , uUserInvitationStatus
    , uPersonalPIN
    , uLicenseType
    , uRegisteredOn
    , uAccountId
    , uUserRegistrationStatus
    , uInvitedOn
    , uDisplayName
    , uPrimaryEmail
    , uUserId

    -- ** UserError
    , UserError
    , userError
    , ueUserId
    , ueErrorCode
    , ueErrorMessage
    ) where

import Network.AWS.Chime.BatchSuspendUser
import Network.AWS.Chime.BatchUnsuspendUser
import Network.AWS.Chime.BatchUpdateUser
import Network.AWS.Chime.CreateAccount
import Network.AWS.Chime.DeleteAccount
import Network.AWS.Chime.GetAccount
import Network.AWS.Chime.GetAccountSettings
import Network.AWS.Chime.GetUser
import Network.AWS.Chime.InviteUsers
import Network.AWS.Chime.ListAccounts
import Network.AWS.Chime.ListUsers
import Network.AWS.Chime.LogoutUser
import Network.AWS.Chime.ResetPersonalPIN
import Network.AWS.Chime.Types
import Network.AWS.Chime.UpdateAccount
import Network.AWS.Chime.UpdateAccountSettings
import Network.AWS.Chime.UpdateUser
import Network.AWS.Chime.Waiters
import Network.AWS.Chime.Internal

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Chime'.
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
