{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ComprehendMedical
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Comprehend Medical extracts structured information from unstructured clinical text. Use these actions to gain insight in your documents. 
--
--
module Network.AWS.ComprehendMedical
    (
    -- * Service Configuration
      comprehendMedical

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** TooManyRequestsException
    , _TooManyRequestsException

    -- ** InvalidEncodingException
    , _InvalidEncodingException

    -- ** InternalServerException
    , _InternalServerException

    -- ** ServiceUnavailableException
    , _ServiceUnavailableException

    -- ** TextSizeLimitExceededException
    , _TextSizeLimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DetectEntities 
    , module Network.AWS.ComprehendMedical.DetectEntities

    -- ** DetectPHI 
    , module Network.AWS.ComprehendMedical.DetectPHI

    -- * Types

    -- ** Common
    , module Network.AWS.ComprehendMedical.Internal

    -- ** AttributeName
    , AttributeName (..)

    -- ** EntitySubType
    , EntitySubType (..)

    -- ** EntityType
    , EntityType (..)

    -- ** Attribute
    , Attribute
    , attribute
    , aRelationshipScore
    , aBeginOffset
    , aText
    , aScore
    , aTraits
    , aEndOffset
    , aId
    , aType

    -- ** Entity
    , Entity
    , entity
    , eBeginOffset
    , eText
    , eCategory
    , eScore
    , eTraits
    , eAttributes
    , eEndOffset
    , eId
    , eType

    -- ** Trait
    , Trait
    , trait
    , tScore
    , tName

    -- ** UnmappedAttribute
    , UnmappedAttribute
    , unmappedAttribute
    , uaAttribute
    , uaType
    ) where

import Network.AWS.ComprehendMedical.DetectEntities
import Network.AWS.ComprehendMedical.DetectPHI
import Network.AWS.ComprehendMedical.Types
import Network.AWS.ComprehendMedical.Waiters
import Network.AWS.ComprehendMedical.Internal

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'ComprehendMedical'.
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
