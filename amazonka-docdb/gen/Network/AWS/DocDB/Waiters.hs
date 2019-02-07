{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DocDB.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DocDB.Waiters where

import Network.AWS.DocDB.DescribeDBInstances
import Network.AWS.DocDB.DescribeDBInstances
import Network.AWS.DocDB.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.DocDB.DescribeDBInstances' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
dbInstanceAvailable :: Wait DescribeDBInstances
dbInstanceAvailable =
  Wait
    { _waitName = "DBInstanceAvailable"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors =
        [ matchAll
            "available"
            AcceptSuccess
            (folding (concatOf ddbirsDBInstances) .
             diDBInstanceStatus . _Just . to toTextCI)
        , matchAny
            "deleted"
            AcceptFailure
            (folding (concatOf ddbirsDBInstances) .
             diDBInstanceStatus . _Just . to toTextCI)
        , matchAny
            "deleting"
            AcceptFailure
            (folding (concatOf ddbirsDBInstances) .
             diDBInstanceStatus . _Just . to toTextCI)
        , matchAny
            "failed"
            AcceptFailure
            (folding (concatOf ddbirsDBInstances) .
             diDBInstanceStatus . _Just . to toTextCI)
        , matchAny
            "incompatible-restore"
            AcceptFailure
            (folding (concatOf ddbirsDBInstances) .
             diDBInstanceStatus . _Just . to toTextCI)
        , matchAny
            "incompatible-parameters"
            AcceptFailure
            (folding (concatOf ddbirsDBInstances) .
             diDBInstanceStatus . _Just . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.DocDB.DescribeDBInstances' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
dbInstanceDeleted :: Wait DescribeDBInstances
dbInstanceDeleted =
  Wait
    { _waitName = "DBInstanceDeleted"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors =
        [ matchAll
            "deleted"
            AcceptSuccess
            (folding (concatOf ddbirsDBInstances) .
             diDBInstanceStatus . _Just . to toTextCI)
        , matchError "DBInstanceNotFound" AcceptSuccess
        , matchAny
            "creating"
            AcceptFailure
            (folding (concatOf ddbirsDBInstances) .
             diDBInstanceStatus . _Just . to toTextCI)
        , matchAny
            "modifying"
            AcceptFailure
            (folding (concatOf ddbirsDBInstances) .
             diDBInstanceStatus . _Just . to toTextCI)
        , matchAny
            "rebooting"
            AcceptFailure
            (folding (concatOf ddbirsDBInstances) .
             diDBInstanceStatus . _Just . to toTextCI)
        , matchAny
            "resetting-master-credentials"
            AcceptFailure
            (folding (concatOf ddbirsDBInstances) .
             diDBInstanceStatus . _Just . to toTextCI)
        ]
    }

