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
-- Module      : Network.AWS.Backup.StopBackupJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to cancel a job to create a one-time backup of a resource.
--
--
module Network.AWS.Backup.StopBackupJob
    (
    -- * Creating a Request
      stopBackupJob
    , StopBackupJob
    -- * Request Lenses
    , sbjBackupJobId

    -- * Destructuring the Response
    , stopBackupJobResponse
    , StopBackupJobResponse
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopBackupJob' smart constructor.
newtype StopBackupJob = StopBackupJob'
  { _sbjBackupJobId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopBackupJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbjBackupJobId' - Uniquely identifies a request to AWS Backup to back up a resource.
stopBackupJob
    :: Text -- ^ 'sbjBackupJobId'
    -> StopBackupJob
stopBackupJob pBackupJobId_ = StopBackupJob' {_sbjBackupJobId = pBackupJobId_}


-- | Uniquely identifies a request to AWS Backup to back up a resource.
sbjBackupJobId :: Lens' StopBackupJob Text
sbjBackupJobId = lens _sbjBackupJobId (\ s a -> s{_sbjBackupJobId = a})

instance AWSRequest StopBackupJob where
        type Rs StopBackupJob = StopBackupJobResponse
        request = postJSON backup
        response = receiveNull StopBackupJobResponse'

instance Hashable StopBackupJob where

instance NFData StopBackupJob where

instance ToHeaders StopBackupJob where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopBackupJob where
        toJSON = const (Object mempty)

instance ToPath StopBackupJob where
        toPath StopBackupJob'{..}
          = mconcat ["/backup-jobs/", toBS _sbjBackupJobId]

instance ToQuery StopBackupJob where
        toQuery = const mempty

-- | /See:/ 'stopBackupJobResponse' smart constructor.
data StopBackupJobResponse =
  StopBackupJobResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopBackupJobResponse' with the minimum fields required to make a request.
--
stopBackupJobResponse
    :: StopBackupJobResponse
stopBackupJobResponse = StopBackupJobResponse'


instance NFData StopBackupJobResponse where
