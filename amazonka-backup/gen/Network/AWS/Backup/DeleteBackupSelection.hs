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
-- Module      : Network.AWS.Backup.DeleteBackupSelection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the resource selection associated with a backup plan that is specified by the @SelectionId@ .
--
--
module Network.AWS.Backup.DeleteBackupSelection
    (
    -- * Creating a Request
      deleteBackupSelection
    , DeleteBackupSelection
    -- * Request Lenses
    , dbsBackupPlanId
    , dbsSelectionId

    -- * Destructuring the Response
    , deleteBackupSelectionResponse
    , DeleteBackupSelectionResponse
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteBackupSelection' smart constructor.
data DeleteBackupSelection = DeleteBackupSelection'
  { _dbsBackupPlanId :: !Text
  , _dbsSelectionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBackupSelection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbsBackupPlanId' - Uniquely identifies a backup plan.
--
-- * 'dbsSelectionId' - Uniquely identifies the body of a request to assign a set of resources to a backup plan.
deleteBackupSelection
    :: Text -- ^ 'dbsBackupPlanId'
    -> Text -- ^ 'dbsSelectionId'
    -> DeleteBackupSelection
deleteBackupSelection pBackupPlanId_ pSelectionId_ =
  DeleteBackupSelection'
    {_dbsBackupPlanId = pBackupPlanId_, _dbsSelectionId = pSelectionId_}


-- | Uniquely identifies a backup plan.
dbsBackupPlanId :: Lens' DeleteBackupSelection Text
dbsBackupPlanId = lens _dbsBackupPlanId (\ s a -> s{_dbsBackupPlanId = a})

-- | Uniquely identifies the body of a request to assign a set of resources to a backup plan.
dbsSelectionId :: Lens' DeleteBackupSelection Text
dbsSelectionId = lens _dbsSelectionId (\ s a -> s{_dbsSelectionId = a})

instance AWSRequest DeleteBackupSelection where
        type Rs DeleteBackupSelection =
             DeleteBackupSelectionResponse
        request = delete backup
        response = receiveNull DeleteBackupSelectionResponse'

instance Hashable DeleteBackupSelection where

instance NFData DeleteBackupSelection where

instance ToHeaders DeleteBackupSelection where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteBackupSelection where
        toPath DeleteBackupSelection'{..}
          = mconcat
              ["/backup/plans/", toBS _dbsBackupPlanId,
               "/selections/", toBS _dbsSelectionId]

instance ToQuery DeleteBackupSelection where
        toQuery = const mempty

-- | /See:/ 'deleteBackupSelectionResponse' smart constructor.
data DeleteBackupSelectionResponse =
  DeleteBackupSelectionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBackupSelectionResponse' with the minimum fields required to make a request.
--
deleteBackupSelectionResponse
    :: DeleteBackupSelectionResponse
deleteBackupSelectionResponse = DeleteBackupSelectionResponse'


instance NFData DeleteBackupSelectionResponse where
