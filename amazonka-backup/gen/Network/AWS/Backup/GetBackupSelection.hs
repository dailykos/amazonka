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
-- Module      : Network.AWS.Backup.GetBackupSelection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns selection metadata and a document in JSON format that specifies a list of resources that are associated with a backup plan.
--
--
module Network.AWS.Backup.GetBackupSelection
    (
    -- * Creating a Request
      getBackupSelection
    , GetBackupSelection
    -- * Request Lenses
    , gbsBackupPlanId
    , gbsSelectionId

    -- * Destructuring the Response
    , getBackupSelectionResponse
    , GetBackupSelectionResponse
    -- * Response Lenses
    , gbsrsSelectionId
    , gbsrsBackupPlanId
    , gbsrsCreatorRequestId
    , gbsrsCreationDate
    , gbsrsBackupSelection
    , gbsrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getBackupSelection' smart constructor.
data GetBackupSelection = GetBackupSelection'
  { _gbsBackupPlanId :: !Text
  , _gbsSelectionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBackupSelection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbsBackupPlanId' - Uniquely identifies a backup plan.
--
-- * 'gbsSelectionId' - Uniquely identifies the body of a request to assign a set of resources to a backup plan.
getBackupSelection
    :: Text -- ^ 'gbsBackupPlanId'
    -> Text -- ^ 'gbsSelectionId'
    -> GetBackupSelection
getBackupSelection pBackupPlanId_ pSelectionId_ =
  GetBackupSelection'
    {_gbsBackupPlanId = pBackupPlanId_, _gbsSelectionId = pSelectionId_}


-- | Uniquely identifies a backup plan.
gbsBackupPlanId :: Lens' GetBackupSelection Text
gbsBackupPlanId = lens _gbsBackupPlanId (\ s a -> s{_gbsBackupPlanId = a})

-- | Uniquely identifies the body of a request to assign a set of resources to a backup plan.
gbsSelectionId :: Lens' GetBackupSelection Text
gbsSelectionId = lens _gbsSelectionId (\ s a -> s{_gbsSelectionId = a})

instance AWSRequest GetBackupSelection where
        type Rs GetBackupSelection =
             GetBackupSelectionResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 GetBackupSelectionResponse' <$>
                   (x .?> "SelectionId") <*> (x .?> "BackupPlanId") <*>
                     (x .?> "CreatorRequestId")
                     <*> (x .?> "CreationDate")
                     <*> (x .?> "BackupSelection")
                     <*> (pure (fromEnum s)))

instance Hashable GetBackupSelection where

instance NFData GetBackupSelection where

instance ToHeaders GetBackupSelection where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetBackupSelection where
        toPath GetBackupSelection'{..}
          = mconcat
              ["/backup/plans/", toBS _gbsBackupPlanId,
               "/selections/", toBS _gbsSelectionId]

instance ToQuery GetBackupSelection where
        toQuery = const mempty

-- | /See:/ 'getBackupSelectionResponse' smart constructor.
data GetBackupSelectionResponse = GetBackupSelectionResponse'
  { _gbsrsSelectionId :: !(Maybe Text)
  , _gbsrsBackupPlanId :: !(Maybe Text)
  , _gbsrsCreatorRequestId :: !(Maybe Text)
  , _gbsrsCreationDate :: !(Maybe POSIX)
  , _gbsrsBackupSelection :: !(Maybe BackupSelection)
  , _gbsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBackupSelectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbsrsSelectionId' - Uniquely identifies the body of a request to assign a set of resources to a backup plan.
--
-- * 'gbsrsBackupPlanId' - Uniquely identifies a backup plan.
--
-- * 'gbsrsCreatorRequestId' - A unique string that identifies the request and allows failed requests to be retried without the risk of executing the operation twice.
--
-- * 'gbsrsCreationDate' - The date and time a backup selection is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'gbsrsBackupSelection' - Specifies the body of a request to assign a set of resources to a backup plan. It includes an array of resources, an optional array of patterns to exclude resources, an optional role to provide access to the AWS service that the resource belongs to, and an optional array of tags used to identify a set of resources.
--
-- * 'gbsrsResponseStatus' - -- | The response status code.
getBackupSelectionResponse
    :: Int -- ^ 'gbsrsResponseStatus'
    -> GetBackupSelectionResponse
getBackupSelectionResponse pResponseStatus_ =
  GetBackupSelectionResponse'
    { _gbsrsSelectionId = Nothing
    , _gbsrsBackupPlanId = Nothing
    , _gbsrsCreatorRequestId = Nothing
    , _gbsrsCreationDate = Nothing
    , _gbsrsBackupSelection = Nothing
    , _gbsrsResponseStatus = pResponseStatus_
    }


-- | Uniquely identifies the body of a request to assign a set of resources to a backup plan.
gbsrsSelectionId :: Lens' GetBackupSelectionResponse (Maybe Text)
gbsrsSelectionId = lens _gbsrsSelectionId (\ s a -> s{_gbsrsSelectionId = a})

-- | Uniquely identifies a backup plan.
gbsrsBackupPlanId :: Lens' GetBackupSelectionResponse (Maybe Text)
gbsrsBackupPlanId = lens _gbsrsBackupPlanId (\ s a -> s{_gbsrsBackupPlanId = a})

-- | A unique string that identifies the request and allows failed requests to be retried without the risk of executing the operation twice.
gbsrsCreatorRequestId :: Lens' GetBackupSelectionResponse (Maybe Text)
gbsrsCreatorRequestId = lens _gbsrsCreatorRequestId (\ s a -> s{_gbsrsCreatorRequestId = a})

-- | The date and time a backup selection is created, in Unix format and Coordinated Universal Time (UTC). The value of @CreationDate@ is accurate to milliseconds. For example, the value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
gbsrsCreationDate :: Lens' GetBackupSelectionResponse (Maybe UTCTime)
gbsrsCreationDate = lens _gbsrsCreationDate (\ s a -> s{_gbsrsCreationDate = a}) . mapping _Time

-- | Specifies the body of a request to assign a set of resources to a backup plan. It includes an array of resources, an optional array of patterns to exclude resources, an optional role to provide access to the AWS service that the resource belongs to, and an optional array of tags used to identify a set of resources.
gbsrsBackupSelection :: Lens' GetBackupSelectionResponse (Maybe BackupSelection)
gbsrsBackupSelection = lens _gbsrsBackupSelection (\ s a -> s{_gbsrsBackupSelection = a})

-- | -- | The response status code.
gbsrsResponseStatus :: Lens' GetBackupSelectionResponse Int
gbsrsResponseStatus = lens _gbsrsResponseStatus (\ s a -> s{_gbsrsResponseStatus = a})

instance NFData GetBackupSelectionResponse where
