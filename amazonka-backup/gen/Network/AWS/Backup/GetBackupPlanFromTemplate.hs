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
-- Module      : Network.AWS.Backup.GetBackupPlanFromTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the template specified by its @templateId@ as a backup plan.
--
--
module Network.AWS.Backup.GetBackupPlanFromTemplate
    (
    -- * Creating a Request
      getBackupPlanFromTemplate
    , GetBackupPlanFromTemplate
    -- * Request Lenses
    , gbpftBackupPlanTemplateId

    -- * Destructuring the Response
    , getBackupPlanFromTemplateResponse
    , GetBackupPlanFromTemplateResponse
    -- * Response Lenses
    , gbpftrsBackupPlanDocument
    , gbpftrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getBackupPlanFromTemplate' smart constructor.
newtype GetBackupPlanFromTemplate = GetBackupPlanFromTemplate'
  { _gbpftBackupPlanTemplateId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBackupPlanFromTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbpftBackupPlanTemplateId' - Uniquely identifies a stored backup plan template.
getBackupPlanFromTemplate
    :: Text -- ^ 'gbpftBackupPlanTemplateId'
    -> GetBackupPlanFromTemplate
getBackupPlanFromTemplate pBackupPlanTemplateId_ =
  GetBackupPlanFromTemplate'
    {_gbpftBackupPlanTemplateId = pBackupPlanTemplateId_}


-- | Uniquely identifies a stored backup plan template.
gbpftBackupPlanTemplateId :: Lens' GetBackupPlanFromTemplate Text
gbpftBackupPlanTemplateId = lens _gbpftBackupPlanTemplateId (\ s a -> s{_gbpftBackupPlanTemplateId = a})

instance AWSRequest GetBackupPlanFromTemplate where
        type Rs GetBackupPlanFromTemplate =
             GetBackupPlanFromTemplateResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 GetBackupPlanFromTemplateResponse' <$>
                   (x .?> "BackupPlanDocument") <*> (pure (fromEnum s)))

instance Hashable GetBackupPlanFromTemplate where

instance NFData GetBackupPlanFromTemplate where

instance ToHeaders GetBackupPlanFromTemplate where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetBackupPlanFromTemplate where
        toPath GetBackupPlanFromTemplate'{..}
          = mconcat
              ["/backup/template/plans/",
               toBS _gbpftBackupPlanTemplateId, "/toPlan"]

instance ToQuery GetBackupPlanFromTemplate where
        toQuery = const mempty

-- | /See:/ 'getBackupPlanFromTemplateResponse' smart constructor.
data GetBackupPlanFromTemplateResponse = GetBackupPlanFromTemplateResponse'
  { _gbpftrsBackupPlanDocument :: !(Maybe BackupPlan)
  , _gbpftrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBackupPlanFromTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbpftrsBackupPlanDocument' - Returns the body of a backup plan based on the target template, including the name, rules, and backup vault of the plan.
--
-- * 'gbpftrsResponseStatus' - -- | The response status code.
getBackupPlanFromTemplateResponse
    :: Int -- ^ 'gbpftrsResponseStatus'
    -> GetBackupPlanFromTemplateResponse
getBackupPlanFromTemplateResponse pResponseStatus_ =
  GetBackupPlanFromTemplateResponse'
    { _gbpftrsBackupPlanDocument = Nothing
    , _gbpftrsResponseStatus = pResponseStatus_
    }


-- | Returns the body of a backup plan based on the target template, including the name, rules, and backup vault of the plan.
gbpftrsBackupPlanDocument :: Lens' GetBackupPlanFromTemplateResponse (Maybe BackupPlan)
gbpftrsBackupPlanDocument = lens _gbpftrsBackupPlanDocument (\ s a -> s{_gbpftrsBackupPlanDocument = a})

-- | -- | The response status code.
gbpftrsResponseStatus :: Lens' GetBackupPlanFromTemplateResponse Int
gbpftrsResponseStatus = lens _gbpftrsResponseStatus (\ s a -> s{_gbpftrsResponseStatus = a})

instance NFData GetBackupPlanFromTemplateResponse
         where
