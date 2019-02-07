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
-- Module      : Network.AWS.Backup.GetBackupPlanFromJSON
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a valid JSON document specifying a backup plan or an error.
--
--
module Network.AWS.Backup.GetBackupPlanFromJSON
    (
    -- * Creating a Request
      getBackupPlanFromJSON
    , GetBackupPlanFromJSON
    -- * Request Lenses
    , gbpfjBackupPlanTemplateJSON

    -- * Destructuring the Response
    , getBackupPlanFromJSONResponse
    , GetBackupPlanFromJSONResponse
    -- * Response Lenses
    , gbpfjrsBackupPlan
    , gbpfjrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getBackupPlanFromJSON' smart constructor.
newtype GetBackupPlanFromJSON = GetBackupPlanFromJSON'
  { _gbpfjBackupPlanTemplateJSON :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBackupPlanFromJSON' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbpfjBackupPlanTemplateJSON' - A customer-supplied backup plan document in JSON format.
getBackupPlanFromJSON
    :: Text -- ^ 'gbpfjBackupPlanTemplateJSON'
    -> GetBackupPlanFromJSON
getBackupPlanFromJSON pBackupPlanTemplateJSON_ =
  GetBackupPlanFromJSON'
    {_gbpfjBackupPlanTemplateJSON = pBackupPlanTemplateJSON_}


-- | A customer-supplied backup plan document in JSON format.
gbpfjBackupPlanTemplateJSON :: Lens' GetBackupPlanFromJSON Text
gbpfjBackupPlanTemplateJSON = lens _gbpfjBackupPlanTemplateJSON (\ s a -> s{_gbpfjBackupPlanTemplateJSON = a})

instance AWSRequest GetBackupPlanFromJSON where
        type Rs GetBackupPlanFromJSON =
             GetBackupPlanFromJSONResponse
        request = postJSON backup
        response
          = receiveJSON
              (\ s h x ->
                 GetBackupPlanFromJSONResponse' <$>
                   (x .?> "BackupPlan") <*> (pure (fromEnum s)))

instance Hashable GetBackupPlanFromJSON where

instance NFData GetBackupPlanFromJSON where

instance ToHeaders GetBackupPlanFromJSON where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetBackupPlanFromJSON where
        toJSON GetBackupPlanFromJSON'{..}
          = object
              (catMaybes
                 [Just
                    ("BackupPlanTemplateJson" .=
                       _gbpfjBackupPlanTemplateJSON)])

instance ToPath GetBackupPlanFromJSON where
        toPath = const "/backup/template/json/toPlan"

instance ToQuery GetBackupPlanFromJSON where
        toQuery = const mempty

-- | /See:/ 'getBackupPlanFromJSONResponse' smart constructor.
data GetBackupPlanFromJSONResponse = GetBackupPlanFromJSONResponse'
  { _gbpfjrsBackupPlan :: !(Maybe BackupPlan)
  , _gbpfjrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBackupPlanFromJSONResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbpfjrsBackupPlan' - Specifies the body of a backup plan. Includes a @BackupPlanName@ and one or more sets of @Rules@ .
--
-- * 'gbpfjrsResponseStatus' - -- | The response status code.
getBackupPlanFromJSONResponse
    :: Int -- ^ 'gbpfjrsResponseStatus'
    -> GetBackupPlanFromJSONResponse
getBackupPlanFromJSONResponse pResponseStatus_ =
  GetBackupPlanFromJSONResponse'
    {_gbpfjrsBackupPlan = Nothing, _gbpfjrsResponseStatus = pResponseStatus_}


-- | Specifies the body of a backup plan. Includes a @BackupPlanName@ and one or more sets of @Rules@ .
gbpfjrsBackupPlan :: Lens' GetBackupPlanFromJSONResponse (Maybe BackupPlan)
gbpfjrsBackupPlan = lens _gbpfjrsBackupPlan (\ s a -> s{_gbpfjrsBackupPlan = a})

-- | -- | The response status code.
gbpfjrsResponseStatus :: Lens' GetBackupPlanFromJSONResponse Int
gbpfjrsResponseStatus = lens _gbpfjrsResponseStatus (\ s a -> s{_gbpfjrsResponseStatus = a})

instance NFData GetBackupPlanFromJSONResponse where
