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
-- Module      : Network.AWS.Backup.ExportBackupPlanTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the backup plan that is specified by the plan ID as a backup template.
--
--
module Network.AWS.Backup.ExportBackupPlanTemplate
    (
    -- * Creating a Request
      exportBackupPlanTemplate
    , ExportBackupPlanTemplate
    -- * Request Lenses
    , ebptBackupPlanId

    -- * Destructuring the Response
    , exportBackupPlanTemplateResponse
    , ExportBackupPlanTemplateResponse
    -- * Response Lenses
    , ebptrsBackupPlanTemplateJSON
    , ebptrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'exportBackupPlanTemplate' smart constructor.
newtype ExportBackupPlanTemplate = ExportBackupPlanTemplate'
  { _ebptBackupPlanId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportBackupPlanTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebptBackupPlanId' - Uniquely identifies a backup plan.
exportBackupPlanTemplate
    :: Text -- ^ 'ebptBackupPlanId'
    -> ExportBackupPlanTemplate
exportBackupPlanTemplate pBackupPlanId_ =
  ExportBackupPlanTemplate' {_ebptBackupPlanId = pBackupPlanId_}


-- | Uniquely identifies a backup plan.
ebptBackupPlanId :: Lens' ExportBackupPlanTemplate Text
ebptBackupPlanId = lens _ebptBackupPlanId (\ s a -> s{_ebptBackupPlanId = a})

instance AWSRequest ExportBackupPlanTemplate where
        type Rs ExportBackupPlanTemplate =
             ExportBackupPlanTemplateResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 ExportBackupPlanTemplateResponse' <$>
                   (x .?> "BackupPlanTemplateJson") <*>
                     (pure (fromEnum s)))

instance Hashable ExportBackupPlanTemplate where

instance NFData ExportBackupPlanTemplate where

instance ToHeaders ExportBackupPlanTemplate where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ExportBackupPlanTemplate where
        toPath ExportBackupPlanTemplate'{..}
          = mconcat
              ["/backup/plans/", toBS _ebptBackupPlanId,
               "/toTemplate/"]

instance ToQuery ExportBackupPlanTemplate where
        toQuery = const mempty

-- | /See:/ 'exportBackupPlanTemplateResponse' smart constructor.
data ExportBackupPlanTemplateResponse = ExportBackupPlanTemplateResponse'
  { _ebptrsBackupPlanTemplateJSON :: !(Maybe Text)
  , _ebptrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportBackupPlanTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebptrsBackupPlanTemplateJSON' - The body of a backup plan template in JSON format.
--
-- * 'ebptrsResponseStatus' - -- | The response status code.
exportBackupPlanTemplateResponse
    :: Int -- ^ 'ebptrsResponseStatus'
    -> ExportBackupPlanTemplateResponse
exportBackupPlanTemplateResponse pResponseStatus_ =
  ExportBackupPlanTemplateResponse'
    { _ebptrsBackupPlanTemplateJSON = Nothing
    , _ebptrsResponseStatus = pResponseStatus_
    }


-- | The body of a backup plan template in JSON format.
ebptrsBackupPlanTemplateJSON :: Lens' ExportBackupPlanTemplateResponse (Maybe Text)
ebptrsBackupPlanTemplateJSON = lens _ebptrsBackupPlanTemplateJSON (\ s a -> s{_ebptrsBackupPlanTemplateJSON = a})

-- | -- | The response status code.
ebptrsResponseStatus :: Lens' ExportBackupPlanTemplateResponse Int
ebptrsResponseStatus = lens _ebptrsResponseStatus (\ s a -> s{_ebptrsResponseStatus = a})

instance NFData ExportBackupPlanTemplateResponse
         where
