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
-- Module      : Network.AWS.APIGatewayV2.DeleteIntegration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Integration.
--
--
module Network.AWS.APIGatewayV2.DeleteIntegration
    (
    -- * Creating a Request
      deleteIntegration
    , DeleteIntegration
    -- * Request Lenses
    , delAPIId
    , delIntegrationId

    -- * Destructuring the Response
    , deleteIntegrationResponse'
    , DeleteIntegrationResponse'
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteIntegration' smart constructor.
data DeleteIntegration = DeleteIntegration'
  { _delAPIId :: !Text
  , _delIntegrationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteIntegration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delAPIId' - The API identifier.
--
-- * 'delIntegrationId' - The integration ID.
deleteIntegration
    :: Text -- ^ 'delAPIId'
    -> Text -- ^ 'delIntegrationId'
    -> DeleteIntegration
deleteIntegration pAPIId_ pIntegrationId_ =
  DeleteIntegration' {_delAPIId = pAPIId_, _delIntegrationId = pIntegrationId_}


-- | The API identifier.
delAPIId :: Lens' DeleteIntegration Text
delAPIId = lens _delAPIId (\ s a -> s{_delAPIId = a})

-- | The integration ID.
delIntegrationId :: Lens' DeleteIntegration Text
delIntegrationId = lens _delIntegrationId (\ s a -> s{_delIntegrationId = a})

instance AWSRequest DeleteIntegration where
        type Rs DeleteIntegration =
             DeleteIntegrationResponse'
        request = delete apiGatewayV2
        response = receiveNull DeleteIntegrationResponse''

instance Hashable DeleteIntegration where

instance NFData DeleteIntegration where

instance ToHeaders DeleteIntegration where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteIntegration where
        toPath DeleteIntegration'{..}
          = mconcat
              ["/v2/apis/", toBS _delAPIId, "/integrations/",
               toBS _delIntegrationId]

instance ToQuery DeleteIntegration where
        toQuery = const mempty

-- | /See:/ 'deleteIntegrationResponse'' smart constructor.
data DeleteIntegrationResponse' =
  DeleteIntegrationResponse''
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteIntegrationResponse'' with the minimum fields required to make a request.
--
deleteIntegrationResponse'
    :: DeleteIntegrationResponse'
deleteIntegrationResponse' = DeleteIntegrationResponse''


instance NFData DeleteIntegrationResponse' where
