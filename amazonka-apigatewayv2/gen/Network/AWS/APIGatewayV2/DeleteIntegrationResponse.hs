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
-- Module      : Network.AWS.APIGatewayV2.DeleteIntegrationResponse
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an IntegrationResponses.
--
--
module Network.AWS.APIGatewayV2.DeleteIntegrationResponse
    (
    -- * Creating a Request
      deleteIntegrationResponse
    , DeleteIntegrationResponse
    -- * Request Lenses
    , diAPIId
    , diIntegrationResponseId
    , diIntegrationId

    -- * Destructuring the Response
    , deleteIntegrationResponseResponse
    , DeleteIntegrationResponseResponse
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteIntegrationResponse' smart constructor.
data DeleteIntegrationResponse = DeleteIntegrationResponse'
  { _diAPIId :: !Text
  , _diIntegrationResponseId :: !Text
  , _diIntegrationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteIntegrationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diAPIId' - The API identifier.
--
-- * 'diIntegrationResponseId' - The integration response ID.
--
-- * 'diIntegrationId' - The integration ID.
deleteIntegrationResponse
    :: Text -- ^ 'diAPIId'
    -> Text -- ^ 'diIntegrationResponseId'
    -> Text -- ^ 'diIntegrationId'
    -> DeleteIntegrationResponse
deleteIntegrationResponse pAPIId_ pIntegrationResponseId_ pIntegrationId_ =
  DeleteIntegrationResponse'
    { _diAPIId = pAPIId_
    , _diIntegrationResponseId = pIntegrationResponseId_
    , _diIntegrationId = pIntegrationId_
    }


-- | The API identifier.
diAPIId :: Lens' DeleteIntegrationResponse Text
diAPIId = lens _diAPIId (\ s a -> s{_diAPIId = a})

-- | The integration response ID.
diIntegrationResponseId :: Lens' DeleteIntegrationResponse Text
diIntegrationResponseId = lens _diIntegrationResponseId (\ s a -> s{_diIntegrationResponseId = a})

-- | The integration ID.
diIntegrationId :: Lens' DeleteIntegrationResponse Text
diIntegrationId = lens _diIntegrationId (\ s a -> s{_diIntegrationId = a})

instance AWSRequest DeleteIntegrationResponse where
        type Rs DeleteIntegrationResponse =
             DeleteIntegrationResponseResponse
        request = delete apiGatewayV2
        response
          = receiveNull DeleteIntegrationResponseResponse'

instance Hashable DeleteIntegrationResponse where

instance NFData DeleteIntegrationResponse where

instance ToHeaders DeleteIntegrationResponse where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteIntegrationResponse where
        toPath DeleteIntegrationResponse'{..}
          = mconcat
              ["/v2/apis/", toBS _diAPIId, "/integrations/",
               toBS _diIntegrationId, "/integrationresponses/",
               toBS _diIntegrationResponseId]

instance ToQuery DeleteIntegrationResponse where
        toQuery = const mempty

-- | /See:/ 'deleteIntegrationResponseResponse' smart constructor.
data DeleteIntegrationResponseResponse =
  DeleteIntegrationResponseResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteIntegrationResponseResponse' with the minimum fields required to make a request.
--
deleteIntegrationResponseResponse
    :: DeleteIntegrationResponseResponse
deleteIntegrationResponseResponse = DeleteIntegrationResponseResponse'


instance NFData DeleteIntegrationResponseResponse
         where
