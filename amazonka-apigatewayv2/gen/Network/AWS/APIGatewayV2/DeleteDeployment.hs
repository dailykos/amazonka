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
-- Module      : Network.AWS.APIGatewayV2.DeleteDeployment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Deployment.
--
--
module Network.AWS.APIGatewayV2.DeleteDeployment
    (
    -- * Creating a Request
      deleteDeployment
    , DeleteDeployment
    -- * Request Lenses
    , ddAPIId
    , ddDeploymentId

    -- * Destructuring the Response
    , deleteDeploymentResponse
    , DeleteDeploymentResponse
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDeployment' smart constructor.
data DeleteDeployment = DeleteDeployment'
  { _ddAPIId :: !Text
  , _ddDeploymentId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDeployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddAPIId' - The API identifier.
--
-- * 'ddDeploymentId' - The deployment ID.
deleteDeployment
    :: Text -- ^ 'ddAPIId'
    -> Text -- ^ 'ddDeploymentId'
    -> DeleteDeployment
deleteDeployment pAPIId_ pDeploymentId_ =
  DeleteDeployment' {_ddAPIId = pAPIId_, _ddDeploymentId = pDeploymentId_}


-- | The API identifier.
ddAPIId :: Lens' DeleteDeployment Text
ddAPIId = lens _ddAPIId (\ s a -> s{_ddAPIId = a})

-- | The deployment ID.
ddDeploymentId :: Lens' DeleteDeployment Text
ddDeploymentId = lens _ddDeploymentId (\ s a -> s{_ddDeploymentId = a})

instance AWSRequest DeleteDeployment where
        type Rs DeleteDeployment = DeleteDeploymentResponse
        request = delete apiGatewayV2
        response = receiveNull DeleteDeploymentResponse'

instance Hashable DeleteDeployment where

instance NFData DeleteDeployment where

instance ToHeaders DeleteDeployment where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteDeployment where
        toPath DeleteDeployment'{..}
          = mconcat
              ["/v2/apis/", toBS _ddAPIId, "/deployments/",
               toBS _ddDeploymentId]

instance ToQuery DeleteDeployment where
        toQuery = const mempty

-- | /See:/ 'deleteDeploymentResponse' smart constructor.
data DeleteDeploymentResponse =
  DeleteDeploymentResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDeploymentResponse' with the minimum fields required to make a request.
--
deleteDeploymentResponse
    :: DeleteDeploymentResponse
deleteDeploymentResponse = DeleteDeploymentResponse'


instance NFData DeleteDeploymentResponse where
