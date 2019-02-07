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
-- Module      : Network.AWS.APIGatewayV2.UpdateDeployment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Deployment.
--
--
module Network.AWS.APIGatewayV2.UpdateDeployment
    (
    -- * Creating a Request
      updateDeployment
    , UpdateDeployment
    -- * Request Lenses
    , udDescription
    , udAPIId
    , udDeploymentId

    -- * Destructuring the Response
    , updateDeploymentResponse
    , UpdateDeploymentResponse
    -- * Response Lenses
    , udrsDeploymentId
    , udrsDeploymentStatusMessage
    , udrsCreatedDate
    , udrsDeploymentStatus
    , udrsDescription
    , udrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateDeployment' smart constructor.
data UpdateDeployment = UpdateDeployment'
  { _udDescription :: !(Maybe Text)
  , _udAPIId :: !Text
  , _udDeploymentId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDeployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udDescription' - The description for the deployment resource.
--
-- * 'udAPIId' - The API identifier.
--
-- * 'udDeploymentId' - The deployment ID.
updateDeployment
    :: Text -- ^ 'udAPIId'
    -> Text -- ^ 'udDeploymentId'
    -> UpdateDeployment
updateDeployment pAPIId_ pDeploymentId_ =
  UpdateDeployment'
    { _udDescription = Nothing
    , _udAPIId = pAPIId_
    , _udDeploymentId = pDeploymentId_
    }


-- | The description for the deployment resource.
udDescription :: Lens' UpdateDeployment (Maybe Text)
udDescription = lens _udDescription (\ s a -> s{_udDescription = a})

-- | The API identifier.
udAPIId :: Lens' UpdateDeployment Text
udAPIId = lens _udAPIId (\ s a -> s{_udAPIId = a})

-- | The deployment ID.
udDeploymentId :: Lens' UpdateDeployment Text
udDeploymentId = lens _udDeploymentId (\ s a -> s{_udDeploymentId = a})

instance AWSRequest UpdateDeployment where
        type Rs UpdateDeployment = UpdateDeploymentResponse
        request = patchJSON apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 UpdateDeploymentResponse' <$>
                   (x .?> "deploymentId") <*>
                     (x .?> "deploymentStatusMessage")
                     <*> (x .?> "createdDate")
                     <*> (x .?> "deploymentStatus")
                     <*> (x .?> "description")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateDeployment where

instance NFData UpdateDeployment where

instance ToHeaders UpdateDeployment where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateDeployment where
        toJSON UpdateDeployment'{..}
          = object
              (catMaybes [("description" .=) <$> _udDescription])

instance ToPath UpdateDeployment where
        toPath UpdateDeployment'{..}
          = mconcat
              ["/v2/apis/", toBS _udAPIId, "/deployments/",
               toBS _udDeploymentId]

instance ToQuery UpdateDeployment where
        toQuery = const mempty

-- | /See:/ 'updateDeploymentResponse' smart constructor.
data UpdateDeploymentResponse = UpdateDeploymentResponse'
  { _udrsDeploymentId :: !(Maybe Text)
  , _udrsDeploymentStatusMessage :: !(Maybe Text)
  , _udrsCreatedDate :: !(Maybe POSIX)
  , _udrsDeploymentStatus :: !(Maybe DeploymentStatus)
  , _udrsDescription :: !(Maybe Text)
  , _udrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDeploymentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udrsDeploymentId' - The identifier for the deployment.
--
-- * 'udrsDeploymentStatusMessage' - May contain additional feedback on the status of an API deployment.
--
-- * 'udrsCreatedDate' - The date and time when the Deployment resource was created.
--
-- * 'udrsDeploymentStatus' - The status of the deployment: PENDING, FAILED, or SUCCEEDED.
--
-- * 'udrsDescription' - The description for the deployment.
--
-- * 'udrsResponseStatus' - -- | The response status code.
updateDeploymentResponse
    :: Int -- ^ 'udrsResponseStatus'
    -> UpdateDeploymentResponse
updateDeploymentResponse pResponseStatus_ =
  UpdateDeploymentResponse'
    { _udrsDeploymentId = Nothing
    , _udrsDeploymentStatusMessage = Nothing
    , _udrsCreatedDate = Nothing
    , _udrsDeploymentStatus = Nothing
    , _udrsDescription = Nothing
    , _udrsResponseStatus = pResponseStatus_
    }


-- | The identifier for the deployment.
udrsDeploymentId :: Lens' UpdateDeploymentResponse (Maybe Text)
udrsDeploymentId = lens _udrsDeploymentId (\ s a -> s{_udrsDeploymentId = a})

-- | May contain additional feedback on the status of an API deployment.
udrsDeploymentStatusMessage :: Lens' UpdateDeploymentResponse (Maybe Text)
udrsDeploymentStatusMessage = lens _udrsDeploymentStatusMessage (\ s a -> s{_udrsDeploymentStatusMessage = a})

-- | The date and time when the Deployment resource was created.
udrsCreatedDate :: Lens' UpdateDeploymentResponse (Maybe UTCTime)
udrsCreatedDate = lens _udrsCreatedDate (\ s a -> s{_udrsCreatedDate = a}) . mapping _Time

-- | The status of the deployment: PENDING, FAILED, or SUCCEEDED.
udrsDeploymentStatus :: Lens' UpdateDeploymentResponse (Maybe DeploymentStatus)
udrsDeploymentStatus = lens _udrsDeploymentStatus (\ s a -> s{_udrsDeploymentStatus = a})

-- | The description for the deployment.
udrsDescription :: Lens' UpdateDeploymentResponse (Maybe Text)
udrsDescription = lens _udrsDescription (\ s a -> s{_udrsDescription = a})

-- | -- | The response status code.
udrsResponseStatus :: Lens' UpdateDeploymentResponse Int
udrsResponseStatus = lens _udrsResponseStatus (\ s a -> s{_udrsResponseStatus = a})

instance NFData UpdateDeploymentResponse where
