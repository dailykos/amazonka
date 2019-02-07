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
-- Module      : Network.AWS.APIGatewayV2.GetDeployment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a Deployment.
--
--
module Network.AWS.APIGatewayV2.GetDeployment
    (
    -- * Creating a Request
      getDeployment
    , GetDeployment
    -- * Request Lenses
    , gAPIId
    , gDeploymentId

    -- * Destructuring the Response
    , getDeploymentResponse
    , GetDeploymentResponse
    -- * Response Lenses
    , gdrsDeploymentId
    , gdrsDeploymentStatusMessage
    , gdrsCreatedDate
    , gdrsDeploymentStatus
    , gdrsDescription
    , gdrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDeployment' smart constructor.
data GetDeployment = GetDeployment'
  { _gAPIId :: !Text
  , _gDeploymentId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDeployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gAPIId' - The API identifier.
--
-- * 'gDeploymentId' - The deployment ID.
getDeployment
    :: Text -- ^ 'gAPIId'
    -> Text -- ^ 'gDeploymentId'
    -> GetDeployment
getDeployment pAPIId_ pDeploymentId_ =
  GetDeployment' {_gAPIId = pAPIId_, _gDeploymentId = pDeploymentId_}


-- | The API identifier.
gAPIId :: Lens' GetDeployment Text
gAPIId = lens _gAPIId (\ s a -> s{_gAPIId = a})

-- | The deployment ID.
gDeploymentId :: Lens' GetDeployment Text
gDeploymentId = lens _gDeploymentId (\ s a -> s{_gDeploymentId = a})

instance AWSRequest GetDeployment where
        type Rs GetDeployment = GetDeploymentResponse
        request = get apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 GetDeploymentResponse' <$>
                   (x .?> "deploymentId") <*>
                     (x .?> "deploymentStatusMessage")
                     <*> (x .?> "createdDate")
                     <*> (x .?> "deploymentStatus")
                     <*> (x .?> "description")
                     <*> (pure (fromEnum s)))

instance Hashable GetDeployment where

instance NFData GetDeployment where

instance ToHeaders GetDeployment where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetDeployment where
        toPath GetDeployment'{..}
          = mconcat
              ["/v2/apis/", toBS _gAPIId, "/deployments/",
               toBS _gDeploymentId]

instance ToQuery GetDeployment where
        toQuery = const mempty

-- | /See:/ 'getDeploymentResponse' smart constructor.
data GetDeploymentResponse = GetDeploymentResponse'
  { _gdrsDeploymentId :: !(Maybe Text)
  , _gdrsDeploymentStatusMessage :: !(Maybe Text)
  , _gdrsCreatedDate :: !(Maybe POSIX)
  , _gdrsDeploymentStatus :: !(Maybe DeploymentStatus)
  , _gdrsDescription :: !(Maybe Text)
  , _gdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDeploymentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrsDeploymentId' - The identifier for the deployment.
--
-- * 'gdrsDeploymentStatusMessage' - May contain additional feedback on the status of an API deployment.
--
-- * 'gdrsCreatedDate' - The date and time when the Deployment resource was created.
--
-- * 'gdrsDeploymentStatus' - The status of the deployment: PENDING, FAILED, or SUCCEEDED.
--
-- * 'gdrsDescription' - The description for the deployment.
--
-- * 'gdrsResponseStatus' - -- | The response status code.
getDeploymentResponse
    :: Int -- ^ 'gdrsResponseStatus'
    -> GetDeploymentResponse
getDeploymentResponse pResponseStatus_ =
  GetDeploymentResponse'
    { _gdrsDeploymentId = Nothing
    , _gdrsDeploymentStatusMessage = Nothing
    , _gdrsCreatedDate = Nothing
    , _gdrsDeploymentStatus = Nothing
    , _gdrsDescription = Nothing
    , _gdrsResponseStatus = pResponseStatus_
    }


-- | The identifier for the deployment.
gdrsDeploymentId :: Lens' GetDeploymentResponse (Maybe Text)
gdrsDeploymentId = lens _gdrsDeploymentId (\ s a -> s{_gdrsDeploymentId = a})

-- | May contain additional feedback on the status of an API deployment.
gdrsDeploymentStatusMessage :: Lens' GetDeploymentResponse (Maybe Text)
gdrsDeploymentStatusMessage = lens _gdrsDeploymentStatusMessage (\ s a -> s{_gdrsDeploymentStatusMessage = a})

-- | The date and time when the Deployment resource was created.
gdrsCreatedDate :: Lens' GetDeploymentResponse (Maybe UTCTime)
gdrsCreatedDate = lens _gdrsCreatedDate (\ s a -> s{_gdrsCreatedDate = a}) . mapping _Time

-- | The status of the deployment: PENDING, FAILED, or SUCCEEDED.
gdrsDeploymentStatus :: Lens' GetDeploymentResponse (Maybe DeploymentStatus)
gdrsDeploymentStatus = lens _gdrsDeploymentStatus (\ s a -> s{_gdrsDeploymentStatus = a})

-- | The description for the deployment.
gdrsDescription :: Lens' GetDeploymentResponse (Maybe Text)
gdrsDescription = lens _gdrsDescription (\ s a -> s{_gdrsDescription = a})

-- | -- | The response status code.
gdrsResponseStatus :: Lens' GetDeploymentResponse Int
gdrsResponseStatus = lens _gdrsResponseStatus (\ s a -> s{_gdrsResponseStatus = a})

instance NFData GetDeploymentResponse where
