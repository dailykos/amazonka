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
-- Module      : Network.AWS.APIGatewayV2.CreateDeployment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Deployment for an API.
--
--
module Network.AWS.APIGatewayV2.CreateDeployment
    (
    -- * Creating a Request
      createDeployment
    , CreateDeployment
    -- * Request Lenses
    , cdStageName
    , cdDescription
    , cdAPIId

    -- * Destructuring the Response
    , createDeploymentResponse
    , CreateDeploymentResponse
    -- * Response Lenses
    , cdrsDeploymentId
    , cdrsDeploymentStatusMessage
    , cdrsCreatedDate
    , cdrsDeploymentStatus
    , cdrsDescription
    , cdrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
  { _cdStageName :: !(Maybe Text)
  , _cdDescription :: !(Maybe Text)
  , _cdAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDeployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdStageName' - The name of the Stage resource for the Deployment resource to create.
--
-- * 'cdDescription' - The description for the deployment resource.
--
-- * 'cdAPIId' - The API identifier.
createDeployment
    :: Text -- ^ 'cdAPIId'
    -> CreateDeployment
createDeployment pAPIId_ =
  CreateDeployment'
    {_cdStageName = Nothing, _cdDescription = Nothing, _cdAPIId = pAPIId_}


-- | The name of the Stage resource for the Deployment resource to create.
cdStageName :: Lens' CreateDeployment (Maybe Text)
cdStageName = lens _cdStageName (\ s a -> s{_cdStageName = a})

-- | The description for the deployment resource.
cdDescription :: Lens' CreateDeployment (Maybe Text)
cdDescription = lens _cdDescription (\ s a -> s{_cdDescription = a})

-- | The API identifier.
cdAPIId :: Lens' CreateDeployment Text
cdAPIId = lens _cdAPIId (\ s a -> s{_cdAPIId = a})

instance AWSRequest CreateDeployment where
        type Rs CreateDeployment = CreateDeploymentResponse
        request = postJSON apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 CreateDeploymentResponse' <$>
                   (x .?> "deploymentId") <*>
                     (x .?> "deploymentStatusMessage")
                     <*> (x .?> "createdDate")
                     <*> (x .?> "deploymentStatus")
                     <*> (x .?> "description")
                     <*> (pure (fromEnum s)))

instance Hashable CreateDeployment where

instance NFData CreateDeployment where

instance ToHeaders CreateDeployment where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDeployment where
        toJSON CreateDeployment'{..}
          = object
              (catMaybes
                 [("stageName" .=) <$> _cdStageName,
                  ("description" .=) <$> _cdDescription])

instance ToPath CreateDeployment where
        toPath CreateDeployment'{..}
          = mconcat
              ["/v2/apis/", toBS _cdAPIId, "/deployments"]

instance ToQuery CreateDeployment where
        toQuery = const mempty

-- | /See:/ 'createDeploymentResponse' smart constructor.
data CreateDeploymentResponse = CreateDeploymentResponse'
  { _cdrsDeploymentId :: !(Maybe Text)
  , _cdrsDeploymentStatusMessage :: !(Maybe Text)
  , _cdrsCreatedDate :: !(Maybe POSIX)
  , _cdrsDeploymentStatus :: !(Maybe DeploymentStatus)
  , _cdrsDescription :: !(Maybe Text)
  , _cdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDeploymentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdrsDeploymentId' - The identifier for the deployment.
--
-- * 'cdrsDeploymentStatusMessage' - May contain additional feedback on the status of an API deployment.
--
-- * 'cdrsCreatedDate' - The date and time when the Deployment resource was created.
--
-- * 'cdrsDeploymentStatus' - The status of the deployment: PENDING, FAILED, or SUCCEEDED.
--
-- * 'cdrsDescription' - The description for the deployment.
--
-- * 'cdrsResponseStatus' - -- | The response status code.
createDeploymentResponse
    :: Int -- ^ 'cdrsResponseStatus'
    -> CreateDeploymentResponse
createDeploymentResponse pResponseStatus_ =
  CreateDeploymentResponse'
    { _cdrsDeploymentId = Nothing
    , _cdrsDeploymentStatusMessage = Nothing
    , _cdrsCreatedDate = Nothing
    , _cdrsDeploymentStatus = Nothing
    , _cdrsDescription = Nothing
    , _cdrsResponseStatus = pResponseStatus_
    }


-- | The identifier for the deployment.
cdrsDeploymentId :: Lens' CreateDeploymentResponse (Maybe Text)
cdrsDeploymentId = lens _cdrsDeploymentId (\ s a -> s{_cdrsDeploymentId = a})

-- | May contain additional feedback on the status of an API deployment.
cdrsDeploymentStatusMessage :: Lens' CreateDeploymentResponse (Maybe Text)
cdrsDeploymentStatusMessage = lens _cdrsDeploymentStatusMessage (\ s a -> s{_cdrsDeploymentStatusMessage = a})

-- | The date and time when the Deployment resource was created.
cdrsCreatedDate :: Lens' CreateDeploymentResponse (Maybe UTCTime)
cdrsCreatedDate = lens _cdrsCreatedDate (\ s a -> s{_cdrsCreatedDate = a}) . mapping _Time

-- | The status of the deployment: PENDING, FAILED, or SUCCEEDED.
cdrsDeploymentStatus :: Lens' CreateDeploymentResponse (Maybe DeploymentStatus)
cdrsDeploymentStatus = lens _cdrsDeploymentStatus (\ s a -> s{_cdrsDeploymentStatus = a})

-- | The description for the deployment.
cdrsDescription :: Lens' CreateDeploymentResponse (Maybe Text)
cdrsDescription = lens _cdrsDescription (\ s a -> s{_cdrsDescription = a})

-- | -- | The response status code.
cdrsResponseStatus :: Lens' CreateDeploymentResponse Int
cdrsResponseStatus = lens _cdrsResponseStatus (\ s a -> s{_cdrsResponseStatus = a})

instance NFData CreateDeploymentResponse where
