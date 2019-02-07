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
-- Module      : Network.AWS.APIGatewayV2.UpdateStage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Stage.
--
--
module Network.AWS.APIGatewayV2.UpdateStage
    (
    -- * Creating a Request
      updateStage
    , UpdateStage
    -- * Request Lenses
    , usDeploymentId
    , usRouteSettings
    , usAccessLogSettings
    , usClientCertificateId
    , usStageVariables
    , usDefaultRouteSettings
    , usDescription
    , usStageName
    , usAPIId

    -- * Destructuring the Response
    , updateStageResponse
    , UpdateStageResponse
    -- * Response Lenses
    , usrsDeploymentId
    , usrsRouteSettings
    , usrsAccessLogSettings
    , usrsClientCertificateId
    , usrsStageVariables
    , usrsCreatedDate
    , usrsDefaultRouteSettings
    , usrsStageName
    , usrsLastUpdatedDate
    , usrsDescription
    , usrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateStage' smart constructor.
data UpdateStage = UpdateStage'
  { _usDeploymentId :: !(Maybe Text)
  , _usRouteSettings :: !(Maybe (Map Text RouteSettings))
  , _usAccessLogSettings :: !(Maybe AccessLogSettings)
  , _usClientCertificateId :: !(Maybe Text)
  , _usStageVariables :: !(Maybe (Map Text Text))
  , _usDefaultRouteSettings :: !(Maybe RouteSettings)
  , _usDescription :: !(Maybe Text)
  , _usStageName :: !Text
  , _usAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateStage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usDeploymentId' - The deployment identifier for the API stage.
--
-- * 'usRouteSettings' - Route settings for the stage.
--
-- * 'usAccessLogSettings' - Settings for logging access in this stage.
--
-- * 'usClientCertificateId' - The identifier of a client certificate for a Stage.
--
-- * 'usStageVariables' - A map that defines the stage variables for a Stage. Variable names can have alphanumeric and underscore characters, and the values must match [A-Za-z0-9-._~:/?#&=,]+.
--
-- * 'usDefaultRouteSettings' - The default route settings for the stage.
--
-- * 'usDescription' - The description for the API stage.
--
-- * 'usStageName' - The stage name.
--
-- * 'usAPIId' - The API identifier.
updateStage
    :: Text -- ^ 'usStageName'
    -> Text -- ^ 'usAPIId'
    -> UpdateStage
updateStage pStageName_ pAPIId_ =
  UpdateStage'
    { _usDeploymentId = Nothing
    , _usRouteSettings = Nothing
    , _usAccessLogSettings = Nothing
    , _usClientCertificateId = Nothing
    , _usStageVariables = Nothing
    , _usDefaultRouteSettings = Nothing
    , _usDescription = Nothing
    , _usStageName = pStageName_
    , _usAPIId = pAPIId_
    }


-- | The deployment identifier for the API stage.
usDeploymentId :: Lens' UpdateStage (Maybe Text)
usDeploymentId = lens _usDeploymentId (\ s a -> s{_usDeploymentId = a})

-- | Route settings for the stage.
usRouteSettings :: Lens' UpdateStage (HashMap Text RouteSettings)
usRouteSettings = lens _usRouteSettings (\ s a -> s{_usRouteSettings = a}) . _Default . _Map

-- | Settings for logging access in this stage.
usAccessLogSettings :: Lens' UpdateStage (Maybe AccessLogSettings)
usAccessLogSettings = lens _usAccessLogSettings (\ s a -> s{_usAccessLogSettings = a})

-- | The identifier of a client certificate for a Stage.
usClientCertificateId :: Lens' UpdateStage (Maybe Text)
usClientCertificateId = lens _usClientCertificateId (\ s a -> s{_usClientCertificateId = a})

-- | A map that defines the stage variables for a Stage. Variable names can have alphanumeric and underscore characters, and the values must match [A-Za-z0-9-._~:/?#&=,]+.
usStageVariables :: Lens' UpdateStage (HashMap Text Text)
usStageVariables = lens _usStageVariables (\ s a -> s{_usStageVariables = a}) . _Default . _Map

-- | The default route settings for the stage.
usDefaultRouteSettings :: Lens' UpdateStage (Maybe RouteSettings)
usDefaultRouteSettings = lens _usDefaultRouteSettings (\ s a -> s{_usDefaultRouteSettings = a})

-- | The description for the API stage.
usDescription :: Lens' UpdateStage (Maybe Text)
usDescription = lens _usDescription (\ s a -> s{_usDescription = a})

-- | The stage name.
usStageName :: Lens' UpdateStage Text
usStageName = lens _usStageName (\ s a -> s{_usStageName = a})

-- | The API identifier.
usAPIId :: Lens' UpdateStage Text
usAPIId = lens _usAPIId (\ s a -> s{_usAPIId = a})

instance AWSRequest UpdateStage where
        type Rs UpdateStage = UpdateStageResponse
        request = patchJSON apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 UpdateStageResponse' <$>
                   (x .?> "deploymentId") <*>
                     (x .?> "routeSettings" .!@ mempty)
                     <*> (x .?> "accessLogSettings")
                     <*> (x .?> "clientCertificateId")
                     <*> (x .?> "stageVariables" .!@ mempty)
                     <*> (x .?> "createdDate")
                     <*> (x .?> "defaultRouteSettings")
                     <*> (x .?> "stageName")
                     <*> (x .?> "lastUpdatedDate")
                     <*> (x .?> "description")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateStage where

instance NFData UpdateStage where

instance ToHeaders UpdateStage where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateStage where
        toJSON UpdateStage'{..}
          = object
              (catMaybes
                 [("deploymentId" .=) <$> _usDeploymentId,
                  ("routeSettings" .=) <$> _usRouteSettings,
                  ("accessLogSettings" .=) <$> _usAccessLogSettings,
                  ("clientCertificateId" .=) <$>
                    _usClientCertificateId,
                  ("stageVariables" .=) <$> _usStageVariables,
                  ("defaultRouteSettings" .=) <$>
                    _usDefaultRouteSettings,
                  ("description" .=) <$> _usDescription])

instance ToPath UpdateStage where
        toPath UpdateStage'{..}
          = mconcat
              ["/v2/apis/", toBS _usAPIId, "/stages/",
               toBS _usStageName]

instance ToQuery UpdateStage where
        toQuery = const mempty

-- | /See:/ 'updateStageResponse' smart constructor.
data UpdateStageResponse = UpdateStageResponse'
  { _usrsDeploymentId :: !(Maybe Text)
  , _usrsRouteSettings :: !(Maybe (Map Text RouteSettings))
  , _usrsAccessLogSettings :: !(Maybe AccessLogSettings)
  , _usrsClientCertificateId :: !(Maybe Text)
  , _usrsStageVariables :: !(Maybe (Map Text Text))
  , _usrsCreatedDate :: !(Maybe POSIX)
  , _usrsDefaultRouteSettings :: !(Maybe RouteSettings)
  , _usrsStageName :: !(Maybe Text)
  , _usrsLastUpdatedDate :: !(Maybe POSIX)
  , _usrsDescription :: !(Maybe Text)
  , _usrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateStageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usrsDeploymentId' - The identifier of the Deployment that the Stage is associated with.
--
-- * 'usrsRouteSettings' - Route settings for the stage.
--
-- * 'usrsAccessLogSettings' - Settings for logging access in this stage.
--
-- * 'usrsClientCertificateId' - The identifier of a client certificate for a Stage.
--
-- * 'usrsStageVariables' - A map that defines the stage variables for a stage resource. Variable names can have alphanumeric and underscore characters, and the values must match [A-Za-z0-9-._~:/?#&=,]+.
--
-- * 'usrsCreatedDate' - The timestamp when the stage was created.
--
-- * 'usrsDefaultRouteSettings' - Default route settings for the stage.
--
-- * 'usrsStageName' - The name of the stage.
--
-- * 'usrsLastUpdatedDate' - The timestamp when the stage was last updated.
--
-- * 'usrsDescription' - The description of the stage.
--
-- * 'usrsResponseStatus' - -- | The response status code.
updateStageResponse
    :: Int -- ^ 'usrsResponseStatus'
    -> UpdateStageResponse
updateStageResponse pResponseStatus_ =
  UpdateStageResponse'
    { _usrsDeploymentId = Nothing
    , _usrsRouteSettings = Nothing
    , _usrsAccessLogSettings = Nothing
    , _usrsClientCertificateId = Nothing
    , _usrsStageVariables = Nothing
    , _usrsCreatedDate = Nothing
    , _usrsDefaultRouteSettings = Nothing
    , _usrsStageName = Nothing
    , _usrsLastUpdatedDate = Nothing
    , _usrsDescription = Nothing
    , _usrsResponseStatus = pResponseStatus_
    }


-- | The identifier of the Deployment that the Stage is associated with.
usrsDeploymentId :: Lens' UpdateStageResponse (Maybe Text)
usrsDeploymentId = lens _usrsDeploymentId (\ s a -> s{_usrsDeploymentId = a})

-- | Route settings for the stage.
usrsRouteSettings :: Lens' UpdateStageResponse (HashMap Text RouteSettings)
usrsRouteSettings = lens _usrsRouteSettings (\ s a -> s{_usrsRouteSettings = a}) . _Default . _Map

-- | Settings for logging access in this stage.
usrsAccessLogSettings :: Lens' UpdateStageResponse (Maybe AccessLogSettings)
usrsAccessLogSettings = lens _usrsAccessLogSettings (\ s a -> s{_usrsAccessLogSettings = a})

-- | The identifier of a client certificate for a Stage.
usrsClientCertificateId :: Lens' UpdateStageResponse (Maybe Text)
usrsClientCertificateId = lens _usrsClientCertificateId (\ s a -> s{_usrsClientCertificateId = a})

-- | A map that defines the stage variables for a stage resource. Variable names can have alphanumeric and underscore characters, and the values must match [A-Za-z0-9-._~:/?#&=,]+.
usrsStageVariables :: Lens' UpdateStageResponse (HashMap Text Text)
usrsStageVariables = lens _usrsStageVariables (\ s a -> s{_usrsStageVariables = a}) . _Default . _Map

-- | The timestamp when the stage was created.
usrsCreatedDate :: Lens' UpdateStageResponse (Maybe UTCTime)
usrsCreatedDate = lens _usrsCreatedDate (\ s a -> s{_usrsCreatedDate = a}) . mapping _Time

-- | Default route settings for the stage.
usrsDefaultRouteSettings :: Lens' UpdateStageResponse (Maybe RouteSettings)
usrsDefaultRouteSettings = lens _usrsDefaultRouteSettings (\ s a -> s{_usrsDefaultRouteSettings = a})

-- | The name of the stage.
usrsStageName :: Lens' UpdateStageResponse (Maybe Text)
usrsStageName = lens _usrsStageName (\ s a -> s{_usrsStageName = a})

-- | The timestamp when the stage was last updated.
usrsLastUpdatedDate :: Lens' UpdateStageResponse (Maybe UTCTime)
usrsLastUpdatedDate = lens _usrsLastUpdatedDate (\ s a -> s{_usrsLastUpdatedDate = a}) . mapping _Time

-- | The description of the stage.
usrsDescription :: Lens' UpdateStageResponse (Maybe Text)
usrsDescription = lens _usrsDescription (\ s a -> s{_usrsDescription = a})

-- | -- | The response status code.
usrsResponseStatus :: Lens' UpdateStageResponse Int
usrsResponseStatus = lens _usrsResponseStatus (\ s a -> s{_usrsResponseStatus = a})

instance NFData UpdateStageResponse where
