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
-- Module      : Network.AWS.APIGatewayV2.CreateStage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Stage for an API.
--
--
module Network.AWS.APIGatewayV2.CreateStage
    (
    -- * Creating a Request
      createStage
    , CreateStage
    -- * Request Lenses
    , csDeploymentId
    , csRouteSettings
    , csAccessLogSettings
    , csClientCertificateId
    , csStageVariables
    , csDefaultRouteSettings
    , csDescription
    , csAPIId
    , csStageName

    -- * Destructuring the Response
    , createStageResponse
    , CreateStageResponse
    -- * Response Lenses
    , csrsDeploymentId
    , csrsRouteSettings
    , csrsAccessLogSettings
    , csrsClientCertificateId
    , csrsStageVariables
    , csrsCreatedDate
    , csrsDefaultRouteSettings
    , csrsStageName
    , csrsLastUpdatedDate
    , csrsDescription
    , csrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createStage' smart constructor.
data CreateStage = CreateStage'
  { _csDeploymentId :: !(Maybe Text)
  , _csRouteSettings :: !(Maybe (Map Text RouteSettings))
  , _csAccessLogSettings :: !(Maybe AccessLogSettings)
  , _csClientCertificateId :: !(Maybe Text)
  , _csStageVariables :: !(Maybe (Map Text Text))
  , _csDefaultRouteSettings :: !(Maybe RouteSettings)
  , _csDescription :: !(Maybe Text)
  , _csAPIId :: !Text
  , _csStageName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csDeploymentId' - The deployment identifier of the API stage.
--
-- * 'csRouteSettings' - Route settings for the stage.
--
-- * 'csAccessLogSettings' - Settings for logging access in this stage.
--
-- * 'csClientCertificateId' - The identifier of a client certificate for a Stage.
--
-- * 'csStageVariables' - A map that defines the stage variables for a Stage. Variable names can have alphanumeric and underscore characters, and the values must match [A-Za-z0-9-._~:/?#&=,]+.
--
-- * 'csDefaultRouteSettings' - The default route settings for the stage.
--
-- * 'csDescription' - The description for the API stage.
--
-- * 'csAPIId' - The API identifier.
--
-- * 'csStageName' - The name of the stage.
createStage
    :: Text -- ^ 'csAPIId'
    -> Text -- ^ 'csStageName'
    -> CreateStage
createStage pAPIId_ pStageName_ =
  CreateStage'
    { _csDeploymentId = Nothing
    , _csRouteSettings = Nothing
    , _csAccessLogSettings = Nothing
    , _csClientCertificateId = Nothing
    , _csStageVariables = Nothing
    , _csDefaultRouteSettings = Nothing
    , _csDescription = Nothing
    , _csAPIId = pAPIId_
    , _csStageName = pStageName_
    }


-- | The deployment identifier of the API stage.
csDeploymentId :: Lens' CreateStage (Maybe Text)
csDeploymentId = lens _csDeploymentId (\ s a -> s{_csDeploymentId = a})

-- | Route settings for the stage.
csRouteSettings :: Lens' CreateStage (HashMap Text RouteSettings)
csRouteSettings = lens _csRouteSettings (\ s a -> s{_csRouteSettings = a}) . _Default . _Map

-- | Settings for logging access in this stage.
csAccessLogSettings :: Lens' CreateStage (Maybe AccessLogSettings)
csAccessLogSettings = lens _csAccessLogSettings (\ s a -> s{_csAccessLogSettings = a})

-- | The identifier of a client certificate for a Stage.
csClientCertificateId :: Lens' CreateStage (Maybe Text)
csClientCertificateId = lens _csClientCertificateId (\ s a -> s{_csClientCertificateId = a})

-- | A map that defines the stage variables for a Stage. Variable names can have alphanumeric and underscore characters, and the values must match [A-Za-z0-9-._~:/?#&=,]+.
csStageVariables :: Lens' CreateStage (HashMap Text Text)
csStageVariables = lens _csStageVariables (\ s a -> s{_csStageVariables = a}) . _Default . _Map

-- | The default route settings for the stage.
csDefaultRouteSettings :: Lens' CreateStage (Maybe RouteSettings)
csDefaultRouteSettings = lens _csDefaultRouteSettings (\ s a -> s{_csDefaultRouteSettings = a})

-- | The description for the API stage.
csDescription :: Lens' CreateStage (Maybe Text)
csDescription = lens _csDescription (\ s a -> s{_csDescription = a})

-- | The API identifier.
csAPIId :: Lens' CreateStage Text
csAPIId = lens _csAPIId (\ s a -> s{_csAPIId = a})

-- | The name of the stage.
csStageName :: Lens' CreateStage Text
csStageName = lens _csStageName (\ s a -> s{_csStageName = a})

instance AWSRequest CreateStage where
        type Rs CreateStage = CreateStageResponse
        request = postJSON apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 CreateStageResponse' <$>
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

instance Hashable CreateStage where

instance NFData CreateStage where

instance ToHeaders CreateStage where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateStage where
        toJSON CreateStage'{..}
          = object
              (catMaybes
                 [("deploymentId" .=) <$> _csDeploymentId,
                  ("routeSettings" .=) <$> _csRouteSettings,
                  ("accessLogSettings" .=) <$> _csAccessLogSettings,
                  ("clientCertificateId" .=) <$>
                    _csClientCertificateId,
                  ("stageVariables" .=) <$> _csStageVariables,
                  ("defaultRouteSettings" .=) <$>
                    _csDefaultRouteSettings,
                  ("description" .=) <$> _csDescription,
                  Just ("stageName" .= _csStageName)])

instance ToPath CreateStage where
        toPath CreateStage'{..}
          = mconcat ["/v2/apis/", toBS _csAPIId, "/stages"]

instance ToQuery CreateStage where
        toQuery = const mempty

-- | /See:/ 'createStageResponse' smart constructor.
data CreateStageResponse = CreateStageResponse'
  { _csrsDeploymentId :: !(Maybe Text)
  , _csrsRouteSettings :: !(Maybe (Map Text RouteSettings))
  , _csrsAccessLogSettings :: !(Maybe AccessLogSettings)
  , _csrsClientCertificateId :: !(Maybe Text)
  , _csrsStageVariables :: !(Maybe (Map Text Text))
  , _csrsCreatedDate :: !(Maybe POSIX)
  , _csrsDefaultRouteSettings :: !(Maybe RouteSettings)
  , _csrsStageName :: !(Maybe Text)
  , _csrsLastUpdatedDate :: !(Maybe POSIX)
  , _csrsDescription :: !(Maybe Text)
  , _csrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsDeploymentId' - The identifier of the Deployment that the Stage is associated with.
--
-- * 'csrsRouteSettings' - Route settings for the stage.
--
-- * 'csrsAccessLogSettings' - Settings for logging access in this stage.
--
-- * 'csrsClientCertificateId' - The identifier of a client certificate for a Stage.
--
-- * 'csrsStageVariables' - A map that defines the stage variables for a stage resource. Variable names can have alphanumeric and underscore characters, and the values must match [A-Za-z0-9-._~:/?#&=,]+.
--
-- * 'csrsCreatedDate' - The timestamp when the stage was created.
--
-- * 'csrsDefaultRouteSettings' - Default route settings for the stage.
--
-- * 'csrsStageName' - The name of the stage.
--
-- * 'csrsLastUpdatedDate' - The timestamp when the stage was last updated.
--
-- * 'csrsDescription' - The description of the stage.
--
-- * 'csrsResponseStatus' - -- | The response status code.
createStageResponse
    :: Int -- ^ 'csrsResponseStatus'
    -> CreateStageResponse
createStageResponse pResponseStatus_ =
  CreateStageResponse'
    { _csrsDeploymentId = Nothing
    , _csrsRouteSettings = Nothing
    , _csrsAccessLogSettings = Nothing
    , _csrsClientCertificateId = Nothing
    , _csrsStageVariables = Nothing
    , _csrsCreatedDate = Nothing
    , _csrsDefaultRouteSettings = Nothing
    , _csrsStageName = Nothing
    , _csrsLastUpdatedDate = Nothing
    , _csrsDescription = Nothing
    , _csrsResponseStatus = pResponseStatus_
    }


-- | The identifier of the Deployment that the Stage is associated with.
csrsDeploymentId :: Lens' CreateStageResponse (Maybe Text)
csrsDeploymentId = lens _csrsDeploymentId (\ s a -> s{_csrsDeploymentId = a})

-- | Route settings for the stage.
csrsRouteSettings :: Lens' CreateStageResponse (HashMap Text RouteSettings)
csrsRouteSettings = lens _csrsRouteSettings (\ s a -> s{_csrsRouteSettings = a}) . _Default . _Map

-- | Settings for logging access in this stage.
csrsAccessLogSettings :: Lens' CreateStageResponse (Maybe AccessLogSettings)
csrsAccessLogSettings = lens _csrsAccessLogSettings (\ s a -> s{_csrsAccessLogSettings = a})

-- | The identifier of a client certificate for a Stage.
csrsClientCertificateId :: Lens' CreateStageResponse (Maybe Text)
csrsClientCertificateId = lens _csrsClientCertificateId (\ s a -> s{_csrsClientCertificateId = a})

-- | A map that defines the stage variables for a stage resource. Variable names can have alphanumeric and underscore characters, and the values must match [A-Za-z0-9-._~:/?#&=,]+.
csrsStageVariables :: Lens' CreateStageResponse (HashMap Text Text)
csrsStageVariables = lens _csrsStageVariables (\ s a -> s{_csrsStageVariables = a}) . _Default . _Map

-- | The timestamp when the stage was created.
csrsCreatedDate :: Lens' CreateStageResponse (Maybe UTCTime)
csrsCreatedDate = lens _csrsCreatedDate (\ s a -> s{_csrsCreatedDate = a}) . mapping _Time

-- | Default route settings for the stage.
csrsDefaultRouteSettings :: Lens' CreateStageResponse (Maybe RouteSettings)
csrsDefaultRouteSettings = lens _csrsDefaultRouteSettings (\ s a -> s{_csrsDefaultRouteSettings = a})

-- | The name of the stage.
csrsStageName :: Lens' CreateStageResponse (Maybe Text)
csrsStageName = lens _csrsStageName (\ s a -> s{_csrsStageName = a})

-- | The timestamp when the stage was last updated.
csrsLastUpdatedDate :: Lens' CreateStageResponse (Maybe UTCTime)
csrsLastUpdatedDate = lens _csrsLastUpdatedDate (\ s a -> s{_csrsLastUpdatedDate = a}) . mapping _Time

-- | The description of the stage.
csrsDescription :: Lens' CreateStageResponse (Maybe Text)
csrsDescription = lens _csrsDescription (\ s a -> s{_csrsDescription = a})

-- | -- | The response status code.
csrsResponseStatus :: Lens' CreateStageResponse Int
csrsResponseStatus = lens _csrsResponseStatus (\ s a -> s{_csrsResponseStatus = a})

instance NFData CreateStageResponse where
