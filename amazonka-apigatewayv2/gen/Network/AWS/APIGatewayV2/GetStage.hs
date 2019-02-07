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
-- Module      : Network.AWS.APIGatewayV2.GetStage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a Stage.
--
--
module Network.AWS.APIGatewayV2.GetStage
    (
    -- * Creating a Request
      getStage
    , GetStage
    -- * Request Lenses
    , gssStageName
    , gssAPIId

    -- * Destructuring the Response
    , getStageResponse
    , GetStageResponse
    -- * Response Lenses
    , gssrsDeploymentId
    , gssrsRouteSettings
    , gssrsAccessLogSettings
    , gssrsClientCertificateId
    , gssrsStageVariables
    , gssrsCreatedDate
    , gssrsDefaultRouteSettings
    , gssrsStageName
    , gssrsLastUpdatedDate
    , gssrsDescription
    , gssrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getStage' smart constructor.
data GetStage = GetStage'
  { _gssStageName :: !Text
  , _gssAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetStage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gssStageName' - The stage name.
--
-- * 'gssAPIId' - The API identifier.
getStage
    :: Text -- ^ 'gssStageName'
    -> Text -- ^ 'gssAPIId'
    -> GetStage
getStage pStageName_ pAPIId_ =
  GetStage' {_gssStageName = pStageName_, _gssAPIId = pAPIId_}


-- | The stage name.
gssStageName :: Lens' GetStage Text
gssStageName = lens _gssStageName (\ s a -> s{_gssStageName = a})

-- | The API identifier.
gssAPIId :: Lens' GetStage Text
gssAPIId = lens _gssAPIId (\ s a -> s{_gssAPIId = a})

instance AWSRequest GetStage where
        type Rs GetStage = GetStageResponse
        request = get apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 GetStageResponse' <$>
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

instance Hashable GetStage where

instance NFData GetStage where

instance ToHeaders GetStage where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetStage where
        toPath GetStage'{..}
          = mconcat
              ["/v2/apis/", toBS _gssAPIId, "/stages/",
               toBS _gssStageName]

instance ToQuery GetStage where
        toQuery = const mempty

-- | /See:/ 'getStageResponse' smart constructor.
data GetStageResponse = GetStageResponse'
  { _gssrsDeploymentId :: !(Maybe Text)
  , _gssrsRouteSettings :: !(Maybe (Map Text RouteSettings))
  , _gssrsAccessLogSettings :: !(Maybe AccessLogSettings)
  , _gssrsClientCertificateId :: !(Maybe Text)
  , _gssrsStageVariables :: !(Maybe (Map Text Text))
  , _gssrsCreatedDate :: !(Maybe POSIX)
  , _gssrsDefaultRouteSettings :: !(Maybe RouteSettings)
  , _gssrsStageName :: !(Maybe Text)
  , _gssrsLastUpdatedDate :: !(Maybe POSIX)
  , _gssrsDescription :: !(Maybe Text)
  , _gssrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetStageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gssrsDeploymentId' - The identifier of the Deployment that the Stage is associated with.
--
-- * 'gssrsRouteSettings' - Route settings for the stage.
--
-- * 'gssrsAccessLogSettings' - Settings for logging access in this stage.
--
-- * 'gssrsClientCertificateId' - The identifier of a client certificate for a Stage.
--
-- * 'gssrsStageVariables' - A map that defines the stage variables for a stage resource. Variable names can have alphanumeric and underscore characters, and the values must match [A-Za-z0-9-._~:/?#&=,]+.
--
-- * 'gssrsCreatedDate' - The timestamp when the stage was created.
--
-- * 'gssrsDefaultRouteSettings' - Default route settings for the stage.
--
-- * 'gssrsStageName' - The name of the stage.
--
-- * 'gssrsLastUpdatedDate' - The timestamp when the stage was last updated.
--
-- * 'gssrsDescription' - The description of the stage.
--
-- * 'gssrsResponseStatus' - -- | The response status code.
getStageResponse
    :: Int -- ^ 'gssrsResponseStatus'
    -> GetStageResponse
getStageResponse pResponseStatus_ =
  GetStageResponse'
    { _gssrsDeploymentId = Nothing
    , _gssrsRouteSettings = Nothing
    , _gssrsAccessLogSettings = Nothing
    , _gssrsClientCertificateId = Nothing
    , _gssrsStageVariables = Nothing
    , _gssrsCreatedDate = Nothing
    , _gssrsDefaultRouteSettings = Nothing
    , _gssrsStageName = Nothing
    , _gssrsLastUpdatedDate = Nothing
    , _gssrsDescription = Nothing
    , _gssrsResponseStatus = pResponseStatus_
    }


-- | The identifier of the Deployment that the Stage is associated with.
gssrsDeploymentId :: Lens' GetStageResponse (Maybe Text)
gssrsDeploymentId = lens _gssrsDeploymentId (\ s a -> s{_gssrsDeploymentId = a})

-- | Route settings for the stage.
gssrsRouteSettings :: Lens' GetStageResponse (HashMap Text RouteSettings)
gssrsRouteSettings = lens _gssrsRouteSettings (\ s a -> s{_gssrsRouteSettings = a}) . _Default . _Map

-- | Settings for logging access in this stage.
gssrsAccessLogSettings :: Lens' GetStageResponse (Maybe AccessLogSettings)
gssrsAccessLogSettings = lens _gssrsAccessLogSettings (\ s a -> s{_gssrsAccessLogSettings = a})

-- | The identifier of a client certificate for a Stage.
gssrsClientCertificateId :: Lens' GetStageResponse (Maybe Text)
gssrsClientCertificateId = lens _gssrsClientCertificateId (\ s a -> s{_gssrsClientCertificateId = a})

-- | A map that defines the stage variables for a stage resource. Variable names can have alphanumeric and underscore characters, and the values must match [A-Za-z0-9-._~:/?#&=,]+.
gssrsStageVariables :: Lens' GetStageResponse (HashMap Text Text)
gssrsStageVariables = lens _gssrsStageVariables (\ s a -> s{_gssrsStageVariables = a}) . _Default . _Map

-- | The timestamp when the stage was created.
gssrsCreatedDate :: Lens' GetStageResponse (Maybe UTCTime)
gssrsCreatedDate = lens _gssrsCreatedDate (\ s a -> s{_gssrsCreatedDate = a}) . mapping _Time

-- | Default route settings for the stage.
gssrsDefaultRouteSettings :: Lens' GetStageResponse (Maybe RouteSettings)
gssrsDefaultRouteSettings = lens _gssrsDefaultRouteSettings (\ s a -> s{_gssrsDefaultRouteSettings = a})

-- | The name of the stage.
gssrsStageName :: Lens' GetStageResponse (Maybe Text)
gssrsStageName = lens _gssrsStageName (\ s a -> s{_gssrsStageName = a})

-- | The timestamp when the stage was last updated.
gssrsLastUpdatedDate :: Lens' GetStageResponse (Maybe UTCTime)
gssrsLastUpdatedDate = lens _gssrsLastUpdatedDate (\ s a -> s{_gssrsLastUpdatedDate = a}) . mapping _Time

-- | The description of the stage.
gssrsDescription :: Lens' GetStageResponse (Maybe Text)
gssrsDescription = lens _gssrsDescription (\ s a -> s{_gssrsDescription = a})

-- | -- | The response status code.
gssrsResponseStatus :: Lens' GetStageResponse Int
gssrsResponseStatus = lens _gssrsResponseStatus (\ s a -> s{_gssrsResponseStatus = a})

instance NFData GetStageResponse where
