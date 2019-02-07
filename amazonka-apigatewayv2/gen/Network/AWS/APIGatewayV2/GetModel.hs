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
-- Module      : Network.AWS.APIGatewayV2.GetModel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a Model.
--
--
module Network.AWS.APIGatewayV2.GetModel
    (
    -- * Creating a Request
      getModel
    , GetModel
    -- * Request Lenses
    , gmmModelId
    , gmmAPIId

    -- * Destructuring the Response
    , getModelResponse
    , GetModelResponse
    -- * Response Lenses
    , gmrsModelId
    , gmrsSchema
    , gmrsName
    , gmrsDescription
    , gmrsContentType
    , gmrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getModel' smart constructor.
data GetModel = GetModel'
  { _gmmModelId :: !Text
  , _gmmAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmmModelId' - The model ID.
--
-- * 'gmmAPIId' - The API identifier.
getModel
    :: Text -- ^ 'gmmModelId'
    -> Text -- ^ 'gmmAPIId'
    -> GetModel
getModel pModelId_ pAPIId_ =
  GetModel' {_gmmModelId = pModelId_, _gmmAPIId = pAPIId_}


-- | The model ID.
gmmModelId :: Lens' GetModel Text
gmmModelId = lens _gmmModelId (\ s a -> s{_gmmModelId = a})

-- | The API identifier.
gmmAPIId :: Lens' GetModel Text
gmmAPIId = lens _gmmAPIId (\ s a -> s{_gmmAPIId = a})

instance AWSRequest GetModel where
        type Rs GetModel = GetModelResponse
        request = get apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 GetModelResponse' <$>
                   (x .?> "modelId") <*> (x .?> "schema") <*>
                     (x .?> "name")
                     <*> (x .?> "description")
                     <*> (x .?> "contentType")
                     <*> (pure (fromEnum s)))

instance Hashable GetModel where

instance NFData GetModel where

instance ToHeaders GetModel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetModel where
        toPath GetModel'{..}
          = mconcat
              ["/v2/apis/", toBS _gmmAPIId, "/models/",
               toBS _gmmModelId]

instance ToQuery GetModel where
        toQuery = const mempty

-- | /See:/ 'getModelResponse' smart constructor.
data GetModelResponse = GetModelResponse'
  { _gmrsModelId :: !(Maybe Text)
  , _gmrsSchema :: !(Maybe Text)
  , _gmrsName :: !(Maybe Text)
  , _gmrsDescription :: !(Maybe Text)
  , _gmrsContentType :: !(Maybe Text)
  , _gmrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetModelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmrsModelId' - The model identifier.
--
-- * 'gmrsSchema' - The schema for the model. For application/json models, this should be JSON schema draft 4 model.
--
-- * 'gmrsName' - The name of the model. Must be alphanumeric.
--
-- * 'gmrsDescription' - The description of the model.
--
-- * 'gmrsContentType' - The content-type for the model, for example, "application/json".
--
-- * 'gmrsResponseStatus' - -- | The response status code.
getModelResponse
    :: Int -- ^ 'gmrsResponseStatus'
    -> GetModelResponse
getModelResponse pResponseStatus_ =
  GetModelResponse'
    { _gmrsModelId = Nothing
    , _gmrsSchema = Nothing
    , _gmrsName = Nothing
    , _gmrsDescription = Nothing
    , _gmrsContentType = Nothing
    , _gmrsResponseStatus = pResponseStatus_
    }


-- | The model identifier.
gmrsModelId :: Lens' GetModelResponse (Maybe Text)
gmrsModelId = lens _gmrsModelId (\ s a -> s{_gmrsModelId = a})

-- | The schema for the model. For application/json models, this should be JSON schema draft 4 model.
gmrsSchema :: Lens' GetModelResponse (Maybe Text)
gmrsSchema = lens _gmrsSchema (\ s a -> s{_gmrsSchema = a})

-- | The name of the model. Must be alphanumeric.
gmrsName :: Lens' GetModelResponse (Maybe Text)
gmrsName = lens _gmrsName (\ s a -> s{_gmrsName = a})

-- | The description of the model.
gmrsDescription :: Lens' GetModelResponse (Maybe Text)
gmrsDescription = lens _gmrsDescription (\ s a -> s{_gmrsDescription = a})

-- | The content-type for the model, for example, "application/json".
gmrsContentType :: Lens' GetModelResponse (Maybe Text)
gmrsContentType = lens _gmrsContentType (\ s a -> s{_gmrsContentType = a})

-- | -- | The response status code.
gmrsResponseStatus :: Lens' GetModelResponse Int
gmrsResponseStatus = lens _gmrsResponseStatus (\ s a -> s{_gmrsResponseStatus = a})

instance NFData GetModelResponse where
