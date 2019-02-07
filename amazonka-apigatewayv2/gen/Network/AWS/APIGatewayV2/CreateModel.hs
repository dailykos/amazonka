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
-- Module      : Network.AWS.APIGatewayV2.CreateModel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Model for an API.
--
--
module Network.AWS.APIGatewayV2.CreateModel
    (
    -- * Creating a Request
      createModel
    , CreateModel
    -- * Request Lenses
    , cmSchema
    , cmDescription
    , cmContentType
    , cmAPIId
    , cmName

    -- * Destructuring the Response
    , createModelResponse
    , CreateModelResponse
    -- * Response Lenses
    , cmrsModelId
    , cmrsSchema
    , cmrsName
    , cmrsDescription
    , cmrsContentType
    , cmrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createModel' smart constructor.
data CreateModel = CreateModel'
  { _cmSchema :: !(Maybe Text)
  , _cmDescription :: !(Maybe Text)
  , _cmContentType :: !(Maybe Text)
  , _cmAPIId :: !Text
  , _cmName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmSchema' - The schema for the model. For application/json models, this should be JSON schema draft 4 model.
--
-- * 'cmDescription' - The description of the model.
--
-- * 'cmContentType' - The content-type for the model, for example, "application/json".
--
-- * 'cmAPIId' - The API identifier.
--
-- * 'cmName' - The name of the model. Must be alphanumeric.
createModel
    :: Text -- ^ 'cmAPIId'
    -> Text -- ^ 'cmName'
    -> CreateModel
createModel pAPIId_ pName_ =
  CreateModel'
    { _cmSchema = Nothing
    , _cmDescription = Nothing
    , _cmContentType = Nothing
    , _cmAPIId = pAPIId_
    , _cmName = pName_
    }


-- | The schema for the model. For application/json models, this should be JSON schema draft 4 model.
cmSchema :: Lens' CreateModel (Maybe Text)
cmSchema = lens _cmSchema (\ s a -> s{_cmSchema = a})

-- | The description of the model.
cmDescription :: Lens' CreateModel (Maybe Text)
cmDescription = lens _cmDescription (\ s a -> s{_cmDescription = a})

-- | The content-type for the model, for example, "application/json".
cmContentType :: Lens' CreateModel (Maybe Text)
cmContentType = lens _cmContentType (\ s a -> s{_cmContentType = a})

-- | The API identifier.
cmAPIId :: Lens' CreateModel Text
cmAPIId = lens _cmAPIId (\ s a -> s{_cmAPIId = a})

-- | The name of the model. Must be alphanumeric.
cmName :: Lens' CreateModel Text
cmName = lens _cmName (\ s a -> s{_cmName = a})

instance AWSRequest CreateModel where
        type Rs CreateModel = CreateModelResponse
        request = postJSON apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 CreateModelResponse' <$>
                   (x .?> "modelId") <*> (x .?> "schema") <*>
                     (x .?> "name")
                     <*> (x .?> "description")
                     <*> (x .?> "contentType")
                     <*> (pure (fromEnum s)))

instance Hashable CreateModel where

instance NFData CreateModel where

instance ToHeaders CreateModel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateModel where
        toJSON CreateModel'{..}
          = object
              (catMaybes
                 [("schema" .=) <$> _cmSchema,
                  ("description" .=) <$> _cmDescription,
                  ("contentType" .=) <$> _cmContentType,
                  Just ("name" .= _cmName)])

instance ToPath CreateModel where
        toPath CreateModel'{..}
          = mconcat ["/v2/apis/", toBS _cmAPIId, "/models"]

instance ToQuery CreateModel where
        toQuery = const mempty

-- | /See:/ 'createModelResponse' smart constructor.
data CreateModelResponse = CreateModelResponse'
  { _cmrsModelId :: !(Maybe Text)
  , _cmrsSchema :: !(Maybe Text)
  , _cmrsName :: !(Maybe Text)
  , _cmrsDescription :: !(Maybe Text)
  , _cmrsContentType :: !(Maybe Text)
  , _cmrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateModelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmrsModelId' - The model identifier.
--
-- * 'cmrsSchema' - The schema for the model. For application/json models, this should be JSON schema draft 4 model.
--
-- * 'cmrsName' - The name of the model. Must be alphanumeric.
--
-- * 'cmrsDescription' - The description of the model.
--
-- * 'cmrsContentType' - The content-type for the model, for example, "application/json".
--
-- * 'cmrsResponseStatus' - -- | The response status code.
createModelResponse
    :: Int -- ^ 'cmrsResponseStatus'
    -> CreateModelResponse
createModelResponse pResponseStatus_ =
  CreateModelResponse'
    { _cmrsModelId = Nothing
    , _cmrsSchema = Nothing
    , _cmrsName = Nothing
    , _cmrsDescription = Nothing
    , _cmrsContentType = Nothing
    , _cmrsResponseStatus = pResponseStatus_
    }


-- | The model identifier.
cmrsModelId :: Lens' CreateModelResponse (Maybe Text)
cmrsModelId = lens _cmrsModelId (\ s a -> s{_cmrsModelId = a})

-- | The schema for the model. For application/json models, this should be JSON schema draft 4 model.
cmrsSchema :: Lens' CreateModelResponse (Maybe Text)
cmrsSchema = lens _cmrsSchema (\ s a -> s{_cmrsSchema = a})

-- | The name of the model. Must be alphanumeric.
cmrsName :: Lens' CreateModelResponse (Maybe Text)
cmrsName = lens _cmrsName (\ s a -> s{_cmrsName = a})

-- | The description of the model.
cmrsDescription :: Lens' CreateModelResponse (Maybe Text)
cmrsDescription = lens _cmrsDescription (\ s a -> s{_cmrsDescription = a})

-- | The content-type for the model, for example, "application/json".
cmrsContentType :: Lens' CreateModelResponse (Maybe Text)
cmrsContentType = lens _cmrsContentType (\ s a -> s{_cmrsContentType = a})

-- | -- | The response status code.
cmrsResponseStatus :: Lens' CreateModelResponse Int
cmrsResponseStatus = lens _cmrsResponseStatus (\ s a -> s{_cmrsResponseStatus = a})

instance NFData CreateModelResponse where
