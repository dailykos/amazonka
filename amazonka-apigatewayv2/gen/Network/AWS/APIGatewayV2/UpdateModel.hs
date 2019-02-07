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
-- Module      : Network.AWS.APIGatewayV2.UpdateModel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Model.
--
--
module Network.AWS.APIGatewayV2.UpdateModel
    (
    -- * Creating a Request
      updateModel
    , UpdateModel
    -- * Request Lenses
    , umSchema
    , umName
    , umDescription
    , umContentType
    , umModelId
    , umAPIId

    -- * Destructuring the Response
    , updateModelResponse
    , UpdateModelResponse
    -- * Response Lenses
    , umrsModelId
    , umrsSchema
    , umrsName
    , umrsDescription
    , umrsContentType
    , umrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateModel' smart constructor.
data UpdateModel = UpdateModel'
  { _umSchema :: !(Maybe Text)
  , _umName :: !(Maybe Text)
  , _umDescription :: !(Maybe Text)
  , _umContentType :: !(Maybe Text)
  , _umModelId :: !Text
  , _umAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umSchema' - The schema for the model. For application/json models, this should be JSON schema draft 4 model.
--
-- * 'umName' - The name of the model.
--
-- * 'umDescription' - The description of the model.
--
-- * 'umContentType' - The content-type for the model, for example, "application/json".
--
-- * 'umModelId' - The model ID.
--
-- * 'umAPIId' - The API identifier.
updateModel
    :: Text -- ^ 'umModelId'
    -> Text -- ^ 'umAPIId'
    -> UpdateModel
updateModel pModelId_ pAPIId_ =
  UpdateModel'
    { _umSchema = Nothing
    , _umName = Nothing
    , _umDescription = Nothing
    , _umContentType = Nothing
    , _umModelId = pModelId_
    , _umAPIId = pAPIId_
    }


-- | The schema for the model. For application/json models, this should be JSON schema draft 4 model.
umSchema :: Lens' UpdateModel (Maybe Text)
umSchema = lens _umSchema (\ s a -> s{_umSchema = a})

-- | The name of the model.
umName :: Lens' UpdateModel (Maybe Text)
umName = lens _umName (\ s a -> s{_umName = a})

-- | The description of the model.
umDescription :: Lens' UpdateModel (Maybe Text)
umDescription = lens _umDescription (\ s a -> s{_umDescription = a})

-- | The content-type for the model, for example, "application/json".
umContentType :: Lens' UpdateModel (Maybe Text)
umContentType = lens _umContentType (\ s a -> s{_umContentType = a})

-- | The model ID.
umModelId :: Lens' UpdateModel Text
umModelId = lens _umModelId (\ s a -> s{_umModelId = a})

-- | The API identifier.
umAPIId :: Lens' UpdateModel Text
umAPIId = lens _umAPIId (\ s a -> s{_umAPIId = a})

instance AWSRequest UpdateModel where
        type Rs UpdateModel = UpdateModelResponse
        request = patchJSON apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 UpdateModelResponse' <$>
                   (x .?> "modelId") <*> (x .?> "schema") <*>
                     (x .?> "name")
                     <*> (x .?> "description")
                     <*> (x .?> "contentType")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateModel where

instance NFData UpdateModel where

instance ToHeaders UpdateModel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateModel where
        toJSON UpdateModel'{..}
          = object
              (catMaybes
                 [("schema" .=) <$> _umSchema,
                  ("name" .=) <$> _umName,
                  ("description" .=) <$> _umDescription,
                  ("contentType" .=) <$> _umContentType])

instance ToPath UpdateModel where
        toPath UpdateModel'{..}
          = mconcat
              ["/v2/apis/", toBS _umAPIId, "/models/",
               toBS _umModelId]

instance ToQuery UpdateModel where
        toQuery = const mempty

-- | /See:/ 'updateModelResponse' smart constructor.
data UpdateModelResponse = UpdateModelResponse'
  { _umrsModelId :: !(Maybe Text)
  , _umrsSchema :: !(Maybe Text)
  , _umrsName :: !(Maybe Text)
  , _umrsDescription :: !(Maybe Text)
  , _umrsContentType :: !(Maybe Text)
  , _umrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateModelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umrsModelId' - The model identifier.
--
-- * 'umrsSchema' - The schema for the model. For application/json models, this should be JSON schema draft 4 model.
--
-- * 'umrsName' - The name of the model. Must be alphanumeric.
--
-- * 'umrsDescription' - The description of the model.
--
-- * 'umrsContentType' - The content-type for the model, for example, "application/json".
--
-- * 'umrsResponseStatus' - -- | The response status code.
updateModelResponse
    :: Int -- ^ 'umrsResponseStatus'
    -> UpdateModelResponse
updateModelResponse pResponseStatus_ =
  UpdateModelResponse'
    { _umrsModelId = Nothing
    , _umrsSchema = Nothing
    , _umrsName = Nothing
    , _umrsDescription = Nothing
    , _umrsContentType = Nothing
    , _umrsResponseStatus = pResponseStatus_
    }


-- | The model identifier.
umrsModelId :: Lens' UpdateModelResponse (Maybe Text)
umrsModelId = lens _umrsModelId (\ s a -> s{_umrsModelId = a})

-- | The schema for the model. For application/json models, this should be JSON schema draft 4 model.
umrsSchema :: Lens' UpdateModelResponse (Maybe Text)
umrsSchema = lens _umrsSchema (\ s a -> s{_umrsSchema = a})

-- | The name of the model. Must be alphanumeric.
umrsName :: Lens' UpdateModelResponse (Maybe Text)
umrsName = lens _umrsName (\ s a -> s{_umrsName = a})

-- | The description of the model.
umrsDescription :: Lens' UpdateModelResponse (Maybe Text)
umrsDescription = lens _umrsDescription (\ s a -> s{_umrsDescription = a})

-- | The content-type for the model, for example, "application/json".
umrsContentType :: Lens' UpdateModelResponse (Maybe Text)
umrsContentType = lens _umrsContentType (\ s a -> s{_umrsContentType = a})

-- | -- | The response status code.
umrsResponseStatus :: Lens' UpdateModelResponse Int
umrsResponseStatus = lens _umrsResponseStatus (\ s a -> s{_umrsResponseStatus = a})

instance NFData UpdateModelResponse where
