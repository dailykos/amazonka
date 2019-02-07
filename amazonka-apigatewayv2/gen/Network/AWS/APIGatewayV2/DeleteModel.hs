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
-- Module      : Network.AWS.APIGatewayV2.DeleteModel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Model.
--
--
module Network.AWS.APIGatewayV2.DeleteModel
    (
    -- * Creating a Request
      deleteModel
    , DeleteModel
    -- * Request Lenses
    , dmModelId
    , dmAPIId

    -- * Destructuring the Response
    , deleteModelResponse
    , DeleteModelResponse
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteModel' smart constructor.
data DeleteModel = DeleteModel'
  { _dmModelId :: !Text
  , _dmAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmModelId' - The model ID.
--
-- * 'dmAPIId' - The API identifier.
deleteModel
    :: Text -- ^ 'dmModelId'
    -> Text -- ^ 'dmAPIId'
    -> DeleteModel
deleteModel pModelId_ pAPIId_ =
  DeleteModel' {_dmModelId = pModelId_, _dmAPIId = pAPIId_}


-- | The model ID.
dmModelId :: Lens' DeleteModel Text
dmModelId = lens _dmModelId (\ s a -> s{_dmModelId = a})

-- | The API identifier.
dmAPIId :: Lens' DeleteModel Text
dmAPIId = lens _dmAPIId (\ s a -> s{_dmAPIId = a})

instance AWSRequest DeleteModel where
        type Rs DeleteModel = DeleteModelResponse
        request = delete apiGatewayV2
        response = receiveNull DeleteModelResponse'

instance Hashable DeleteModel where

instance NFData DeleteModel where

instance ToHeaders DeleteModel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteModel where
        toPath DeleteModel'{..}
          = mconcat
              ["/v2/apis/", toBS _dmAPIId, "/models/",
               toBS _dmModelId]

instance ToQuery DeleteModel where
        toQuery = const mempty

-- | /See:/ 'deleteModelResponse' smart constructor.
data DeleteModelResponse =
  DeleteModelResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteModelResponse' with the minimum fields required to make a request.
--
deleteModelResponse
    :: DeleteModelResponse
deleteModelResponse = DeleteModelResponse'


instance NFData DeleteModelResponse where
