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
-- Module      : Network.AWS.APIGatewayV2.GetModelTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a model template.
--
--
module Network.AWS.APIGatewayV2.GetModelTemplate
    (
    -- * Creating a Request
      getModelTemplate
    , GetModelTemplate
    -- * Request Lenses
    , gmtModelId
    , gmtAPIId

    -- * Destructuring the Response
    , getModelTemplateResponse
    , GetModelTemplateResponse
    -- * Response Lenses
    , gmtrsValue
    , gmtrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getModelTemplate' smart constructor.
data GetModelTemplate = GetModelTemplate'
  { _gmtModelId :: !Text
  , _gmtAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetModelTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmtModelId' - The model ID.
--
-- * 'gmtAPIId' - The API identifier.
getModelTemplate
    :: Text -- ^ 'gmtModelId'
    -> Text -- ^ 'gmtAPIId'
    -> GetModelTemplate
getModelTemplate pModelId_ pAPIId_ =
  GetModelTemplate' {_gmtModelId = pModelId_, _gmtAPIId = pAPIId_}


-- | The model ID.
gmtModelId :: Lens' GetModelTemplate Text
gmtModelId = lens _gmtModelId (\ s a -> s{_gmtModelId = a})

-- | The API identifier.
gmtAPIId :: Lens' GetModelTemplate Text
gmtAPIId = lens _gmtAPIId (\ s a -> s{_gmtAPIId = a})

instance AWSRequest GetModelTemplate where
        type Rs GetModelTemplate = GetModelTemplateResponse
        request = get apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 GetModelTemplateResponse' <$>
                   (x .?> "value") <*> (pure (fromEnum s)))

instance Hashable GetModelTemplate where

instance NFData GetModelTemplate where

instance ToHeaders GetModelTemplate where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetModelTemplate where
        toPath GetModelTemplate'{..}
          = mconcat
              ["/v2/apis/", toBS _gmtAPIId, "/models/",
               toBS _gmtModelId, "/template"]

instance ToQuery GetModelTemplate where
        toQuery = const mempty

-- | /See:/ 'getModelTemplateResponse' smart constructor.
data GetModelTemplateResponse = GetModelTemplateResponse'
  { _gmtrsValue :: !(Maybe Text)
  , _gmtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetModelTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmtrsValue' - The template value.
--
-- * 'gmtrsResponseStatus' - -- | The response status code.
getModelTemplateResponse
    :: Int -- ^ 'gmtrsResponseStatus'
    -> GetModelTemplateResponse
getModelTemplateResponse pResponseStatus_ =
  GetModelTemplateResponse'
    {_gmtrsValue = Nothing, _gmtrsResponseStatus = pResponseStatus_}


-- | The template value.
gmtrsValue :: Lens' GetModelTemplateResponse (Maybe Text)
gmtrsValue = lens _gmtrsValue (\ s a -> s{_gmtrsValue = a})

-- | -- | The response status code.
gmtrsResponseStatus :: Lens' GetModelTemplateResponse Int
gmtrsResponseStatus = lens _gmtrsResponseStatus (\ s a -> s{_gmtrsResponseStatus = a})

instance NFData GetModelTemplateResponse where
