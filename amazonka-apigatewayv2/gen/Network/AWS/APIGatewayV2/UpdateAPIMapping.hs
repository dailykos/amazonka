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
-- Module      : Network.AWS.APIGatewayV2.UpdateAPIMapping
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The API mapping.
--
--
module Network.AWS.APIGatewayV2.UpdateAPIMapping
    (
    -- * Creating a Request
      updateAPIMapping
    , UpdateAPIMapping
    -- * Request Lenses
    , uamStage
    , uamAPIMappingKey
    , uamAPIMappingId
    , uamAPIId
    , uamDomainName

    -- * Destructuring the Response
    , updateAPIMappingResponse
    , UpdateAPIMappingResponse
    -- * Response Lenses
    , uamrsStage
    , uamrsAPIId
    , uamrsAPIMappingKey
    , uamrsAPIMappingId
    , uamrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateAPIMapping' smart constructor.
data UpdateAPIMapping = UpdateAPIMapping'
  { _uamStage :: !(Maybe Text)
  , _uamAPIMappingKey :: !(Maybe Text)
  , _uamAPIMappingId :: !Text
  , _uamAPIId :: !Text
  , _uamDomainName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAPIMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uamStage' - The API stage.
--
-- * 'uamAPIMappingKey' - The API mapping key.
--
-- * 'uamAPIMappingId' - The API mapping identifier.
--
-- * 'uamAPIId' - The API identifier.
--
-- * 'uamDomainName' - The domain name.
updateAPIMapping
    :: Text -- ^ 'uamAPIMappingId'
    -> Text -- ^ 'uamAPIId'
    -> Text -- ^ 'uamDomainName'
    -> UpdateAPIMapping
updateAPIMapping pAPIMappingId_ pAPIId_ pDomainName_ =
  UpdateAPIMapping'
    { _uamStage = Nothing
    , _uamAPIMappingKey = Nothing
    , _uamAPIMappingId = pAPIMappingId_
    , _uamAPIId = pAPIId_
    , _uamDomainName = pDomainName_
    }


-- | The API stage.
uamStage :: Lens' UpdateAPIMapping (Maybe Text)
uamStage = lens _uamStage (\ s a -> s{_uamStage = a})

-- | The API mapping key.
uamAPIMappingKey :: Lens' UpdateAPIMapping (Maybe Text)
uamAPIMappingKey = lens _uamAPIMappingKey (\ s a -> s{_uamAPIMappingKey = a})

-- | The API mapping identifier.
uamAPIMappingId :: Lens' UpdateAPIMapping Text
uamAPIMappingId = lens _uamAPIMappingId (\ s a -> s{_uamAPIMappingId = a})

-- | The API identifier.
uamAPIId :: Lens' UpdateAPIMapping Text
uamAPIId = lens _uamAPIId (\ s a -> s{_uamAPIId = a})

-- | The domain name.
uamDomainName :: Lens' UpdateAPIMapping Text
uamDomainName = lens _uamDomainName (\ s a -> s{_uamDomainName = a})

instance AWSRequest UpdateAPIMapping where
        type Rs UpdateAPIMapping = UpdateAPIMappingResponse
        request = patchJSON apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 UpdateAPIMappingResponse' <$>
                   (x .?> "stage") <*> (x .?> "apiId") <*>
                     (x .?> "apiMappingKey")
                     <*> (x .?> "apiMappingId")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateAPIMapping where

instance NFData UpdateAPIMapping where

instance ToHeaders UpdateAPIMapping where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateAPIMapping where
        toJSON UpdateAPIMapping'{..}
          = object
              (catMaybes
                 [("stage" .=) <$> _uamStage,
                  ("apiMappingKey" .=) <$> _uamAPIMappingKey,
                  Just ("apiId" .= _uamAPIId)])

instance ToPath UpdateAPIMapping where
        toPath UpdateAPIMapping'{..}
          = mconcat
              ["/v2/domainnames/", toBS _uamDomainName,
               "/apimappings/", toBS _uamAPIMappingId]

instance ToQuery UpdateAPIMapping where
        toQuery = const mempty

-- | /See:/ 'updateAPIMappingResponse' smart constructor.
data UpdateAPIMappingResponse = UpdateAPIMappingResponse'
  { _uamrsStage :: !(Maybe Text)
  , _uamrsAPIId :: !(Maybe Text)
  , _uamrsAPIMappingKey :: !(Maybe Text)
  , _uamrsAPIMappingId :: !(Maybe Text)
  , _uamrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAPIMappingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uamrsStage' - The API stage.
--
-- * 'uamrsAPIId' - The API identifier.
--
-- * 'uamrsAPIMappingKey' - The API mapping key.
--
-- * 'uamrsAPIMappingId' - The API mapping identifier.
--
-- * 'uamrsResponseStatus' - -- | The response status code.
updateAPIMappingResponse
    :: Int -- ^ 'uamrsResponseStatus'
    -> UpdateAPIMappingResponse
updateAPIMappingResponse pResponseStatus_ =
  UpdateAPIMappingResponse'
    { _uamrsStage = Nothing
    , _uamrsAPIId = Nothing
    , _uamrsAPIMappingKey = Nothing
    , _uamrsAPIMappingId = Nothing
    , _uamrsResponseStatus = pResponseStatus_
    }


-- | The API stage.
uamrsStage :: Lens' UpdateAPIMappingResponse (Maybe Text)
uamrsStage = lens _uamrsStage (\ s a -> s{_uamrsStage = a})

-- | The API identifier.
uamrsAPIId :: Lens' UpdateAPIMappingResponse (Maybe Text)
uamrsAPIId = lens _uamrsAPIId (\ s a -> s{_uamrsAPIId = a})

-- | The API mapping key.
uamrsAPIMappingKey :: Lens' UpdateAPIMappingResponse (Maybe Text)
uamrsAPIMappingKey = lens _uamrsAPIMappingKey (\ s a -> s{_uamrsAPIMappingKey = a})

-- | The API mapping identifier.
uamrsAPIMappingId :: Lens' UpdateAPIMappingResponse (Maybe Text)
uamrsAPIMappingId = lens _uamrsAPIMappingId (\ s a -> s{_uamrsAPIMappingId = a})

-- | -- | The response status code.
uamrsResponseStatus :: Lens' UpdateAPIMappingResponse Int
uamrsResponseStatus = lens _uamrsResponseStatus (\ s a -> s{_uamrsResponseStatus = a})

instance NFData UpdateAPIMappingResponse where
