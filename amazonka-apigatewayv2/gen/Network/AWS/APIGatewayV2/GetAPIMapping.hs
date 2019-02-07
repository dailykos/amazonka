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
-- Module      : Network.AWS.APIGatewayV2.GetAPIMapping
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The API mapping.
--
--
module Network.AWS.APIGatewayV2.GetAPIMapping
    (
    -- * Creating a Request
      getAPIMapping
    , GetAPIMapping
    -- * Request Lenses
    , gapimAPIMappingId
    , gapimAPIId
    , gapimDomainName

    -- * Destructuring the Response
    , getAPIMappingResponse
    , GetAPIMappingResponse
    -- * Response Lenses
    , gapimrsStage
    , gapimrsAPIId
    , gapimrsAPIMappingKey
    , gapimrsAPIMappingId
    , gapimrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAPIMapping' smart constructor.
data GetAPIMapping = GetAPIMapping'
  { _gapimAPIMappingId :: !Text
  , _gapimAPIId :: !Text
  , _gapimDomainName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAPIMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gapimAPIMappingId' - The API mapping identifier.
--
-- * 'gapimAPIId' - The identifier of the API.
--
-- * 'gapimDomainName' - The domain name.
getAPIMapping
    :: Text -- ^ 'gapimAPIMappingId'
    -> Text -- ^ 'gapimAPIId'
    -> Text -- ^ 'gapimDomainName'
    -> GetAPIMapping
getAPIMapping pAPIMappingId_ pAPIId_ pDomainName_ =
  GetAPIMapping'
    { _gapimAPIMappingId = pAPIMappingId_
    , _gapimAPIId = pAPIId_
    , _gapimDomainName = pDomainName_
    }


-- | The API mapping identifier.
gapimAPIMappingId :: Lens' GetAPIMapping Text
gapimAPIMappingId = lens _gapimAPIMappingId (\ s a -> s{_gapimAPIMappingId = a})

-- | The identifier of the API.
gapimAPIId :: Lens' GetAPIMapping Text
gapimAPIId = lens _gapimAPIId (\ s a -> s{_gapimAPIId = a})

-- | The domain name.
gapimDomainName :: Lens' GetAPIMapping Text
gapimDomainName = lens _gapimDomainName (\ s a -> s{_gapimDomainName = a})

instance AWSRequest GetAPIMapping where
        type Rs GetAPIMapping = GetAPIMappingResponse
        request = get apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 GetAPIMappingResponse' <$>
                   (x .?> "stage") <*> (x .?> "apiId") <*>
                     (x .?> "apiMappingKey")
                     <*> (x .?> "apiMappingId")
                     <*> (pure (fromEnum s)))

instance Hashable GetAPIMapping where

instance NFData GetAPIMapping where

instance ToHeaders GetAPIMapping where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetAPIMapping where
        toPath GetAPIMapping'{..}
          = mconcat
              ["/v2/domainnames/", toBS _gapimDomainName,
               "/apimappings/", toBS _gapimAPIMappingId]

instance ToQuery GetAPIMapping where
        toQuery GetAPIMapping'{..}
          = mconcat ["apiId" =: _gapimAPIId]

-- | /See:/ 'getAPIMappingResponse' smart constructor.
data GetAPIMappingResponse = GetAPIMappingResponse'
  { _gapimrsStage :: !(Maybe Text)
  , _gapimrsAPIId :: !(Maybe Text)
  , _gapimrsAPIMappingKey :: !(Maybe Text)
  , _gapimrsAPIMappingId :: !(Maybe Text)
  , _gapimrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAPIMappingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gapimrsStage' - The API stage.
--
-- * 'gapimrsAPIId' - The API identifier.
--
-- * 'gapimrsAPIMappingKey' - The API mapping key.
--
-- * 'gapimrsAPIMappingId' - The API mapping identifier.
--
-- * 'gapimrsResponseStatus' - -- | The response status code.
getAPIMappingResponse
    :: Int -- ^ 'gapimrsResponseStatus'
    -> GetAPIMappingResponse
getAPIMappingResponse pResponseStatus_ =
  GetAPIMappingResponse'
    { _gapimrsStage = Nothing
    , _gapimrsAPIId = Nothing
    , _gapimrsAPIMappingKey = Nothing
    , _gapimrsAPIMappingId = Nothing
    , _gapimrsResponseStatus = pResponseStatus_
    }


-- | The API stage.
gapimrsStage :: Lens' GetAPIMappingResponse (Maybe Text)
gapimrsStage = lens _gapimrsStage (\ s a -> s{_gapimrsStage = a})

-- | The API identifier.
gapimrsAPIId :: Lens' GetAPIMappingResponse (Maybe Text)
gapimrsAPIId = lens _gapimrsAPIId (\ s a -> s{_gapimrsAPIId = a})

-- | The API mapping key.
gapimrsAPIMappingKey :: Lens' GetAPIMappingResponse (Maybe Text)
gapimrsAPIMappingKey = lens _gapimrsAPIMappingKey (\ s a -> s{_gapimrsAPIMappingKey = a})

-- | The API mapping identifier.
gapimrsAPIMappingId :: Lens' GetAPIMappingResponse (Maybe Text)
gapimrsAPIMappingId = lens _gapimrsAPIMappingId (\ s a -> s{_gapimrsAPIMappingId = a})

-- | -- | The response status code.
gapimrsResponseStatus :: Lens' GetAPIMappingResponse Int
gapimrsResponseStatus = lens _gapimrsResponseStatus (\ s a -> s{_gapimrsResponseStatus = a})

instance NFData GetAPIMappingResponse where
