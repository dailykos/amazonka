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
-- Module      : Network.AWS.APIGatewayV2.GetAPIMappings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The API mappings.
--
--
module Network.AWS.APIGatewayV2.GetAPIMappings
    (
    -- * Creating a Request
      getAPIMappings
    , GetAPIMappings
    -- * Request Lenses
    , gamNextToken
    , gamMaxResults
    , gamDomainName

    -- * Destructuring the Response
    , getAPIMappingsResponse
    , GetAPIMappingsResponse
    -- * Response Lenses
    , gamrsStage
    , gamrsAPIId
    , gamrsAPIMappingKey
    , gamrsAPIMappingId
    , gamrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAPIMappings' smart constructor.
data GetAPIMappings = GetAPIMappings'
  { _gamNextToken :: !(Maybe Text)
  , _gamMaxResults :: !(Maybe Text)
  , _gamDomainName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAPIMappings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gamNextToken' - The next page of elements from this collection. Not valid for the last element of the collection.
--
-- * 'gamMaxResults' - The maximum number of elements to be returned for this resource.
--
-- * 'gamDomainName' - The domain name.
getAPIMappings
    :: Text -- ^ 'gamDomainName'
    -> GetAPIMappings
getAPIMappings pDomainName_ =
  GetAPIMappings'
    { _gamNextToken = Nothing
    , _gamMaxResults = Nothing
    , _gamDomainName = pDomainName_
    }


-- | The next page of elements from this collection. Not valid for the last element of the collection.
gamNextToken :: Lens' GetAPIMappings (Maybe Text)
gamNextToken = lens _gamNextToken (\ s a -> s{_gamNextToken = a})

-- | The maximum number of elements to be returned for this resource.
gamMaxResults :: Lens' GetAPIMappings (Maybe Text)
gamMaxResults = lens _gamMaxResults (\ s a -> s{_gamMaxResults = a})

-- | The domain name.
gamDomainName :: Lens' GetAPIMappings Text
gamDomainName = lens _gamDomainName (\ s a -> s{_gamDomainName = a})

instance AWSRequest GetAPIMappings where
        type Rs GetAPIMappings = GetAPIMappingsResponse
        request = get apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 GetAPIMappingsResponse' <$>
                   (x .?> "stage") <*> (x .?> "apiId") <*>
                     (x .?> "apiMappingKey")
                     <*> (x .?> "apiMappingId")
                     <*> (pure (fromEnum s)))

instance Hashable GetAPIMappings where

instance NFData GetAPIMappings where

instance ToHeaders GetAPIMappings where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetAPIMappings where
        toPath GetAPIMappings'{..}
          = mconcat
              ["/v2/domainnames/", toBS _gamDomainName,
               "/apimappings"]

instance ToQuery GetAPIMappings where
        toQuery GetAPIMappings'{..}
          = mconcat
              ["nextToken" =: _gamNextToken,
               "maxResults" =: _gamMaxResults]

-- | /See:/ 'getAPIMappingsResponse' smart constructor.
data GetAPIMappingsResponse = GetAPIMappingsResponse'
  { _gamrsStage :: !(Maybe Text)
  , _gamrsAPIId :: !(Maybe Text)
  , _gamrsAPIMappingKey :: !(Maybe Text)
  , _gamrsAPIMappingId :: !(Maybe Text)
  , _gamrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAPIMappingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gamrsStage' - The API stage.
--
-- * 'gamrsAPIId' - The API identifier.
--
-- * 'gamrsAPIMappingKey' - The API mapping key.
--
-- * 'gamrsAPIMappingId' - The API mapping identifier.
--
-- * 'gamrsResponseStatus' - -- | The response status code.
getAPIMappingsResponse
    :: Int -- ^ 'gamrsResponseStatus'
    -> GetAPIMappingsResponse
getAPIMappingsResponse pResponseStatus_ =
  GetAPIMappingsResponse'
    { _gamrsStage = Nothing
    , _gamrsAPIId = Nothing
    , _gamrsAPIMappingKey = Nothing
    , _gamrsAPIMappingId = Nothing
    , _gamrsResponseStatus = pResponseStatus_
    }


-- | The API stage.
gamrsStage :: Lens' GetAPIMappingsResponse (Maybe Text)
gamrsStage = lens _gamrsStage (\ s a -> s{_gamrsStage = a})

-- | The API identifier.
gamrsAPIId :: Lens' GetAPIMappingsResponse (Maybe Text)
gamrsAPIId = lens _gamrsAPIId (\ s a -> s{_gamrsAPIId = a})

-- | The API mapping key.
gamrsAPIMappingKey :: Lens' GetAPIMappingsResponse (Maybe Text)
gamrsAPIMappingKey = lens _gamrsAPIMappingKey (\ s a -> s{_gamrsAPIMappingKey = a})

-- | The API mapping identifier.
gamrsAPIMappingId :: Lens' GetAPIMappingsResponse (Maybe Text)
gamrsAPIMappingId = lens _gamrsAPIMappingId (\ s a -> s{_gamrsAPIMappingId = a})

-- | -- | The response status code.
gamrsResponseStatus :: Lens' GetAPIMappingsResponse Int
gamrsResponseStatus = lens _gamrsResponseStatus (\ s a -> s{_gamrsResponseStatus = a})

instance NFData GetAPIMappingsResponse where
