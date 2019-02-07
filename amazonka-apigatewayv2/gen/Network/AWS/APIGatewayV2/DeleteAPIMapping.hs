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
-- Module      : Network.AWS.APIGatewayV2.DeleteAPIMapping
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an API mapping.
--
--
module Network.AWS.APIGatewayV2.DeleteAPIMapping
    (
    -- * Creating a Request
      deleteAPIMapping
    , DeleteAPIMapping
    -- * Request Lenses
    , damAPIMappingId
    , damAPIId
    , damDomainName

    -- * Destructuring the Response
    , deleteAPIMappingResponse
    , DeleteAPIMappingResponse
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAPIMapping' smart constructor.
data DeleteAPIMapping = DeleteAPIMapping'
  { _damAPIMappingId :: !Text
  , _damAPIId :: !Text
  , _damDomainName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAPIMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'damAPIMappingId' - The API mapping identifier.
--
-- * 'damAPIId' - The identifier of the API.
--
-- * 'damDomainName' - The domain name.
deleteAPIMapping
    :: Text -- ^ 'damAPIMappingId'
    -> Text -- ^ 'damAPIId'
    -> Text -- ^ 'damDomainName'
    -> DeleteAPIMapping
deleteAPIMapping pAPIMappingId_ pAPIId_ pDomainName_ =
  DeleteAPIMapping'
    { _damAPIMappingId = pAPIMappingId_
    , _damAPIId = pAPIId_
    , _damDomainName = pDomainName_
    }


-- | The API mapping identifier.
damAPIMappingId :: Lens' DeleteAPIMapping Text
damAPIMappingId = lens _damAPIMappingId (\ s a -> s{_damAPIMappingId = a})

-- | The identifier of the API.
damAPIId :: Lens' DeleteAPIMapping Text
damAPIId = lens _damAPIId (\ s a -> s{_damAPIId = a})

-- | The domain name.
damDomainName :: Lens' DeleteAPIMapping Text
damDomainName = lens _damDomainName (\ s a -> s{_damDomainName = a})

instance AWSRequest DeleteAPIMapping where
        type Rs DeleteAPIMapping = DeleteAPIMappingResponse
        request = delete apiGatewayV2
        response = receiveNull DeleteAPIMappingResponse'

instance Hashable DeleteAPIMapping where

instance NFData DeleteAPIMapping where

instance ToHeaders DeleteAPIMapping where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteAPIMapping where
        toPath DeleteAPIMapping'{..}
          = mconcat
              ["/v2/domainnames/", toBS _damDomainName,
               "/apimappings/", toBS _damAPIMappingId]

instance ToQuery DeleteAPIMapping where
        toQuery DeleteAPIMapping'{..}
          = mconcat ["apiId" =: _damAPIId]

-- | /See:/ 'deleteAPIMappingResponse' smart constructor.
data DeleteAPIMappingResponse =
  DeleteAPIMappingResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAPIMappingResponse' with the minimum fields required to make a request.
--
deleteAPIMappingResponse
    :: DeleteAPIMappingResponse
deleteAPIMappingResponse = DeleteAPIMappingResponse'


instance NFData DeleteAPIMappingResponse where
