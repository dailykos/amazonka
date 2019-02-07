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
-- Module      : Network.AWS.APIGatewayV2.DeleteAPI
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Api resource.
--
--
module Network.AWS.APIGatewayV2.DeleteAPI
    (
    -- * Creating a Request
      deleteAPI
    , DeleteAPI
    -- * Request Lenses
    , dapiAPIId

    -- * Destructuring the Response
    , deleteAPIResponse
    , DeleteAPIResponse
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAPI' smart constructor.
newtype DeleteAPI = DeleteAPI'
  { _dapiAPIId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAPI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dapiAPIId' - The API identifier.
deleteAPI
    :: Text -- ^ 'dapiAPIId'
    -> DeleteAPI
deleteAPI pAPIId_ = DeleteAPI' {_dapiAPIId = pAPIId_}


-- | The API identifier.
dapiAPIId :: Lens' DeleteAPI Text
dapiAPIId = lens _dapiAPIId (\ s a -> s{_dapiAPIId = a})

instance AWSRequest DeleteAPI where
        type Rs DeleteAPI = DeleteAPIResponse
        request = delete apiGatewayV2
        response = receiveNull DeleteAPIResponse'

instance Hashable DeleteAPI where

instance NFData DeleteAPI where

instance ToHeaders DeleteAPI where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteAPI where
        toPath DeleteAPI'{..}
          = mconcat ["/v2/apis/", toBS _dapiAPIId]

instance ToQuery DeleteAPI where
        toQuery = const mempty

-- | /See:/ 'deleteAPIResponse' smart constructor.
data DeleteAPIResponse =
  DeleteAPIResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAPIResponse' with the minimum fields required to make a request.
--
deleteAPIResponse
    :: DeleteAPIResponse
deleteAPIResponse = DeleteAPIResponse'


instance NFData DeleteAPIResponse where
