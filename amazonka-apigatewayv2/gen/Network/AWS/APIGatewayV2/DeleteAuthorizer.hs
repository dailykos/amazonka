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
-- Module      : Network.AWS.APIGatewayV2.DeleteAuthorizer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Authorizer.
--
--
module Network.AWS.APIGatewayV2.DeleteAuthorizer
    (
    -- * Creating a Request
      deleteAuthorizer
    , DeleteAuthorizer
    -- * Request Lenses
    , daAuthorizerId
    , daAPIId

    -- * Destructuring the Response
    , deleteAuthorizerResponse
    , DeleteAuthorizerResponse
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAuthorizer' smart constructor.
data DeleteAuthorizer = DeleteAuthorizer'
  { _daAuthorizerId :: !Text
  , _daAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAuthorizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daAuthorizerId' - The authorizer identifier.
--
-- * 'daAPIId' - The API identifier.
deleteAuthorizer
    :: Text -- ^ 'daAuthorizerId'
    -> Text -- ^ 'daAPIId'
    -> DeleteAuthorizer
deleteAuthorizer pAuthorizerId_ pAPIId_ =
  DeleteAuthorizer' {_daAuthorizerId = pAuthorizerId_, _daAPIId = pAPIId_}


-- | The authorizer identifier.
daAuthorizerId :: Lens' DeleteAuthorizer Text
daAuthorizerId = lens _daAuthorizerId (\ s a -> s{_daAuthorizerId = a})

-- | The API identifier.
daAPIId :: Lens' DeleteAuthorizer Text
daAPIId = lens _daAPIId (\ s a -> s{_daAPIId = a})

instance AWSRequest DeleteAuthorizer where
        type Rs DeleteAuthorizer = DeleteAuthorizerResponse
        request = delete apiGatewayV2
        response = receiveNull DeleteAuthorizerResponse'

instance Hashable DeleteAuthorizer where

instance NFData DeleteAuthorizer where

instance ToHeaders DeleteAuthorizer where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteAuthorizer where
        toPath DeleteAuthorizer'{..}
          = mconcat
              ["/v2/apis/", toBS _daAPIId, "/authorizers/",
               toBS _daAuthorizerId]

instance ToQuery DeleteAuthorizer where
        toQuery = const mempty

-- | /See:/ 'deleteAuthorizerResponse' smart constructor.
data DeleteAuthorizerResponse =
  DeleteAuthorizerResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAuthorizerResponse' with the minimum fields required to make a request.
--
deleteAuthorizerResponse
    :: DeleteAuthorizerResponse
deleteAuthorizerResponse = DeleteAuthorizerResponse'


instance NFData DeleteAuthorizerResponse where
