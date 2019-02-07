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
-- Module      : Network.AWS.APIGatewayV2.DeleteRouteResponse
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a RouteResponse.
--
--
module Network.AWS.APIGatewayV2.DeleteRouteResponse
    (
    -- * Creating a Request
      deleteRouteResponse
    , DeleteRouteResponse
    -- * Request Lenses
    , dRouteResponseId
    , dAPIId
    , dRouteId

    -- * Destructuring the Response
    , deleteRouteResponseResponse
    , DeleteRouteResponseResponse
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRouteResponse' smart constructor.
data DeleteRouteResponse = DeleteRouteResponse'
  { _dRouteResponseId :: !Text
  , _dAPIId :: !Text
  , _dRouteId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRouteResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dRouteResponseId' - The route response ID.
--
-- * 'dAPIId' - The API identifier.
--
-- * 'dRouteId' - The route ID.
deleteRouteResponse
    :: Text -- ^ 'dRouteResponseId'
    -> Text -- ^ 'dAPIId'
    -> Text -- ^ 'dRouteId'
    -> DeleteRouteResponse
deleteRouteResponse pRouteResponseId_ pAPIId_ pRouteId_ =
  DeleteRouteResponse'
    { _dRouteResponseId = pRouteResponseId_
    , _dAPIId = pAPIId_
    , _dRouteId = pRouteId_
    }


-- | The route response ID.
dRouteResponseId :: Lens' DeleteRouteResponse Text
dRouteResponseId = lens _dRouteResponseId (\ s a -> s{_dRouteResponseId = a})

-- | The API identifier.
dAPIId :: Lens' DeleteRouteResponse Text
dAPIId = lens _dAPIId (\ s a -> s{_dAPIId = a})

-- | The route ID.
dRouteId :: Lens' DeleteRouteResponse Text
dRouteId = lens _dRouteId (\ s a -> s{_dRouteId = a})

instance AWSRequest DeleteRouteResponse where
        type Rs DeleteRouteResponse =
             DeleteRouteResponseResponse
        request = delete apiGatewayV2
        response = receiveNull DeleteRouteResponseResponse'

instance Hashable DeleteRouteResponse where

instance NFData DeleteRouteResponse where

instance ToHeaders DeleteRouteResponse where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteRouteResponse where
        toPath DeleteRouteResponse'{..}
          = mconcat
              ["/v2/apis/", toBS _dAPIId, "/routes/",
               toBS _dRouteId, "/routeresponses/",
               toBS _dRouteResponseId]

instance ToQuery DeleteRouteResponse where
        toQuery = const mempty

-- | /See:/ 'deleteRouteResponseResponse' smart constructor.
data DeleteRouteResponseResponse =
  DeleteRouteResponseResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRouteResponseResponse' with the minimum fields required to make a request.
--
deleteRouteResponseResponse
    :: DeleteRouteResponseResponse
deleteRouteResponseResponse = DeleteRouteResponseResponse'


instance NFData DeleteRouteResponseResponse where
