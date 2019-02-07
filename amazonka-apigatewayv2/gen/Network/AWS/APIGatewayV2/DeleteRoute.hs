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
-- Module      : Network.AWS.APIGatewayV2.DeleteRoute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Route.
--
--
module Network.AWS.APIGatewayV2.DeleteRoute
    (
    -- * Creating a Request
      deleteRoute
    , DeleteRoute
    -- * Request Lenses
    , drAPIId
    , drRouteId

    -- * Destructuring the Response
    , deleteRouteResponse'
    , DeleteRouteResponse'
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRoute' smart constructor.
data DeleteRoute = DeleteRoute'
  { _drAPIId :: !Text
  , _drRouteId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drAPIId' - The API identifier.
--
-- * 'drRouteId' - The route ID.
deleteRoute
    :: Text -- ^ 'drAPIId'
    -> Text -- ^ 'drRouteId'
    -> DeleteRoute
deleteRoute pAPIId_ pRouteId_ =
  DeleteRoute' {_drAPIId = pAPIId_, _drRouteId = pRouteId_}


-- | The API identifier.
drAPIId :: Lens' DeleteRoute Text
drAPIId = lens _drAPIId (\ s a -> s{_drAPIId = a})

-- | The route ID.
drRouteId :: Lens' DeleteRoute Text
drRouteId = lens _drRouteId (\ s a -> s{_drRouteId = a})

instance AWSRequest DeleteRoute where
        type Rs DeleteRoute = DeleteRouteResponse'
        request = delete apiGatewayV2
        response = receiveNull DeleteRouteResponse''

instance Hashable DeleteRoute where

instance NFData DeleteRoute where

instance ToHeaders DeleteRoute where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteRoute where
        toPath DeleteRoute'{..}
          = mconcat
              ["/v2/apis/", toBS _drAPIId, "/routes/",
               toBS _drRouteId]

instance ToQuery DeleteRoute where
        toQuery = const mempty

-- | /See:/ 'deleteRouteResponse'' smart constructor.
data DeleteRouteResponse' =
  DeleteRouteResponse''
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRouteResponse'' with the minimum fields required to make a request.
--
deleteRouteResponse'
    :: DeleteRouteResponse'
deleteRouteResponse' = DeleteRouteResponse''


instance NFData DeleteRouteResponse' where
