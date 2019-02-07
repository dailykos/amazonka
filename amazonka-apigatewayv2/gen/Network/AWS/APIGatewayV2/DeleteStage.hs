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
-- Module      : Network.AWS.APIGatewayV2.DeleteStage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Stage.
--
--
module Network.AWS.APIGatewayV2.DeleteStage
    (
    -- * Creating a Request
      deleteStage
    , DeleteStage
    -- * Request Lenses
    , dsStageName
    , dsAPIId

    -- * Destructuring the Response
    , deleteStageResponse
    , DeleteStageResponse
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteStage' smart constructor.
data DeleteStage = DeleteStage'
  { _dsStageName :: !Text
  , _dsAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteStage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsStageName' - The stage name.
--
-- * 'dsAPIId' - The API identifier.
deleteStage
    :: Text -- ^ 'dsStageName'
    -> Text -- ^ 'dsAPIId'
    -> DeleteStage
deleteStage pStageName_ pAPIId_ =
  DeleteStage' {_dsStageName = pStageName_, _dsAPIId = pAPIId_}


-- | The stage name.
dsStageName :: Lens' DeleteStage Text
dsStageName = lens _dsStageName (\ s a -> s{_dsStageName = a})

-- | The API identifier.
dsAPIId :: Lens' DeleteStage Text
dsAPIId = lens _dsAPIId (\ s a -> s{_dsAPIId = a})

instance AWSRequest DeleteStage where
        type Rs DeleteStage = DeleteStageResponse
        request = delete apiGatewayV2
        response = receiveNull DeleteStageResponse'

instance Hashable DeleteStage where

instance NFData DeleteStage where

instance ToHeaders DeleteStage where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteStage where
        toPath DeleteStage'{..}
          = mconcat
              ["/v2/apis/", toBS _dsAPIId, "/stages/",
               toBS _dsStageName]

instance ToQuery DeleteStage where
        toQuery = const mempty

-- | /See:/ 'deleteStageResponse' smart constructor.
data DeleteStageResponse =
  DeleteStageResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteStageResponse' with the minimum fields required to make a request.
--
deleteStageResponse
    :: DeleteStageResponse
deleteStageResponse = DeleteStageResponse'


instance NFData DeleteStageResponse where
