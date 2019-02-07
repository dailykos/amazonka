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
-- Module      : Network.AWS.DataSync.DeleteLocation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the configuration of a location used by AWS DataSync. 
--
--
module Network.AWS.DataSync.DeleteLocation
    (
    -- * Creating a Request
      deleteLocation
    , DeleteLocation
    -- * Request Lenses
    , dlLocationARN

    -- * Destructuring the Response
    , deleteLocationResponse
    , DeleteLocationResponse
    -- * Response Lenses
    , dlrsResponseStatus
    ) where

import Network.AWS.DataSync.Types
import Network.AWS.DataSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | DeleteLocation
--
--
--
-- /See:/ 'deleteLocation' smart constructor.
newtype DeleteLocation = DeleteLocation'
  { _dlLocationARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlLocationARN' - The Amazon Resource Name (ARN) of the location to delete.
deleteLocation
    :: Text -- ^ 'dlLocationARN'
    -> DeleteLocation
deleteLocation pLocationARN_ = DeleteLocation' {_dlLocationARN = pLocationARN_}


-- | The Amazon Resource Name (ARN) of the location to delete.
dlLocationARN :: Lens' DeleteLocation Text
dlLocationARN = lens _dlLocationARN (\ s a -> s{_dlLocationARN = a})

instance AWSRequest DeleteLocation where
        type Rs DeleteLocation = DeleteLocationResponse
        request = postJSON dataSync
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteLocationResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteLocation where

instance NFData DeleteLocation where

instance ToHeaders DeleteLocation where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("FmrsService.DeleteLocation" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteLocation where
        toJSON DeleteLocation'{..}
          = object
              (catMaybes [Just ("LocationArn" .= _dlLocationARN)])

instance ToPath DeleteLocation where
        toPath = const "/"

instance ToQuery DeleteLocation where
        toQuery = const mempty

-- | /See:/ 'deleteLocationResponse' smart constructor.
newtype DeleteLocationResponse = DeleteLocationResponse'
  { _dlrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLocationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlrsResponseStatus' - -- | The response status code.
deleteLocationResponse
    :: Int -- ^ 'dlrsResponseStatus'
    -> DeleteLocationResponse
deleteLocationResponse pResponseStatus_ =
  DeleteLocationResponse' {_dlrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dlrsResponseStatus :: Lens' DeleteLocationResponse Int
dlrsResponseStatus = lens _dlrsResponseStatus (\ s a -> s{_dlrsResponseStatus = a})

instance NFData DeleteLocationResponse where
