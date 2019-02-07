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
-- Module      : Network.AWS.DataSync.DeleteTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a task.
--
--
module Network.AWS.DataSync.DeleteTask
    (
    -- * Creating a Request
      deleteTask
    , DeleteTask
    -- * Request Lenses
    , dtTaskARN

    -- * Destructuring the Response
    , deleteTaskResponse
    , DeleteTaskResponse
    -- * Response Lenses
    , dtrsResponseStatus
    ) where

import Network.AWS.DataSync.Types
import Network.AWS.DataSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | DeleteTask
--
--
--
-- /See:/ 'deleteTask' smart constructor.
newtype DeleteTask = DeleteTask'
  { _dtTaskARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtTaskARN' - The Amazon Resource Name (ARN) of the task to delete.
deleteTask
    :: Text -- ^ 'dtTaskARN'
    -> DeleteTask
deleteTask pTaskARN_ = DeleteTask' {_dtTaskARN = pTaskARN_}


-- | The Amazon Resource Name (ARN) of the task to delete.
dtTaskARN :: Lens' DeleteTask Text
dtTaskARN = lens _dtTaskARN (\ s a -> s{_dtTaskARN = a})

instance AWSRequest DeleteTask where
        type Rs DeleteTask = DeleteTaskResponse
        request = postJSON dataSync
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteTaskResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteTask where

instance NFData DeleteTask where

instance ToHeaders DeleteTask where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("FmrsService.DeleteTask" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteTask where
        toJSON DeleteTask'{..}
          = object (catMaybes [Just ("TaskArn" .= _dtTaskARN)])

instance ToPath DeleteTask where
        toPath = const "/"

instance ToQuery DeleteTask where
        toQuery = const mempty

-- | /See:/ 'deleteTaskResponse' smart constructor.
newtype DeleteTaskResponse = DeleteTaskResponse'
  { _dtrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrsResponseStatus' - -- | The response status code.
deleteTaskResponse
    :: Int -- ^ 'dtrsResponseStatus'
    -> DeleteTaskResponse
deleteTaskResponse pResponseStatus_ =
  DeleteTaskResponse' {_dtrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dtrsResponseStatus :: Lens' DeleteTaskResponse Int
dtrsResponseStatus = lens _dtrsResponseStatus (\ s a -> s{_dtrsResponseStatus = a})

instance NFData DeleteTaskResponse where
