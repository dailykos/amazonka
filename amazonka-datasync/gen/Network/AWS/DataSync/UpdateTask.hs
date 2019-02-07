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
-- Module      : Network.AWS.DataSync.UpdateTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the metadata associated with a task.
--
--
module Network.AWS.DataSync.UpdateTask
    (
    -- * Creating a Request
      updateTask
    , UpdateTask
    -- * Request Lenses
    , utName
    , utOptions
    , utTaskARN

    -- * Destructuring the Response
    , updateTaskResponse
    , UpdateTaskResponse
    -- * Response Lenses
    , utrsResponseStatus
    ) where

import Network.AWS.DataSync.Types
import Network.AWS.DataSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | UpdateTaskResponse
--
--
--
-- /See:/ 'updateTask' smart constructor.
data UpdateTask = UpdateTask'
  { _utName :: !(Maybe Text)
  , _utOptions :: !(Maybe Options)
  , _utTaskARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utName' - The name of the task to update.
--
-- * 'utOptions' - Undocumented member.
--
-- * 'utTaskARN' - The Amazon Resource Name (ARN) of the resource name of the task to update.
updateTask
    :: Text -- ^ 'utTaskARN'
    -> UpdateTask
updateTask pTaskARN_ =
  UpdateTask' {_utName = Nothing, _utOptions = Nothing, _utTaskARN = pTaskARN_}


-- | The name of the task to update.
utName :: Lens' UpdateTask (Maybe Text)
utName = lens _utName (\ s a -> s{_utName = a})

-- | Undocumented member.
utOptions :: Lens' UpdateTask (Maybe Options)
utOptions = lens _utOptions (\ s a -> s{_utOptions = a})

-- | The Amazon Resource Name (ARN) of the resource name of the task to update.
utTaskARN :: Lens' UpdateTask Text
utTaskARN = lens _utTaskARN (\ s a -> s{_utTaskARN = a})

instance AWSRequest UpdateTask where
        type Rs UpdateTask = UpdateTaskResponse
        request = postJSON dataSync
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateTaskResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateTask where

instance NFData UpdateTask where

instance ToHeaders UpdateTask where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("FmrsService.UpdateTask" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateTask where
        toJSON UpdateTask'{..}
          = object
              (catMaybes
                 [("Name" .=) <$> _utName,
                  ("Options" .=) <$> _utOptions,
                  Just ("TaskArn" .= _utTaskARN)])

instance ToPath UpdateTask where
        toPath = const "/"

instance ToQuery UpdateTask where
        toQuery = const mempty

-- | /See:/ 'updateTaskResponse' smart constructor.
newtype UpdateTaskResponse = UpdateTaskResponse'
  { _utrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utrsResponseStatus' - -- | The response status code.
updateTaskResponse
    :: Int -- ^ 'utrsResponseStatus'
    -> UpdateTaskResponse
updateTaskResponse pResponseStatus_ =
  UpdateTaskResponse' {_utrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
utrsResponseStatus :: Lens' UpdateTaskResponse Int
utrsResponseStatus = lens _utrsResponseStatus (\ s a -> s{_utrsResponseStatus = a})

instance NFData UpdateTaskResponse where
