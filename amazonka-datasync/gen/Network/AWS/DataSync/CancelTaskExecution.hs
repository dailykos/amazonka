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
-- Module      : Network.AWS.DataSync.CancelTaskExecution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels execution of a task. 
--
--
-- When you cancel a task execution, the transfer of some files are abruptly interrupted. The contents of files that are transferred to the destination might be incomplete or inconsistent with the source files. However, if you start a new task execution on the same task and you allow the task execution to complete, file content on the destination is complete and consistent. This applies to other unexpected failures that interrupt a task execution. In all of these cases, AWS DataSync successfully complete the transfer when you start the next task execution. 
--
module Network.AWS.DataSync.CancelTaskExecution
    (
    -- * Creating a Request
      cancelTaskExecution
    , CancelTaskExecution
    -- * Request Lenses
    , cteTaskExecutionARN

    -- * Destructuring the Response
    , cancelTaskExecutionResponse
    , CancelTaskExecutionResponse
    -- * Response Lenses
    , ctersResponseStatus
    ) where

import Network.AWS.DataSync.Types
import Network.AWS.DataSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | CancelTaskExecutionRequest
--
--
--
-- /See:/ 'cancelTaskExecution' smart constructor.
newtype CancelTaskExecution = CancelTaskExecution'
  { _cteTaskExecutionARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelTaskExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cteTaskExecutionARN' - The Amazon Resource Name (ARN) of the task execution to cancel.
cancelTaskExecution
    :: Text -- ^ 'cteTaskExecutionARN'
    -> CancelTaskExecution
cancelTaskExecution pTaskExecutionARN_ =
  CancelTaskExecution' {_cteTaskExecutionARN = pTaskExecutionARN_}


-- | The Amazon Resource Name (ARN) of the task execution to cancel.
cteTaskExecutionARN :: Lens' CancelTaskExecution Text
cteTaskExecutionARN = lens _cteTaskExecutionARN (\ s a -> s{_cteTaskExecutionARN = a})

instance AWSRequest CancelTaskExecution where
        type Rs CancelTaskExecution =
             CancelTaskExecutionResponse
        request = postJSON dataSync
        response
          = receiveEmpty
              (\ s h x ->
                 CancelTaskExecutionResponse' <$> (pure (fromEnum s)))

instance Hashable CancelTaskExecution where

instance NFData CancelTaskExecution where

instance ToHeaders CancelTaskExecution where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("FmrsService.CancelTaskExecution" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CancelTaskExecution where
        toJSON CancelTaskExecution'{..}
          = object
              (catMaybes
                 [Just ("TaskExecutionArn" .= _cteTaskExecutionARN)])

instance ToPath CancelTaskExecution where
        toPath = const "/"

instance ToQuery CancelTaskExecution where
        toQuery = const mempty

-- | /See:/ 'cancelTaskExecutionResponse' smart constructor.
newtype CancelTaskExecutionResponse = CancelTaskExecutionResponse'
  { _ctersResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelTaskExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctersResponseStatus' - -- | The response status code.
cancelTaskExecutionResponse
    :: Int -- ^ 'ctersResponseStatus'
    -> CancelTaskExecutionResponse
cancelTaskExecutionResponse pResponseStatus_ =
  CancelTaskExecutionResponse' {_ctersResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ctersResponseStatus :: Lens' CancelTaskExecutionResponse Int
ctersResponseStatus = lens _ctersResponseStatus (\ s a -> s{_ctersResponseStatus = a})

instance NFData CancelTaskExecutionResponse where
