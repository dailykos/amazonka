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
-- Module      : Network.AWS.DataSync.StartTaskExecution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a specific invocation of a task. A @TaskExecution@ value represents an individual run of a task. Each task can have at most one @TaskExecution@ at a time.
--
--
-- @TaskExecution@ has the following transition phases: INITIALIZING | PREPARING | TRANSFERRING | VERIFYING | SUCCESS/FAILURE. 
--
-- For detailed information, see /Task Execution/ in <https://docs.aws.amazon.com/sync-service/latest/userguide/how-awssync-works.html#terminology Components and Terminology> in the /AWS DataSync User Guide/ .
--
module Network.AWS.DataSync.StartTaskExecution
    (
    -- * Creating a Request
      startTaskExecution
    , StartTaskExecution
    -- * Request Lenses
    , steOverrideOptions
    , steTaskARN

    -- * Destructuring the Response
    , startTaskExecutionResponse
    , StartTaskExecutionResponse
    -- * Response Lenses
    , stersTaskExecutionARN
    , stersResponseStatus
    ) where

import Network.AWS.DataSync.Types
import Network.AWS.DataSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | StartTaskExecutionRequest
--
--
--
-- /See:/ 'startTaskExecution' smart constructor.
data StartTaskExecution = StartTaskExecution'
  { _steOverrideOptions :: !(Maybe Options)
  , _steTaskARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartTaskExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'steOverrideOptions' - Undocumented member.
--
-- * 'steTaskARN' - The Amazon Resource Name (ARN) of the task to start.
startTaskExecution
    :: Text -- ^ 'steTaskARN'
    -> StartTaskExecution
startTaskExecution pTaskARN_ =
  StartTaskExecution' {_steOverrideOptions = Nothing, _steTaskARN = pTaskARN_}


-- | Undocumented member.
steOverrideOptions :: Lens' StartTaskExecution (Maybe Options)
steOverrideOptions = lens _steOverrideOptions (\ s a -> s{_steOverrideOptions = a})

-- | The Amazon Resource Name (ARN) of the task to start.
steTaskARN :: Lens' StartTaskExecution Text
steTaskARN = lens _steTaskARN (\ s a -> s{_steTaskARN = a})

instance AWSRequest StartTaskExecution where
        type Rs StartTaskExecution =
             StartTaskExecutionResponse
        request = postJSON dataSync
        response
          = receiveJSON
              (\ s h x ->
                 StartTaskExecutionResponse' <$>
                   (x .?> "TaskExecutionArn") <*> (pure (fromEnum s)))

instance Hashable StartTaskExecution where

instance NFData StartTaskExecution where

instance ToHeaders StartTaskExecution where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("FmrsService.StartTaskExecution" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartTaskExecution where
        toJSON StartTaskExecution'{..}
          = object
              (catMaybes
                 [("OverrideOptions" .=) <$> _steOverrideOptions,
                  Just ("TaskArn" .= _steTaskARN)])

instance ToPath StartTaskExecution where
        toPath = const "/"

instance ToQuery StartTaskExecution where
        toQuery = const mempty

-- | StartTaskExecutionResponse
--
--
--
-- /See:/ 'startTaskExecutionResponse' smart constructor.
data StartTaskExecutionResponse = StartTaskExecutionResponse'
  { _stersTaskExecutionARN :: !(Maybe Text)
  , _stersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartTaskExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stersTaskExecutionARN' - The Amazon Resource Name (ARN) of the specific task execution that was started.
--
-- * 'stersResponseStatus' - -- | The response status code.
startTaskExecutionResponse
    :: Int -- ^ 'stersResponseStatus'
    -> StartTaskExecutionResponse
startTaskExecutionResponse pResponseStatus_ =
  StartTaskExecutionResponse'
    {_stersTaskExecutionARN = Nothing, _stersResponseStatus = pResponseStatus_}


-- | The Amazon Resource Name (ARN) of the specific task execution that was started.
stersTaskExecutionARN :: Lens' StartTaskExecutionResponse (Maybe Text)
stersTaskExecutionARN = lens _stersTaskExecutionARN (\ s a -> s{_stersTaskExecutionARN = a})

-- | -- | The response status code.
stersResponseStatus :: Lens' StartTaskExecutionResponse Int
stersResponseStatus = lens _stersResponseStatus (\ s a -> s{_stersResponseStatus = a})

instance NFData StartTaskExecutionResponse where
