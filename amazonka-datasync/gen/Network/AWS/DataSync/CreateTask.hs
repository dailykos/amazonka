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
-- Module      : Network.AWS.DataSync.CreateTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a task. A task is a set of two locations (source and destination) and a set of default @OverrideOptions@ that you use to control the behavior of a task. If you don't specify default values for @Options@ when you create a task, AWS DataSync populates them with safe service defaults.
--
--
-- When you initially create a task, it enters the INITIALIZING status and then the CREATING status. In CREATING status, AWS DataSync attempts to mount the source Network File System (NFS) location. The task transitions to the AVAILABLE status without waiting for the destination location to mount. Instead, AWS DataSync mounts a destination before every task execution and then unmounts it after every task execution. 
--
-- If an agent that is associated with a source (NFS) location goes offline, the task transitions to the UNAVAILABLE status. If the status of the task remains in the CREATING status for more than a few minutes, it means that your agent might be having trouble mounting the source NFS file system. Check the task's @ErrorCode@ and @ErrorDetail@ . Mount issues are often caused by either a misconfigured firewall or a mistyped NFS server host name.
--
module Network.AWS.DataSync.CreateTask
    (
    -- * Creating a Request
      createTask
    , CreateTask
    -- * Request Lenses
    , ctName
    , ctOptions
    , ctCloudWatchLogGroupARN
    , ctTags
    , ctSourceLocationARN
    , ctDestinationLocationARN

    -- * Destructuring the Response
    , createTaskResponse
    , CreateTaskResponse
    -- * Response Lenses
    , ctrsTaskARN
    , ctrsResponseStatus
    ) where

import Network.AWS.DataSync.Types
import Network.AWS.DataSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | CreateTaskRequest
--
--
--
-- /See:/ 'createTask' smart constructor.
data CreateTask = CreateTask'
  { _ctName :: !(Maybe Text)
  , _ctOptions :: !(Maybe Options)
  , _ctCloudWatchLogGroupARN :: !(Maybe Text)
  , _ctTags :: !(Maybe [TagListEntry])
  , _ctSourceLocationARN :: !Text
  , _ctDestinationLocationARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctName' - The name of a task. This value is a text reference that is used to identify the task in the console. 
--
-- * 'ctOptions' - The set of configuration options that control the behavior of a single execution of the task that occurs when you call @StartTaskExecution@ . You can configure these options to preserve metadata such as user ID (UID) and group ID (GID), file permissions, data integrity verification, and so on. For each individual task execution, you can override these options by specifying the @OverrideOptions@ before starting a the task execution. For more information, see the operation. 
--
-- * 'ctCloudWatchLogGroupARN' - The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that is used to monitor and log events in the task. For more information on these groups, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams> in the /Amazon CloudWatch User Guide. /  For more information about how to useCloudWatchLogs with DataSync, see <https://docs.aws.amazon.com/datasync/latest/userguide/monitor-datasync.html Monitoring Your Task> .
--
-- * 'ctTags' - The key-value pair that represents the tag that you want to add to the resource. The value can be an empty string. 
--
-- * 'ctSourceLocationARN' - The Amazon Resource Name (ARN) of the source location for the task.
--
-- * 'ctDestinationLocationARN' - The Amazon Resource Name (ARN) of an AWS storage resource's location. 
createTask
    :: Text -- ^ 'ctSourceLocationARN'
    -> Text -- ^ 'ctDestinationLocationARN'
    -> CreateTask
createTask pSourceLocationARN_ pDestinationLocationARN_ =
  CreateTask'
    { _ctName = Nothing
    , _ctOptions = Nothing
    , _ctCloudWatchLogGroupARN = Nothing
    , _ctTags = Nothing
    , _ctSourceLocationARN = pSourceLocationARN_
    , _ctDestinationLocationARN = pDestinationLocationARN_
    }


-- | The name of a task. This value is a text reference that is used to identify the task in the console. 
ctName :: Lens' CreateTask (Maybe Text)
ctName = lens _ctName (\ s a -> s{_ctName = a})

-- | The set of configuration options that control the behavior of a single execution of the task that occurs when you call @StartTaskExecution@ . You can configure these options to preserve metadata such as user ID (UID) and group ID (GID), file permissions, data integrity verification, and so on. For each individual task execution, you can override these options by specifying the @OverrideOptions@ before starting a the task execution. For more information, see the operation. 
ctOptions :: Lens' CreateTask (Maybe Options)
ctOptions = lens _ctOptions (\ s a -> s{_ctOptions = a})

-- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that is used to monitor and log events in the task. For more information on these groups, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams> in the /Amazon CloudWatch User Guide. /  For more information about how to useCloudWatchLogs with DataSync, see <https://docs.aws.amazon.com/datasync/latest/userguide/monitor-datasync.html Monitoring Your Task> .
ctCloudWatchLogGroupARN :: Lens' CreateTask (Maybe Text)
ctCloudWatchLogGroupARN = lens _ctCloudWatchLogGroupARN (\ s a -> s{_ctCloudWatchLogGroupARN = a})

-- | The key-value pair that represents the tag that you want to add to the resource. The value can be an empty string. 
ctTags :: Lens' CreateTask [TagListEntry]
ctTags = lens _ctTags (\ s a -> s{_ctTags = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the source location for the task.
ctSourceLocationARN :: Lens' CreateTask Text
ctSourceLocationARN = lens _ctSourceLocationARN (\ s a -> s{_ctSourceLocationARN = a})

-- | The Amazon Resource Name (ARN) of an AWS storage resource's location. 
ctDestinationLocationARN :: Lens' CreateTask Text
ctDestinationLocationARN = lens _ctDestinationLocationARN (\ s a -> s{_ctDestinationLocationARN = a})

instance AWSRequest CreateTask where
        type Rs CreateTask = CreateTaskResponse
        request = postJSON dataSync
        response
          = receiveJSON
              (\ s h x ->
                 CreateTaskResponse' <$>
                   (x .?> "TaskArn") <*> (pure (fromEnum s)))

instance Hashable CreateTask where

instance NFData CreateTask where

instance ToHeaders CreateTask where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("FmrsService.CreateTask" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateTask where
        toJSON CreateTask'{..}
          = object
              (catMaybes
                 [("Name" .=) <$> _ctName,
                  ("Options" .=) <$> _ctOptions,
                  ("CloudWatchLogGroupArn" .=) <$>
                    _ctCloudWatchLogGroupARN,
                  ("Tags" .=) <$> _ctTags,
                  Just ("SourceLocationArn" .= _ctSourceLocationARN),
                  Just
                    ("DestinationLocationArn" .=
                       _ctDestinationLocationARN)])

instance ToPath CreateTask where
        toPath = const "/"

instance ToQuery CreateTask where
        toQuery = const mempty

-- | CreateTaskResponse
--
--
--
-- /See:/ 'createTaskResponse' smart constructor.
data CreateTaskResponse = CreateTaskResponse'
  { _ctrsTaskARN :: !(Maybe Text)
  , _ctrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctrsTaskARN' - The Amazon Resource Name (ARN) of the task.
--
-- * 'ctrsResponseStatus' - -- | The response status code.
createTaskResponse
    :: Int -- ^ 'ctrsResponseStatus'
    -> CreateTaskResponse
createTaskResponse pResponseStatus_ =
  CreateTaskResponse'
    {_ctrsTaskARN = Nothing, _ctrsResponseStatus = pResponseStatus_}


-- | The Amazon Resource Name (ARN) of the task.
ctrsTaskARN :: Lens' CreateTaskResponse (Maybe Text)
ctrsTaskARN = lens _ctrsTaskARN (\ s a -> s{_ctrsTaskARN = a})

-- | -- | The response status code.
ctrsResponseStatus :: Lens' CreateTaskResponse Int
ctrsResponseStatus = lens _ctrsResponseStatus (\ s a -> s{_ctrsResponseStatus = a})

instance NFData CreateTaskResponse where
