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
-- Module      : Network.AWS.DataSync.DescribeTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata about a task.
--
--
module Network.AWS.DataSync.DescribeTask
    (
    -- * Creating a Request
      describeTask
    , DescribeTask
    -- * Request Lenses
    , dTaskARN

    -- * Destructuring the Response
    , describeTaskResponse
    , DescribeTaskResponse
    -- * Response Lenses
    , drsCreationTime
    , drsStatus
    , drsTaskARN
    , drsCurrentTaskExecutionARN
    , drsDestinationLocationARN
    , drsName
    , drsErrorCode
    , drsSourceLocationARN
    , drsOptions
    , drsCloudWatchLogGroupARN
    , drsErrorDetail
    , drsResponseStatus
    ) where

import Network.AWS.DataSync.Types
import Network.AWS.DataSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | DescribeTaskRequest
--
--
--
-- /See:/ 'describeTask' smart constructor.
newtype DescribeTask = DescribeTask'
  { _dTaskARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dTaskARN' - The Amazon Resource Name (ARN) of the task to describe.
describeTask
    :: Text -- ^ 'dTaskARN'
    -> DescribeTask
describeTask pTaskARN_ = DescribeTask' {_dTaskARN = pTaskARN_}


-- | The Amazon Resource Name (ARN) of the task to describe.
dTaskARN :: Lens' DescribeTask Text
dTaskARN = lens _dTaskARN (\ s a -> s{_dTaskARN = a})

instance AWSRequest DescribeTask where
        type Rs DescribeTask = DescribeTaskResponse
        request = postJSON dataSync
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTaskResponse' <$>
                   (x .?> "CreationTime") <*> (x .?> "Status") <*>
                     (x .?> "TaskArn")
                     <*> (x .?> "CurrentTaskExecutionArn")
                     <*> (x .?> "DestinationLocationArn")
                     <*> (x .?> "Name")
                     <*> (x .?> "ErrorCode")
                     <*> (x .?> "SourceLocationArn")
                     <*> (x .?> "Options")
                     <*> (x .?> "CloudWatchLogGroupArn")
                     <*> (x .?> "ErrorDetail")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeTask where

instance NFData DescribeTask where

instance ToHeaders DescribeTask where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("FmrsService.DescribeTask" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeTask where
        toJSON DescribeTask'{..}
          = object (catMaybes [Just ("TaskArn" .= _dTaskARN)])

instance ToPath DescribeTask where
        toPath = const "/"

instance ToQuery DescribeTask where
        toQuery = const mempty

-- | DescribeTaskResponse
--
--
--
-- /See:/ 'describeTaskResponse' smart constructor.
data DescribeTaskResponse = DescribeTaskResponse'
  { _drsCreationTime :: !(Maybe POSIX)
  , _drsStatus :: !(Maybe TaskStatus)
  , _drsTaskARN :: !(Maybe Text)
  , _drsCurrentTaskExecutionARN :: !(Maybe Text)
  , _drsDestinationLocationARN :: !(Maybe Text)
  , _drsName :: !(Maybe Text)
  , _drsErrorCode :: !(Maybe Text)
  , _drsSourceLocationARN :: !(Maybe Text)
  , _drsOptions :: !(Maybe Options)
  , _drsCloudWatchLogGroupARN :: !(Maybe Text)
  , _drsErrorDetail :: !(Maybe Text)
  , _drsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsCreationTime' - The time that the task was created.
--
-- * 'drsStatus' - The status of the task that was described. For detailed information about sync statuses, see <https://docs.aws.amazon.com/sync-service/latest/userguide/understand-sync-task-statuses.html Understanding Sync Task Statuses> .
--
-- * 'drsTaskARN' - The Amazon Resource Name (ARN) of the task that was described.
--
-- * 'drsCurrentTaskExecutionARN' - The Amazon Resource Name (ARN) of the task execution that is syncing files.
--
-- * 'drsDestinationLocationARN' - The Amazon Resource Name (ARN) of the AWS storage resource's location.
--
-- * 'drsName' - The name of the task that was described.
--
-- * 'drsErrorCode' - Errors that AWS DataSync encountered during execution of the task. You can use this error code to help troubleshoot issues.
--
-- * 'drsSourceLocationARN' - The Amazon Resource Name (ARN) of the source file system's location.
--
-- * 'drsOptions' - The set of configuration options that control the behavior of a single execution of the task that occurs when you call @StartTaskExecution@ . You can configure these options to preserve metadata such as user ID (UID) and group (GID), file permissions, data integrity verification, and so on. For each individual task execution, you can override these options by specifying the overriding @OverrideOptions@ value to operation. 
--
-- * 'drsCloudWatchLogGroupARN' - The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that was used to monitor and log events in the task. For more information on these groups, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams> in the /Amazon CloudWatch User Guide./ 
--
-- * 'drsErrorDetail' - Detailed description of an error that was encountered during the task execution. You can use this information to help troubleshoot issues. 
--
-- * 'drsResponseStatus' - -- | The response status code.
describeTaskResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeTaskResponse
describeTaskResponse pResponseStatus_ =
  DescribeTaskResponse'
    { _drsCreationTime = Nothing
    , _drsStatus = Nothing
    , _drsTaskARN = Nothing
    , _drsCurrentTaskExecutionARN = Nothing
    , _drsDestinationLocationARN = Nothing
    , _drsName = Nothing
    , _drsErrorCode = Nothing
    , _drsSourceLocationARN = Nothing
    , _drsOptions = Nothing
    , _drsCloudWatchLogGroupARN = Nothing
    , _drsErrorDetail = Nothing
    , _drsResponseStatus = pResponseStatus_
    }


-- | The time that the task was created.
drsCreationTime :: Lens' DescribeTaskResponse (Maybe UTCTime)
drsCreationTime = lens _drsCreationTime (\ s a -> s{_drsCreationTime = a}) . mapping _Time

-- | The status of the task that was described. For detailed information about sync statuses, see <https://docs.aws.amazon.com/sync-service/latest/userguide/understand-sync-task-statuses.html Understanding Sync Task Statuses> .
drsStatus :: Lens' DescribeTaskResponse (Maybe TaskStatus)
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a})

-- | The Amazon Resource Name (ARN) of the task that was described.
drsTaskARN :: Lens' DescribeTaskResponse (Maybe Text)
drsTaskARN = lens _drsTaskARN (\ s a -> s{_drsTaskARN = a})

-- | The Amazon Resource Name (ARN) of the task execution that is syncing files.
drsCurrentTaskExecutionARN :: Lens' DescribeTaskResponse (Maybe Text)
drsCurrentTaskExecutionARN = lens _drsCurrentTaskExecutionARN (\ s a -> s{_drsCurrentTaskExecutionARN = a})

-- | The Amazon Resource Name (ARN) of the AWS storage resource's location.
drsDestinationLocationARN :: Lens' DescribeTaskResponse (Maybe Text)
drsDestinationLocationARN = lens _drsDestinationLocationARN (\ s a -> s{_drsDestinationLocationARN = a})

-- | The name of the task that was described.
drsName :: Lens' DescribeTaskResponse (Maybe Text)
drsName = lens _drsName (\ s a -> s{_drsName = a})

-- | Errors that AWS DataSync encountered during execution of the task. You can use this error code to help troubleshoot issues.
drsErrorCode :: Lens' DescribeTaskResponse (Maybe Text)
drsErrorCode = lens _drsErrorCode (\ s a -> s{_drsErrorCode = a})

-- | The Amazon Resource Name (ARN) of the source file system's location.
drsSourceLocationARN :: Lens' DescribeTaskResponse (Maybe Text)
drsSourceLocationARN = lens _drsSourceLocationARN (\ s a -> s{_drsSourceLocationARN = a})

-- | The set of configuration options that control the behavior of a single execution of the task that occurs when you call @StartTaskExecution@ . You can configure these options to preserve metadata such as user ID (UID) and group (GID), file permissions, data integrity verification, and so on. For each individual task execution, you can override these options by specifying the overriding @OverrideOptions@ value to operation. 
drsOptions :: Lens' DescribeTaskResponse (Maybe Options)
drsOptions = lens _drsOptions (\ s a -> s{_drsOptions = a})

-- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that was used to monitor and log events in the task. For more information on these groups, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams> in the /Amazon CloudWatch User Guide./ 
drsCloudWatchLogGroupARN :: Lens' DescribeTaskResponse (Maybe Text)
drsCloudWatchLogGroupARN = lens _drsCloudWatchLogGroupARN (\ s a -> s{_drsCloudWatchLogGroupARN = a})

-- | Detailed description of an error that was encountered during the task execution. You can use this information to help troubleshoot issues. 
drsErrorDetail :: Lens' DescribeTaskResponse (Maybe Text)
drsErrorDetail = lens _drsErrorDetail (\ s a -> s{_drsErrorDetail = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeTaskResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeTaskResponse where
