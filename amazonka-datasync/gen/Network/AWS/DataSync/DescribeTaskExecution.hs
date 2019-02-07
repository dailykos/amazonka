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
-- Module      : Network.AWS.DataSync.DescribeTaskExecution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed metadata about a task that is being executed.
--
--
module Network.AWS.DataSync.DescribeTaskExecution
    (
    -- * Creating a Request
      describeTaskExecution
    , DescribeTaskExecution
    -- * Request Lenses
    , dteTaskExecutionARN

    -- * Destructuring the Response
    , describeTaskExecutionResponse
    , DescribeTaskExecutionResponse
    -- * Response Lenses
    , dtersStatus
    , dtersTaskExecutionARN
    , dtersStartTime
    , dtersFilesTransferred
    , dtersBytesWritten
    , dtersBytesTransferred
    , dtersResult
    , dtersEstimatedFilesToTransfer
    , dtersOptions
    , dtersEstimatedBytesToTransfer
    , dtersResponseStatus
    ) where

import Network.AWS.DataSync.Types
import Network.AWS.DataSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | DescribeTaskExecutionRequest
--
--
--
-- /See:/ 'describeTaskExecution' smart constructor.
newtype DescribeTaskExecution = DescribeTaskExecution'
  { _dteTaskExecutionARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTaskExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dteTaskExecutionARN' - The Amazon Resource Name (ARN) of the task that is being executed.
describeTaskExecution
    :: Text -- ^ 'dteTaskExecutionARN'
    -> DescribeTaskExecution
describeTaskExecution pTaskExecutionARN_ =
  DescribeTaskExecution' {_dteTaskExecutionARN = pTaskExecutionARN_}


-- | The Amazon Resource Name (ARN) of the task that is being executed.
dteTaskExecutionARN :: Lens' DescribeTaskExecution Text
dteTaskExecutionARN = lens _dteTaskExecutionARN (\ s a -> s{_dteTaskExecutionARN = a})

instance AWSRequest DescribeTaskExecution where
        type Rs DescribeTaskExecution =
             DescribeTaskExecutionResponse
        request = postJSON dataSync
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTaskExecutionResponse' <$>
                   (x .?> "Status") <*> (x .?> "TaskExecutionArn") <*>
                     (x .?> "StartTime")
                     <*> (x .?> "FilesTransferred")
                     <*> (x .?> "BytesWritten")
                     <*> (x .?> "BytesTransferred")
                     <*> (x .?> "Result")
                     <*> (x .?> "EstimatedFilesToTransfer")
                     <*> (x .?> "Options")
                     <*> (x .?> "EstimatedBytesToTransfer")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeTaskExecution where

instance NFData DescribeTaskExecution where

instance ToHeaders DescribeTaskExecution where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("FmrsService.DescribeTaskExecution" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeTaskExecution where
        toJSON DescribeTaskExecution'{..}
          = object
              (catMaybes
                 [Just ("TaskExecutionArn" .= _dteTaskExecutionARN)])

instance ToPath DescribeTaskExecution where
        toPath = const "/"

instance ToQuery DescribeTaskExecution where
        toQuery = const mempty

-- | DescribeTaskExecutionResponse
--
--
--
-- /See:/ 'describeTaskExecutionResponse' smart constructor.
data DescribeTaskExecutionResponse = DescribeTaskExecutionResponse'
  { _dtersStatus :: !(Maybe TaskExecutionStatus)
  , _dtersTaskExecutionARN :: !(Maybe Text)
  , _dtersStartTime :: !(Maybe POSIX)
  , _dtersFilesTransferred :: !(Maybe Integer)
  , _dtersBytesWritten :: !(Maybe Integer)
  , _dtersBytesTransferred :: !(Maybe Integer)
  , _dtersResult :: !(Maybe TaskExecutionResultDetail)
  , _dtersEstimatedFilesToTransfer :: !(Maybe Integer)
  , _dtersOptions :: !(Maybe Options)
  , _dtersEstimatedBytesToTransfer :: !(Maybe Integer)
  , _dtersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTaskExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtersStatus' - The status of the task. For detailed information about sync statuses, see <https://docs.aws.amazon.com/sync-service/latest/userguide/understand-sync-task-statuses.html Understanding Sync Task Statuses> .
--
-- * 'dtersTaskExecutionARN' - The Amazon Resource Name (ARN) of the task execution that was described. @TaskExecutionArn@ is hierarchical and includes @TaskArn@ for the task that was executed.  For example, a @TaskExecution@ value with the ARN @arn:aws:sync:us-east-1:209870788375:task/task-0208075f79cedf4a2/execution/exec-08ef1e88ec491019b@ executed the task with the ARN @arn:aws:sync:us-east-1:209870788375:task/task-0208075f79cedf4a2@ . 
--
-- * 'dtersStartTime' - The time that the task execution was started.
--
-- * 'dtersFilesTransferred' - The actual number of files that was transferred over the network. This value is calculated and updated on an ongoing basis during the TRANSFERRING phase. It's updated periodically when each file is read from the source and sent over the network.  If failures occur during a transfer, this value can be less than @EstimatedFilesToTransfer@ . This value can also be greater than @EstimatedFilesTransferred@ in some cases. This element is implementation-specific for some location types, so don't use it as an indicator for a correct file number or to monitor your task execution.
--
-- * 'dtersBytesWritten' - The number of logical bytes written to the destination AWS storage resource.
--
-- * 'dtersBytesTransferred' - The physical number of bytes transferred over the network.
--
-- * 'dtersResult' - The result of the task execution.
--
-- * 'dtersEstimatedFilesToTransfer' - The expected number of files that is to be transferred over the network. This value is calculated during the PREPARING phase, before the TRANSFERRING phase. This value is the expected number of files to be transferred. It's calculated based on comparing the content of the source and destination locations and finding the delta that needs to be transferred. 
--
-- * 'dtersOptions' - Undocumented member.
--
-- * 'dtersEstimatedBytesToTransfer' - The estimated physical number of bytes that is to be transferred over the network.
--
-- * 'dtersResponseStatus' - -- | The response status code.
describeTaskExecutionResponse
    :: Int -- ^ 'dtersResponseStatus'
    -> DescribeTaskExecutionResponse
describeTaskExecutionResponse pResponseStatus_ =
  DescribeTaskExecutionResponse'
    { _dtersStatus = Nothing
    , _dtersTaskExecutionARN = Nothing
    , _dtersStartTime = Nothing
    , _dtersFilesTransferred = Nothing
    , _dtersBytesWritten = Nothing
    , _dtersBytesTransferred = Nothing
    , _dtersResult = Nothing
    , _dtersEstimatedFilesToTransfer = Nothing
    , _dtersOptions = Nothing
    , _dtersEstimatedBytesToTransfer = Nothing
    , _dtersResponseStatus = pResponseStatus_
    }


-- | The status of the task. For detailed information about sync statuses, see <https://docs.aws.amazon.com/sync-service/latest/userguide/understand-sync-task-statuses.html Understanding Sync Task Statuses> .
dtersStatus :: Lens' DescribeTaskExecutionResponse (Maybe TaskExecutionStatus)
dtersStatus = lens _dtersStatus (\ s a -> s{_dtersStatus = a})

-- | The Amazon Resource Name (ARN) of the task execution that was described. @TaskExecutionArn@ is hierarchical and includes @TaskArn@ for the task that was executed.  For example, a @TaskExecution@ value with the ARN @arn:aws:sync:us-east-1:209870788375:task/task-0208075f79cedf4a2/execution/exec-08ef1e88ec491019b@ executed the task with the ARN @arn:aws:sync:us-east-1:209870788375:task/task-0208075f79cedf4a2@ . 
dtersTaskExecutionARN :: Lens' DescribeTaskExecutionResponse (Maybe Text)
dtersTaskExecutionARN = lens _dtersTaskExecutionARN (\ s a -> s{_dtersTaskExecutionARN = a})

-- | The time that the task execution was started.
dtersStartTime :: Lens' DescribeTaskExecutionResponse (Maybe UTCTime)
dtersStartTime = lens _dtersStartTime (\ s a -> s{_dtersStartTime = a}) . mapping _Time

-- | The actual number of files that was transferred over the network. This value is calculated and updated on an ongoing basis during the TRANSFERRING phase. It's updated periodically when each file is read from the source and sent over the network.  If failures occur during a transfer, this value can be less than @EstimatedFilesToTransfer@ . This value can also be greater than @EstimatedFilesTransferred@ in some cases. This element is implementation-specific for some location types, so don't use it as an indicator for a correct file number or to monitor your task execution.
dtersFilesTransferred :: Lens' DescribeTaskExecutionResponse (Maybe Integer)
dtersFilesTransferred = lens _dtersFilesTransferred (\ s a -> s{_dtersFilesTransferred = a})

-- | The number of logical bytes written to the destination AWS storage resource.
dtersBytesWritten :: Lens' DescribeTaskExecutionResponse (Maybe Integer)
dtersBytesWritten = lens _dtersBytesWritten (\ s a -> s{_dtersBytesWritten = a})

-- | The physical number of bytes transferred over the network.
dtersBytesTransferred :: Lens' DescribeTaskExecutionResponse (Maybe Integer)
dtersBytesTransferred = lens _dtersBytesTransferred (\ s a -> s{_dtersBytesTransferred = a})

-- | The result of the task execution.
dtersResult :: Lens' DescribeTaskExecutionResponse (Maybe TaskExecutionResultDetail)
dtersResult = lens _dtersResult (\ s a -> s{_dtersResult = a})

-- | The expected number of files that is to be transferred over the network. This value is calculated during the PREPARING phase, before the TRANSFERRING phase. This value is the expected number of files to be transferred. It's calculated based on comparing the content of the source and destination locations and finding the delta that needs to be transferred. 
dtersEstimatedFilesToTransfer :: Lens' DescribeTaskExecutionResponse (Maybe Integer)
dtersEstimatedFilesToTransfer = lens _dtersEstimatedFilesToTransfer (\ s a -> s{_dtersEstimatedFilesToTransfer = a})

-- | Undocumented member.
dtersOptions :: Lens' DescribeTaskExecutionResponse (Maybe Options)
dtersOptions = lens _dtersOptions (\ s a -> s{_dtersOptions = a})

-- | The estimated physical number of bytes that is to be transferred over the network.
dtersEstimatedBytesToTransfer :: Lens' DescribeTaskExecutionResponse (Maybe Integer)
dtersEstimatedBytesToTransfer = lens _dtersEstimatedBytesToTransfer (\ s a -> s{_dtersEstimatedBytesToTransfer = a})

-- | -- | The response status code.
dtersResponseStatus :: Lens' DescribeTaskExecutionResponse Int
dtersResponseStatus = lens _dtersResponseStatus (\ s a -> s{_dtersResponseStatus = a})

instance NFData DescribeTaskExecutionResponse where
