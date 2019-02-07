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
-- Module      : Network.AWS.Amplify.StartJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new job for a branch, part of an Amplify App. 
--
--
module Network.AWS.Amplify.StartJob
    (
    -- * Creating a Request
      startJob
    , StartJob
    -- * Request Lenses
    , sCommitId
    , sJobId
    , sJobReason
    , sCommitTime
    , sCommitMessage
    , sAppId
    , sBranchName
    , sJobType

    -- * Destructuring the Response
    , startJobResponse
    , StartJobResponse
    -- * Response Lenses
    , srsResponseStatus
    , srsJobSummary
    ) where

import Network.AWS.Amplify.Types
import Network.AWS.Amplify.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure for Start job request. 
--
--
--
-- /See:/ 'startJob' smart constructor.
data StartJob = StartJob'
  { _sCommitId :: !(Maybe Text)
  , _sJobId :: !(Maybe Text)
  , _sJobReason :: !(Maybe Text)
  , _sCommitTime :: !(Maybe POSIX)
  , _sCommitMessage :: !(Maybe Text)
  , _sAppId :: !Text
  , _sBranchName :: !Text
  , _sJobType :: !JobType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sCommitId' - Commit Id from 3rd party repository provider for the Job. 
--
-- * 'sJobId' - Unique Id for the Job. 
--
-- * 'sJobReason' - Reason for the Job. 
--
-- * 'sCommitTime' - Commit date / time for the Job. 
--
-- * 'sCommitMessage' - Commit message from 3rd party repository provider for the Job. 
--
-- * 'sAppId' - Unique Id for an Amplify App. 
--
-- * 'sBranchName' - Name for the branch, for the Job. 
--
-- * 'sJobType' - Type for the Job. 
startJob
    :: Text -- ^ 'sAppId'
    -> Text -- ^ 'sBranchName'
    -> JobType -- ^ 'sJobType'
    -> StartJob
startJob pAppId_ pBranchName_ pJobType_ =
  StartJob'
    { _sCommitId = Nothing
    , _sJobId = Nothing
    , _sJobReason = Nothing
    , _sCommitTime = Nothing
    , _sCommitMessage = Nothing
    , _sAppId = pAppId_
    , _sBranchName = pBranchName_
    , _sJobType = pJobType_
    }


-- | Commit Id from 3rd party repository provider for the Job. 
sCommitId :: Lens' StartJob (Maybe Text)
sCommitId = lens _sCommitId (\ s a -> s{_sCommitId = a})

-- | Unique Id for the Job. 
sJobId :: Lens' StartJob (Maybe Text)
sJobId = lens _sJobId (\ s a -> s{_sJobId = a})

-- | Reason for the Job. 
sJobReason :: Lens' StartJob (Maybe Text)
sJobReason = lens _sJobReason (\ s a -> s{_sJobReason = a})

-- | Commit date / time for the Job. 
sCommitTime :: Lens' StartJob (Maybe UTCTime)
sCommitTime = lens _sCommitTime (\ s a -> s{_sCommitTime = a}) . mapping _Time

-- | Commit message from 3rd party repository provider for the Job. 
sCommitMessage :: Lens' StartJob (Maybe Text)
sCommitMessage = lens _sCommitMessage (\ s a -> s{_sCommitMessage = a})

-- | Unique Id for an Amplify App. 
sAppId :: Lens' StartJob Text
sAppId = lens _sAppId (\ s a -> s{_sAppId = a})

-- | Name for the branch, for the Job. 
sBranchName :: Lens' StartJob Text
sBranchName = lens _sBranchName (\ s a -> s{_sBranchName = a})

-- | Type for the Job. 
sJobType :: Lens' StartJob JobType
sJobType = lens _sJobType (\ s a -> s{_sJobType = a})

instance AWSRequest StartJob where
        type Rs StartJob = StartJobResponse
        request = postJSON amplify
        response
          = receiveJSON
              (\ s h x ->
                 StartJobResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "jobSummary"))

instance Hashable StartJob where

instance NFData StartJob where

instance ToHeaders StartJob where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartJob where
        toJSON StartJob'{..}
          = object
              (catMaybes
                 [("commitId" .=) <$> _sCommitId,
                  ("jobId" .=) <$> _sJobId,
                  ("jobReason" .=) <$> _sJobReason,
                  ("commitTime" .=) <$> _sCommitTime,
                  ("commitMessage" .=) <$> _sCommitMessage,
                  Just ("jobType" .= _sJobType)])

instance ToPath StartJob where
        toPath StartJob'{..}
          = mconcat
              ["/apps/", toBS _sAppId, "/branches/",
               toBS _sBranchName, "/jobs"]

instance ToQuery StartJob where
        toQuery = const mempty

-- | Result structure for run job request. 
--
--
--
-- /See:/ 'startJobResponse' smart constructor.
data StartJobResponse = StartJobResponse'
  { _srsResponseStatus :: !Int
  , _srsJobSummary :: !JobSummary
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsResponseStatus' - -- | The response status code.
--
-- * 'srsJobSummary' - Summary for the Job. 
startJobResponse
    :: Int -- ^ 'srsResponseStatus'
    -> JobSummary -- ^ 'srsJobSummary'
    -> StartJobResponse
startJobResponse pResponseStatus_ pJobSummary_ =
  StartJobResponse'
    {_srsResponseStatus = pResponseStatus_, _srsJobSummary = pJobSummary_}


-- | -- | The response status code.
srsResponseStatus :: Lens' StartJobResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

-- | Summary for the Job. 
srsJobSummary :: Lens' StartJobResponse JobSummary
srsJobSummary = lens _srsJobSummary (\ s a -> s{_srsJobSummary = a})

instance NFData StartJobResponse where
