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
-- Module      : Network.AWS.Amplify.StopJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stop a job that is in progress, for an Amplify branch, part of Amplify App. 
--
--
module Network.AWS.Amplify.StopJob
    (
    -- * Creating a Request
      stopJob
    , StopJob
    -- * Request Lenses
    , sjAppId
    , sjBranchName
    , sjJobId

    -- * Destructuring the Response
    , stopJobResponse
    , StopJobResponse
    -- * Response Lenses
    , sjrsResponseStatus
    , sjrsJobSummary
    ) where

import Network.AWS.Amplify.Types
import Network.AWS.Amplify.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure for stop job request. 
--
--
--
-- /See:/ 'stopJob' smart constructor.
data StopJob = StopJob'
  { _sjAppId :: !Text
  , _sjBranchName :: !Text
  , _sjJobId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sjAppId' - Unique Id for an Amplify App. 
--
-- * 'sjBranchName' - Name for the branch, for the Job. 
--
-- * 'sjJobId' - Unique Id for the Job. 
stopJob
    :: Text -- ^ 'sjAppId'
    -> Text -- ^ 'sjBranchName'
    -> Text -- ^ 'sjJobId'
    -> StopJob
stopJob pAppId_ pBranchName_ pJobId_ =
  StopJob'
    {_sjAppId = pAppId_, _sjBranchName = pBranchName_, _sjJobId = pJobId_}


-- | Unique Id for an Amplify App. 
sjAppId :: Lens' StopJob Text
sjAppId = lens _sjAppId (\ s a -> s{_sjAppId = a})

-- | Name for the branch, for the Job. 
sjBranchName :: Lens' StopJob Text
sjBranchName = lens _sjBranchName (\ s a -> s{_sjBranchName = a})

-- | Unique Id for the Job. 
sjJobId :: Lens' StopJob Text
sjJobId = lens _sjJobId (\ s a -> s{_sjJobId = a})

instance AWSRequest StopJob where
        type Rs StopJob = StopJobResponse
        request = delete amplify
        response
          = receiveJSON
              (\ s h x ->
                 StopJobResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "jobSummary"))

instance Hashable StopJob where

instance NFData StopJob where

instance ToHeaders StopJob where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath StopJob where
        toPath StopJob'{..}
          = mconcat
              ["/apps/", toBS _sjAppId, "/branches/",
               toBS _sjBranchName, "/jobs/", toBS _sjJobId, "/stop"]

instance ToQuery StopJob where
        toQuery = const mempty

-- | Result structure for the stop job request. 
--
--
--
-- /See:/ 'stopJobResponse' smart constructor.
data StopJobResponse = StopJobResponse'
  { _sjrsResponseStatus :: !Int
  , _sjrsJobSummary :: !JobSummary
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sjrsResponseStatus' - -- | The response status code.
--
-- * 'sjrsJobSummary' - Summary for the Job. 
stopJobResponse
    :: Int -- ^ 'sjrsResponseStatus'
    -> JobSummary -- ^ 'sjrsJobSummary'
    -> StopJobResponse
stopJobResponse pResponseStatus_ pJobSummary_ =
  StopJobResponse'
    {_sjrsResponseStatus = pResponseStatus_, _sjrsJobSummary = pJobSummary_}


-- | -- | The response status code.
sjrsResponseStatus :: Lens' StopJobResponse Int
sjrsResponseStatus = lens _sjrsResponseStatus (\ s a -> s{_sjrsResponseStatus = a})

-- | Summary for the Job. 
sjrsJobSummary :: Lens' StopJobResponse JobSummary
sjrsJobSummary = lens _sjrsJobSummary (\ s a -> s{_sjrsJobSummary = a})

instance NFData StopJobResponse where
