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
-- Module      : Network.AWS.Amplify.GetJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a job for a branch, part of an Amplify App. 
--
--
module Network.AWS.Amplify.GetJob
    (
    -- * Creating a Request
      getJob
    , GetJob
    -- * Request Lenses
    , gjAppId
    , gjBranchName
    , gjJobId

    -- * Destructuring the Response
    , getJobResponse
    , GetJobResponse
    -- * Response Lenses
    , gjrsResponseStatus
    , gjrsJob
    ) where

import Network.AWS.Amplify.Types
import Network.AWS.Amplify.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure for get job request. 
--
--
--
-- /See:/ 'getJob' smart constructor.
data GetJob = GetJob'
  { _gjAppId :: !Text
  , _gjBranchName :: !Text
  , _gjJobId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjAppId' - Unique Id for an Amplify App. 
--
-- * 'gjBranchName' - Name for the branch, for the Job. 
--
-- * 'gjJobId' - Unique Id for the Job. 
getJob
    :: Text -- ^ 'gjAppId'
    -> Text -- ^ 'gjBranchName'
    -> Text -- ^ 'gjJobId'
    -> GetJob
getJob pAppId_ pBranchName_ pJobId_ =
  GetJob' {_gjAppId = pAppId_, _gjBranchName = pBranchName_, _gjJobId = pJobId_}


-- | Unique Id for an Amplify App. 
gjAppId :: Lens' GetJob Text
gjAppId = lens _gjAppId (\ s a -> s{_gjAppId = a})

-- | Name for the branch, for the Job. 
gjBranchName :: Lens' GetJob Text
gjBranchName = lens _gjBranchName (\ s a -> s{_gjBranchName = a})

-- | Unique Id for the Job. 
gjJobId :: Lens' GetJob Text
gjJobId = lens _gjJobId (\ s a -> s{_gjJobId = a})

instance AWSRequest GetJob where
        type Rs GetJob = GetJobResponse
        request = get amplify
        response
          = receiveJSON
              (\ s h x ->
                 GetJobResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "job"))

instance Hashable GetJob where

instance NFData GetJob where

instance ToHeaders GetJob where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetJob where
        toPath GetJob'{..}
          = mconcat
              ["/apps/", toBS _gjAppId, "/branches/",
               toBS _gjBranchName, "/jobs/", toBS _gjJobId]

instance ToQuery GetJob where
        toQuery = const mempty

-- | /See:/ 'getJobResponse' smart constructor.
data GetJobResponse = GetJobResponse'
  { _gjrsResponseStatus :: !Int
  , _gjrsJob :: !Job
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjrsResponseStatus' - -- | The response status code.
--
-- * 'gjrsJob' - Undocumented member.
getJobResponse
    :: Int -- ^ 'gjrsResponseStatus'
    -> Job -- ^ 'gjrsJob'
    -> GetJobResponse
getJobResponse pResponseStatus_ pJob_ =
  GetJobResponse' {_gjrsResponseStatus = pResponseStatus_, _gjrsJob = pJob_}


-- | -- | The response status code.
gjrsResponseStatus :: Lens' GetJobResponse Int
gjrsResponseStatus = lens _gjrsResponseStatus (\ s a -> s{_gjrsResponseStatus = a})

-- | Undocumented member.
gjrsJob :: Lens' GetJobResponse Job
gjrsJob = lens _gjrsJob (\ s a -> s{_gjrsJob = a})

instance NFData GetJobResponse where
