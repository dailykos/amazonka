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
-- Module      : Network.AWS.Amplify.DeleteJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a job, for an Amplify branch, part of Amplify App. 
--
--
module Network.AWS.Amplify.DeleteJob
    (
    -- * Creating a Request
      deleteJob
    , DeleteJob
    -- * Request Lenses
    , djAppId
    , djBranchName
    , djJobId

    -- * Destructuring the Response
    , deleteJobResponse
    , DeleteJobResponse
    -- * Response Lenses
    , djrsResponseStatus
    , djrsJobSummary
    ) where

import Network.AWS.Amplify.Types
import Network.AWS.Amplify.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure for delete job request. 
--
--
--
-- /See:/ 'deleteJob' smart constructor.
data DeleteJob = DeleteJob'
  { _djAppId :: !Text
  , _djBranchName :: !Text
  , _djJobId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djAppId' - Unique Id for an Amplify App. 
--
-- * 'djBranchName' - Name for the branch, for the Job. 
--
-- * 'djJobId' - Unique Id for the Job. 
deleteJob
    :: Text -- ^ 'djAppId'
    -> Text -- ^ 'djBranchName'
    -> Text -- ^ 'djJobId'
    -> DeleteJob
deleteJob pAppId_ pBranchName_ pJobId_ =
  DeleteJob'
    {_djAppId = pAppId_, _djBranchName = pBranchName_, _djJobId = pJobId_}


-- | Unique Id for an Amplify App. 
djAppId :: Lens' DeleteJob Text
djAppId = lens _djAppId (\ s a -> s{_djAppId = a})

-- | Name for the branch, for the Job. 
djBranchName :: Lens' DeleteJob Text
djBranchName = lens _djBranchName (\ s a -> s{_djBranchName = a})

-- | Unique Id for the Job. 
djJobId :: Lens' DeleteJob Text
djJobId = lens _djJobId (\ s a -> s{_djJobId = a})

instance AWSRequest DeleteJob where
        type Rs DeleteJob = DeleteJobResponse
        request = delete amplify
        response
          = receiveJSON
              (\ s h x ->
                 DeleteJobResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "jobSummary"))

instance Hashable DeleteJob where

instance NFData DeleteJob where

instance ToHeaders DeleteJob where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteJob where
        toPath DeleteJob'{..}
          = mconcat
              ["/apps/", toBS _djAppId, "/branches/",
               toBS _djBranchName, "/jobs/", toBS _djJobId]

instance ToQuery DeleteJob where
        toQuery = const mempty

-- | Result structure for the delete job request. 
--
--
--
-- /See:/ 'deleteJobResponse' smart constructor.
data DeleteJobResponse = DeleteJobResponse'
  { _djrsResponseStatus :: !Int
  , _djrsJobSummary :: !JobSummary
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djrsResponseStatus' - -- | The response status code.
--
-- * 'djrsJobSummary' - Undocumented member.
deleteJobResponse
    :: Int -- ^ 'djrsResponseStatus'
    -> JobSummary -- ^ 'djrsJobSummary'
    -> DeleteJobResponse
deleteJobResponse pResponseStatus_ pJobSummary_ =
  DeleteJobResponse'
    {_djrsResponseStatus = pResponseStatus_, _djrsJobSummary = pJobSummary_}


-- | -- | The response status code.
djrsResponseStatus :: Lens' DeleteJobResponse Int
djrsResponseStatus = lens _djrsResponseStatus (\ s a -> s{_djrsResponseStatus = a})

-- | Undocumented member.
djrsJobSummary :: Lens' DeleteJobResponse JobSummary
djrsJobSummary = lens _djrsJobSummary (\ s a -> s{_djrsJobSummary = a})

instance NFData DeleteJobResponse where
