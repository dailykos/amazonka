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
-- Module      : Network.AWS.DLM.GetLifecyclePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets detailed information about the specified lifecycle policy.
--
--
module Network.AWS.DLM.GetLifecyclePolicy
    (
    -- * Creating a Request
      getLifecyclePolicy
    , GetLifecyclePolicy
    -- * Request Lenses
    , glpPolicyId

    -- * Destructuring the Response
    , getLifecyclePolicyResponse
    , GetLifecyclePolicyResponse
    -- * Response Lenses
    , grsPolicy
    , grsResponseStatus
    ) where

import Network.AWS.DLM.Types
import Network.AWS.DLM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getLifecyclePolicy' smart constructor.
newtype GetLifecyclePolicy = GetLifecyclePolicy'
  { _glpPolicyId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLifecyclePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glpPolicyId' - The identifier of the lifecycle policy.
getLifecyclePolicy
    :: Text -- ^ 'glpPolicyId'
    -> GetLifecyclePolicy
getLifecyclePolicy pPolicyId_ = GetLifecyclePolicy' {_glpPolicyId = pPolicyId_}


-- | The identifier of the lifecycle policy.
glpPolicyId :: Lens' GetLifecyclePolicy Text
glpPolicyId = lens _glpPolicyId (\ s a -> s{_glpPolicyId = a})

instance AWSRequest GetLifecyclePolicy where
        type Rs GetLifecyclePolicy =
             GetLifecyclePolicyResponse
        request = get dlm
        response
          = receiveJSON
              (\ s h x ->
                 GetLifecyclePolicyResponse' <$>
                   (x .?> "Policy") <*> (pure (fromEnum s)))

instance Hashable GetLifecyclePolicy where

instance NFData GetLifecyclePolicy where

instance ToHeaders GetLifecyclePolicy where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetLifecyclePolicy where
        toPath GetLifecyclePolicy'{..}
          = mconcat ["/policies/", toBS _glpPolicyId, "/"]

instance ToQuery GetLifecyclePolicy where
        toQuery = const mempty

-- | /See:/ 'getLifecyclePolicyResponse' smart constructor.
data GetLifecyclePolicyResponse = GetLifecyclePolicyResponse'
  { _grsPolicy :: !(Maybe LifecyclePolicy)
  , _grsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLifecyclePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grsPolicy' - Detailed information about the lifecycle policy.
--
-- * 'grsResponseStatus' - -- | The response status code.
getLifecyclePolicyResponse
    :: Int -- ^ 'grsResponseStatus'
    -> GetLifecyclePolicyResponse
getLifecyclePolicyResponse pResponseStatus_ =
  GetLifecyclePolicyResponse'
    {_grsPolicy = Nothing, _grsResponseStatus = pResponseStatus_}


-- | Detailed information about the lifecycle policy.
grsPolicy :: Lens' GetLifecyclePolicyResponse (Maybe LifecyclePolicy)
grsPolicy = lens _grsPolicy (\ s a -> s{_grsPolicy = a})

-- | -- | The response status code.
grsResponseStatus :: Lens' GetLifecyclePolicyResponse Int
grsResponseStatus = lens _grsResponseStatus (\ s a -> s{_grsResponseStatus = a})

instance NFData GetLifecyclePolicyResponse where
