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
-- Module      : Network.AWS.DLM.CreateLifecyclePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a policy to manage the lifecycle of the specified AWS resources. You can create up to 100 lifecycle policies.
--
--
module Network.AWS.DLM.CreateLifecyclePolicy
    (
    -- * Creating a Request
      createLifecyclePolicy
    , CreateLifecyclePolicy
    -- * Request Lenses
    , clpExecutionRoleARN
    , clpDescription
    , clpState
    , clpPolicyDetails

    -- * Destructuring the Response
    , createLifecyclePolicyResponse
    , CreateLifecyclePolicyResponse
    -- * Response Lenses
    , clprsPolicyId
    , clprsResponseStatus
    ) where

import Network.AWS.DLM.Types
import Network.AWS.DLM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createLifecyclePolicy' smart constructor.
data CreateLifecyclePolicy = CreateLifecyclePolicy'
  { _clpExecutionRoleARN :: !Text
  , _clpDescription :: !Text
  , _clpState :: !SettablePolicyStateValues
  , _clpPolicyDetails :: !PolicyDetails
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLifecyclePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clpExecutionRoleARN' - The Amazon Resource Name (ARN) of the IAM role used to run the operations specified by the lifecycle policy.
--
-- * 'clpDescription' - A description of the lifecycle policy. The characters ^[0-9A-Za-z _-]+$ are supported.
--
-- * 'clpState' - The desired activation state of the lifecycle policy after creation.
--
-- * 'clpPolicyDetails' - The configuration details of the lifecycle policy. Target tags cannot be re-used across lifecycle policies.
createLifecyclePolicy
    :: Text -- ^ 'clpExecutionRoleARN'
    -> Text -- ^ 'clpDescription'
    -> SettablePolicyStateValues -- ^ 'clpState'
    -> PolicyDetails -- ^ 'clpPolicyDetails'
    -> CreateLifecyclePolicy
createLifecyclePolicy pExecutionRoleARN_ pDescription_ pState_ pPolicyDetails_ =
  CreateLifecyclePolicy'
    { _clpExecutionRoleARN = pExecutionRoleARN_
    , _clpDescription = pDescription_
    , _clpState = pState_
    , _clpPolicyDetails = pPolicyDetails_
    }


-- | The Amazon Resource Name (ARN) of the IAM role used to run the operations specified by the lifecycle policy.
clpExecutionRoleARN :: Lens' CreateLifecyclePolicy Text
clpExecutionRoleARN = lens _clpExecutionRoleARN (\ s a -> s{_clpExecutionRoleARN = a})

-- | A description of the lifecycle policy. The characters ^[0-9A-Za-z _-]+$ are supported.
clpDescription :: Lens' CreateLifecyclePolicy Text
clpDescription = lens _clpDescription (\ s a -> s{_clpDescription = a})

-- | The desired activation state of the lifecycle policy after creation.
clpState :: Lens' CreateLifecyclePolicy SettablePolicyStateValues
clpState = lens _clpState (\ s a -> s{_clpState = a})

-- | The configuration details of the lifecycle policy. Target tags cannot be re-used across lifecycle policies.
clpPolicyDetails :: Lens' CreateLifecyclePolicy PolicyDetails
clpPolicyDetails = lens _clpPolicyDetails (\ s a -> s{_clpPolicyDetails = a})

instance AWSRequest CreateLifecyclePolicy where
        type Rs CreateLifecyclePolicy =
             CreateLifecyclePolicyResponse
        request = postJSON dlm
        response
          = receiveJSON
              (\ s h x ->
                 CreateLifecyclePolicyResponse' <$>
                   (x .?> "PolicyId") <*> (pure (fromEnum s)))

instance Hashable CreateLifecyclePolicy where

instance NFData CreateLifecyclePolicy where

instance ToHeaders CreateLifecyclePolicy where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateLifecyclePolicy where
        toJSON CreateLifecyclePolicy'{..}
          = object
              (catMaybes
                 [Just ("ExecutionRoleArn" .= _clpExecutionRoleARN),
                  Just ("Description" .= _clpDescription),
                  Just ("State" .= _clpState),
                  Just ("PolicyDetails" .= _clpPolicyDetails)])

instance ToPath CreateLifecyclePolicy where
        toPath = const "/policies"

instance ToQuery CreateLifecyclePolicy where
        toQuery = const mempty

-- | /See:/ 'createLifecyclePolicyResponse' smart constructor.
data CreateLifecyclePolicyResponse = CreateLifecyclePolicyResponse'
  { _clprsPolicyId :: !(Maybe Text)
  , _clprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLifecyclePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clprsPolicyId' - The identifier of the lifecycle policy.
--
-- * 'clprsResponseStatus' - -- | The response status code.
createLifecyclePolicyResponse
    :: Int -- ^ 'clprsResponseStatus'
    -> CreateLifecyclePolicyResponse
createLifecyclePolicyResponse pResponseStatus_ =
  CreateLifecyclePolicyResponse'
    {_clprsPolicyId = Nothing, _clprsResponseStatus = pResponseStatus_}


-- | The identifier of the lifecycle policy.
clprsPolicyId :: Lens' CreateLifecyclePolicyResponse (Maybe Text)
clprsPolicyId = lens _clprsPolicyId (\ s a -> s{_clprsPolicyId = a})

-- | -- | The response status code.
clprsResponseStatus :: Lens' CreateLifecyclePolicyResponse Int
clprsResponseStatus = lens _clprsResponseStatus (\ s a -> s{_clprsResponseStatus = a})

instance NFData CreateLifecyclePolicyResponse where
