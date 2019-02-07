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
-- Module      : Network.AWS.DLM.UpdateLifecyclePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified lifecycle policy.
--
--
module Network.AWS.DLM.UpdateLifecyclePolicy
    (
    -- * Creating a Request
      updateLifecyclePolicy
    , UpdateLifecyclePolicy
    -- * Request Lenses
    , ulpState
    , ulpPolicyDetails
    , ulpExecutionRoleARN
    , ulpDescription
    , ulpPolicyId

    -- * Destructuring the Response
    , updateLifecyclePolicyResponse
    , UpdateLifecyclePolicyResponse
    -- * Response Lenses
    , ulprsResponseStatus
    ) where

import Network.AWS.DLM.Types
import Network.AWS.DLM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateLifecyclePolicy' smart constructor.
data UpdateLifecyclePolicy = UpdateLifecyclePolicy'
  { _ulpState :: !(Maybe SettablePolicyStateValues)
  , _ulpPolicyDetails :: !(Maybe PolicyDetails)
  , _ulpExecutionRoleARN :: !(Maybe Text)
  , _ulpDescription :: !(Maybe Text)
  , _ulpPolicyId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateLifecyclePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ulpState' - The desired activation state of the lifecycle policy after creation.
--
-- * 'ulpPolicyDetails' - The configuration of the lifecycle policy. Target tags cannot be re-used across policies.
--
-- * 'ulpExecutionRoleARN' - The Amazon Resource Name (ARN) of the IAM role used to run the operations specified by the lifecycle policy.
--
-- * 'ulpDescription' - A description of the lifecycle policy.
--
-- * 'ulpPolicyId' - The identifier of the lifecycle policy.
updateLifecyclePolicy
    :: Text -- ^ 'ulpPolicyId'
    -> UpdateLifecyclePolicy
updateLifecyclePolicy pPolicyId_ =
  UpdateLifecyclePolicy'
    { _ulpState = Nothing
    , _ulpPolicyDetails = Nothing
    , _ulpExecutionRoleARN = Nothing
    , _ulpDescription = Nothing
    , _ulpPolicyId = pPolicyId_
    }


-- | The desired activation state of the lifecycle policy after creation.
ulpState :: Lens' UpdateLifecyclePolicy (Maybe SettablePolicyStateValues)
ulpState = lens _ulpState (\ s a -> s{_ulpState = a})

-- | The configuration of the lifecycle policy. Target tags cannot be re-used across policies.
ulpPolicyDetails :: Lens' UpdateLifecyclePolicy (Maybe PolicyDetails)
ulpPolicyDetails = lens _ulpPolicyDetails (\ s a -> s{_ulpPolicyDetails = a})

-- | The Amazon Resource Name (ARN) of the IAM role used to run the operations specified by the lifecycle policy.
ulpExecutionRoleARN :: Lens' UpdateLifecyclePolicy (Maybe Text)
ulpExecutionRoleARN = lens _ulpExecutionRoleARN (\ s a -> s{_ulpExecutionRoleARN = a})

-- | A description of the lifecycle policy.
ulpDescription :: Lens' UpdateLifecyclePolicy (Maybe Text)
ulpDescription = lens _ulpDescription (\ s a -> s{_ulpDescription = a})

-- | The identifier of the lifecycle policy.
ulpPolicyId :: Lens' UpdateLifecyclePolicy Text
ulpPolicyId = lens _ulpPolicyId (\ s a -> s{_ulpPolicyId = a})

instance AWSRequest UpdateLifecyclePolicy where
        type Rs UpdateLifecyclePolicy =
             UpdateLifecyclePolicyResponse
        request = patchJSON dlm
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateLifecyclePolicyResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateLifecyclePolicy where

instance NFData UpdateLifecyclePolicy where

instance ToHeaders UpdateLifecyclePolicy where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateLifecyclePolicy where
        toJSON UpdateLifecyclePolicy'{..}
          = object
              (catMaybes
                 [("State" .=) <$> _ulpState,
                  ("PolicyDetails" .=) <$> _ulpPolicyDetails,
                  ("ExecutionRoleArn" .=) <$> _ulpExecutionRoleARN,
                  ("Description" .=) <$> _ulpDescription])

instance ToPath UpdateLifecyclePolicy where
        toPath UpdateLifecyclePolicy'{..}
          = mconcat ["/policies/", toBS _ulpPolicyId]

instance ToQuery UpdateLifecyclePolicy where
        toQuery = const mempty

-- | /See:/ 'updateLifecyclePolicyResponse' smart constructor.
newtype UpdateLifecyclePolicyResponse = UpdateLifecyclePolicyResponse'
  { _ulprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateLifecyclePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ulprsResponseStatus' - -- | The response status code.
updateLifecyclePolicyResponse
    :: Int -- ^ 'ulprsResponseStatus'
    -> UpdateLifecyclePolicyResponse
updateLifecyclePolicyResponse pResponseStatus_ =
  UpdateLifecyclePolicyResponse' {_ulprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ulprsResponseStatus :: Lens' UpdateLifecyclePolicyResponse Int
ulprsResponseStatus = lens _ulprsResponseStatus (\ s a -> s{_ulprsResponseStatus = a})

instance NFData UpdateLifecyclePolicyResponse where
