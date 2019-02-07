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
-- Module      : Network.AWS.DLM.DeleteLifecyclePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified lifecycle policy and halts the automated operations that the policy specified.
--
--
module Network.AWS.DLM.DeleteLifecyclePolicy
    (
    -- * Creating a Request
      deleteLifecyclePolicy
    , DeleteLifecyclePolicy
    -- * Request Lenses
    , dlpPolicyId

    -- * Destructuring the Response
    , deleteLifecyclePolicyResponse
    , DeleteLifecyclePolicyResponse
    -- * Response Lenses
    , dlprsResponseStatus
    ) where

import Network.AWS.DLM.Types
import Network.AWS.DLM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLifecyclePolicy' smart constructor.
newtype DeleteLifecyclePolicy = DeleteLifecyclePolicy'
  { _dlpPolicyId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLifecyclePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlpPolicyId' - The identifier of the lifecycle policy.
deleteLifecyclePolicy
    :: Text -- ^ 'dlpPolicyId'
    -> DeleteLifecyclePolicy
deleteLifecyclePolicy pPolicyId_ =
  DeleteLifecyclePolicy' {_dlpPolicyId = pPolicyId_}


-- | The identifier of the lifecycle policy.
dlpPolicyId :: Lens' DeleteLifecyclePolicy Text
dlpPolicyId = lens _dlpPolicyId (\ s a -> s{_dlpPolicyId = a})

instance AWSRequest DeleteLifecyclePolicy where
        type Rs DeleteLifecyclePolicy =
             DeleteLifecyclePolicyResponse
        request = delete dlm
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteLifecyclePolicyResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteLifecyclePolicy where

instance NFData DeleteLifecyclePolicy where

instance ToHeaders DeleteLifecyclePolicy where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteLifecyclePolicy where
        toPath DeleteLifecyclePolicy'{..}
          = mconcat ["/policies/", toBS _dlpPolicyId, "/"]

instance ToQuery DeleteLifecyclePolicy where
        toQuery = const mempty

-- | /See:/ 'deleteLifecyclePolicyResponse' smart constructor.
newtype DeleteLifecyclePolicyResponse = DeleteLifecyclePolicyResponse'
  { _dlprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLifecyclePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlprsResponseStatus' - -- | The response status code.
deleteLifecyclePolicyResponse
    :: Int -- ^ 'dlprsResponseStatus'
    -> DeleteLifecyclePolicyResponse
deleteLifecyclePolicyResponse pResponseStatus_ =
  DeleteLifecyclePolicyResponse' {_dlprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dlprsResponseStatus :: Lens' DeleteLifecyclePolicyResponse Int
dlprsResponseStatus = lens _dlprsResponseStatus (\ s a -> s{_dlprsResponseStatus = a})

instance NFData DeleteLifecyclePolicyResponse where
