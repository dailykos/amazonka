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
-- Module      : Network.AWS.DLM.GetLifecyclePolicies
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets summary information about all or the specified data lifecycle policies.
--
--
-- To get complete information about a policy, use 'GetLifecyclePolicy' .
--
module Network.AWS.DLM.GetLifecyclePolicies
    (
    -- * Creating a Request
      getLifecyclePolicies
    , GetLifecyclePolicies
    -- * Request Lenses
    , glpState
    , glpTargetTags
    , glpTagsToAdd
    , glpPolicyIds
    , glpResourceTypes

    -- * Destructuring the Response
    , getLifecyclePoliciesResponse
    , GetLifecyclePoliciesResponse
    -- * Response Lenses
    , glprsPolicies
    , glprsResponseStatus
    ) where

import Network.AWS.DLM.Types
import Network.AWS.DLM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getLifecyclePolicies' smart constructor.
data GetLifecyclePolicies = GetLifecyclePolicies'
  { _glpState :: !(Maybe GettablePolicyStateValues)
  , _glpTargetTags :: !(Maybe (List1 Text))
  , _glpTagsToAdd :: !(Maybe [Text])
  , _glpPolicyIds :: !(Maybe [Text])
  , _glpResourceTypes :: !(Maybe (List1 ResourceTypeValues))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLifecyclePolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glpState' - The activation state.
--
-- * 'glpTargetTags' - The target tag for a policy. Tags are strings in the format @key=value@ .
--
-- * 'glpTagsToAdd' - The tags to add to objects created by the policy. Tags are strings in the format @key=value@ . These user-defined tags are added in addition to the AWS-added lifecycle tags.
--
-- * 'glpPolicyIds' - The identifiers of the data lifecycle policies.
--
-- * 'glpResourceTypes' - The resource type.
getLifecyclePolicies
    :: GetLifecyclePolicies
getLifecyclePolicies =
  GetLifecyclePolicies'
    { _glpState = Nothing
    , _glpTargetTags = Nothing
    , _glpTagsToAdd = Nothing
    , _glpPolicyIds = Nothing
    , _glpResourceTypes = Nothing
    }


-- | The activation state.
glpState :: Lens' GetLifecyclePolicies (Maybe GettablePolicyStateValues)
glpState = lens _glpState (\ s a -> s{_glpState = a})

-- | The target tag for a policy. Tags are strings in the format @key=value@ .
glpTargetTags :: Lens' GetLifecyclePolicies (Maybe (NonEmpty Text))
glpTargetTags = lens _glpTargetTags (\ s a -> s{_glpTargetTags = a}) . mapping _List1

-- | The tags to add to objects created by the policy. Tags are strings in the format @key=value@ . These user-defined tags are added in addition to the AWS-added lifecycle tags.
glpTagsToAdd :: Lens' GetLifecyclePolicies [Text]
glpTagsToAdd = lens _glpTagsToAdd (\ s a -> s{_glpTagsToAdd = a}) . _Default . _Coerce

-- | The identifiers of the data lifecycle policies.
glpPolicyIds :: Lens' GetLifecyclePolicies [Text]
glpPolicyIds = lens _glpPolicyIds (\ s a -> s{_glpPolicyIds = a}) . _Default . _Coerce

-- | The resource type.
glpResourceTypes :: Lens' GetLifecyclePolicies (Maybe (NonEmpty ResourceTypeValues))
glpResourceTypes = lens _glpResourceTypes (\ s a -> s{_glpResourceTypes = a}) . mapping _List1

instance AWSRequest GetLifecyclePolicies where
        type Rs GetLifecyclePolicies =
             GetLifecyclePoliciesResponse
        request = get dlm
        response
          = receiveJSON
              (\ s h x ->
                 GetLifecyclePoliciesResponse' <$>
                   (x .?> "Policies" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable GetLifecyclePolicies where

instance NFData GetLifecyclePolicies where

instance ToHeaders GetLifecyclePolicies where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetLifecyclePolicies where
        toPath = const "/policies"

instance ToQuery GetLifecyclePolicies where
        toQuery GetLifecyclePolicies'{..}
          = mconcat
              ["state" =: _glpState,
               "targetTags" =:
                 toQuery (toQueryList "member" <$> _glpTargetTags),
               "tagsToAdd" =:
                 toQuery (toQueryList "member" <$> _glpTagsToAdd),
               "policyIds" =:
                 toQuery (toQueryList "member" <$> _glpPolicyIds),
               "resourceTypes" =:
                 toQuery (toQueryList "member" <$> _glpResourceTypes)]

-- | /See:/ 'getLifecyclePoliciesResponse' smart constructor.
data GetLifecyclePoliciesResponse = GetLifecyclePoliciesResponse'
  { _glprsPolicies :: !(Maybe [LifecyclePolicySummary])
  , _glprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLifecyclePoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glprsPolicies' - Summary information about the lifecycle policies.
--
-- * 'glprsResponseStatus' - -- | The response status code.
getLifecyclePoliciesResponse
    :: Int -- ^ 'glprsResponseStatus'
    -> GetLifecyclePoliciesResponse
getLifecyclePoliciesResponse pResponseStatus_ =
  GetLifecyclePoliciesResponse'
    {_glprsPolicies = Nothing, _glprsResponseStatus = pResponseStatus_}


-- | Summary information about the lifecycle policies.
glprsPolicies :: Lens' GetLifecyclePoliciesResponse [LifecyclePolicySummary]
glprsPolicies = lens _glprsPolicies (\ s a -> s{_glprsPolicies = a}) . _Default . _Coerce

-- | -- | The response status code.
glprsResponseStatus :: Lens' GetLifecyclePoliciesResponse Int
glprsResponseStatus = lens _glprsResponseStatus (\ s a -> s{_glprsResponseStatus = a})

instance NFData GetLifecyclePoliciesResponse where
