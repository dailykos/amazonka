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
-- Module      : Network.AWS.DocDB.AddTagsToResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds metadata tags to an Amazon DocumentDB resource. You can use these tags with cost allocation reporting to track costs that are associated with Amazon DocumentDB resources. or in a @Condition@ statement in an AWS Identity and Access Management (IAM) policy for Amazon DocumentDB.
--
--
module Network.AWS.DocDB.AddTagsToResource
    (
    -- * Creating a Request
      addTagsToResource
    , AddTagsToResource
    -- * Request Lenses
    , attrResourceName
    , attrTags

    -- * Destructuring the Response
    , addTagsToResourceResponse
    , AddTagsToResourceResponse
    ) where

import Network.AWS.DocDB.Types
import Network.AWS.DocDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input to 'AddTagsToResource' .
--
--
--
-- /See:/ 'addTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { _attrResourceName :: !Text
  , _attrTags :: ![Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTagsToResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'attrResourceName' - The Amazon DocumentDB resource that the tags are added to. This value is an Amazon Resource Name (ARN).
--
-- * 'attrTags' - The tags to be assigned to the Amazon DocumentDB resource. 
addTagsToResource
    :: Text -- ^ 'attrResourceName'
    -> AddTagsToResource
addTagsToResource pResourceName_ =
  AddTagsToResource' {_attrResourceName = pResourceName_, _attrTags = mempty}


-- | The Amazon DocumentDB resource that the tags are added to. This value is an Amazon Resource Name (ARN).
attrResourceName :: Lens' AddTagsToResource Text
attrResourceName = lens _attrResourceName (\ s a -> s{_attrResourceName = a})

-- | The tags to be assigned to the Amazon DocumentDB resource. 
attrTags :: Lens' AddTagsToResource [Tag]
attrTags = lens _attrTags (\ s a -> s{_attrTags = a}) . _Coerce

instance AWSRequest AddTagsToResource where
        type Rs AddTagsToResource = AddTagsToResourceResponse
        request = postQuery docDB
        response = receiveNull AddTagsToResourceResponse'

instance Hashable AddTagsToResource where

instance NFData AddTagsToResource where

instance ToHeaders AddTagsToResource where
        toHeaders = const mempty

instance ToPath AddTagsToResource where
        toPath = const "/"

instance ToQuery AddTagsToResource where
        toQuery AddTagsToResource'{..}
          = mconcat
              ["Action" =: ("AddTagsToResource" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "ResourceName" =: _attrResourceName,
               "Tags" =: toQueryList "Tag" _attrTags]

-- | /See:/ 'addTagsToResourceResponse' smart constructor.
data AddTagsToResourceResponse =
  AddTagsToResourceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTagsToResourceResponse' with the minimum fields required to make a request.
--
addTagsToResourceResponse
    :: AddTagsToResourceResponse
addTagsToResourceResponse = AddTagsToResourceResponse'


instance NFData AddTagsToResourceResponse where
