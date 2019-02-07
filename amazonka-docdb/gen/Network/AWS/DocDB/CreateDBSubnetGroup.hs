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
-- Module      : Network.AWS.DocDB.CreateDBSubnetGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB subnet group. DB subnet groups must contain at least one subnet in at least two Availability Zones in the AWS Region.
--
--
module Network.AWS.DocDB.CreateDBSubnetGroup
    (
    -- * Creating a Request
      createDBSubnetGroup
    , CreateDBSubnetGroup
    -- * Request Lenses
    , cdsgTags
    , cdsgDBSubnetGroupName
    , cdsgDBSubnetGroupDescription
    , cdsgSubnetIds

    -- * Destructuring the Response
    , createDBSubnetGroupResponse
    , CreateDBSubnetGroupResponse
    -- * Response Lenses
    , cdsgrsDBSubnetGroup
    , cdsgrsResponseStatus
    ) where

import Network.AWS.DocDB.Types
import Network.AWS.DocDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input to 'CreateDBSubnetGroup' .
--
--
--
-- /See:/ 'createDBSubnetGroup' smart constructor.
data CreateDBSubnetGroup = CreateDBSubnetGroup'
  { _cdsgTags :: !(Maybe [Tag])
  , _cdsgDBSubnetGroupName :: !Text
  , _cdsgDBSubnetGroupDescription :: !Text
  , _cdsgSubnetIds :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDBSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdsgTags' - The tags to be assigned to the DB subnet group.
--
-- * 'cdsgDBSubnetGroupName' - The name for the DB subnet group. This value is stored as a lowercase string. Constraints: Must contain no more than 255 letters, numbers, periods, underscores, spaces, or hyphens. Must not be default. Example: @mySubnetgroup@ 
--
-- * 'cdsgDBSubnetGroupDescription' - The description for the DB subnet group.
--
-- * 'cdsgSubnetIds' - The Amazon EC2 subnet IDs for the DB subnet group.
createDBSubnetGroup
    :: Text -- ^ 'cdsgDBSubnetGroupName'
    -> Text -- ^ 'cdsgDBSubnetGroupDescription'
    -> CreateDBSubnetGroup
createDBSubnetGroup pDBSubnetGroupName_ pDBSubnetGroupDescription_ =
  CreateDBSubnetGroup'
    { _cdsgTags = Nothing
    , _cdsgDBSubnetGroupName = pDBSubnetGroupName_
    , _cdsgDBSubnetGroupDescription = pDBSubnetGroupDescription_
    , _cdsgSubnetIds = mempty
    }


-- | The tags to be assigned to the DB subnet group.
cdsgTags :: Lens' CreateDBSubnetGroup [Tag]
cdsgTags = lens _cdsgTags (\ s a -> s{_cdsgTags = a}) . _Default . _Coerce

-- | The name for the DB subnet group. This value is stored as a lowercase string. Constraints: Must contain no more than 255 letters, numbers, periods, underscores, spaces, or hyphens. Must not be default. Example: @mySubnetgroup@ 
cdsgDBSubnetGroupName :: Lens' CreateDBSubnetGroup Text
cdsgDBSubnetGroupName = lens _cdsgDBSubnetGroupName (\ s a -> s{_cdsgDBSubnetGroupName = a})

-- | The description for the DB subnet group.
cdsgDBSubnetGroupDescription :: Lens' CreateDBSubnetGroup Text
cdsgDBSubnetGroupDescription = lens _cdsgDBSubnetGroupDescription (\ s a -> s{_cdsgDBSubnetGroupDescription = a})

-- | The Amazon EC2 subnet IDs for the DB subnet group.
cdsgSubnetIds :: Lens' CreateDBSubnetGroup [Text]
cdsgSubnetIds = lens _cdsgSubnetIds (\ s a -> s{_cdsgSubnetIds = a}) . _Coerce

instance AWSRequest CreateDBSubnetGroup where
        type Rs CreateDBSubnetGroup =
             CreateDBSubnetGroupResponse
        request = postQuery docDB
        response
          = receiveXMLWrapper "CreateDBSubnetGroupResult"
              (\ s h x ->
                 CreateDBSubnetGroupResponse' <$>
                   (x .@? "DBSubnetGroup") <*> (pure (fromEnum s)))

instance Hashable CreateDBSubnetGroup where

instance NFData CreateDBSubnetGroup where

instance ToHeaders CreateDBSubnetGroup where
        toHeaders = const mempty

instance ToPath CreateDBSubnetGroup where
        toPath = const "/"

instance ToQuery CreateDBSubnetGroup where
        toQuery CreateDBSubnetGroup'{..}
          = mconcat
              ["Action" =: ("CreateDBSubnetGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdsgTags),
               "DBSubnetGroupName" =: _cdsgDBSubnetGroupName,
               "DBSubnetGroupDescription" =:
                 _cdsgDBSubnetGroupDescription,
               "SubnetIds" =:
                 toQueryList "SubnetIdentifier" _cdsgSubnetIds]

-- | /See:/ 'createDBSubnetGroupResponse' smart constructor.
data CreateDBSubnetGroupResponse = CreateDBSubnetGroupResponse'
  { _cdsgrsDBSubnetGroup :: !(Maybe DBSubnetGroup)
  , _cdsgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDBSubnetGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdsgrsDBSubnetGroup' - Undocumented member.
--
-- * 'cdsgrsResponseStatus' - -- | The response status code.
createDBSubnetGroupResponse
    :: Int -- ^ 'cdsgrsResponseStatus'
    -> CreateDBSubnetGroupResponse
createDBSubnetGroupResponse pResponseStatus_ =
  CreateDBSubnetGroupResponse'
    {_cdsgrsDBSubnetGroup = Nothing, _cdsgrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
cdsgrsDBSubnetGroup :: Lens' CreateDBSubnetGroupResponse (Maybe DBSubnetGroup)
cdsgrsDBSubnetGroup = lens _cdsgrsDBSubnetGroup (\ s a -> s{_cdsgrsDBSubnetGroup = a})

-- | -- | The response status code.
cdsgrsResponseStatus :: Lens' CreateDBSubnetGroupResponse Int
cdsgrsResponseStatus = lens _cdsgrsResponseStatus (\ s a -> s{_cdsgrsResponseStatus = a})

instance NFData CreateDBSubnetGroupResponse where
