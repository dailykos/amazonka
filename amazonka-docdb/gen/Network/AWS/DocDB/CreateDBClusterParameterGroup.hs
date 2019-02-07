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
-- Module      : Network.AWS.DocDB.CreateDBClusterParameterGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB cluster parameter group.
--
--
-- Parameters in a DB cluster parameter group apply to all of the instances in a DB cluster.
--
-- A DB cluster parameter group is initially created with the default parameters for the database engine used by instances in the DB cluster. To provide custom values for any of the parameters, you must modify the group after you create it. After you create a DB cluster parameter group, you must associate it with your DB cluster. For the new DB cluster parameter group and associated settings to take effect, you must then reboot the DB instances in the DB cluster without failover.
--
-- /Important:/ After you create a DB cluster parameter group, you should wait at least 5 minutes before creating your first DB cluster that uses that DB cluster parameter group as the default parameter group. This allows Amazon DocumentDB to fully complete the create action before the DB cluster parameter group is used as the default for a new DB cluster. This step is especially important for parameters that are critical when creating the default database for a DB cluster, such as the character set for the default database defined by the @character_set_database@ parameter.
--
module Network.AWS.DocDB.CreateDBClusterParameterGroup
    (
    -- * Creating a Request
      createDBClusterParameterGroup
    , CreateDBClusterParameterGroup
    -- * Request Lenses
    , cdcpgTags
    , cdcpgDBClusterParameterGroupName
    , cdcpgDBParameterGroupFamily
    , cdcpgDescription

    -- * Destructuring the Response
    , createDBClusterParameterGroupResponse
    , CreateDBClusterParameterGroupResponse
    -- * Response Lenses
    , cdbcpgrsDBClusterParameterGroup
    , cdbcpgrsResponseStatus
    ) where

import Network.AWS.DocDB.Types
import Network.AWS.DocDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of 'CreateDBClusterParameterGroup' .
--
--
--
-- /See:/ 'createDBClusterParameterGroup' smart constructor.
data CreateDBClusterParameterGroup = CreateDBClusterParameterGroup'
  { _cdcpgTags :: !(Maybe [Tag])
  , _cdcpgDBClusterParameterGroupName :: !Text
  , _cdcpgDBParameterGroupFamily :: !Text
  , _cdcpgDescription :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDBClusterParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcpgTags' - The tags to be assigned to the DB cluster parameter group.
--
-- * 'cdcpgDBClusterParameterGroupName' - The name of the DB cluster parameter group. Constraints:     * Must match the name of an existing @DBClusterParameterGroup@ .
--
-- * 'cdcpgDBParameterGroupFamily' - The DB cluster parameter group family name.
--
-- * 'cdcpgDescription' - The description for the DB cluster parameter group.
createDBClusterParameterGroup
    :: Text -- ^ 'cdcpgDBClusterParameterGroupName'
    -> Text -- ^ 'cdcpgDBParameterGroupFamily'
    -> Text -- ^ 'cdcpgDescription'
    -> CreateDBClusterParameterGroup
createDBClusterParameterGroup pDBClusterParameterGroupName_ pDBParameterGroupFamily_ pDescription_ =
  CreateDBClusterParameterGroup'
    { _cdcpgTags = Nothing
    , _cdcpgDBClusterParameterGroupName = pDBClusterParameterGroupName_
    , _cdcpgDBParameterGroupFamily = pDBParameterGroupFamily_
    , _cdcpgDescription = pDescription_
    }


-- | The tags to be assigned to the DB cluster parameter group.
cdcpgTags :: Lens' CreateDBClusterParameterGroup [Tag]
cdcpgTags = lens _cdcpgTags (\ s a -> s{_cdcpgTags = a}) . _Default . _Coerce

-- | The name of the DB cluster parameter group. Constraints:     * Must match the name of an existing @DBClusterParameterGroup@ .
cdcpgDBClusterParameterGroupName :: Lens' CreateDBClusterParameterGroup Text
cdcpgDBClusterParameterGroupName = lens _cdcpgDBClusterParameterGroupName (\ s a -> s{_cdcpgDBClusterParameterGroupName = a})

-- | The DB cluster parameter group family name.
cdcpgDBParameterGroupFamily :: Lens' CreateDBClusterParameterGroup Text
cdcpgDBParameterGroupFamily = lens _cdcpgDBParameterGroupFamily (\ s a -> s{_cdcpgDBParameterGroupFamily = a})

-- | The description for the DB cluster parameter group.
cdcpgDescription :: Lens' CreateDBClusterParameterGroup Text
cdcpgDescription = lens _cdcpgDescription (\ s a -> s{_cdcpgDescription = a})

instance AWSRequest CreateDBClusterParameterGroup
         where
        type Rs CreateDBClusterParameterGroup =
             CreateDBClusterParameterGroupResponse
        request = postQuery docDB
        response
          = receiveXMLWrapper
              "CreateDBClusterParameterGroupResult"
              (\ s h x ->
                 CreateDBClusterParameterGroupResponse' <$>
                   (x .@? "DBClusterParameterGroup") <*>
                     (pure (fromEnum s)))

instance Hashable CreateDBClusterParameterGroup where

instance NFData CreateDBClusterParameterGroup where

instance ToHeaders CreateDBClusterParameterGroup
         where
        toHeaders = const mempty

instance ToPath CreateDBClusterParameterGroup where
        toPath = const "/"

instance ToQuery CreateDBClusterParameterGroup where
        toQuery CreateDBClusterParameterGroup'{..}
          = mconcat
              ["Action" =:
                 ("CreateDBClusterParameterGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdcpgTags),
               "DBClusterParameterGroupName" =:
                 _cdcpgDBClusterParameterGroupName,
               "DBParameterGroupFamily" =:
                 _cdcpgDBParameterGroupFamily,
               "Description" =: _cdcpgDescription]

-- | /See:/ 'createDBClusterParameterGroupResponse' smart constructor.
data CreateDBClusterParameterGroupResponse = CreateDBClusterParameterGroupResponse'
  { _cdbcpgrsDBClusterParameterGroup :: !(Maybe DBClusterParameterGroup)
  , _cdbcpgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDBClusterParameterGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdbcpgrsDBClusterParameterGroup' - Undocumented member.
--
-- * 'cdbcpgrsResponseStatus' - -- | The response status code.
createDBClusterParameterGroupResponse
    :: Int -- ^ 'cdbcpgrsResponseStatus'
    -> CreateDBClusterParameterGroupResponse
createDBClusterParameterGroupResponse pResponseStatus_ =
  CreateDBClusterParameterGroupResponse'
    { _cdbcpgrsDBClusterParameterGroup = Nothing
    , _cdbcpgrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
cdbcpgrsDBClusterParameterGroup :: Lens' CreateDBClusterParameterGroupResponse (Maybe DBClusterParameterGroup)
cdbcpgrsDBClusterParameterGroup = lens _cdbcpgrsDBClusterParameterGroup (\ s a -> s{_cdbcpgrsDBClusterParameterGroup = a})

-- | -- | The response status code.
cdbcpgrsResponseStatus :: Lens' CreateDBClusterParameterGroupResponse Int
cdbcpgrsResponseStatus = lens _cdbcpgrsResponseStatus (\ s a -> s{_cdbcpgrsResponseStatus = a})

instance NFData CreateDBClusterParameterGroupResponse
         where
