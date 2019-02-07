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
-- Module      : Network.AWS.DocDB.CreateDBInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB instance.
--
--
module Network.AWS.DocDB.CreateDBInstance
    (
    -- * Creating a Request
      createDBInstance
    , CreateDBInstance
    -- * Request Lenses
    , cdiAutoMinorVersionUpgrade
    , cdiPromotionTier
    , cdiPreferredMaintenanceWindow
    , cdiAvailabilityZone
    , cdiTags
    , cdiDBInstanceIdentifier
    , cdiDBInstanceClass
    , cdiEngine
    , cdiDBClusterIdentifier

    -- * Destructuring the Response
    , createDBInstanceResponse
    , CreateDBInstanceResponse
    -- * Response Lenses
    , cdirsDBInstance
    , cdirsResponseStatus
    ) where

import Network.AWS.DocDB.Types
import Network.AWS.DocDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input to 'CreateDBInstance' .
--
--
--
-- /See:/ 'createDBInstance' smart constructor.
data CreateDBInstance = CreateDBInstance'
  { _cdiAutoMinorVersionUpgrade :: !(Maybe Bool)
  , _cdiPromotionTier :: !(Maybe Int)
  , _cdiPreferredMaintenanceWindow :: !(Maybe Text)
  , _cdiAvailabilityZone :: !(Maybe Text)
  , _cdiTags :: !(Maybe [Tag])
  , _cdiDBInstanceIdentifier :: !Text
  , _cdiDBInstanceClass :: !Text
  , _cdiEngine :: !Text
  , _cdiDBClusterIdentifier :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdiAutoMinorVersionUpgrade' - Indicates that minor engine upgrades are applied automatically to the DB instance during the maintenance window. Default: @true@ 
--
-- * 'cdiPromotionTier' - A value that specifies the order in which an Amazon DocumentDB replica is promoted to the primary instance after a failure of the existing primary instance. Default: 1 Valid values: 0-15
--
-- * 'cdiPreferredMaintenanceWindow' - The time range each week during which system maintenance can occur, in Universal Coordinated Time (UTC). Format: @ddd:hh24:mi-ddd:hh24:mi@  The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week.  Valid days: Mon, Tue, Wed, Thu, Fri, Sat, Sun Constraints: Minimum 30-minute window.
--
-- * 'cdiAvailabilityZone' - The Amazon EC2 Availability Zone that the DB instance is created in. Default: A random, system-chosen Availability Zone in the endpoint's AWS Region. Example: @us-east-1d@  Constraint: The @AvailabilityZone@ parameter can't be specified if the @MultiAZ@ parameter is set to @true@ . The specified Availability Zone must be in the same AWS Region as the current endpoint. 
--
-- * 'cdiTags' - The tags to be assigned to the DB instance.
--
-- * 'cdiDBInstanceIdentifier' - The DB instance identifier. This parameter is stored as a lowercase string. Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens.     * The first character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @mydbinstance@ 
--
-- * 'cdiDBInstanceClass' - The compute and memory capacity of the DB instance; for example, @db.m4.large@ . 
--
-- * 'cdiEngine' - The name of the database engine to be used for this instance. Valid value: @docdb@ 
--
-- * 'cdiDBClusterIdentifier' - The identifier of the DB cluster that the instance will belong to.
createDBInstance
    :: Text -- ^ 'cdiDBInstanceIdentifier'
    -> Text -- ^ 'cdiDBInstanceClass'
    -> Text -- ^ 'cdiEngine'
    -> Text -- ^ 'cdiDBClusterIdentifier'
    -> CreateDBInstance
createDBInstance pDBInstanceIdentifier_ pDBInstanceClass_ pEngine_ pDBClusterIdentifier_ =
  CreateDBInstance'
    { _cdiAutoMinorVersionUpgrade = Nothing
    , _cdiPromotionTier = Nothing
    , _cdiPreferredMaintenanceWindow = Nothing
    , _cdiAvailabilityZone = Nothing
    , _cdiTags = Nothing
    , _cdiDBInstanceIdentifier = pDBInstanceIdentifier_
    , _cdiDBInstanceClass = pDBInstanceClass_
    , _cdiEngine = pEngine_
    , _cdiDBClusterIdentifier = pDBClusterIdentifier_
    }


-- | Indicates that minor engine upgrades are applied automatically to the DB instance during the maintenance window. Default: @true@ 
cdiAutoMinorVersionUpgrade :: Lens' CreateDBInstance (Maybe Bool)
cdiAutoMinorVersionUpgrade = lens _cdiAutoMinorVersionUpgrade (\ s a -> s{_cdiAutoMinorVersionUpgrade = a})

-- | A value that specifies the order in which an Amazon DocumentDB replica is promoted to the primary instance after a failure of the existing primary instance. Default: 1 Valid values: 0-15
cdiPromotionTier :: Lens' CreateDBInstance (Maybe Int)
cdiPromotionTier = lens _cdiPromotionTier (\ s a -> s{_cdiPromotionTier = a})

-- | The time range each week during which system maintenance can occur, in Universal Coordinated Time (UTC). Format: @ddd:hh24:mi-ddd:hh24:mi@  The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week.  Valid days: Mon, Tue, Wed, Thu, Fri, Sat, Sun Constraints: Minimum 30-minute window.
cdiPreferredMaintenanceWindow :: Lens' CreateDBInstance (Maybe Text)
cdiPreferredMaintenanceWindow = lens _cdiPreferredMaintenanceWindow (\ s a -> s{_cdiPreferredMaintenanceWindow = a})

-- | The Amazon EC2 Availability Zone that the DB instance is created in. Default: A random, system-chosen Availability Zone in the endpoint's AWS Region. Example: @us-east-1d@  Constraint: The @AvailabilityZone@ parameter can't be specified if the @MultiAZ@ parameter is set to @true@ . The specified Availability Zone must be in the same AWS Region as the current endpoint. 
cdiAvailabilityZone :: Lens' CreateDBInstance (Maybe Text)
cdiAvailabilityZone = lens _cdiAvailabilityZone (\ s a -> s{_cdiAvailabilityZone = a})

-- | The tags to be assigned to the DB instance.
cdiTags :: Lens' CreateDBInstance [Tag]
cdiTags = lens _cdiTags (\ s a -> s{_cdiTags = a}) . _Default . _Coerce

-- | The DB instance identifier. This parameter is stored as a lowercase string. Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens.     * The first character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @mydbinstance@ 
cdiDBInstanceIdentifier :: Lens' CreateDBInstance Text
cdiDBInstanceIdentifier = lens _cdiDBInstanceIdentifier (\ s a -> s{_cdiDBInstanceIdentifier = a})

-- | The compute and memory capacity of the DB instance; for example, @db.m4.large@ . 
cdiDBInstanceClass :: Lens' CreateDBInstance Text
cdiDBInstanceClass = lens _cdiDBInstanceClass (\ s a -> s{_cdiDBInstanceClass = a})

-- | The name of the database engine to be used for this instance. Valid value: @docdb@ 
cdiEngine :: Lens' CreateDBInstance Text
cdiEngine = lens _cdiEngine (\ s a -> s{_cdiEngine = a})

-- | The identifier of the DB cluster that the instance will belong to.
cdiDBClusterIdentifier :: Lens' CreateDBInstance Text
cdiDBClusterIdentifier = lens _cdiDBClusterIdentifier (\ s a -> s{_cdiDBClusterIdentifier = a})

instance AWSRequest CreateDBInstance where
        type Rs CreateDBInstance = CreateDBInstanceResponse
        request = postQuery docDB
        response
          = receiveXMLWrapper "CreateDBInstanceResult"
              (\ s h x ->
                 CreateDBInstanceResponse' <$>
                   (x .@? "DBInstance") <*> (pure (fromEnum s)))

instance Hashable CreateDBInstance where

instance NFData CreateDBInstance where

instance ToHeaders CreateDBInstance where
        toHeaders = const mempty

instance ToPath CreateDBInstance where
        toPath = const "/"

instance ToQuery CreateDBInstance where
        toQuery CreateDBInstance'{..}
          = mconcat
              ["Action" =: ("CreateDBInstance" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "AutoMinorVersionUpgrade" =:
                 _cdiAutoMinorVersionUpgrade,
               "PromotionTier" =: _cdiPromotionTier,
               "PreferredMaintenanceWindow" =:
                 _cdiPreferredMaintenanceWindow,
               "AvailabilityZone" =: _cdiAvailabilityZone,
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdiTags),
               "DBInstanceIdentifier" =: _cdiDBInstanceIdentifier,
               "DBInstanceClass" =: _cdiDBInstanceClass,
               "Engine" =: _cdiEngine,
               "DBClusterIdentifier" =: _cdiDBClusterIdentifier]

-- | /See:/ 'createDBInstanceResponse' smart constructor.
data CreateDBInstanceResponse = CreateDBInstanceResponse'
  { _cdirsDBInstance :: !(Maybe DBInstance)
  , _cdirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDBInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdirsDBInstance' - Undocumented member.
--
-- * 'cdirsResponseStatus' - -- | The response status code.
createDBInstanceResponse
    :: Int -- ^ 'cdirsResponseStatus'
    -> CreateDBInstanceResponse
createDBInstanceResponse pResponseStatus_ =
  CreateDBInstanceResponse'
    {_cdirsDBInstance = Nothing, _cdirsResponseStatus = pResponseStatus_}


-- | Undocumented member.
cdirsDBInstance :: Lens' CreateDBInstanceResponse (Maybe DBInstance)
cdirsDBInstance = lens _cdirsDBInstance (\ s a -> s{_cdirsDBInstance = a})

-- | -- | The response status code.
cdirsResponseStatus :: Lens' CreateDBInstanceResponse Int
cdirsResponseStatus = lens _cdirsResponseStatus (\ s a -> s{_cdirsResponseStatus = a})

instance NFData CreateDBInstanceResponse where
