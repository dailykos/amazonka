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
-- Module      : Network.AWS.DocDB.ModifyDBInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies settings for a DB instance. You can change one or more database configuration parameters by specifying these parameters and the new values in the request.
--
--
module Network.AWS.DocDB.ModifyDBInstance
    (
    -- * Creating a Request
      modifyDBInstance
    , ModifyDBInstance
    -- * Request Lenses
    , mdiAutoMinorVersionUpgrade
    , mdiNewDBInstanceIdentifier
    , mdiDBInstanceClass
    , mdiPromotionTier
    , mdiPreferredMaintenanceWindow
    , mdiApplyImmediately
    , mdiDBInstanceIdentifier

    -- * Destructuring the Response
    , modifyDBInstanceResponse
    , ModifyDBInstanceResponse
    -- * Response Lenses
    , mdirsDBInstance
    , mdirsResponseStatus
    ) where

import Network.AWS.DocDB.Types
import Network.AWS.DocDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input to 'ModifyDBInstance' .
--
--
--
-- /See:/ 'modifyDBInstance' smart constructor.
data ModifyDBInstance = ModifyDBInstance'
  { _mdiAutoMinorVersionUpgrade :: !(Maybe Bool)
  , _mdiNewDBInstanceIdentifier :: !(Maybe Text)
  , _mdiDBInstanceClass :: !(Maybe Text)
  , _mdiPromotionTier :: !(Maybe Int)
  , _mdiPreferredMaintenanceWindow :: !(Maybe Text)
  , _mdiApplyImmediately :: !(Maybe Bool)
  , _mdiDBInstanceIdentifier :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyDBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdiAutoMinorVersionUpgrade' - Indicates that minor version upgrades are applied automatically to the DB instance during the maintenance window. Changing this parameter doesn't result in an outage except in the following case, and the change is asynchronously applied as soon as possible. An outage results if this parameter is set to @true@ during the maintenance window, and a newer minor version is available, and Amazon DocumentDB has enabled automatic patching for that engine version. 
--
-- * 'mdiNewDBInstanceIdentifier' - The new DB instance identifier for the DB instance when renaming a DB instance. When you change the DB instance identifier, an instance reboot occurs immediately if you set @Apply Immediately@ to @true@ . It occurs during the next maintenance window if you set @Apply Immediately@ to @false@ . This value is stored as a lowercase string.  Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens.     * The first character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @mydbinstance@ 
--
-- * 'mdiDBInstanceClass' - The new compute and memory capacity of the DB instance; for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions.  If you modify the DB instance class, an outage occurs during the change. The change is applied during the next maintenance window, unless @ApplyImmediately@ is specified as @true@ for this request.  Default: Uses existing setting.
--
-- * 'mdiPromotionTier' - A value that specifies the order in which an Amazon DocumentDB replica is promoted to the primary instance after a failure of the existing primary instance. Default: 1 Valid values: 0-15
--
-- * 'mdiPreferredMaintenanceWindow' - The weekly time range (in UTC) during which system maintenance can occur, which might result in an outage. Changing this parameter doesn't result in an outage except in the following situation, and the change is asynchronously applied as soon as possible. If there are pending actions that cause a reboot, and the maintenance window is changed to include the current time, changing this parameter causes a reboot of the DB instance. If you are moving this window to the current time, there must be at least 30 minutes between the current time and end of the window to ensure that pending changes are applied. Default: Uses existing setting. Format: @ddd:hh24:mi-ddd:hh24:mi@  Valid days: Mon, Tue, Wed, Thu, Fri, Sat, Sun Constraints: Must be at least 30 minutes.
--
-- * 'mdiApplyImmediately' - Specifies whether the modifications in this request and any pending modifications are asynchronously applied as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the DB instance.  If this parameter is set to @false@ , changes to the DB instance are applied during the next maintenance window. Some parameter changes can cause an outage and are applied on the next reboot. Default: @false@ 
--
-- * 'mdiDBInstanceIdentifier' - The DB instance identifier. This value is stored as a lowercase string. Constraints:     * Must match the identifier of an existing @DBInstance@ .
modifyDBInstance
    :: Text -- ^ 'mdiDBInstanceIdentifier'
    -> ModifyDBInstance
modifyDBInstance pDBInstanceIdentifier_ =
  ModifyDBInstance'
    { _mdiAutoMinorVersionUpgrade = Nothing
    , _mdiNewDBInstanceIdentifier = Nothing
    , _mdiDBInstanceClass = Nothing
    , _mdiPromotionTier = Nothing
    , _mdiPreferredMaintenanceWindow = Nothing
    , _mdiApplyImmediately = Nothing
    , _mdiDBInstanceIdentifier = pDBInstanceIdentifier_
    }


-- | Indicates that minor version upgrades are applied automatically to the DB instance during the maintenance window. Changing this parameter doesn't result in an outage except in the following case, and the change is asynchronously applied as soon as possible. An outage results if this parameter is set to @true@ during the maintenance window, and a newer minor version is available, and Amazon DocumentDB has enabled automatic patching for that engine version. 
mdiAutoMinorVersionUpgrade :: Lens' ModifyDBInstance (Maybe Bool)
mdiAutoMinorVersionUpgrade = lens _mdiAutoMinorVersionUpgrade (\ s a -> s{_mdiAutoMinorVersionUpgrade = a})

-- | The new DB instance identifier for the DB instance when renaming a DB instance. When you change the DB instance identifier, an instance reboot occurs immediately if you set @Apply Immediately@ to @true@ . It occurs during the next maintenance window if you set @Apply Immediately@ to @false@ . This value is stored as a lowercase string.  Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens.     * The first character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @mydbinstance@ 
mdiNewDBInstanceIdentifier :: Lens' ModifyDBInstance (Maybe Text)
mdiNewDBInstanceIdentifier = lens _mdiNewDBInstanceIdentifier (\ s a -> s{_mdiNewDBInstanceIdentifier = a})

-- | The new compute and memory capacity of the DB instance; for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions.  If you modify the DB instance class, an outage occurs during the change. The change is applied during the next maintenance window, unless @ApplyImmediately@ is specified as @true@ for this request.  Default: Uses existing setting.
mdiDBInstanceClass :: Lens' ModifyDBInstance (Maybe Text)
mdiDBInstanceClass = lens _mdiDBInstanceClass (\ s a -> s{_mdiDBInstanceClass = a})

-- | A value that specifies the order in which an Amazon DocumentDB replica is promoted to the primary instance after a failure of the existing primary instance. Default: 1 Valid values: 0-15
mdiPromotionTier :: Lens' ModifyDBInstance (Maybe Int)
mdiPromotionTier = lens _mdiPromotionTier (\ s a -> s{_mdiPromotionTier = a})

-- | The weekly time range (in UTC) during which system maintenance can occur, which might result in an outage. Changing this parameter doesn't result in an outage except in the following situation, and the change is asynchronously applied as soon as possible. If there are pending actions that cause a reboot, and the maintenance window is changed to include the current time, changing this parameter causes a reboot of the DB instance. If you are moving this window to the current time, there must be at least 30 minutes between the current time and end of the window to ensure that pending changes are applied. Default: Uses existing setting. Format: @ddd:hh24:mi-ddd:hh24:mi@  Valid days: Mon, Tue, Wed, Thu, Fri, Sat, Sun Constraints: Must be at least 30 minutes.
mdiPreferredMaintenanceWindow :: Lens' ModifyDBInstance (Maybe Text)
mdiPreferredMaintenanceWindow = lens _mdiPreferredMaintenanceWindow (\ s a -> s{_mdiPreferredMaintenanceWindow = a})

-- | Specifies whether the modifications in this request and any pending modifications are asynchronously applied as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the DB instance.  If this parameter is set to @false@ , changes to the DB instance are applied during the next maintenance window. Some parameter changes can cause an outage and are applied on the next reboot. Default: @false@ 
mdiApplyImmediately :: Lens' ModifyDBInstance (Maybe Bool)
mdiApplyImmediately = lens _mdiApplyImmediately (\ s a -> s{_mdiApplyImmediately = a})

-- | The DB instance identifier. This value is stored as a lowercase string. Constraints:     * Must match the identifier of an existing @DBInstance@ .
mdiDBInstanceIdentifier :: Lens' ModifyDBInstance Text
mdiDBInstanceIdentifier = lens _mdiDBInstanceIdentifier (\ s a -> s{_mdiDBInstanceIdentifier = a})

instance AWSRequest ModifyDBInstance where
        type Rs ModifyDBInstance = ModifyDBInstanceResponse
        request = postQuery docDB
        response
          = receiveXMLWrapper "ModifyDBInstanceResult"
              (\ s h x ->
                 ModifyDBInstanceResponse' <$>
                   (x .@? "DBInstance") <*> (pure (fromEnum s)))

instance Hashable ModifyDBInstance where

instance NFData ModifyDBInstance where

instance ToHeaders ModifyDBInstance where
        toHeaders = const mempty

instance ToPath ModifyDBInstance where
        toPath = const "/"

instance ToQuery ModifyDBInstance where
        toQuery ModifyDBInstance'{..}
          = mconcat
              ["Action" =: ("ModifyDBInstance" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "AutoMinorVersionUpgrade" =:
                 _mdiAutoMinorVersionUpgrade,
               "NewDBInstanceIdentifier" =:
                 _mdiNewDBInstanceIdentifier,
               "DBInstanceClass" =: _mdiDBInstanceClass,
               "PromotionTier" =: _mdiPromotionTier,
               "PreferredMaintenanceWindow" =:
                 _mdiPreferredMaintenanceWindow,
               "ApplyImmediately" =: _mdiApplyImmediately,
               "DBInstanceIdentifier" =: _mdiDBInstanceIdentifier]

-- | /See:/ 'modifyDBInstanceResponse' smart constructor.
data ModifyDBInstanceResponse = ModifyDBInstanceResponse'
  { _mdirsDBInstance :: !(Maybe DBInstance)
  , _mdirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyDBInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdirsDBInstance' - Undocumented member.
--
-- * 'mdirsResponseStatus' - -- | The response status code.
modifyDBInstanceResponse
    :: Int -- ^ 'mdirsResponseStatus'
    -> ModifyDBInstanceResponse
modifyDBInstanceResponse pResponseStatus_ =
  ModifyDBInstanceResponse'
    {_mdirsDBInstance = Nothing, _mdirsResponseStatus = pResponseStatus_}


-- | Undocumented member.
mdirsDBInstance :: Lens' ModifyDBInstanceResponse (Maybe DBInstance)
mdirsDBInstance = lens _mdirsDBInstance (\ s a -> s{_mdirsDBInstance = a})

-- | -- | The response status code.
mdirsResponseStatus :: Lens' ModifyDBInstanceResponse Int
mdirsResponseStatus = lens _mdirsResponseStatus (\ s a -> s{_mdirsResponseStatus = a})

instance NFData ModifyDBInstanceResponse where
