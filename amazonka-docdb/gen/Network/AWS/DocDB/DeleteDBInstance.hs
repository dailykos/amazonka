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
-- Module      : Network.AWS.DocDB.DeleteDBInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a previously provisioned DB instance. 
--
--
module Network.AWS.DocDB.DeleteDBInstance
    (
    -- * Creating a Request
      deleteDBInstance
    , DeleteDBInstance
    -- * Request Lenses
    , ddiDBInstanceIdentifier

    -- * Destructuring the Response
    , deleteDBInstanceResponse
    , DeleteDBInstanceResponse
    -- * Response Lenses
    , ddirsDBInstance
    , ddirsResponseStatus
    ) where

import Network.AWS.DocDB.Types
import Network.AWS.DocDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input to 'DeleteDBInstance' .
--
--
--
-- /See:/ 'deleteDBInstance' smart constructor.
newtype DeleteDBInstance = DeleteDBInstance'
  { _ddiDBInstanceIdentifier :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddiDBInstanceIdentifier' - The DB instance identifier for the DB instance to be deleted. This parameter isn't case sensitive. Constraints:     * Must match the name of an existing DB instance.
deleteDBInstance
    :: Text -- ^ 'ddiDBInstanceIdentifier'
    -> DeleteDBInstance
deleteDBInstance pDBInstanceIdentifier_ =
  DeleteDBInstance' {_ddiDBInstanceIdentifier = pDBInstanceIdentifier_}


-- | The DB instance identifier for the DB instance to be deleted. This parameter isn't case sensitive. Constraints:     * Must match the name of an existing DB instance.
ddiDBInstanceIdentifier :: Lens' DeleteDBInstance Text
ddiDBInstanceIdentifier = lens _ddiDBInstanceIdentifier (\ s a -> s{_ddiDBInstanceIdentifier = a})

instance AWSRequest DeleteDBInstance where
        type Rs DeleteDBInstance = DeleteDBInstanceResponse
        request = postQuery docDB
        response
          = receiveXMLWrapper "DeleteDBInstanceResult"
              (\ s h x ->
                 DeleteDBInstanceResponse' <$>
                   (x .@? "DBInstance") <*> (pure (fromEnum s)))

instance Hashable DeleteDBInstance where

instance NFData DeleteDBInstance where

instance ToHeaders DeleteDBInstance where
        toHeaders = const mempty

instance ToPath DeleteDBInstance where
        toPath = const "/"

instance ToQuery DeleteDBInstance where
        toQuery DeleteDBInstance'{..}
          = mconcat
              ["Action" =: ("DeleteDBInstance" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBInstanceIdentifier" =: _ddiDBInstanceIdentifier]

-- | /See:/ 'deleteDBInstanceResponse' smart constructor.
data DeleteDBInstanceResponse = DeleteDBInstanceResponse'
  { _ddirsDBInstance :: !(Maybe DBInstance)
  , _ddirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDBInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddirsDBInstance' - Undocumented member.
--
-- * 'ddirsResponseStatus' - -- | The response status code.
deleteDBInstanceResponse
    :: Int -- ^ 'ddirsResponseStatus'
    -> DeleteDBInstanceResponse
deleteDBInstanceResponse pResponseStatus_ =
  DeleteDBInstanceResponse'
    {_ddirsDBInstance = Nothing, _ddirsResponseStatus = pResponseStatus_}


-- | Undocumented member.
ddirsDBInstance :: Lens' DeleteDBInstanceResponse (Maybe DBInstance)
ddirsDBInstance = lens _ddirsDBInstance (\ s a -> s{_ddirsDBInstance = a})

-- | -- | The response status code.
ddirsResponseStatus :: Lens' DeleteDBInstanceResponse Int
ddirsResponseStatus = lens _ddirsResponseStatus (\ s a -> s{_ddirsResponseStatus = a})

instance NFData DeleteDBInstanceResponse where
