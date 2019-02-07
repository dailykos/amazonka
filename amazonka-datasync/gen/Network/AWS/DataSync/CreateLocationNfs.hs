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
-- Module      : Network.AWS.DataSync.CreateLocationNfs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint for a Network File System (NFS) file system.
--
--
module Network.AWS.DataSync.CreateLocationNfs
    (
    -- * Creating a Request
      createLocationNfs
    , CreateLocationNfs
    -- * Request Lenses
    , clnTags
    , clnSubdirectory
    , clnServerHostname
    , clnOnPremConfig

    -- * Destructuring the Response
    , createLocationNfsResponse
    , CreateLocationNfsResponse
    -- * Response Lenses
    , clnrsLocationARN
    , clnrsResponseStatus
    ) where

import Network.AWS.DataSync.Types
import Network.AWS.DataSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | CreateLocationNfsRequest
--
--
--
-- /See:/ 'createLocationNfs' smart constructor.
data CreateLocationNfs = CreateLocationNfs'
  { _clnTags :: !(Maybe [TagListEntry])
  , _clnSubdirectory :: !Text
  , _clnServerHostname :: !Text
  , _clnOnPremConfig :: !OnPremConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLocationNfs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clnTags' - The key-value pair that represents the tag that you want to add to the location. The value can be an empty string. We recommend using tags to name your resources.
--
-- * 'clnSubdirectory' - The subdirectory in the NFS file system that is used to read data from the NFS source location or write data to the NFS destination. The NFS path should be a path that's exported by the NFS server, or a subdirectory of that path. The path should be such that it can be mounted by other NFS clients in your network.  To see all the paths exported by your NFS server. run "@showmount -e nfs-server-name@ " from an NFS client that has access to your server. You can specify any directory that appears in the results, and any subdirectory of that directory. Ensure that the NFS export is accessible without Kerberos authentication.  To transfer all the data in the folder you specified, DataSync needs to have permissions to read all the data. To ensure this, either configure the NFS export with @no_root_squash,@ or ensure that the permissions for all of the files that you want sync allow read access for all users. Doing either enables the agent to read the files. For the agent to access directories, you must additionally enable all execute access. For information about NFS export configuration, see <https://www.centos.org/docs/5/html/Deployment_Guide-en-US/s1-nfs-server-config-exports.html 18.7. The /etc/exports Configuration File> in the Centos documentation. 
--
-- * 'clnServerHostname' - The name of the NFS server. This value is the IP address or Domain Name Service (DNS) name of the NFS server. An agent that is installed on-premises uses this host name to mount the NFS server in a network. 
--
-- * 'clnOnPremConfig' - Contains a list of Amazon Resource Names (ARNs) of agents that are used to connect to an NFS server.
createLocationNfs
    :: Text -- ^ 'clnSubdirectory'
    -> Text -- ^ 'clnServerHostname'
    -> OnPremConfig -- ^ 'clnOnPremConfig'
    -> CreateLocationNfs
createLocationNfs pSubdirectory_ pServerHostname_ pOnPremConfig_ =
  CreateLocationNfs'
    { _clnTags = Nothing
    , _clnSubdirectory = pSubdirectory_
    , _clnServerHostname = pServerHostname_
    , _clnOnPremConfig = pOnPremConfig_
    }


-- | The key-value pair that represents the tag that you want to add to the location. The value can be an empty string. We recommend using tags to name your resources.
clnTags :: Lens' CreateLocationNfs [TagListEntry]
clnTags = lens _clnTags (\ s a -> s{_clnTags = a}) . _Default . _Coerce

-- | The subdirectory in the NFS file system that is used to read data from the NFS source location or write data to the NFS destination. The NFS path should be a path that's exported by the NFS server, or a subdirectory of that path. The path should be such that it can be mounted by other NFS clients in your network.  To see all the paths exported by your NFS server. run "@showmount -e nfs-server-name@ " from an NFS client that has access to your server. You can specify any directory that appears in the results, and any subdirectory of that directory. Ensure that the NFS export is accessible without Kerberos authentication.  To transfer all the data in the folder you specified, DataSync needs to have permissions to read all the data. To ensure this, either configure the NFS export with @no_root_squash,@ or ensure that the permissions for all of the files that you want sync allow read access for all users. Doing either enables the agent to read the files. For the agent to access directories, you must additionally enable all execute access. For information about NFS export configuration, see <https://www.centos.org/docs/5/html/Deployment_Guide-en-US/s1-nfs-server-config-exports.html 18.7. The /etc/exports Configuration File> in the Centos documentation. 
clnSubdirectory :: Lens' CreateLocationNfs Text
clnSubdirectory = lens _clnSubdirectory (\ s a -> s{_clnSubdirectory = a})

-- | The name of the NFS server. This value is the IP address or Domain Name Service (DNS) name of the NFS server. An agent that is installed on-premises uses this host name to mount the NFS server in a network. 
clnServerHostname :: Lens' CreateLocationNfs Text
clnServerHostname = lens _clnServerHostname (\ s a -> s{_clnServerHostname = a})

-- | Contains a list of Amazon Resource Names (ARNs) of agents that are used to connect to an NFS server.
clnOnPremConfig :: Lens' CreateLocationNfs OnPremConfig
clnOnPremConfig = lens _clnOnPremConfig (\ s a -> s{_clnOnPremConfig = a})

instance AWSRequest CreateLocationNfs where
        type Rs CreateLocationNfs = CreateLocationNfsResponse
        request = postJSON dataSync
        response
          = receiveJSON
              (\ s h x ->
                 CreateLocationNfsResponse' <$>
                   (x .?> "LocationArn") <*> (pure (fromEnum s)))

instance Hashable CreateLocationNfs where

instance NFData CreateLocationNfs where

instance ToHeaders CreateLocationNfs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("FmrsService.CreateLocationNfs" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateLocationNfs where
        toJSON CreateLocationNfs'{..}
          = object
              (catMaybes
                 [("Tags" .=) <$> _clnTags,
                  Just ("Subdirectory" .= _clnSubdirectory),
                  Just ("ServerHostname" .= _clnServerHostname),
                  Just ("OnPremConfig" .= _clnOnPremConfig)])

instance ToPath CreateLocationNfs where
        toPath = const "/"

instance ToQuery CreateLocationNfs where
        toQuery = const mempty

-- | CreateLocationNfsResponse
--
--
--
-- /See:/ 'createLocationNfsResponse' smart constructor.
data CreateLocationNfsResponse = CreateLocationNfsResponse'
  { _clnrsLocationARN :: !(Maybe Text)
  , _clnrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLocationNfsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clnrsLocationARN' - The Amazon Resource Name (ARN) of the source NFS file system location that is created.
--
-- * 'clnrsResponseStatus' - -- | The response status code.
createLocationNfsResponse
    :: Int -- ^ 'clnrsResponseStatus'
    -> CreateLocationNfsResponse
createLocationNfsResponse pResponseStatus_ =
  CreateLocationNfsResponse'
    {_clnrsLocationARN = Nothing, _clnrsResponseStatus = pResponseStatus_}


-- | The Amazon Resource Name (ARN) of the source NFS file system location that is created.
clnrsLocationARN :: Lens' CreateLocationNfsResponse (Maybe Text)
clnrsLocationARN = lens _clnrsLocationARN (\ s a -> s{_clnrsLocationARN = a})

-- | -- | The response status code.
clnrsResponseStatus :: Lens' CreateLocationNfsResponse Int
clnrsResponseStatus = lens _clnrsResponseStatus (\ s a -> s{_clnrsResponseStatus = a})

instance NFData CreateLocationNfsResponse where
