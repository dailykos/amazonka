{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataSync.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DataSync.Types.Product where

import Network.AWS.DataSync.Internal
import Network.AWS.DataSync.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a single entry in a list of agents. @AgentListEntry@ returns an array that contains a list of agents when the 'ListAgents' operation is called.
--
--
--
-- /See:/ 'agentListEntry' smart constructor.
data AgentListEntry = AgentListEntry'
  { _aleStatus :: !(Maybe AgentStatus)
  , _aleAgentARN :: !(Maybe Text)
  , _aleName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AgentListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aleStatus' - The status of the agent.
--
-- * 'aleAgentARN' - The Amazon Resource Name (ARN) of the agent.
--
-- * 'aleName' - The name of the agent.
agentListEntry
    :: AgentListEntry
agentListEntry =
  AgentListEntry'
    {_aleStatus = Nothing, _aleAgentARN = Nothing, _aleName = Nothing}


-- | The status of the agent.
aleStatus :: Lens' AgentListEntry (Maybe AgentStatus)
aleStatus = lens _aleStatus (\ s a -> s{_aleStatus = a})

-- | The Amazon Resource Name (ARN) of the agent.
aleAgentARN :: Lens' AgentListEntry (Maybe Text)
aleAgentARN = lens _aleAgentARN (\ s a -> s{_aleAgentARN = a})

-- | The name of the agent.
aleName :: Lens' AgentListEntry (Maybe Text)
aleName = lens _aleName (\ s a -> s{_aleName = a})

instance FromJSON AgentListEntry where
        parseJSON
          = withObject "AgentListEntry"
              (\ x ->
                 AgentListEntry' <$>
                   (x .:? "Status") <*> (x .:? "AgentArn") <*>
                     (x .:? "Name"))

instance Hashable AgentListEntry where

instance NFData AgentListEntry where

-- | The subnet and the security group that the target Amazon EFS file system uses. The subnet must have at least one mount target for that file system. The security group that you provide needs to be able to communicate with the security group on the mount target in the subnet specified. 
--
--
-- The exact relationship between security group M (of the mount target) and security group S (which you provide for DataSync to use at this stage) is as follows: 
--
--     * Security group M (which you associate with the mount target) must allow inbound access for the Transmission Control Protocol (TCP) on the NFS port (2049) from security group S. You can enable inbound connections either by IP address (CIDR range) or security group. 
--
--     * Security group S (provided to DataSync to access EFS) should have a rule that enables outbound connections to the NFS port on one of the file system’s mount targets. You can enable outbound connections either by IP address (CIDR range) or security group. For information about security groups and mount targets, see <https://docs.aws.amazon.com/efs/latest/ug/security-considerations.html#network-access Security Groups for Amazon EC2 Instances and Mount Targets> in the /Amazon EFS User Guide./ 
--
--
--
--
-- /See:/ 'ec2Config' smart constructor.
data EC2Config = EC2Config'
  { _ecSubnetARN :: !Text
  , _ecSecurityGroupARNs :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EC2Config' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecSubnetARN' - The ARN of the subnet that the Amazon EC2 resource belongs in. 
--
-- * 'ecSecurityGroupARNs' - The Amazon Resource Names (ARNs) of the security groups that are configured for the Amazon EC2 resource.
ec2Config
    :: Text -- ^ 'ecSubnetARN'
    -> NonEmpty Text -- ^ 'ecSecurityGroupARNs'
    -> EC2Config
ec2Config pSubnetARN_ pSecurityGroupARNs_ =
  EC2Config'
    { _ecSubnetARN = pSubnetARN_
    , _ecSecurityGroupARNs = _List1 # pSecurityGroupARNs_
    }


-- | The ARN of the subnet that the Amazon EC2 resource belongs in. 
ecSubnetARN :: Lens' EC2Config Text
ecSubnetARN = lens _ecSubnetARN (\ s a -> s{_ecSubnetARN = a})

-- | The Amazon Resource Names (ARNs) of the security groups that are configured for the Amazon EC2 resource.
ecSecurityGroupARNs :: Lens' EC2Config (NonEmpty Text)
ecSecurityGroupARNs = lens _ecSecurityGroupARNs (\ s a -> s{_ecSecurityGroupARNs = a}) . _List1

instance FromJSON EC2Config where
        parseJSON
          = withObject "EC2Config"
              (\ x ->
                 EC2Config' <$>
                   (x .: "SubnetArn") <*> (x .: "SecurityGroupArns"))

instance Hashable EC2Config where

instance NFData EC2Config where

instance ToJSON EC2Config where
        toJSON EC2Config'{..}
          = object
              (catMaybes
                 [Just ("SubnetArn" .= _ecSubnetARN),
                  Just ("SecurityGroupArns" .= _ecSecurityGroupARNs)])

-- | Represents a single entry in a list of locations. @LocationListEntry@ returns an array that contains a list of locations when the 'ListLocations' operation is called.
--
--
--
-- /See:/ 'locationListEntry' smart constructor.
data LocationListEntry = LocationListEntry'
  { _lleLocationURI :: !(Maybe Text)
  , _lleLocationARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LocationListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lleLocationURI' - Represents a list of URLs of a location. @LocationUri@ returns an array that contains a list of locations when the 'ListLocations' operation is called. Format: @TYPE://GLOBAL_ID/SUBDIR@ . TYPE designates the type of location. Valid values: NFS | EFS | S3. GLOBAL_ID is the globally unique identifier of the resource that backs the location. An example for EFS is @us-east-2.fs-abcd1234@ . An example for Amazon S3 is the bucket name, such as @myBucket@ . An example for NFS is a valid IPv4 address or a host name compliant with Domain Name Service (DNS). SUBDIR is a valid file system path, delimited by forward slashes as is the *nix convention. For NFS and Amazon EFS, it's the export path to mount the location. For Amazon S3, it's the prefix path that you mount to and treat as the root of the location.
--
-- * 'lleLocationARN' - The Amazon Resource Name (ARN) of the location. For Network File System (NFS) or Amazon EFS, the location is the export path. For Amazon S3, the location is the prefix path that you want to mount and use as the root of the location.
locationListEntry
    :: LocationListEntry
locationListEntry =
  LocationListEntry' {_lleLocationURI = Nothing, _lleLocationARN = Nothing}


-- | Represents a list of URLs of a location. @LocationUri@ returns an array that contains a list of locations when the 'ListLocations' operation is called. Format: @TYPE://GLOBAL_ID/SUBDIR@ . TYPE designates the type of location. Valid values: NFS | EFS | S3. GLOBAL_ID is the globally unique identifier of the resource that backs the location. An example for EFS is @us-east-2.fs-abcd1234@ . An example for Amazon S3 is the bucket name, such as @myBucket@ . An example for NFS is a valid IPv4 address or a host name compliant with Domain Name Service (DNS). SUBDIR is a valid file system path, delimited by forward slashes as is the *nix convention. For NFS and Amazon EFS, it's the export path to mount the location. For Amazon S3, it's the prefix path that you mount to and treat as the root of the location.
lleLocationURI :: Lens' LocationListEntry (Maybe Text)
lleLocationURI = lens _lleLocationURI (\ s a -> s{_lleLocationURI = a})

-- | The Amazon Resource Name (ARN) of the location. For Network File System (NFS) or Amazon EFS, the location is the export path. For Amazon S3, the location is the prefix path that you want to mount and use as the root of the location.
lleLocationARN :: Lens' LocationListEntry (Maybe Text)
lleLocationARN = lens _lleLocationARN (\ s a -> s{_lleLocationARN = a})

instance FromJSON LocationListEntry where
        parseJSON
          = withObject "LocationListEntry"
              (\ x ->
                 LocationListEntry' <$>
                   (x .:? "LocationUri") <*> (x .:? "LocationArn"))

instance Hashable LocationListEntry where

instance NFData LocationListEntry where

-- | A list of Amazon Resource Names (ARNs) of agents to use for a Network File System (NFS) location.
--
--
--
-- /See:/ 'onPremConfig' smart constructor.
newtype OnPremConfig = OnPremConfig'
  { _opcAgentARNs :: List1 Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OnPremConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'opcAgentARNs' - ARNs)of the agents to use for an NFS location.
onPremConfig
    :: NonEmpty Text -- ^ 'opcAgentARNs'
    -> OnPremConfig
onPremConfig pAgentARNs_ = OnPremConfig' {_opcAgentARNs = _List1 # pAgentARNs_}


-- | ARNs)of the agents to use for an NFS location.
opcAgentARNs :: Lens' OnPremConfig (NonEmpty Text)
opcAgentARNs = lens _opcAgentARNs (\ s a -> s{_opcAgentARNs = a}) . _List1

instance FromJSON OnPremConfig where
        parseJSON
          = withObject "OnPremConfig"
              (\ x -> OnPremConfig' <$> (x .: "AgentArns"))

instance Hashable OnPremConfig where

instance NFData OnPremConfig where

instance ToJSON OnPremConfig where
        toJSON OnPremConfig'{..}
          = object
              (catMaybes [Just ("AgentArns" .= _opcAgentARNs)])

-- | Represents the options that are available to control the behavior of a 'StartTaskExecution' operation. Behavior includes preserving metadata such as user ID (UID), group ID (GID), and file permissions, and also overwriting files in the destination, data integrity verification, and so on.
--
--
-- A task has a set of default options associated with it. If you don't specify an option in 'StartTaskExecution' , the default value is used. You can override the defaults options on each task execution by specifying an overriding @Options@ value to 'StartTaskExecution' .
--
--
-- /See:/ 'options' smart constructor.
data Options = Options'
  { _oAtime :: !(Maybe Atime)
  , _oVerifyMode :: !(Maybe VerifyMode)
  , _oPosixPermissions :: !(Maybe PosixPermissions)
  , _oMtime :: !(Maybe Mtime)
  , _oUid :: !(Maybe Uid)
  , _oBytesPerSecond :: !(Maybe Integer)
  , _oGid :: !(Maybe Gid)
  , _oPreserveDeletedFiles :: !(Maybe PreserveDeletedFiles)
  , _oPreserveDevices :: !(Maybe PreserveDevices)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Options' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oAtime' - A file metadata value that shows the last time a file was accessed (that is, when the file was read or written to). If you set @Atime@ to BEST_EFFORT, DataSync attempts to preserve the original @Atime@ attribute on all source files (that is, the version before the PREPARING phase). However, @Atime@ 's behavior is not fully standard across platforms, so AWS DataSync can only do this on a best-effort basis.  Default value: BEST_EFFORT. BEST_EFFORT: Attempt to preserve the per-file @Atime@ value (recommended). NONE: Ignore @Atime@ .
--
-- * 'oVerifyMode' - A value that determines whether a data integrity verification should be performed at the end of a task execution after all data and metadata have been transferred.  Default value: POINT_IN_TIME_CONSISTENT. POINT_IN_TIME_CONSISTENT: Perform verification (recommended).  NONE: Skip verification.
--
-- * 'oPosixPermissions' - A value that determines which users or groups can access a file for a specific purpose such as reading, writing, or execution of the file.  Default value: PRESERVE. PRESERVE: Preserve POSIX-style permissions (recommended). NONE: Ignore permissions. 
--
-- * 'oMtime' - A value that indicates the last time that a file was modified (that is, a file was written to) before the PREPARING phase.  Default value: PRESERVE.  PRESERVE: Preserve original @Mtime@ (recommended) NONE: Ignore @Mtime@ . 
--
-- * 'oUid' - The user ID (UID) of the file's owner.  Default value: INT_VALUE. This preserves the integer value of the ID. INT_VALUE: Preserve the integer value of UID and group ID (GID) (recommended). NONE: Ignore UID and GID. 
--
-- * 'oBytesPerSecond' - A value that limits the bandwidth used by AWS DataSync. For example, if you want AWS DataSync to use a maximum of 1 MB, set this value to @1048576@ (@=1024*1024@ ).
--
-- * 'oGid' - The group ID (GID) of the file's owners.  Default value: INT_VALUE. This preserves the integer value of the ID. INT_VALUE: Preserve the integer value of user ID (UID) and GID (recommended). NONE: Ignore UID and GID. 
--
-- * 'oPreserveDeletedFiles' - A value that specifies whether files in the destination that don't exist in the source file system should be preserved.  Default value: PRESERVE. PRESERVE: Ignore such destination files (recommended).  REMOVE: Delete destination files that aren’t present in the source.
--
-- * 'oPreserveDevices' - A value that determines whether AWS DataSync should preserve the metadata of block and character devices in the source file system, and recreate the files with that device name and metadata on the destination. Default value: NONE. NONE: Ignore special devices (recommended).  PRESERVE: Preserve character and block device metadata. This option isn't currently supported for Amazon EFS. 
options
    :: Options
options =
  Options'
    { _oAtime = Nothing
    , _oVerifyMode = Nothing
    , _oPosixPermissions = Nothing
    , _oMtime = Nothing
    , _oUid = Nothing
    , _oBytesPerSecond = Nothing
    , _oGid = Nothing
    , _oPreserveDeletedFiles = Nothing
    , _oPreserveDevices = Nothing
    }


-- | A file metadata value that shows the last time a file was accessed (that is, when the file was read or written to). If you set @Atime@ to BEST_EFFORT, DataSync attempts to preserve the original @Atime@ attribute on all source files (that is, the version before the PREPARING phase). However, @Atime@ 's behavior is not fully standard across platforms, so AWS DataSync can only do this on a best-effort basis.  Default value: BEST_EFFORT. BEST_EFFORT: Attempt to preserve the per-file @Atime@ value (recommended). NONE: Ignore @Atime@ .
oAtime :: Lens' Options (Maybe Atime)
oAtime = lens _oAtime (\ s a -> s{_oAtime = a})

-- | A value that determines whether a data integrity verification should be performed at the end of a task execution after all data and metadata have been transferred.  Default value: POINT_IN_TIME_CONSISTENT. POINT_IN_TIME_CONSISTENT: Perform verification (recommended).  NONE: Skip verification.
oVerifyMode :: Lens' Options (Maybe VerifyMode)
oVerifyMode = lens _oVerifyMode (\ s a -> s{_oVerifyMode = a})

-- | A value that determines which users or groups can access a file for a specific purpose such as reading, writing, or execution of the file.  Default value: PRESERVE. PRESERVE: Preserve POSIX-style permissions (recommended). NONE: Ignore permissions. 
oPosixPermissions :: Lens' Options (Maybe PosixPermissions)
oPosixPermissions = lens _oPosixPermissions (\ s a -> s{_oPosixPermissions = a})

-- | A value that indicates the last time that a file was modified (that is, a file was written to) before the PREPARING phase.  Default value: PRESERVE.  PRESERVE: Preserve original @Mtime@ (recommended) NONE: Ignore @Mtime@ . 
oMtime :: Lens' Options (Maybe Mtime)
oMtime = lens _oMtime (\ s a -> s{_oMtime = a})

-- | The user ID (UID) of the file's owner.  Default value: INT_VALUE. This preserves the integer value of the ID. INT_VALUE: Preserve the integer value of UID and group ID (GID) (recommended). NONE: Ignore UID and GID. 
oUid :: Lens' Options (Maybe Uid)
oUid = lens _oUid (\ s a -> s{_oUid = a})

-- | A value that limits the bandwidth used by AWS DataSync. For example, if you want AWS DataSync to use a maximum of 1 MB, set this value to @1048576@ (@=1024*1024@ ).
oBytesPerSecond :: Lens' Options (Maybe Integer)
oBytesPerSecond = lens _oBytesPerSecond (\ s a -> s{_oBytesPerSecond = a})

-- | The group ID (GID) of the file's owners.  Default value: INT_VALUE. This preserves the integer value of the ID. INT_VALUE: Preserve the integer value of user ID (UID) and GID (recommended). NONE: Ignore UID and GID. 
oGid :: Lens' Options (Maybe Gid)
oGid = lens _oGid (\ s a -> s{_oGid = a})

-- | A value that specifies whether files in the destination that don't exist in the source file system should be preserved.  Default value: PRESERVE. PRESERVE: Ignore such destination files (recommended).  REMOVE: Delete destination files that aren’t present in the source.
oPreserveDeletedFiles :: Lens' Options (Maybe PreserveDeletedFiles)
oPreserveDeletedFiles = lens _oPreserveDeletedFiles (\ s a -> s{_oPreserveDeletedFiles = a})

-- | A value that determines whether AWS DataSync should preserve the metadata of block and character devices in the source file system, and recreate the files with that device name and metadata on the destination. Default value: NONE. NONE: Ignore special devices (recommended).  PRESERVE: Preserve character and block device metadata. This option isn't currently supported for Amazon EFS. 
oPreserveDevices :: Lens' Options (Maybe PreserveDevices)
oPreserveDevices = lens _oPreserveDevices (\ s a -> s{_oPreserveDevices = a})

instance FromJSON Options where
        parseJSON
          = withObject "Options"
              (\ x ->
                 Options' <$>
                   (x .:? "Atime") <*> (x .:? "VerifyMode") <*>
                     (x .:? "PosixPermissions")
                     <*> (x .:? "Mtime")
                     <*> (x .:? "Uid")
                     <*> (x .:? "BytesPerSecond")
                     <*> (x .:? "Gid")
                     <*> (x .:? "PreserveDeletedFiles")
                     <*> (x .:? "PreserveDevices"))

instance Hashable Options where

instance NFData Options where

instance ToJSON Options where
        toJSON Options'{..}
          = object
              (catMaybes
                 [("Atime" .=) <$> _oAtime,
                  ("VerifyMode" .=) <$> _oVerifyMode,
                  ("PosixPermissions" .=) <$> _oPosixPermissions,
                  ("Mtime" .=) <$> _oMtime, ("Uid" .=) <$> _oUid,
                  ("BytesPerSecond" .=) <$> _oBytesPerSecond,
                  ("Gid" .=) <$> _oGid,
                  ("PreserveDeletedFiles" .=) <$>
                    _oPreserveDeletedFiles,
                  ("PreserveDevices" .=) <$> _oPreserveDevices])

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that is used to access an Amazon S3 bucket. For detailed information about using such a role, see <https://alpha-aws-docs.aws.amazon.com/sync-service/latest/userguide/create-locations-cli.html#create-location-s3-cli Components and Terminology> in the /AWS DataSync User Guide/ .
--
--
--
-- /See:/ 's3Config' smart constructor.
newtype S3Config = S3Config'
  { _scBucketAccessRoleARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3Config' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scBucketAccessRoleARN' - The Amazon S3 bucket to access. This bucket is used as a parameter in the 'CreateLocationS3' operation. 
s3Config
    :: Text -- ^ 'scBucketAccessRoleARN'
    -> S3Config
s3Config pBucketAccessRoleARN_ =
  S3Config' {_scBucketAccessRoleARN = pBucketAccessRoleARN_}


-- | The Amazon S3 bucket to access. This bucket is used as a parameter in the 'CreateLocationS3' operation. 
scBucketAccessRoleARN :: Lens' S3Config Text
scBucketAccessRoleARN = lens _scBucketAccessRoleARN (\ s a -> s{_scBucketAccessRoleARN = a})

instance FromJSON S3Config where
        parseJSON
          = withObject "S3Config"
              (\ x -> S3Config' <$> (x .: "BucketAccessRoleArn"))

instance Hashable S3Config where

instance NFData S3Config where

instance ToJSON S3Config where
        toJSON S3Config'{..}
          = object
              (catMaybes
                 [Just
                    ("BucketAccessRoleArn" .= _scBucketAccessRoleARN)])

-- | Represents a single entry in a list of AWS resource tags. @TagListEntry@ returns an array that contains a list of tasks when the 'ListTagsForResource' operation is called.
--
--
--
-- /See:/ 'tagListEntry' smart constructor.
data TagListEntry = TagListEntry'
  { _tleValue :: !(Maybe Text)
  , _tleKey :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tleValue' - The value for an AWS resource tag.
--
-- * 'tleKey' - The key for an AWS resource tag.
tagListEntry
    :: TagListEntry
tagListEntry = TagListEntry' {_tleValue = Nothing, _tleKey = Nothing}


-- | The value for an AWS resource tag.
tleValue :: Lens' TagListEntry (Maybe Text)
tleValue = lens _tleValue (\ s a -> s{_tleValue = a})

-- | The key for an AWS resource tag.
tleKey :: Lens' TagListEntry (Maybe Text)
tleKey = lens _tleKey (\ s a -> s{_tleKey = a})

instance FromJSON TagListEntry where
        parseJSON
          = withObject "TagListEntry"
              (\ x ->
                 TagListEntry' <$> (x .:? "Value") <*> (x .:? "Key"))

instance Hashable TagListEntry where

instance NFData TagListEntry where

instance ToJSON TagListEntry where
        toJSON TagListEntry'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _tleValue, ("Key" .=) <$> _tleKey])

-- | Represents a single entry in a list of task executions. @TaskExecutionListEntry@ returns an array that contains a list of specific invocations of a task when 'ListTaskExecutions' operation is called.
--
--
--
-- /See:/ 'taskExecutionListEntry' smart constructor.
data TaskExecutionListEntry = TaskExecutionListEntry'
  { _teleStatus :: !(Maybe TaskExecutionStatus)
  , _teleTaskExecutionARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TaskExecutionListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'teleStatus' - The status of a task execution.
--
-- * 'teleTaskExecutionARN' - The Amazon Resource Name (ARN) of the task that was executed.
taskExecutionListEntry
    :: TaskExecutionListEntry
taskExecutionListEntry =
  TaskExecutionListEntry'
    {_teleStatus = Nothing, _teleTaskExecutionARN = Nothing}


-- | The status of a task execution.
teleStatus :: Lens' TaskExecutionListEntry (Maybe TaskExecutionStatus)
teleStatus = lens _teleStatus (\ s a -> s{_teleStatus = a})

-- | The Amazon Resource Name (ARN) of the task that was executed.
teleTaskExecutionARN :: Lens' TaskExecutionListEntry (Maybe Text)
teleTaskExecutionARN = lens _teleTaskExecutionARN (\ s a -> s{_teleTaskExecutionARN = a})

instance FromJSON TaskExecutionListEntry where
        parseJSON
          = withObject "TaskExecutionListEntry"
              (\ x ->
                 TaskExecutionListEntry' <$>
                   (x .:? "Status") <*> (x .:? "TaskExecutionArn"))

instance Hashable TaskExecutionListEntry where

instance NFData TaskExecutionListEntry where

-- | Describes the detailed result of a @TaskExecution@ operation. This result includes the time in milliseconds spent in each phase, the status of the task execution, and the errors encountered.
--
--
--
-- /See:/ 'taskExecutionResultDetail' smart constructor.
data TaskExecutionResultDetail = TaskExecutionResultDetail'
  { _terdPrepareDuration :: !(Maybe Nat)
  , _terdPrepareStatus :: !(Maybe PhaseStatus)
  , _terdVerifyStatus :: !(Maybe PhaseStatus)
  , _terdVerifyDuration :: !(Maybe Nat)
  , _terdTransferStatus :: !(Maybe PhaseStatus)
  , _terdErrorCode :: !(Maybe Text)
  , _terdTransferDuration :: !(Maybe Nat)
  , _terdErrorDetail :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TaskExecutionResultDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'terdPrepareDuration' - The total time in milliseconds that AWS DataSync spent in the PREPARING phase. 
--
-- * 'terdPrepareStatus' - The status of the PREPARING phase.
--
-- * 'terdVerifyStatus' - The status of the VERIFYING Phase.
--
-- * 'terdVerifyDuration' - The total time in milliseconds that AWS DataSync spent in the VERIFYING phase.
--
-- * 'terdTransferStatus' - The status of the TRANSFERRING Phase.
--
-- * 'terdErrorCode' - Errors that AWS DataSync encountered during execution of the task. You can use this error code to help troubleshoot issues.
--
-- * 'terdTransferDuration' - The total time in milliseconds that AWS DataSync spent in the TRANSFERRING phase.
--
-- * 'terdErrorDetail' - Detailed description of an error that was encountered during the task execution. You can use this information to help troubleshoot issues. 
taskExecutionResultDetail
    :: TaskExecutionResultDetail
taskExecutionResultDetail =
  TaskExecutionResultDetail'
    { _terdPrepareDuration = Nothing
    , _terdPrepareStatus = Nothing
    , _terdVerifyStatus = Nothing
    , _terdVerifyDuration = Nothing
    , _terdTransferStatus = Nothing
    , _terdErrorCode = Nothing
    , _terdTransferDuration = Nothing
    , _terdErrorDetail = Nothing
    }


-- | The total time in milliseconds that AWS DataSync spent in the PREPARING phase. 
terdPrepareDuration :: Lens' TaskExecutionResultDetail (Maybe Natural)
terdPrepareDuration = lens _terdPrepareDuration (\ s a -> s{_terdPrepareDuration = a}) . mapping _Nat

-- | The status of the PREPARING phase.
terdPrepareStatus :: Lens' TaskExecutionResultDetail (Maybe PhaseStatus)
terdPrepareStatus = lens _terdPrepareStatus (\ s a -> s{_terdPrepareStatus = a})

-- | The status of the VERIFYING Phase.
terdVerifyStatus :: Lens' TaskExecutionResultDetail (Maybe PhaseStatus)
terdVerifyStatus = lens _terdVerifyStatus (\ s a -> s{_terdVerifyStatus = a})

-- | The total time in milliseconds that AWS DataSync spent in the VERIFYING phase.
terdVerifyDuration :: Lens' TaskExecutionResultDetail (Maybe Natural)
terdVerifyDuration = lens _terdVerifyDuration (\ s a -> s{_terdVerifyDuration = a}) . mapping _Nat

-- | The status of the TRANSFERRING Phase.
terdTransferStatus :: Lens' TaskExecutionResultDetail (Maybe PhaseStatus)
terdTransferStatus = lens _terdTransferStatus (\ s a -> s{_terdTransferStatus = a})

-- | Errors that AWS DataSync encountered during execution of the task. You can use this error code to help troubleshoot issues.
terdErrorCode :: Lens' TaskExecutionResultDetail (Maybe Text)
terdErrorCode = lens _terdErrorCode (\ s a -> s{_terdErrorCode = a})

-- | The total time in milliseconds that AWS DataSync spent in the TRANSFERRING phase.
terdTransferDuration :: Lens' TaskExecutionResultDetail (Maybe Natural)
terdTransferDuration = lens _terdTransferDuration (\ s a -> s{_terdTransferDuration = a}) . mapping _Nat

-- | Detailed description of an error that was encountered during the task execution. You can use this information to help troubleshoot issues. 
terdErrorDetail :: Lens' TaskExecutionResultDetail (Maybe Text)
terdErrorDetail = lens _terdErrorDetail (\ s a -> s{_terdErrorDetail = a})

instance FromJSON TaskExecutionResultDetail where
        parseJSON
          = withObject "TaskExecutionResultDetail"
              (\ x ->
                 TaskExecutionResultDetail' <$>
                   (x .:? "PrepareDuration") <*> (x .:? "PrepareStatus")
                     <*> (x .:? "VerifyStatus")
                     <*> (x .:? "VerifyDuration")
                     <*> (x .:? "TransferStatus")
                     <*> (x .:? "ErrorCode")
                     <*> (x .:? "TransferDuration")
                     <*> (x .:? "ErrorDetail"))

instance Hashable TaskExecutionResultDetail where

instance NFData TaskExecutionResultDetail where

-- | Represents a single entry in a list of tasks. @TaskListEntry@ returns an array that contains a list of tasks when the 'ListTasks' operation is called. A task includes the source and destination file systems to sync and the options to use for the tasks.
--
--
--
-- /See:/ 'taskListEntry' smart constructor.
data TaskListEntry = TaskListEntry'
  { _tleStatus :: !(Maybe TaskStatus)
  , _tleTaskARN :: !(Maybe Text)
  , _tleName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TaskListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tleStatus' - The status of the task.
--
-- * 'tleTaskARN' - The Amazon Resource Name (ARN) of the task.
--
-- * 'tleName' - The name of the task.
taskListEntry
    :: TaskListEntry
taskListEntry =
  TaskListEntry'
    {_tleStatus = Nothing, _tleTaskARN = Nothing, _tleName = Nothing}


-- | The status of the task.
tleStatus :: Lens' TaskListEntry (Maybe TaskStatus)
tleStatus = lens _tleStatus (\ s a -> s{_tleStatus = a})

-- | The Amazon Resource Name (ARN) of the task.
tleTaskARN :: Lens' TaskListEntry (Maybe Text)
tleTaskARN = lens _tleTaskARN (\ s a -> s{_tleTaskARN = a})

-- | The name of the task.
tleName :: Lens' TaskListEntry (Maybe Text)
tleName = lens _tleName (\ s a -> s{_tleName = a})

instance FromJSON TaskListEntry where
        parseJSON
          = withObject "TaskListEntry"
              (\ x ->
                 TaskListEntry' <$>
                   (x .:? "Status") <*> (x .:? "TaskArn") <*>
                     (x .:? "Name"))

instance Hashable TaskListEntry where

instance NFData TaskListEntry where
