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
-- Module      : Network.AWS.DocDB.CopyDBClusterSnapshot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies a snapshot of a DB cluster.
--
--
-- To copy a DB cluster snapshot from a shared manual DB cluster snapshot, @SourceDBClusterSnapshotIdentifier@ must be the Amazon Resource Name (ARN) of the shared DB cluster snapshot.
--
-- To cancel the copy operation after it is in progress, delete the target DB cluster snapshot identified by @TargetDBClusterSnapshotIdentifier@ while that DB cluster snapshot is in the /copying/ status.
--
module Network.AWS.DocDB.CopyDBClusterSnapshot
    (
    -- * Creating a Request
      copyDBClusterSnapshot
    , CopyDBClusterSnapshot
    -- * Request Lenses
    , cdbcsPreSignedURL
    , cdbcsCopyTags
    , cdbcsKMSKeyId
    , cdbcsTags
    , cdbcsSourceDBClusterSnapshotIdentifier
    , cdbcsTargetDBClusterSnapshotIdentifier

    -- * Destructuring the Response
    , copyDBClusterSnapshotResponse
    , CopyDBClusterSnapshotResponse
    -- * Response Lenses
    , cdcsrsDBClusterSnapshot
    , cdcsrsResponseStatus
    ) where

import Network.AWS.DocDB.Types
import Network.AWS.DocDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input to 'CopyDBClusterSnapshot' .
--
--
--
-- /See:/ 'copyDBClusterSnapshot' smart constructor.
data CopyDBClusterSnapshot = CopyDBClusterSnapshot'
  { _cdbcsPreSignedURL :: !(Maybe Text)
  , _cdbcsCopyTags :: !(Maybe Bool)
  , _cdbcsKMSKeyId :: !(Maybe Text)
  , _cdbcsTags :: !(Maybe [Tag])
  , _cdbcsSourceDBClusterSnapshotIdentifier :: !Text
  , _cdbcsTargetDBClusterSnapshotIdentifier :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopyDBClusterSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdbcsPreSignedURL' - The URL that contains a Signature Version 4 signed request for the @CopyDBClusterSnapshot@ API action in the AWS Region that contains the source DB cluster snapshot to copy. You must use the @PreSignedUrl@ parameter when copying an encrypted DB cluster snapshot from another AWS Region. The presigned URL must be a valid request for the @CopyDBSClusterSnapshot@ API action that can be executed in the source AWS Region that contains the encrypted DB cluster snapshot to be copied. The presigned URL request must contain the following parameter values:     * @KmsKeyId@ - The AWS KMS key identifier for the key to use to encrypt the copy of the DB cluster snapshot in the destination AWS Region. This is the same identifier for both the @CopyDBClusterSnapshot@ action that is called in the destination AWS Region, and the action contained in the presigned URL.     * @DestinationRegion@ - The name of the AWS Region that the DB cluster snapshot will be created in.     * @SourceDBClusterSnapshotIdentifier@ - The DB cluster snapshot identifier for the encrypted DB cluster snapshot to be copied. This identifier must be in the Amazon Resource Name (ARN) format for the source AWS Region. For example, if you are copying an encrypted DB cluster snapshot from the us-west-2 AWS Region, then your @SourceDBClusterSnapshotIdentifier@ looks like the following example: @arn:aws:rds:us-west-2:123456789012:cluster-snapshot:my-cluster-snapshot-20161115@ .
--
-- * 'cdbcsCopyTags' - Set to @true@ to copy all tags from the source DB cluster snapshot to the target DB cluster snapshot, and otherwise @false@ . The default is @false@ .
--
-- * 'cdbcsKMSKeyId' - The AWS KMS key ID for an encrypted DB cluster snapshot. The AWS KMS key ID is the Amazon Resource Name (ARN), AWS KMS key identifier, or the AWS KMS key alias for the AWS KMS encryption key.  If you copy an encrypted DB cluster snapshot from your AWS account, you can specify a value for @KmsKeyId@ to encrypt the copy with a new AWS KMS encryption key. If you don't specify a value for @KmsKeyId@ , then the copy of the DB cluster snapshot is encrypted with the same AWS KMS key as the source DB cluster snapshot.  If you copy an encrypted DB cluster snapshot that is shared from another AWS account, then you must specify a value for @KmsKeyId@ .  To copy an encrypted DB cluster snapshot to another AWS Region, set @KmsKeyId@ to the AWS KMS key ID that you want to use to encrypt the copy of the DB cluster snapshot in the destination Region. AWS KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one Region in another Region. If you copy an unencrypted DB cluster snapshot and specify a value for the @KmsKeyId@ parameter, an error is returned.
--
-- * 'cdbcsTags' - The tags to be assigned to the DB cluster snapshot.
--
-- * 'cdbcsSourceDBClusterSnapshotIdentifier' - The identifier of the DB cluster snapshot to copy. This parameter is not case sensitive. You can't copy an encrypted, shared DB cluster snapshot from one AWS Region to another. Constraints:     * Must specify a valid system snapshot in the "available" state.     * If the source snapshot is in the same AWS Region as the copy, specify a valid DB snapshot identifier.     * If the source snapshot is in a different AWS Region than the copy, specify a valid DB cluster snapshot ARN. Example: @my-cluster-snapshot1@ 
--
-- * 'cdbcsTargetDBClusterSnapshotIdentifier' - The identifier of the new DB cluster snapshot to create from the source DB cluster snapshot. This parameter is not case sensitive. Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens.     * The first character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @my-cluster-snapshot2@ 
copyDBClusterSnapshot
    :: Text -- ^ 'cdbcsSourceDBClusterSnapshotIdentifier'
    -> Text -- ^ 'cdbcsTargetDBClusterSnapshotIdentifier'
    -> CopyDBClusterSnapshot
copyDBClusterSnapshot pSourceDBClusterSnapshotIdentifier_ pTargetDBClusterSnapshotIdentifier_ =
  CopyDBClusterSnapshot'
    { _cdbcsPreSignedURL = Nothing
    , _cdbcsCopyTags = Nothing
    , _cdbcsKMSKeyId = Nothing
    , _cdbcsTags = Nothing
    , _cdbcsSourceDBClusterSnapshotIdentifier =
        pSourceDBClusterSnapshotIdentifier_
    , _cdbcsTargetDBClusterSnapshotIdentifier =
        pTargetDBClusterSnapshotIdentifier_
    }


-- | The URL that contains a Signature Version 4 signed request for the @CopyDBClusterSnapshot@ API action in the AWS Region that contains the source DB cluster snapshot to copy. You must use the @PreSignedUrl@ parameter when copying an encrypted DB cluster snapshot from another AWS Region. The presigned URL must be a valid request for the @CopyDBSClusterSnapshot@ API action that can be executed in the source AWS Region that contains the encrypted DB cluster snapshot to be copied. The presigned URL request must contain the following parameter values:     * @KmsKeyId@ - The AWS KMS key identifier for the key to use to encrypt the copy of the DB cluster snapshot in the destination AWS Region. This is the same identifier for both the @CopyDBClusterSnapshot@ action that is called in the destination AWS Region, and the action contained in the presigned URL.     * @DestinationRegion@ - The name of the AWS Region that the DB cluster snapshot will be created in.     * @SourceDBClusterSnapshotIdentifier@ - The DB cluster snapshot identifier for the encrypted DB cluster snapshot to be copied. This identifier must be in the Amazon Resource Name (ARN) format for the source AWS Region. For example, if you are copying an encrypted DB cluster snapshot from the us-west-2 AWS Region, then your @SourceDBClusterSnapshotIdentifier@ looks like the following example: @arn:aws:rds:us-west-2:123456789012:cluster-snapshot:my-cluster-snapshot-20161115@ .
cdbcsPreSignedURL :: Lens' CopyDBClusterSnapshot (Maybe Text)
cdbcsPreSignedURL = lens _cdbcsPreSignedURL (\ s a -> s{_cdbcsPreSignedURL = a})

-- | Set to @true@ to copy all tags from the source DB cluster snapshot to the target DB cluster snapshot, and otherwise @false@ . The default is @false@ .
cdbcsCopyTags :: Lens' CopyDBClusterSnapshot (Maybe Bool)
cdbcsCopyTags = lens _cdbcsCopyTags (\ s a -> s{_cdbcsCopyTags = a})

-- | The AWS KMS key ID for an encrypted DB cluster snapshot. The AWS KMS key ID is the Amazon Resource Name (ARN), AWS KMS key identifier, or the AWS KMS key alias for the AWS KMS encryption key.  If you copy an encrypted DB cluster snapshot from your AWS account, you can specify a value for @KmsKeyId@ to encrypt the copy with a new AWS KMS encryption key. If you don't specify a value for @KmsKeyId@ , then the copy of the DB cluster snapshot is encrypted with the same AWS KMS key as the source DB cluster snapshot.  If you copy an encrypted DB cluster snapshot that is shared from another AWS account, then you must specify a value for @KmsKeyId@ .  To copy an encrypted DB cluster snapshot to another AWS Region, set @KmsKeyId@ to the AWS KMS key ID that you want to use to encrypt the copy of the DB cluster snapshot in the destination Region. AWS KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one Region in another Region. If you copy an unencrypted DB cluster snapshot and specify a value for the @KmsKeyId@ parameter, an error is returned.
cdbcsKMSKeyId :: Lens' CopyDBClusterSnapshot (Maybe Text)
cdbcsKMSKeyId = lens _cdbcsKMSKeyId (\ s a -> s{_cdbcsKMSKeyId = a})

-- | The tags to be assigned to the DB cluster snapshot.
cdbcsTags :: Lens' CopyDBClusterSnapshot [Tag]
cdbcsTags = lens _cdbcsTags (\ s a -> s{_cdbcsTags = a}) . _Default . _Coerce

-- | The identifier of the DB cluster snapshot to copy. This parameter is not case sensitive. You can't copy an encrypted, shared DB cluster snapshot from one AWS Region to another. Constraints:     * Must specify a valid system snapshot in the "available" state.     * If the source snapshot is in the same AWS Region as the copy, specify a valid DB snapshot identifier.     * If the source snapshot is in a different AWS Region than the copy, specify a valid DB cluster snapshot ARN. Example: @my-cluster-snapshot1@ 
cdbcsSourceDBClusterSnapshotIdentifier :: Lens' CopyDBClusterSnapshot Text
cdbcsSourceDBClusterSnapshotIdentifier = lens _cdbcsSourceDBClusterSnapshotIdentifier (\ s a -> s{_cdbcsSourceDBClusterSnapshotIdentifier = a})

-- | The identifier of the new DB cluster snapshot to create from the source DB cluster snapshot. This parameter is not case sensitive. Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens.     * The first character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @my-cluster-snapshot2@ 
cdbcsTargetDBClusterSnapshotIdentifier :: Lens' CopyDBClusterSnapshot Text
cdbcsTargetDBClusterSnapshotIdentifier = lens _cdbcsTargetDBClusterSnapshotIdentifier (\ s a -> s{_cdbcsTargetDBClusterSnapshotIdentifier = a})

instance AWSRequest CopyDBClusterSnapshot where
        type Rs CopyDBClusterSnapshot =
             CopyDBClusterSnapshotResponse
        request = postQuery docDB
        response
          = receiveXMLWrapper "CopyDBClusterSnapshotResult"
              (\ s h x ->
                 CopyDBClusterSnapshotResponse' <$>
                   (x .@? "DBClusterSnapshot") <*> (pure (fromEnum s)))

instance Hashable CopyDBClusterSnapshot where

instance NFData CopyDBClusterSnapshot where

instance ToHeaders CopyDBClusterSnapshot where
        toHeaders = const mempty

instance ToPath CopyDBClusterSnapshot where
        toPath = const "/"

instance ToQuery CopyDBClusterSnapshot where
        toQuery CopyDBClusterSnapshot'{..}
          = mconcat
              ["Action" =: ("CopyDBClusterSnapshot" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "PreSignedUrl" =: _cdbcsPreSignedURL,
               "CopyTags" =: _cdbcsCopyTags,
               "KmsKeyId" =: _cdbcsKMSKeyId,
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdbcsTags),
               "SourceDBClusterSnapshotIdentifier" =:
                 _cdbcsSourceDBClusterSnapshotIdentifier,
               "TargetDBClusterSnapshotIdentifier" =:
                 _cdbcsTargetDBClusterSnapshotIdentifier]

-- | /See:/ 'copyDBClusterSnapshotResponse' smart constructor.
data CopyDBClusterSnapshotResponse = CopyDBClusterSnapshotResponse'
  { _cdcsrsDBClusterSnapshot :: !(Maybe DBClusterSnapshot)
  , _cdcsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopyDBClusterSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcsrsDBClusterSnapshot' - Undocumented member.
--
-- * 'cdcsrsResponseStatus' - -- | The response status code.
copyDBClusterSnapshotResponse
    :: Int -- ^ 'cdcsrsResponseStatus'
    -> CopyDBClusterSnapshotResponse
copyDBClusterSnapshotResponse pResponseStatus_ =
  CopyDBClusterSnapshotResponse'
    { _cdcsrsDBClusterSnapshot = Nothing
    , _cdcsrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
cdcsrsDBClusterSnapshot :: Lens' CopyDBClusterSnapshotResponse (Maybe DBClusterSnapshot)
cdcsrsDBClusterSnapshot = lens _cdcsrsDBClusterSnapshot (\ s a -> s{_cdcsrsDBClusterSnapshot = a})

-- | -- | The response status code.
cdcsrsResponseStatus :: Lens' CopyDBClusterSnapshotResponse Int
cdcsrsResponseStatus = lens _cdcsrsResponseStatus (\ s a -> s{_cdcsrsResponseStatus = a})

instance NFData CopyDBClusterSnapshotResponse where
