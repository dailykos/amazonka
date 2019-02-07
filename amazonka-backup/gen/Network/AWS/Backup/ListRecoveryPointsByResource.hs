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
-- Module      : Network.AWS.Backup.ListRecoveryPointsByResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about recovery points of the type specified by a resource Amazon Resource Name (ARN).
--
--
module Network.AWS.Backup.ListRecoveryPointsByResource
    (
    -- * Creating a Request
      listRecoveryPointsByResource
    , ListRecoveryPointsByResource
    -- * Request Lenses
    , lrpbrNextToken
    , lrpbrMaxResults
    , lrpbrResourceARN

    -- * Destructuring the Response
    , listRecoveryPointsByResourceResponse
    , ListRecoveryPointsByResourceResponse
    -- * Response Lenses
    , lrpbrrsRecoveryPoints
    , lrpbrrsNextToken
    , lrpbrrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listRecoveryPointsByResource' smart constructor.
data ListRecoveryPointsByResource = ListRecoveryPointsByResource'
  { _lrpbrNextToken :: !(Maybe Text)
  , _lrpbrMaxResults :: !(Maybe Nat)
  , _lrpbrResourceARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRecoveryPointsByResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrpbrNextToken' - The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
--
-- * 'lrpbrMaxResults' - The maximum number of items to be returned.
--
-- * 'lrpbrResourceARN' - An ARN that uniquely identifies a resource. The format of the ARN depends on the resource type.
listRecoveryPointsByResource
    :: Text -- ^ 'lrpbrResourceARN'
    -> ListRecoveryPointsByResource
listRecoveryPointsByResource pResourceARN_ =
  ListRecoveryPointsByResource'
    { _lrpbrNextToken = Nothing
    , _lrpbrMaxResults = Nothing
    , _lrpbrResourceARN = pResourceARN_
    }


-- | The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
lrpbrNextToken :: Lens' ListRecoveryPointsByResource (Maybe Text)
lrpbrNextToken = lens _lrpbrNextToken (\ s a -> s{_lrpbrNextToken = a})

-- | The maximum number of items to be returned.
lrpbrMaxResults :: Lens' ListRecoveryPointsByResource (Maybe Natural)
lrpbrMaxResults = lens _lrpbrMaxResults (\ s a -> s{_lrpbrMaxResults = a}) . mapping _Nat

-- | An ARN that uniquely identifies a resource. The format of the ARN depends on the resource type.
lrpbrResourceARN :: Lens' ListRecoveryPointsByResource Text
lrpbrResourceARN = lens _lrpbrResourceARN (\ s a -> s{_lrpbrResourceARN = a})

instance AWSRequest ListRecoveryPointsByResource
         where
        type Rs ListRecoveryPointsByResource =
             ListRecoveryPointsByResourceResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 ListRecoveryPointsByResourceResponse' <$>
                   (x .?> "RecoveryPoints" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListRecoveryPointsByResource where

instance NFData ListRecoveryPointsByResource where

instance ToHeaders ListRecoveryPointsByResource where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListRecoveryPointsByResource where
        toPath ListRecoveryPointsByResource'{..}
          = mconcat
              ["/resources/", toBS _lrpbrResourceARN,
               "/recovery-points/"]

instance ToQuery ListRecoveryPointsByResource where
        toQuery ListRecoveryPointsByResource'{..}
          = mconcat
              ["nextToken" =: _lrpbrNextToken,
               "maxResults" =: _lrpbrMaxResults]

-- | /See:/ 'listRecoveryPointsByResourceResponse' smart constructor.
data ListRecoveryPointsByResourceResponse = ListRecoveryPointsByResourceResponse'
  { _lrpbrrsRecoveryPoints :: !(Maybe [RecoveryPointByResource])
  , _lrpbrrsNextToken :: !(Maybe Text)
  , _lrpbrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRecoveryPointsByResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrpbrrsRecoveryPoints' - An array of objects that contain detailed information about recovery points of the specified resource type.
--
-- * 'lrpbrrsNextToken' - The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
--
-- * 'lrpbrrsResponseStatus' - -- | The response status code.
listRecoveryPointsByResourceResponse
    :: Int -- ^ 'lrpbrrsResponseStatus'
    -> ListRecoveryPointsByResourceResponse
listRecoveryPointsByResourceResponse pResponseStatus_ =
  ListRecoveryPointsByResourceResponse'
    { _lrpbrrsRecoveryPoints = Nothing
    , _lrpbrrsNextToken = Nothing
    , _lrpbrrsResponseStatus = pResponseStatus_
    }


-- | An array of objects that contain detailed information about recovery points of the specified resource type.
lrpbrrsRecoveryPoints :: Lens' ListRecoveryPointsByResourceResponse [RecoveryPointByResource]
lrpbrrsRecoveryPoints = lens _lrpbrrsRecoveryPoints (\ s a -> s{_lrpbrrsRecoveryPoints = a}) . _Default . _Coerce

-- | The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
lrpbrrsNextToken :: Lens' ListRecoveryPointsByResourceResponse (Maybe Text)
lrpbrrsNextToken = lens _lrpbrrsNextToken (\ s a -> s{_lrpbrrsNextToken = a})

-- | -- | The response status code.
lrpbrrsResponseStatus :: Lens' ListRecoveryPointsByResourceResponse Int
lrpbrrsResponseStatus = lens _lrpbrrsResponseStatus (\ s a -> s{_lrpbrrsResponseStatus = a})

instance NFData ListRecoveryPointsByResourceResponse
         where
