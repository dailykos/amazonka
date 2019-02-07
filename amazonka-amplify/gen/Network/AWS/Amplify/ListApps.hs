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
-- Module      : Network.AWS.Amplify.ListApps
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists existing Amplify Apps. 
--
--
--
-- This operation returns paginated results.
module Network.AWS.Amplify.ListApps
    (
    -- * Creating a Request
      listApps
    , ListApps
    -- * Request Lenses
    , laNextToken
    , laMaxResults

    -- * Destructuring the Response
    , listAppsResponse
    , ListAppsResponse
    -- * Response Lenses
    , larsNextToken
    , larsResponseStatus
    , larsApps
    ) where

import Network.AWS.Amplify.Types
import Network.AWS.Amplify.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure for an Amplify App list request. 
--
--
--
-- /See:/ 'listApps' smart constructor.
data ListApps = ListApps'
  { _laNextToken :: !(Maybe Text)
  , _laMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListApps' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laNextToken' - Pagination token. If non-null pagination token is returned in a result, then pass its value in another request to fetch more entries. 
--
-- * 'laMaxResults' - Maximum number of records to list in a single response. 
listApps
    :: ListApps
listApps = ListApps' {_laNextToken = Nothing, _laMaxResults = Nothing}


-- | Pagination token. If non-null pagination token is returned in a result, then pass its value in another request to fetch more entries. 
laNextToken :: Lens' ListApps (Maybe Text)
laNextToken = lens _laNextToken (\ s a -> s{_laNextToken = a})

-- | Maximum number of records to list in a single response. 
laMaxResults :: Lens' ListApps (Maybe Natural)
laMaxResults = lens _laMaxResults (\ s a -> s{_laMaxResults = a}) . mapping _Nat

instance AWSPager ListApps where
        page rq rs
          | stop (rs ^. larsNextToken) = Nothing
          | stop (rs ^. larsApps) = Nothing
          | otherwise =
            Just $ rq & laNextToken .~ rs ^. larsNextToken

instance AWSRequest ListApps where
        type Rs ListApps = ListAppsResponse
        request = get amplify
        response
          = receiveJSON
              (\ s h x ->
                 ListAppsResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "apps" .!@ mempty))

instance Hashable ListApps where

instance NFData ListApps where

instance ToHeaders ListApps where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListApps where
        toPath = const "/apps"

instance ToQuery ListApps where
        toQuery ListApps'{..}
          = mconcat
              ["nextToken" =: _laNextToken,
               "maxResults" =: _laMaxResults]

-- | Result structure for an Amplify App list request. 
--
--
--
-- /See:/ 'listAppsResponse' smart constructor.
data ListAppsResponse = ListAppsResponse'
  { _larsNextToken :: !(Maybe Text)
  , _larsResponseStatus :: !Int
  , _larsApps :: ![App]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAppsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larsNextToken' - Pagination token. Set to null to start listing Apps from start. If non-null pagination token is returned in a result, then pass its value in here to list more projects. 
--
-- * 'larsResponseStatus' - -- | The response status code.
--
-- * 'larsApps' - List of Amplify Apps. 
listAppsResponse
    :: Int -- ^ 'larsResponseStatus'
    -> ListAppsResponse
listAppsResponse pResponseStatus_ =
  ListAppsResponse'
    { _larsNextToken = Nothing
    , _larsResponseStatus = pResponseStatus_
    , _larsApps = mempty
    }


-- | Pagination token. Set to null to start listing Apps from start. If non-null pagination token is returned in a result, then pass its value in here to list more projects. 
larsNextToken :: Lens' ListAppsResponse (Maybe Text)
larsNextToken = lens _larsNextToken (\ s a -> s{_larsNextToken = a})

-- | -- | The response status code.
larsResponseStatus :: Lens' ListAppsResponse Int
larsResponseStatus = lens _larsResponseStatus (\ s a -> s{_larsResponseStatus = a})

-- | List of Amplify Apps. 
larsApps :: Lens' ListAppsResponse [App]
larsApps = lens _larsApps (\ s a -> s{_larsApps = a}) . _Coerce

instance NFData ListAppsResponse where
