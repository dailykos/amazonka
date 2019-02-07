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
-- Module      : Network.AWS.Amplify.ListDomainAssociations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List domains with an app 
--
--
--
-- This operation returns paginated results.
module Network.AWS.Amplify.ListDomainAssociations
    (
    -- * Creating a Request
      listDomainAssociations
    , ListDomainAssociations
    -- * Request Lenses
    , ldaNextToken
    , ldaMaxResults
    , ldaAppId

    -- * Destructuring the Response
    , listDomainAssociationsResponse
    , ListDomainAssociationsResponse
    -- * Response Lenses
    , ldarsNextToken
    , ldarsResponseStatus
    , ldarsDomainAssociations
    ) where

import Network.AWS.Amplify.Types
import Network.AWS.Amplify.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure for the list Domain Associations request. 
--
--
--
-- /See:/ 'listDomainAssociations' smart constructor.
data ListDomainAssociations = ListDomainAssociations'
  { _ldaNextToken :: !(Maybe Text)
  , _ldaMaxResults :: !(Maybe Nat)
  , _ldaAppId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDomainAssociations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldaNextToken' - Pagination token. Set to null to start listing Apps from start. If non-null pagination token is returned in a result, then pass its value in here to list more projects. 
--
-- * 'ldaMaxResults' - Maximum number of records to list in a single response. 
--
-- * 'ldaAppId' - Unique Id for an Amplify App. 
listDomainAssociations
    :: Text -- ^ 'ldaAppId'
    -> ListDomainAssociations
listDomainAssociations pAppId_ =
  ListDomainAssociations'
    {_ldaNextToken = Nothing, _ldaMaxResults = Nothing, _ldaAppId = pAppId_}


-- | Pagination token. Set to null to start listing Apps from start. If non-null pagination token is returned in a result, then pass its value in here to list more projects. 
ldaNextToken :: Lens' ListDomainAssociations (Maybe Text)
ldaNextToken = lens _ldaNextToken (\ s a -> s{_ldaNextToken = a})

-- | Maximum number of records to list in a single response. 
ldaMaxResults :: Lens' ListDomainAssociations (Maybe Natural)
ldaMaxResults = lens _ldaMaxResults (\ s a -> s{_ldaMaxResults = a}) . mapping _Nat

-- | Unique Id for an Amplify App. 
ldaAppId :: Lens' ListDomainAssociations Text
ldaAppId = lens _ldaAppId (\ s a -> s{_ldaAppId = a})

instance AWSPager ListDomainAssociations where
        page rq rs
          | stop (rs ^. ldarsNextToken) = Nothing
          | stop (rs ^. ldarsDomainAssociations) = Nothing
          | otherwise =
            Just $ rq & ldaNextToken .~ rs ^. ldarsNextToken

instance AWSRequest ListDomainAssociations where
        type Rs ListDomainAssociations =
             ListDomainAssociationsResponse
        request = get amplify
        response
          = receiveJSON
              (\ s h x ->
                 ListDomainAssociationsResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "domainAssociations" .!@ mempty))

instance Hashable ListDomainAssociations where

instance NFData ListDomainAssociations where

instance ToHeaders ListDomainAssociations where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListDomainAssociations where
        toPath ListDomainAssociations'{..}
          = mconcat ["/apps/", toBS _ldaAppId, "/domains"]

instance ToQuery ListDomainAssociations where
        toQuery ListDomainAssociations'{..}
          = mconcat
              ["nextToken" =: _ldaNextToken,
               "maxResults" =: _ldaMaxResults]

-- | Result structure for the list Domain Association request. 
--
--
--
-- /See:/ 'listDomainAssociationsResponse' smart constructor.
data ListDomainAssociationsResponse = ListDomainAssociationsResponse'
  { _ldarsNextToken :: !(Maybe Text)
  , _ldarsResponseStatus :: !Int
  , _ldarsDomainAssociations :: ![DomainAssociation]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDomainAssociationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldarsNextToken' - Pagination token. If non-null pagination token is returned in a result, then pass its value in another request to fetch more entries. 
--
-- * 'ldarsResponseStatus' - -- | The response status code.
--
-- * 'ldarsDomainAssociations' - List of Domain Associations. 
listDomainAssociationsResponse
    :: Int -- ^ 'ldarsResponseStatus'
    -> ListDomainAssociationsResponse
listDomainAssociationsResponse pResponseStatus_ =
  ListDomainAssociationsResponse'
    { _ldarsNextToken = Nothing
    , _ldarsResponseStatus = pResponseStatus_
    , _ldarsDomainAssociations = mempty
    }


-- | Pagination token. If non-null pagination token is returned in a result, then pass its value in another request to fetch more entries. 
ldarsNextToken :: Lens' ListDomainAssociationsResponse (Maybe Text)
ldarsNextToken = lens _ldarsNextToken (\ s a -> s{_ldarsNextToken = a})

-- | -- | The response status code.
ldarsResponseStatus :: Lens' ListDomainAssociationsResponse Int
ldarsResponseStatus = lens _ldarsResponseStatus (\ s a -> s{_ldarsResponseStatus = a})

-- | List of Domain Associations. 
ldarsDomainAssociations :: Lens' ListDomainAssociationsResponse [DomainAssociation]
ldarsDomainAssociations = lens _ldarsDomainAssociations (\ s a -> s{_ldarsDomainAssociations = a}) . _Coerce

instance NFData ListDomainAssociationsResponse where
