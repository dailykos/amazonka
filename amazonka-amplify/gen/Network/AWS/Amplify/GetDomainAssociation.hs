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
-- Module      : Network.AWS.Amplify.GetDomainAssociation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves domain info that corresponds to an appId and domainName. 
--
--
module Network.AWS.Amplify.GetDomainAssociation
    (
    -- * Creating a Request
      getDomainAssociation
    , GetDomainAssociation
    -- * Request Lenses
    , gdaAppId
    , gdaDomainName

    -- * Destructuring the Response
    , getDomainAssociationResponse
    , GetDomainAssociationResponse
    -- * Response Lenses
    , gdarsResponseStatus
    , gdarsDomainAssociation
    ) where

import Network.AWS.Amplify.Types
import Network.AWS.Amplify.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure for the get Domain Association request. 
--
--
--
-- /See:/ 'getDomainAssociation' smart constructor.
data GetDomainAssociation = GetDomainAssociation'
  { _gdaAppId :: !Text
  , _gdaDomainName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDomainAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdaAppId' - Unique Id for an Amplify App. 
--
-- * 'gdaDomainName' - Name of the domain. 
getDomainAssociation
    :: Text -- ^ 'gdaAppId'
    -> Text -- ^ 'gdaDomainName'
    -> GetDomainAssociation
getDomainAssociation pAppId_ pDomainName_ =
  GetDomainAssociation' {_gdaAppId = pAppId_, _gdaDomainName = pDomainName_}


-- | Unique Id for an Amplify App. 
gdaAppId :: Lens' GetDomainAssociation Text
gdaAppId = lens _gdaAppId (\ s a -> s{_gdaAppId = a})

-- | Name of the domain. 
gdaDomainName :: Lens' GetDomainAssociation Text
gdaDomainName = lens _gdaDomainName (\ s a -> s{_gdaDomainName = a})

instance AWSRequest GetDomainAssociation where
        type Rs GetDomainAssociation =
             GetDomainAssociationResponse
        request = get amplify
        response
          = receiveJSON
              (\ s h x ->
                 GetDomainAssociationResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "domainAssociation"))

instance Hashable GetDomainAssociation where

instance NFData GetDomainAssociation where

instance ToHeaders GetDomainAssociation where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetDomainAssociation where
        toPath GetDomainAssociation'{..}
          = mconcat
              ["/apps/", toBS _gdaAppId, "/domains/",
               toBS _gdaDomainName]

instance ToQuery GetDomainAssociation where
        toQuery = const mempty

-- | Result structure for the get Domain Association request. 
--
--
--
-- /See:/ 'getDomainAssociationResponse' smart constructor.
data GetDomainAssociationResponse = GetDomainAssociationResponse'
  { _gdarsResponseStatus :: !Int
  , _gdarsDomainAssociation :: !DomainAssociation
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDomainAssociationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdarsResponseStatus' - -- | The response status code.
--
-- * 'gdarsDomainAssociation' - Domain Association structure. 
getDomainAssociationResponse
    :: Int -- ^ 'gdarsResponseStatus'
    -> DomainAssociation -- ^ 'gdarsDomainAssociation'
    -> GetDomainAssociationResponse
getDomainAssociationResponse pResponseStatus_ pDomainAssociation_ =
  GetDomainAssociationResponse'
    { _gdarsResponseStatus = pResponseStatus_
    , _gdarsDomainAssociation = pDomainAssociation_
    }


-- | -- | The response status code.
gdarsResponseStatus :: Lens' GetDomainAssociationResponse Int
gdarsResponseStatus = lens _gdarsResponseStatus (\ s a -> s{_gdarsResponseStatus = a})

-- | Domain Association structure. 
gdarsDomainAssociation :: Lens' GetDomainAssociationResponse DomainAssociation
gdarsDomainAssociation = lens _gdarsDomainAssociation (\ s a -> s{_gdarsDomainAssociation = a})

instance NFData GetDomainAssociationResponse where
