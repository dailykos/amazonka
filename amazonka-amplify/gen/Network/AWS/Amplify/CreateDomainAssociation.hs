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
-- Module      : Network.AWS.Amplify.CreateDomainAssociation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new DomainAssociation on an App 
--
--
module Network.AWS.Amplify.CreateDomainAssociation
    (
    -- * Creating a Request
      createDomainAssociation
    , CreateDomainAssociation
    -- * Request Lenses
    , cdaEnableAutoSubDomain
    , cdaAppId
    , cdaDomainName
    , cdaSubDomainSettings

    -- * Destructuring the Response
    , createDomainAssociationResponse
    , CreateDomainAssociationResponse
    -- * Response Lenses
    , cdarsResponseStatus
    , cdarsDomainAssociation
    ) where

import Network.AWS.Amplify.Types
import Network.AWS.Amplify.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure for create Domain Association request. 
--
--
--
-- /See:/ 'createDomainAssociation' smart constructor.
data CreateDomainAssociation = CreateDomainAssociation'
  { _cdaEnableAutoSubDomain :: !(Maybe Bool)
  , _cdaAppId :: !Text
  , _cdaDomainName :: !Text
  , _cdaSubDomainSettings :: ![SubDomainSetting]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDomainAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdaEnableAutoSubDomain' - Enables automated creation of Subdomains for branches. 
--
-- * 'cdaAppId' - Unique Id for an Amplify App. 
--
-- * 'cdaDomainName' - Domain name for the Domain Association. 
--
-- * 'cdaSubDomainSettings' - Setting structure for the Subdomain. 
createDomainAssociation
    :: Text -- ^ 'cdaAppId'
    -> Text -- ^ 'cdaDomainName'
    -> CreateDomainAssociation
createDomainAssociation pAppId_ pDomainName_ =
  CreateDomainAssociation'
    { _cdaEnableAutoSubDomain = Nothing
    , _cdaAppId = pAppId_
    , _cdaDomainName = pDomainName_
    , _cdaSubDomainSettings = mempty
    }


-- | Enables automated creation of Subdomains for branches. 
cdaEnableAutoSubDomain :: Lens' CreateDomainAssociation (Maybe Bool)
cdaEnableAutoSubDomain = lens _cdaEnableAutoSubDomain (\ s a -> s{_cdaEnableAutoSubDomain = a})

-- | Unique Id for an Amplify App. 
cdaAppId :: Lens' CreateDomainAssociation Text
cdaAppId = lens _cdaAppId (\ s a -> s{_cdaAppId = a})

-- | Domain name for the Domain Association. 
cdaDomainName :: Lens' CreateDomainAssociation Text
cdaDomainName = lens _cdaDomainName (\ s a -> s{_cdaDomainName = a})

-- | Setting structure for the Subdomain. 
cdaSubDomainSettings :: Lens' CreateDomainAssociation [SubDomainSetting]
cdaSubDomainSettings = lens _cdaSubDomainSettings (\ s a -> s{_cdaSubDomainSettings = a}) . _Coerce

instance AWSRequest CreateDomainAssociation where
        type Rs CreateDomainAssociation =
             CreateDomainAssociationResponse
        request = postJSON amplify
        response
          = receiveJSON
              (\ s h x ->
                 CreateDomainAssociationResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "domainAssociation"))

instance Hashable CreateDomainAssociation where

instance NFData CreateDomainAssociation where

instance ToHeaders CreateDomainAssociation where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDomainAssociation where
        toJSON CreateDomainAssociation'{..}
          = object
              (catMaybes
                 [("enableAutoSubDomain" .=) <$>
                    _cdaEnableAutoSubDomain,
                  Just ("domainName" .= _cdaDomainName),
                  Just ("subDomainSettings" .= _cdaSubDomainSettings)])

instance ToPath CreateDomainAssociation where
        toPath CreateDomainAssociation'{..}
          = mconcat ["/apps/", toBS _cdaAppId, "/domains"]

instance ToQuery CreateDomainAssociation where
        toQuery = const mempty

-- | Result structure for the create Domain Association request. 
--
--
--
-- /See:/ 'createDomainAssociationResponse' smart constructor.
data CreateDomainAssociationResponse = CreateDomainAssociationResponse'
  { _cdarsResponseStatus :: !Int
  , _cdarsDomainAssociation :: !DomainAssociation
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDomainAssociationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdarsResponseStatus' - -- | The response status code.
--
-- * 'cdarsDomainAssociation' - Domain Association structure. 
createDomainAssociationResponse
    :: Int -- ^ 'cdarsResponseStatus'
    -> DomainAssociation -- ^ 'cdarsDomainAssociation'
    -> CreateDomainAssociationResponse
createDomainAssociationResponse pResponseStatus_ pDomainAssociation_ =
  CreateDomainAssociationResponse'
    { _cdarsResponseStatus = pResponseStatus_
    , _cdarsDomainAssociation = pDomainAssociation_
    }


-- | -- | The response status code.
cdarsResponseStatus :: Lens' CreateDomainAssociationResponse Int
cdarsResponseStatus = lens _cdarsResponseStatus (\ s a -> s{_cdarsResponseStatus = a})

-- | Domain Association structure. 
cdarsDomainAssociation :: Lens' CreateDomainAssociationResponse DomainAssociation
cdarsDomainAssociation = lens _cdarsDomainAssociation (\ s a -> s{_cdarsDomainAssociation = a})

instance NFData CreateDomainAssociationResponse where
