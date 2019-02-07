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
-- Module      : Network.AWS.Amplify.UpdateDomainAssociation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new DomainAssociation on an App 
--
--
module Network.AWS.Amplify.UpdateDomainAssociation
    (
    -- * Creating a Request
      updateDomainAssociation
    , UpdateDomainAssociation
    -- * Request Lenses
    , udaEnableAutoSubDomain
    , udaAppId
    , udaDomainName
    , udaSubDomainSettings

    -- * Destructuring the Response
    , updateDomainAssociationResponse
    , UpdateDomainAssociationResponse
    -- * Response Lenses
    , udarsResponseStatus
    , udarsDomainAssociation
    ) where

import Network.AWS.Amplify.Types
import Network.AWS.Amplify.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure for update Domain Association request. 
--
--
--
-- /See:/ 'updateDomainAssociation' smart constructor.
data UpdateDomainAssociation = UpdateDomainAssociation'
  { _udaEnableAutoSubDomain :: !(Maybe Bool)
  , _udaAppId :: !Text
  , _udaDomainName :: !Text
  , _udaSubDomainSettings :: ![SubDomainSetting]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDomainAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udaEnableAutoSubDomain' - Enables automated creation of Subdomains for branches. 
--
-- * 'udaAppId' - Unique Id for an Amplify App. 
--
-- * 'udaDomainName' - Name of the domain. 
--
-- * 'udaSubDomainSettings' - Setting structure for the Subdomain. 
updateDomainAssociation
    :: Text -- ^ 'udaAppId'
    -> Text -- ^ 'udaDomainName'
    -> UpdateDomainAssociation
updateDomainAssociation pAppId_ pDomainName_ =
  UpdateDomainAssociation'
    { _udaEnableAutoSubDomain = Nothing
    , _udaAppId = pAppId_
    , _udaDomainName = pDomainName_
    , _udaSubDomainSettings = mempty
    }


-- | Enables automated creation of Subdomains for branches. 
udaEnableAutoSubDomain :: Lens' UpdateDomainAssociation (Maybe Bool)
udaEnableAutoSubDomain = lens _udaEnableAutoSubDomain (\ s a -> s{_udaEnableAutoSubDomain = a})

-- | Unique Id for an Amplify App. 
udaAppId :: Lens' UpdateDomainAssociation Text
udaAppId = lens _udaAppId (\ s a -> s{_udaAppId = a})

-- | Name of the domain. 
udaDomainName :: Lens' UpdateDomainAssociation Text
udaDomainName = lens _udaDomainName (\ s a -> s{_udaDomainName = a})

-- | Setting structure for the Subdomain. 
udaSubDomainSettings :: Lens' UpdateDomainAssociation [SubDomainSetting]
udaSubDomainSettings = lens _udaSubDomainSettings (\ s a -> s{_udaSubDomainSettings = a}) . _Coerce

instance AWSRequest UpdateDomainAssociation where
        type Rs UpdateDomainAssociation =
             UpdateDomainAssociationResponse
        request = postJSON amplify
        response
          = receiveJSON
              (\ s h x ->
                 UpdateDomainAssociationResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "domainAssociation"))

instance Hashable UpdateDomainAssociation where

instance NFData UpdateDomainAssociation where

instance ToHeaders UpdateDomainAssociation where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateDomainAssociation where
        toJSON UpdateDomainAssociation'{..}
          = object
              (catMaybes
                 [("enableAutoSubDomain" .=) <$>
                    _udaEnableAutoSubDomain,
                  Just ("subDomainSettings" .= _udaSubDomainSettings)])

instance ToPath UpdateDomainAssociation where
        toPath UpdateDomainAssociation'{..}
          = mconcat
              ["/apps/", toBS _udaAppId, "/domains/",
               toBS _udaDomainName]

instance ToQuery UpdateDomainAssociation where
        toQuery = const mempty

-- | Result structure for the update Domain Association request. 
--
--
--
-- /See:/ 'updateDomainAssociationResponse' smart constructor.
data UpdateDomainAssociationResponse = UpdateDomainAssociationResponse'
  { _udarsResponseStatus :: !Int
  , _udarsDomainAssociation :: !DomainAssociation
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDomainAssociationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udarsResponseStatus' - -- | The response status code.
--
-- * 'udarsDomainAssociation' - Domain Association structure. 
updateDomainAssociationResponse
    :: Int -- ^ 'udarsResponseStatus'
    -> DomainAssociation -- ^ 'udarsDomainAssociation'
    -> UpdateDomainAssociationResponse
updateDomainAssociationResponse pResponseStatus_ pDomainAssociation_ =
  UpdateDomainAssociationResponse'
    { _udarsResponseStatus = pResponseStatus_
    , _udarsDomainAssociation = pDomainAssociation_
    }


-- | -- | The response status code.
udarsResponseStatus :: Lens' UpdateDomainAssociationResponse Int
udarsResponseStatus = lens _udarsResponseStatus (\ s a -> s{_udarsResponseStatus = a})

-- | Domain Association structure. 
udarsDomainAssociation :: Lens' UpdateDomainAssociationResponse DomainAssociation
udarsDomainAssociation = lens _udarsDomainAssociation (\ s a -> s{_udarsDomainAssociation = a})

instance NFData UpdateDomainAssociationResponse where
