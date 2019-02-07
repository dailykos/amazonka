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
-- Module      : Network.AWS.Amplify.DeleteDomainAssociation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DomainAssociation. 
--
--
module Network.AWS.Amplify.DeleteDomainAssociation
    (
    -- * Creating a Request
      deleteDomainAssociation
    , DeleteDomainAssociation
    -- * Request Lenses
    , ddaAppId
    , ddaDomainName

    -- * Destructuring the Response
    , deleteDomainAssociationResponse
    , DeleteDomainAssociationResponse
    -- * Response Lenses
    , ddarsResponseStatus
    , ddarsDomainAssociation
    ) where

import Network.AWS.Amplify.Types
import Network.AWS.Amplify.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure for the delete Domain Association request. 
--
--
--
-- /See:/ 'deleteDomainAssociation' smart constructor.
data DeleteDomainAssociation = DeleteDomainAssociation'
  { _ddaAppId :: !Text
  , _ddaDomainName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDomainAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddaAppId' - Unique Id for an Amplify App. 
--
-- * 'ddaDomainName' - Name of the domain. 
deleteDomainAssociation
    :: Text -- ^ 'ddaAppId'
    -> Text -- ^ 'ddaDomainName'
    -> DeleteDomainAssociation
deleteDomainAssociation pAppId_ pDomainName_ =
  DeleteDomainAssociation' {_ddaAppId = pAppId_, _ddaDomainName = pDomainName_}


-- | Unique Id for an Amplify App. 
ddaAppId :: Lens' DeleteDomainAssociation Text
ddaAppId = lens _ddaAppId (\ s a -> s{_ddaAppId = a})

-- | Name of the domain. 
ddaDomainName :: Lens' DeleteDomainAssociation Text
ddaDomainName = lens _ddaDomainName (\ s a -> s{_ddaDomainName = a})

instance AWSRequest DeleteDomainAssociation where
        type Rs DeleteDomainAssociation =
             DeleteDomainAssociationResponse
        request = delete amplify
        response
          = receiveJSON
              (\ s h x ->
                 DeleteDomainAssociationResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "domainAssociation"))

instance Hashable DeleteDomainAssociation where

instance NFData DeleteDomainAssociation where

instance ToHeaders DeleteDomainAssociation where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteDomainAssociation where
        toPath DeleteDomainAssociation'{..}
          = mconcat
              ["/apps/", toBS _ddaAppId, "/domains/",
               toBS _ddaDomainName]

instance ToQuery DeleteDomainAssociation where
        toQuery = const mempty

-- | /See:/ 'deleteDomainAssociationResponse' smart constructor.
data DeleteDomainAssociationResponse = DeleteDomainAssociationResponse'
  { _ddarsResponseStatus :: !Int
  , _ddarsDomainAssociation :: !DomainAssociation
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDomainAssociationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddarsResponseStatus' - -- | The response status code.
--
-- * 'ddarsDomainAssociation' - Undocumented member.
deleteDomainAssociationResponse
    :: Int -- ^ 'ddarsResponseStatus'
    -> DomainAssociation -- ^ 'ddarsDomainAssociation'
    -> DeleteDomainAssociationResponse
deleteDomainAssociationResponse pResponseStatus_ pDomainAssociation_ =
  DeleteDomainAssociationResponse'
    { _ddarsResponseStatus = pResponseStatus_
    , _ddarsDomainAssociation = pDomainAssociation_
    }


-- | -- | The response status code.
ddarsResponseStatus :: Lens' DeleteDomainAssociationResponse Int
ddarsResponseStatus = lens _ddarsResponseStatus (\ s a -> s{_ddarsResponseStatus = a})

-- | Undocumented member.
ddarsDomainAssociation :: Lens' DeleteDomainAssociationResponse DomainAssociation
ddarsDomainAssociation = lens _ddarsDomainAssociation (\ s a -> s{_ddarsDomainAssociation = a})

instance NFData DeleteDomainAssociationResponse where
