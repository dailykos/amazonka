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
-- Module      : Network.AWS.APIGatewayV2.DeleteDomainName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a domain name.
--
--
module Network.AWS.APIGatewayV2.DeleteDomainName
    (
    -- * Creating a Request
      deleteDomainName
    , DeleteDomainName
    -- * Request Lenses
    , ddnDomainName

    -- * Destructuring the Response
    , deleteDomainNameResponse
    , DeleteDomainNameResponse
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDomainName' smart constructor.
newtype DeleteDomainName = DeleteDomainName'
  { _ddnDomainName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDomainName' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddnDomainName' - The domain name.
deleteDomainName
    :: Text -- ^ 'ddnDomainName'
    -> DeleteDomainName
deleteDomainName pDomainName_ =
  DeleteDomainName' {_ddnDomainName = pDomainName_}


-- | The domain name.
ddnDomainName :: Lens' DeleteDomainName Text
ddnDomainName = lens _ddnDomainName (\ s a -> s{_ddnDomainName = a})

instance AWSRequest DeleteDomainName where
        type Rs DeleteDomainName = DeleteDomainNameResponse
        request = delete apiGatewayV2
        response = receiveNull DeleteDomainNameResponse'

instance Hashable DeleteDomainName where

instance NFData DeleteDomainName where

instance ToHeaders DeleteDomainName where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteDomainName where
        toPath DeleteDomainName'{..}
          = mconcat ["/v2/domainnames/", toBS _ddnDomainName]

instance ToQuery DeleteDomainName where
        toQuery = const mempty

-- | /See:/ 'deleteDomainNameResponse' smart constructor.
data DeleteDomainNameResponse =
  DeleteDomainNameResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDomainNameResponse' with the minimum fields required to make a request.
--
deleteDomainNameResponse
    :: DeleteDomainNameResponse
deleteDomainNameResponse = DeleteDomainNameResponse'


instance NFData DeleteDomainNameResponse where
