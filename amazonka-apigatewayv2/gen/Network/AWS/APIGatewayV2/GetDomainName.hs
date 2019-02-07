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
-- Module      : Network.AWS.APIGatewayV2.GetDomainName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a domain name.
--
--
module Network.AWS.APIGatewayV2.GetDomainName
    (
    -- * Creating a Request
      getDomainName
    , GetDomainName
    -- * Request Lenses
    , gdnDomainName

    -- * Destructuring the Response
    , getDomainNameResponse
    , GetDomainNameResponse
    -- * Response Lenses
    , gdnrsDomainNameConfigurations
    , gdnrsDomainName
    , gdnrsAPIMappingSelectionExpression
    , gdnrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDomainName' smart constructor.
newtype GetDomainName = GetDomainName'
  { _gdnDomainName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDomainName' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdnDomainName' - The domain name.
getDomainName
    :: Text -- ^ 'gdnDomainName'
    -> GetDomainName
getDomainName pDomainName_ = GetDomainName' {_gdnDomainName = pDomainName_}


-- | The domain name.
gdnDomainName :: Lens' GetDomainName Text
gdnDomainName = lens _gdnDomainName (\ s a -> s{_gdnDomainName = a})

instance AWSRequest GetDomainName where
        type Rs GetDomainName = GetDomainNameResponse
        request = get apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 GetDomainNameResponse' <$>
                   (x .?> "domainNameConfigurations" .!@ mempty) <*>
                     (x .?> "domainName")
                     <*> (x .?> "apiMappingSelectionExpression")
                     <*> (pure (fromEnum s)))

instance Hashable GetDomainName where

instance NFData GetDomainName where

instance ToHeaders GetDomainName where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetDomainName where
        toPath GetDomainName'{..}
          = mconcat ["/v2/domainnames/", toBS _gdnDomainName]

instance ToQuery GetDomainName where
        toQuery = const mempty

-- | /See:/ 'getDomainNameResponse' smart constructor.
data GetDomainNameResponse = GetDomainNameResponse'
  { _gdnrsDomainNameConfigurations :: !(Maybe [DomainNameConfiguration])
  , _gdnrsDomainName :: !(Maybe Text)
  , _gdnrsAPIMappingSelectionExpression :: !(Maybe Text)
  , _gdnrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDomainNameResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdnrsDomainNameConfigurations' - The domain name configurations.
--
-- * 'gdnrsDomainName' - The name of the DomainName resource.
--
-- * 'gdnrsAPIMappingSelectionExpression' - The API mapping selection expression.
--
-- * 'gdnrsResponseStatus' - -- | The response status code.
getDomainNameResponse
    :: Int -- ^ 'gdnrsResponseStatus'
    -> GetDomainNameResponse
getDomainNameResponse pResponseStatus_ =
  GetDomainNameResponse'
    { _gdnrsDomainNameConfigurations = Nothing
    , _gdnrsDomainName = Nothing
    , _gdnrsAPIMappingSelectionExpression = Nothing
    , _gdnrsResponseStatus = pResponseStatus_
    }


-- | The domain name configurations.
gdnrsDomainNameConfigurations :: Lens' GetDomainNameResponse [DomainNameConfiguration]
gdnrsDomainNameConfigurations = lens _gdnrsDomainNameConfigurations (\ s a -> s{_gdnrsDomainNameConfigurations = a}) . _Default . _Coerce

-- | The name of the DomainName resource.
gdnrsDomainName :: Lens' GetDomainNameResponse (Maybe Text)
gdnrsDomainName = lens _gdnrsDomainName (\ s a -> s{_gdnrsDomainName = a})

-- | The API mapping selection expression.
gdnrsAPIMappingSelectionExpression :: Lens' GetDomainNameResponse (Maybe Text)
gdnrsAPIMappingSelectionExpression = lens _gdnrsAPIMappingSelectionExpression (\ s a -> s{_gdnrsAPIMappingSelectionExpression = a})

-- | -- | The response status code.
gdnrsResponseStatus :: Lens' GetDomainNameResponse Int
gdnrsResponseStatus = lens _gdnrsResponseStatus (\ s a -> s{_gdnrsResponseStatus = a})

instance NFData GetDomainNameResponse where
