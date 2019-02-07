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
-- Module      : Network.AWS.APIGatewayV2.CreateDomainName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a domain name.
--
--
module Network.AWS.APIGatewayV2.CreateDomainName
    (
    -- * Creating a Request
      createDomainName
    , CreateDomainName
    -- * Request Lenses
    , cdnDomainNameConfigurations
    , cdnDomainName

    -- * Destructuring the Response
    , createDomainNameResponse
    , CreateDomainNameResponse
    -- * Response Lenses
    , cdnrsDomainNameConfigurations
    , cdnrsDomainName
    , cdnrsAPIMappingSelectionExpression
    , cdnrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDomainName' smart constructor.
data CreateDomainName = CreateDomainName'
  { _cdnDomainNameConfigurations :: !(Maybe [DomainNameConfiguration])
  , _cdnDomainName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDomainName' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdnDomainNameConfigurations' - The domain name configurations.
--
-- * 'cdnDomainName' - The domain name.
createDomainName
    :: Text -- ^ 'cdnDomainName'
    -> CreateDomainName
createDomainName pDomainName_ =
  CreateDomainName'
    {_cdnDomainNameConfigurations = Nothing, _cdnDomainName = pDomainName_}


-- | The domain name configurations.
cdnDomainNameConfigurations :: Lens' CreateDomainName [DomainNameConfiguration]
cdnDomainNameConfigurations = lens _cdnDomainNameConfigurations (\ s a -> s{_cdnDomainNameConfigurations = a}) . _Default . _Coerce

-- | The domain name.
cdnDomainName :: Lens' CreateDomainName Text
cdnDomainName = lens _cdnDomainName (\ s a -> s{_cdnDomainName = a})

instance AWSRequest CreateDomainName where
        type Rs CreateDomainName = CreateDomainNameResponse
        request = postJSON apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 CreateDomainNameResponse' <$>
                   (x .?> "domainNameConfigurations" .!@ mempty) <*>
                     (x .?> "domainName")
                     <*> (x .?> "apiMappingSelectionExpression")
                     <*> (pure (fromEnum s)))

instance Hashable CreateDomainName where

instance NFData CreateDomainName where

instance ToHeaders CreateDomainName where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDomainName where
        toJSON CreateDomainName'{..}
          = object
              (catMaybes
                 [("domainNameConfigurations" .=) <$>
                    _cdnDomainNameConfigurations,
                  Just ("domainName" .= _cdnDomainName)])

instance ToPath CreateDomainName where
        toPath = const "/v2/domainnames"

instance ToQuery CreateDomainName where
        toQuery = const mempty

-- | /See:/ 'createDomainNameResponse' smart constructor.
data CreateDomainNameResponse = CreateDomainNameResponse'
  { _cdnrsDomainNameConfigurations :: !(Maybe [DomainNameConfiguration])
  , _cdnrsDomainName :: !(Maybe Text)
  , _cdnrsAPIMappingSelectionExpression :: !(Maybe Text)
  , _cdnrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDomainNameResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdnrsDomainNameConfigurations' - The domain name configurations.
--
-- * 'cdnrsDomainName' - The name of the DomainName resource.
--
-- * 'cdnrsAPIMappingSelectionExpression' - The API mapping selection expression.
--
-- * 'cdnrsResponseStatus' - -- | The response status code.
createDomainNameResponse
    :: Int -- ^ 'cdnrsResponseStatus'
    -> CreateDomainNameResponse
createDomainNameResponse pResponseStatus_ =
  CreateDomainNameResponse'
    { _cdnrsDomainNameConfigurations = Nothing
    , _cdnrsDomainName = Nothing
    , _cdnrsAPIMappingSelectionExpression = Nothing
    , _cdnrsResponseStatus = pResponseStatus_
    }


-- | The domain name configurations.
cdnrsDomainNameConfigurations :: Lens' CreateDomainNameResponse [DomainNameConfiguration]
cdnrsDomainNameConfigurations = lens _cdnrsDomainNameConfigurations (\ s a -> s{_cdnrsDomainNameConfigurations = a}) . _Default . _Coerce

-- | The name of the DomainName resource.
cdnrsDomainName :: Lens' CreateDomainNameResponse (Maybe Text)
cdnrsDomainName = lens _cdnrsDomainName (\ s a -> s{_cdnrsDomainName = a})

-- | The API mapping selection expression.
cdnrsAPIMappingSelectionExpression :: Lens' CreateDomainNameResponse (Maybe Text)
cdnrsAPIMappingSelectionExpression = lens _cdnrsAPIMappingSelectionExpression (\ s a -> s{_cdnrsAPIMappingSelectionExpression = a})

-- | -- | The response status code.
cdnrsResponseStatus :: Lens' CreateDomainNameResponse Int
cdnrsResponseStatus = lens _cdnrsResponseStatus (\ s a -> s{_cdnrsResponseStatus = a})

instance NFData CreateDomainNameResponse where
