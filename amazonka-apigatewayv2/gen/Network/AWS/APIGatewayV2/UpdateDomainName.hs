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
-- Module      : Network.AWS.APIGatewayV2.UpdateDomainName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a domain name.
--
--
module Network.AWS.APIGatewayV2.UpdateDomainName
    (
    -- * Creating a Request
      updateDomainName
    , UpdateDomainName
    -- * Request Lenses
    , udnDomainNameConfigurations
    , udnDomainName

    -- * Destructuring the Response
    , updateDomainNameResponse
    , UpdateDomainNameResponse
    -- * Response Lenses
    , udnrsDomainNameConfigurations
    , udnrsDomainName
    , udnrsAPIMappingSelectionExpression
    , udnrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateDomainName' smart constructor.
data UpdateDomainName = UpdateDomainName'
  { _udnDomainNameConfigurations :: !(Maybe [DomainNameConfiguration])
  , _udnDomainName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDomainName' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udnDomainNameConfigurations' - The domain name configurations.
--
-- * 'udnDomainName' - The domain name.
updateDomainName
    :: Text -- ^ 'udnDomainName'
    -> UpdateDomainName
updateDomainName pDomainName_ =
  UpdateDomainName'
    {_udnDomainNameConfigurations = Nothing, _udnDomainName = pDomainName_}


-- | The domain name configurations.
udnDomainNameConfigurations :: Lens' UpdateDomainName [DomainNameConfiguration]
udnDomainNameConfigurations = lens _udnDomainNameConfigurations (\ s a -> s{_udnDomainNameConfigurations = a}) . _Default . _Coerce

-- | The domain name.
udnDomainName :: Lens' UpdateDomainName Text
udnDomainName = lens _udnDomainName (\ s a -> s{_udnDomainName = a})

instance AWSRequest UpdateDomainName where
        type Rs UpdateDomainName = UpdateDomainNameResponse
        request = patchJSON apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 UpdateDomainNameResponse' <$>
                   (x .?> "domainNameConfigurations" .!@ mempty) <*>
                     (x .?> "domainName")
                     <*> (x .?> "apiMappingSelectionExpression")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateDomainName where

instance NFData UpdateDomainName where

instance ToHeaders UpdateDomainName where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateDomainName where
        toJSON UpdateDomainName'{..}
          = object
              (catMaybes
                 [("domainNameConfigurations" .=) <$>
                    _udnDomainNameConfigurations])

instance ToPath UpdateDomainName where
        toPath UpdateDomainName'{..}
          = mconcat ["/v2/domainnames/", toBS _udnDomainName]

instance ToQuery UpdateDomainName where
        toQuery = const mempty

-- | /See:/ 'updateDomainNameResponse' smart constructor.
data UpdateDomainNameResponse = UpdateDomainNameResponse'
  { _udnrsDomainNameConfigurations :: !(Maybe [DomainNameConfiguration])
  , _udnrsDomainName :: !(Maybe Text)
  , _udnrsAPIMappingSelectionExpression :: !(Maybe Text)
  , _udnrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDomainNameResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udnrsDomainNameConfigurations' - The domain name configurations.
--
-- * 'udnrsDomainName' - The name of the DomainName resource.
--
-- * 'udnrsAPIMappingSelectionExpression' - The API mapping selection expression.
--
-- * 'udnrsResponseStatus' - -- | The response status code.
updateDomainNameResponse
    :: Int -- ^ 'udnrsResponseStatus'
    -> UpdateDomainNameResponse
updateDomainNameResponse pResponseStatus_ =
  UpdateDomainNameResponse'
    { _udnrsDomainNameConfigurations = Nothing
    , _udnrsDomainName = Nothing
    , _udnrsAPIMappingSelectionExpression = Nothing
    , _udnrsResponseStatus = pResponseStatus_
    }


-- | The domain name configurations.
udnrsDomainNameConfigurations :: Lens' UpdateDomainNameResponse [DomainNameConfiguration]
udnrsDomainNameConfigurations = lens _udnrsDomainNameConfigurations (\ s a -> s{_udnrsDomainNameConfigurations = a}) . _Default . _Coerce

-- | The name of the DomainName resource.
udnrsDomainName :: Lens' UpdateDomainNameResponse (Maybe Text)
udnrsDomainName = lens _udnrsDomainName (\ s a -> s{_udnrsDomainName = a})

-- | The API mapping selection expression.
udnrsAPIMappingSelectionExpression :: Lens' UpdateDomainNameResponse (Maybe Text)
udnrsAPIMappingSelectionExpression = lens _udnrsAPIMappingSelectionExpression (\ s a -> s{_udnrsAPIMappingSelectionExpression = a})

-- | -- | The response status code.
udnrsResponseStatus :: Lens' UpdateDomainNameResponse Int
udnrsResponseStatus = lens _udnrsResponseStatus (\ s a -> s{_udnrsResponseStatus = a})

instance NFData UpdateDomainNameResponse where
