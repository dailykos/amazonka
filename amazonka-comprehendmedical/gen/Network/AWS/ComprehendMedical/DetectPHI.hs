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
-- Module      : Network.AWS.ComprehendMedical.DetectPHI
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects the clinical text for personal health information (PHI) entities and entity category, location, and confidence score on that information.
--
--
module Network.AWS.ComprehendMedical.DetectPHI
    (
    -- * Creating a Request
      detectPHI
    , DetectPHI
    -- * Request Lenses
    , dphiText

    -- * Destructuring the Response
    , detectPHIResponse
    , DetectPHIResponse
    -- * Response Lenses
    , dphirsPaginationToken
    , dphirsResponseStatus
    , dphirsEntities
    ) where

import Network.AWS.ComprehendMedical.Types
import Network.AWS.ComprehendMedical.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detectPHI' smart constructor.
newtype DetectPHI = DetectPHI'
  { _dphiText :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetectPHI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dphiText' - A UTF-8 text string containing the clinical content being examined for PHI entities. Each string must contain fewer than 20,000 bytes of characters. 
detectPHI
    :: Text -- ^ 'dphiText'
    -> DetectPHI
detectPHI pText_ = DetectPHI' {_dphiText = pText_}


-- | A UTF-8 text string containing the clinical content being examined for PHI entities. Each string must contain fewer than 20,000 bytes of characters. 
dphiText :: Lens' DetectPHI Text
dphiText = lens _dphiText (\ s a -> s{_dphiText = a})

instance AWSRequest DetectPHI where
        type Rs DetectPHI = DetectPHIResponse
        request = postJSON comprehendMedical
        response
          = receiveJSON
              (\ s h x ->
                 DetectPHIResponse' <$>
                   (x .?> "PaginationToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "Entities" .!@ mempty))

instance Hashable DetectPHI where

instance NFData DetectPHI where

instance ToHeaders DetectPHI where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ComprehendMedical_20181030.DetectPHI" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DetectPHI where
        toJSON DetectPHI'{..}
          = object (catMaybes [Just ("Text" .= _dphiText)])

instance ToPath DetectPHI where
        toPath = const "/"

instance ToQuery DetectPHI where
        toQuery = const mempty

-- | /See:/ 'detectPHIResponse' smart constructor.
data DetectPHIResponse = DetectPHIResponse'
  { _dphirsPaginationToken :: !(Maybe Text)
  , _dphirsResponseStatus :: !Int
  , _dphirsEntities :: ![Entity]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetectPHIResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dphirsPaginationToken' - If the result of the previous request to DetectPHI was truncated, include the Paginationtoken to fetch the next page of PHI entities. 
--
-- * 'dphirsResponseStatus' - -- | The response status code.
--
-- * 'dphirsEntities' - The collection of PHI entities extracted from the input text and their associated information. For each entity, the response provides the entity text, the entity category, where the entity text begins and ends, and the level of confidence that Comprehend Medical has in its detection. 
detectPHIResponse
    :: Int -- ^ 'dphirsResponseStatus'
    -> DetectPHIResponse
detectPHIResponse pResponseStatus_ =
  DetectPHIResponse'
    { _dphirsPaginationToken = Nothing
    , _dphirsResponseStatus = pResponseStatus_
    , _dphirsEntities = mempty
    }


-- | If the result of the previous request to DetectPHI was truncated, include the Paginationtoken to fetch the next page of PHI entities. 
dphirsPaginationToken :: Lens' DetectPHIResponse (Maybe Text)
dphirsPaginationToken = lens _dphirsPaginationToken (\ s a -> s{_dphirsPaginationToken = a})

-- | -- | The response status code.
dphirsResponseStatus :: Lens' DetectPHIResponse Int
dphirsResponseStatus = lens _dphirsResponseStatus (\ s a -> s{_dphirsResponseStatus = a})

-- | The collection of PHI entities extracted from the input text and their associated information. For each entity, the response provides the entity text, the entity category, where the entity text begins and ends, and the level of confidence that Comprehend Medical has in its detection. 
dphirsEntities :: Lens' DetectPHIResponse [Entity]
dphirsEntities = lens _dphirsEntities (\ s a -> s{_dphirsEntities = a}) . _Coerce

instance NFData DetectPHIResponse where
