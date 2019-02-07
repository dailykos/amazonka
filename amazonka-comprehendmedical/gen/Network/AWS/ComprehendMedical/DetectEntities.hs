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
-- Module      : Network.AWS.ComprehendMedical.DetectEntities
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects the clinical text for a variety of medical entities and returns specific information about them such as entity category, location, and confidence score on that information .
--
--
module Network.AWS.ComprehendMedical.DetectEntities
    (
    -- * Creating a Request
      detectEntities
    , DetectEntities
    -- * Request Lenses
    , deText

    -- * Destructuring the Response
    , detectEntitiesResponse
    , DetectEntitiesResponse
    -- * Response Lenses
    , dersPaginationToken
    , dersUnmappedAttributes
    , dersResponseStatus
    , dersEntities
    ) where

import Network.AWS.ComprehendMedical.Types
import Network.AWS.ComprehendMedical.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detectEntities' smart constructor.
newtype DetectEntities = DetectEntities'
  { _deText :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetectEntities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deText' - A UTF-8 text string containing the clinical content being examined for entities. Each string must contain fewer than 20,000 bytes of characters.
detectEntities
    :: Text -- ^ 'deText'
    -> DetectEntities
detectEntities pText_ = DetectEntities' {_deText = pText_}


-- | A UTF-8 text string containing the clinical content being examined for entities. Each string must contain fewer than 20,000 bytes of characters.
deText :: Lens' DetectEntities Text
deText = lens _deText (\ s a -> s{_deText = a})

instance AWSRequest DetectEntities where
        type Rs DetectEntities = DetectEntitiesResponse
        request = postJSON comprehendMedical
        response
          = receiveJSON
              (\ s h x ->
                 DetectEntitiesResponse' <$>
                   (x .?> "PaginationToken") <*>
                     (x .?> "UnmappedAttributes" .!@ mempty)
                     <*> (pure (fromEnum s))
                     <*> (x .?> "Entities" .!@ mempty))

instance Hashable DetectEntities where

instance NFData DetectEntities where

instance ToHeaders DetectEntities where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ComprehendMedical_20181030.DetectEntities" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DetectEntities where
        toJSON DetectEntities'{..}
          = object (catMaybes [Just ("Text" .= _deText)])

instance ToPath DetectEntities where
        toPath = const "/"

instance ToQuery DetectEntities where
        toQuery = const mempty

-- | /See:/ 'detectEntitiesResponse' smart constructor.
data DetectEntitiesResponse = DetectEntitiesResponse'
  { _dersPaginationToken :: !(Maybe Text)
  , _dersUnmappedAttributes :: !(Maybe [UnmappedAttribute])
  , _dersResponseStatus :: !Int
  , _dersEntities :: ![Entity]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetectEntitiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dersPaginationToken' - If the result of the previous request to DetectEntities was truncated, include the Paginationtoken to fetch the next page of entities.
--
-- * 'dersUnmappedAttributes' - Attributes extracted from the input text that we were unable to relate to an entity.
--
-- * 'dersResponseStatus' - -- | The response status code.
--
-- * 'dersEntities' - The collection of medical entities extracted from the input text and their associated information. For each entity, the response provides the entity text, the entity category, where the entity text begins and ends, and the level of confidence that Comprehend Medical has in the detection and analysis. Attributes and traits of the entity are also returned.
detectEntitiesResponse
    :: Int -- ^ 'dersResponseStatus'
    -> DetectEntitiesResponse
detectEntitiesResponse pResponseStatus_ =
  DetectEntitiesResponse'
    { _dersPaginationToken = Nothing
    , _dersUnmappedAttributes = Nothing
    , _dersResponseStatus = pResponseStatus_
    , _dersEntities = mempty
    }


-- | If the result of the previous request to DetectEntities was truncated, include the Paginationtoken to fetch the next page of entities.
dersPaginationToken :: Lens' DetectEntitiesResponse (Maybe Text)
dersPaginationToken = lens _dersPaginationToken (\ s a -> s{_dersPaginationToken = a})

-- | Attributes extracted from the input text that we were unable to relate to an entity.
dersUnmappedAttributes :: Lens' DetectEntitiesResponse [UnmappedAttribute]
dersUnmappedAttributes = lens _dersUnmappedAttributes (\ s a -> s{_dersUnmappedAttributes = a}) . _Default . _Coerce

-- | -- | The response status code.
dersResponseStatus :: Lens' DetectEntitiesResponse Int
dersResponseStatus = lens _dersResponseStatus (\ s a -> s{_dersResponseStatus = a})

-- | The collection of medical entities extracted from the input text and their associated information. For each entity, the response provides the entity text, the entity category, where the entity text begins and ends, and the level of confidence that Comprehend Medical has in the detection and analysis. Attributes and traits of the entity are also returned.
dersEntities :: Lens' DetectEntitiesResponse [Entity]
dersEntities = lens _dersEntities (\ s a -> s{_dersEntities = a}) . _Coerce

instance NFData DetectEntitiesResponse where
