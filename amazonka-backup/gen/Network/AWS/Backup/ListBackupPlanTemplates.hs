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
-- Module      : Network.AWS.Backup.ListBackupPlanTemplates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata of your saved backup plan templates, including the template ID, name, and the creation and deletion dates.
--
--
module Network.AWS.Backup.ListBackupPlanTemplates
    (
    -- * Creating a Request
      listBackupPlanTemplates
    , ListBackupPlanTemplates
    -- * Request Lenses
    , lbptNextToken
    , lbptMaxResults

    -- * Destructuring the Response
    , listBackupPlanTemplatesResponse
    , ListBackupPlanTemplatesResponse
    -- * Response Lenses
    , lbptrsBackupPlanTemplatesList
    , lbptrsNextToken
    , lbptrsResponseStatus
    ) where

import Network.AWS.Backup.Types
import Network.AWS.Backup.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listBackupPlanTemplates' smart constructor.
data ListBackupPlanTemplates = ListBackupPlanTemplates'
  { _lbptNextToken :: !(Maybe Text)
  , _lbptMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBackupPlanTemplates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbptNextToken' - The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
--
-- * 'lbptMaxResults' - The maximum number of items to be returned.
listBackupPlanTemplates
    :: ListBackupPlanTemplates
listBackupPlanTemplates =
  ListBackupPlanTemplates' {_lbptNextToken = Nothing, _lbptMaxResults = Nothing}


-- | The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
lbptNextToken :: Lens' ListBackupPlanTemplates (Maybe Text)
lbptNextToken = lens _lbptNextToken (\ s a -> s{_lbptNextToken = a})

-- | The maximum number of items to be returned.
lbptMaxResults :: Lens' ListBackupPlanTemplates (Maybe Natural)
lbptMaxResults = lens _lbptMaxResults (\ s a -> s{_lbptMaxResults = a}) . mapping _Nat

instance AWSRequest ListBackupPlanTemplates where
        type Rs ListBackupPlanTemplates =
             ListBackupPlanTemplatesResponse
        request = get backup
        response
          = receiveJSON
              (\ s h x ->
                 ListBackupPlanTemplatesResponse' <$>
                   (x .?> "BackupPlanTemplatesList" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListBackupPlanTemplates where

instance NFData ListBackupPlanTemplates where

instance ToHeaders ListBackupPlanTemplates where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListBackupPlanTemplates where
        toPath = const "/backup/template/plans"

instance ToQuery ListBackupPlanTemplates where
        toQuery ListBackupPlanTemplates'{..}
          = mconcat
              ["nextToken" =: _lbptNextToken,
               "maxResults" =: _lbptMaxResults]

-- | /See:/ 'listBackupPlanTemplatesResponse' smart constructor.
data ListBackupPlanTemplatesResponse = ListBackupPlanTemplatesResponse'
  { _lbptrsBackupPlanTemplatesList :: !(Maybe [BackupPlanTemplatesListMember])
  , _lbptrsNextToken :: !(Maybe Text)
  , _lbptrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBackupPlanTemplatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbptrsBackupPlanTemplatesList' - An array of template list items containing metadata about your saved templates.
--
-- * 'lbptrsNextToken' - The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
--
-- * 'lbptrsResponseStatus' - -- | The response status code.
listBackupPlanTemplatesResponse
    :: Int -- ^ 'lbptrsResponseStatus'
    -> ListBackupPlanTemplatesResponse
listBackupPlanTemplatesResponse pResponseStatus_ =
  ListBackupPlanTemplatesResponse'
    { _lbptrsBackupPlanTemplatesList = Nothing
    , _lbptrsNextToken = Nothing
    , _lbptrsResponseStatus = pResponseStatus_
    }


-- | An array of template list items containing metadata about your saved templates.
lbptrsBackupPlanTemplatesList :: Lens' ListBackupPlanTemplatesResponse [BackupPlanTemplatesListMember]
lbptrsBackupPlanTemplatesList = lens _lbptrsBackupPlanTemplatesList (\ s a -> s{_lbptrsBackupPlanTemplatesList = a}) . _Default . _Coerce

-- | The next item following a partial list of returned items. For example, if a request is made to return @maxResults@ number of items, @NextToken@ allows you to return more items in your list starting at the location pointed to by the next token.
lbptrsNextToken :: Lens' ListBackupPlanTemplatesResponse (Maybe Text)
lbptrsNextToken = lens _lbptrsNextToken (\ s a -> s{_lbptrsNextToken = a})

-- | -- | The response status code.
lbptrsResponseStatus :: Lens' ListBackupPlanTemplatesResponse Int
lbptrsResponseStatus = lens _lbptrsResponseStatus (\ s a -> s{_lbptrsResponseStatus = a})

instance NFData ListBackupPlanTemplatesResponse where
