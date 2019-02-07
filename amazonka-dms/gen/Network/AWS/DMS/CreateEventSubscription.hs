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
-- Module      : Network.AWS.DMS.CreateEventSubscription
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS DMS event notification subscription. 
--
--
-- You can specify the type of source (@SourceType@ ) you want to be notified of, provide a list of AWS DMS source IDs (@SourceIds@ ) that triggers the events, and provide a list of event categories (@EventCategories@ ) for events you want to be notified of. If you specify both the @SourceType@ and @SourceIds@ , such as @SourceType = replication-instance@ and @SourceIdentifier = my-replinstance@ , you will be notified of all the replication instance events for the specified source. If you specify a @SourceType@ but don't specify a @SourceIdentifier@ , you receive notice of the events for that source type for all your AWS DMS sources. If you don't specify either @SourceType@ nor @SourceIdentifier@ , you will be notified of events generated from all AWS DMS sources belonging to your customer account.
--
-- For more information about AWS DMS events, see <http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Events.html Working with Events and Notifications> in the /AWS Database Migration Service User Guide./ 
--
module Network.AWS.DMS.CreateEventSubscription
    (
    -- * Creating a Request
      createEventSubscription
    , CreateEventSubscription
    -- * Request Lenses
    , cesEnabled
    , cesSourceType
    , cesEventCategories
    , cesSourceIds
    , cesTags
    , cesSubscriptionName
    , cesSNSTopicARN

    -- * Destructuring the Response
    , createEventSubscriptionResponse
    , CreateEventSubscriptionResponse
    -- * Response Lenses
    , cesrsEventSubscription
    , cesrsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | 
--
--
--
-- /See:/ 'createEventSubscription' smart constructor.
data CreateEventSubscription = CreateEventSubscription'
  { _cesEnabled :: !(Maybe Bool)
  , _cesSourceType :: !(Maybe Text)
  , _cesEventCategories :: !(Maybe [Text])
  , _cesSourceIds :: !(Maybe [Text])
  , _cesTags :: !(Maybe [Tag])
  , _cesSubscriptionName :: !Text
  , _cesSNSTopicARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateEventSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cesEnabled' - A Boolean value; set to @true@ to activate the subscription, or set to @false@ to create the subscription but not activate it. 
--
-- * 'cesSourceType' - The type of AWS DMS resource that generates the events. For example, if you want to be notified of events generated by a replication instance, you set this parameter to @replication-instance@ . If this value is not specified, all events are returned.  Valid values: replication-instance | migration-task
--
-- * 'cesEventCategories' - A list of event categories for a source type that you want to subscribe to. You can see a list of the categories for a given source type by calling the @DescribeEventCategories@ action or in the topic <http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Events.html Working with Events and Notifications> in the /AWS Database Migration Service User Guide./ 
--
-- * 'cesSourceIds' - The list of identifiers of the event sources for which events will be returned. If not specified, then all sources are included in the response. An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it cannot end with a hyphen or contain two consecutive hyphens. 
--
-- * 'cesTags' - A tag to be attached to the event subscription.
--
-- * 'cesSubscriptionName' - The name of the AWS DMS event notification subscription.  Constraints: The name must be less than 255 characters. 
--
-- * 'cesSNSTopicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic created for event notification. The ARN is created by Amazon SNS when you create a topic and subscribe to it. 
createEventSubscription
    :: Text -- ^ 'cesSubscriptionName'
    -> Text -- ^ 'cesSNSTopicARN'
    -> CreateEventSubscription
createEventSubscription pSubscriptionName_ pSNSTopicARN_ =
  CreateEventSubscription'
    { _cesEnabled = Nothing
    , _cesSourceType = Nothing
    , _cesEventCategories = Nothing
    , _cesSourceIds = Nothing
    , _cesTags = Nothing
    , _cesSubscriptionName = pSubscriptionName_
    , _cesSNSTopicARN = pSNSTopicARN_
    }


-- | A Boolean value; set to @true@ to activate the subscription, or set to @false@ to create the subscription but not activate it. 
cesEnabled :: Lens' CreateEventSubscription (Maybe Bool)
cesEnabled = lens _cesEnabled (\ s a -> s{_cesEnabled = a})

-- | The type of AWS DMS resource that generates the events. For example, if you want to be notified of events generated by a replication instance, you set this parameter to @replication-instance@ . If this value is not specified, all events are returned.  Valid values: replication-instance | migration-task
cesSourceType :: Lens' CreateEventSubscription (Maybe Text)
cesSourceType = lens _cesSourceType (\ s a -> s{_cesSourceType = a})

-- | A list of event categories for a source type that you want to subscribe to. You can see a list of the categories for a given source type by calling the @DescribeEventCategories@ action or in the topic <http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Events.html Working with Events and Notifications> in the /AWS Database Migration Service User Guide./ 
cesEventCategories :: Lens' CreateEventSubscription [Text]
cesEventCategories = lens _cesEventCategories (\ s a -> s{_cesEventCategories = a}) . _Default . _Coerce

-- | The list of identifiers of the event sources for which events will be returned. If not specified, then all sources are included in the response. An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it cannot end with a hyphen or contain two consecutive hyphens. 
cesSourceIds :: Lens' CreateEventSubscription [Text]
cesSourceIds = lens _cesSourceIds (\ s a -> s{_cesSourceIds = a}) . _Default . _Coerce

-- | A tag to be attached to the event subscription.
cesTags :: Lens' CreateEventSubscription [Tag]
cesTags = lens _cesTags (\ s a -> s{_cesTags = a}) . _Default . _Coerce

-- | The name of the AWS DMS event notification subscription.  Constraints: The name must be less than 255 characters. 
cesSubscriptionName :: Lens' CreateEventSubscription Text
cesSubscriptionName = lens _cesSubscriptionName (\ s a -> s{_cesSubscriptionName = a})

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic created for event notification. The ARN is created by Amazon SNS when you create a topic and subscribe to it. 
cesSNSTopicARN :: Lens' CreateEventSubscription Text
cesSNSTopicARN = lens _cesSNSTopicARN (\ s a -> s{_cesSNSTopicARN = a})

instance AWSRequest CreateEventSubscription where
        type Rs CreateEventSubscription =
             CreateEventSubscriptionResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 CreateEventSubscriptionResponse' <$>
                   (x .?> "EventSubscription") <*> (pure (fromEnum s)))

instance Hashable CreateEventSubscription where

instance NFData CreateEventSubscription where

instance ToHeaders CreateEventSubscription where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.CreateEventSubscription" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateEventSubscription where
        toJSON CreateEventSubscription'{..}
          = object
              (catMaybes
                 [("Enabled" .=) <$> _cesEnabled,
                  ("SourceType" .=) <$> _cesSourceType,
                  ("EventCategories" .=) <$> _cesEventCategories,
                  ("SourceIds" .=) <$> _cesSourceIds,
                  ("Tags" .=) <$> _cesTags,
                  Just ("SubscriptionName" .= _cesSubscriptionName),
                  Just ("SnsTopicArn" .= _cesSNSTopicARN)])

instance ToPath CreateEventSubscription where
        toPath = const "/"

instance ToQuery CreateEventSubscription where
        toQuery = const mempty

-- | 
--
--
--
-- /See:/ 'createEventSubscriptionResponse' smart constructor.
data CreateEventSubscriptionResponse = CreateEventSubscriptionResponse'
  { _cesrsEventSubscription :: !(Maybe EventSubscription)
  , _cesrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateEventSubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cesrsEventSubscription' - The event subscription that was created.
--
-- * 'cesrsResponseStatus' - -- | The response status code.
createEventSubscriptionResponse
    :: Int -- ^ 'cesrsResponseStatus'
    -> CreateEventSubscriptionResponse
createEventSubscriptionResponse pResponseStatus_ =
  CreateEventSubscriptionResponse'
    {_cesrsEventSubscription = Nothing, _cesrsResponseStatus = pResponseStatus_}


-- | The event subscription that was created.
cesrsEventSubscription :: Lens' CreateEventSubscriptionResponse (Maybe EventSubscription)
cesrsEventSubscription = lens _cesrsEventSubscription (\ s a -> s{_cesrsEventSubscription = a})

-- | -- | The response status code.
cesrsResponseStatus :: Lens' CreateEventSubscriptionResponse Int
cesrsResponseStatus = lens _cesrsResponseStatus (\ s a -> s{_cesrsResponseStatus = a})

instance NFData CreateEventSubscriptionResponse where
