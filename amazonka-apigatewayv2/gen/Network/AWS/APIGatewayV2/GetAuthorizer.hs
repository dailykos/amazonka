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
-- Module      : Network.AWS.APIGatewayV2.GetAuthorizer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an Authorizer.
--
--
module Network.AWS.APIGatewayV2.GetAuthorizer
    (
    -- * Creating a Request
      getAuthorizer
    , GetAuthorizer
    -- * Request Lenses
    , gaaAuthorizerId
    , gaaAPIId

    -- * Destructuring the Response
    , getAuthorizerResponse
    , GetAuthorizerResponse
    -- * Response Lenses
    , getrsAuthorizerCredentialsARN
    , getrsIdentityValidationExpression
    , getrsAuthorizerURI
    , getrsAuthorizerId
    , getrsProviderARNs
    , getrsName
    , getrsAuthorizerResultTtlInSeconds
    , getrsIdentitySource
    , getrsAuthorizerType
    , getrsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAuthorizer' smart constructor.
data GetAuthorizer = GetAuthorizer'
  { _gaaAuthorizerId :: !Text
  , _gaaAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAuthorizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaaAuthorizerId' - The authorizer identifier.
--
-- * 'gaaAPIId' - The API identifier.
getAuthorizer
    :: Text -- ^ 'gaaAuthorizerId'
    -> Text -- ^ 'gaaAPIId'
    -> GetAuthorizer
getAuthorizer pAuthorizerId_ pAPIId_ =
  GetAuthorizer' {_gaaAuthorizerId = pAuthorizerId_, _gaaAPIId = pAPIId_}


-- | The authorizer identifier.
gaaAuthorizerId :: Lens' GetAuthorizer Text
gaaAuthorizerId = lens _gaaAuthorizerId (\ s a -> s{_gaaAuthorizerId = a})

-- | The API identifier.
gaaAPIId :: Lens' GetAuthorizer Text
gaaAPIId = lens _gaaAPIId (\ s a -> s{_gaaAPIId = a})

instance AWSRequest GetAuthorizer where
        type Rs GetAuthorizer = GetAuthorizerResponse
        request = get apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 GetAuthorizerResponse' <$>
                   (x .?> "authorizerCredentialsArn") <*>
                     (x .?> "identityValidationExpression")
                     <*> (x .?> "authorizerUri")
                     <*> (x .?> "authorizerId")
                     <*> (x .?> "providerArns" .!@ mempty)
                     <*> (x .?> "name")
                     <*> (x .?> "authorizerResultTtlInSeconds")
                     <*> (x .?> "identitySource" .!@ mempty)
                     <*> (x .?> "authorizerType")
                     <*> (pure (fromEnum s)))

instance Hashable GetAuthorizer where

instance NFData GetAuthorizer where

instance ToHeaders GetAuthorizer where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetAuthorizer where
        toPath GetAuthorizer'{..}
          = mconcat
              ["/v2/apis/", toBS _gaaAPIId, "/authorizers/",
               toBS _gaaAuthorizerId]

instance ToQuery GetAuthorizer where
        toQuery = const mempty

-- | /See:/ 'getAuthorizerResponse' smart constructor.
data GetAuthorizerResponse = GetAuthorizerResponse'
  { _getrsAuthorizerCredentialsARN :: !(Maybe Text)
  , _getrsIdentityValidationExpression :: !(Maybe Text)
  , _getrsAuthorizerURI :: !(Maybe Text)
  , _getrsAuthorizerId :: !(Maybe Text)
  , _getrsProviderARNs :: !(Maybe [Text])
  , _getrsName :: !(Maybe Text)
  , _getrsAuthorizerResultTtlInSeconds :: !(Maybe Nat)
  , _getrsIdentitySource :: !(Maybe [Text])
  , _getrsAuthorizerType :: !(Maybe AuthorizerType)
  , _getrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAuthorizerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getrsAuthorizerCredentialsARN' - Specifies the required credentials as an IAM role for API Gateway to invoke the authorizer. To specify an IAM role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
--
-- * 'getrsIdentityValidationExpression' - The validation expression does not apply to the REQUEST authorizer.
--
-- * 'getrsAuthorizerURI' - The authorizer's Uniform Resource Identifier (URI). ForREQUEST authorizers, this must be a well-formed Lambda function URI, for example, arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations. In general, the URI has this form: arn:aws:apigateway:{region}:lambda:path/{service_api} , where {region} is the same as the region hosting the Lambda function, path indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial /. For Lambda functions, this is usually of the form /2015-03-31/functions/[FunctionARN]/invocations.
--
-- * 'getrsAuthorizerId' - The authorizer identifier.
--
-- * 'getrsProviderARNs' - For REQUEST authorizer, this is not defined.
--
-- * 'getrsName' - The name of the authorizer.
--
-- * 'getrsAuthorizerResultTtlInSeconds' - The time to live (TTL), in seconds, of cached authorizer results. If it equals 0, authorization caching is disabled. If it is greater than 0, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
--
-- * 'getrsIdentitySource' - The identity source for which authorization is requested. For the REQUEST authorizer, this is required when authorization caching is enabled. The value is a comma-separated string of one or more mapping expressions of the specified request parameters. For example, if an Auth header and a Name query string parameters are defined as identity sources, this value is method.request.header.Auth, method.request.querystring.Name. These parameters will be used to derive the authorization caching key and to perform runtime validation of the REQUEST authorizer by verifying all of the identity-related request parameters are present, not null, and non-empty. Only when this is true does the authorizer invoke the authorizer Lambda function, otherwise, it returns a 401 Unauthorized response without calling the Lambda function. The valid value is a string of comma-separated mapping expressions of the specified request parameters. When the authorization caching is not enabled, this property is optional.
--
-- * 'getrsAuthorizerType' - The authorizer type. Currently the only valid value is REQUEST, for a Lambda function using incoming request parameters.
--
-- * 'getrsResponseStatus' - -- | The response status code.
getAuthorizerResponse
    :: Int -- ^ 'getrsResponseStatus'
    -> GetAuthorizerResponse
getAuthorizerResponse pResponseStatus_ =
  GetAuthorizerResponse'
    { _getrsAuthorizerCredentialsARN = Nothing
    , _getrsIdentityValidationExpression = Nothing
    , _getrsAuthorizerURI = Nothing
    , _getrsAuthorizerId = Nothing
    , _getrsProviderARNs = Nothing
    , _getrsName = Nothing
    , _getrsAuthorizerResultTtlInSeconds = Nothing
    , _getrsIdentitySource = Nothing
    , _getrsAuthorizerType = Nothing
    , _getrsResponseStatus = pResponseStatus_
    }


-- | Specifies the required credentials as an IAM role for API Gateway to invoke the authorizer. To specify an IAM role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
getrsAuthorizerCredentialsARN :: Lens' GetAuthorizerResponse (Maybe Text)
getrsAuthorizerCredentialsARN = lens _getrsAuthorizerCredentialsARN (\ s a -> s{_getrsAuthorizerCredentialsARN = a})

-- | The validation expression does not apply to the REQUEST authorizer.
getrsIdentityValidationExpression :: Lens' GetAuthorizerResponse (Maybe Text)
getrsIdentityValidationExpression = lens _getrsIdentityValidationExpression (\ s a -> s{_getrsIdentityValidationExpression = a})

-- | The authorizer's Uniform Resource Identifier (URI). ForREQUEST authorizers, this must be a well-formed Lambda function URI, for example, arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations. In general, the URI has this form: arn:aws:apigateway:{region}:lambda:path/{service_api} , where {region} is the same as the region hosting the Lambda function, path indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial /. For Lambda functions, this is usually of the form /2015-03-31/functions/[FunctionARN]/invocations.
getrsAuthorizerURI :: Lens' GetAuthorizerResponse (Maybe Text)
getrsAuthorizerURI = lens _getrsAuthorizerURI (\ s a -> s{_getrsAuthorizerURI = a})

-- | The authorizer identifier.
getrsAuthorizerId :: Lens' GetAuthorizerResponse (Maybe Text)
getrsAuthorizerId = lens _getrsAuthorizerId (\ s a -> s{_getrsAuthorizerId = a})

-- | For REQUEST authorizer, this is not defined.
getrsProviderARNs :: Lens' GetAuthorizerResponse [Text]
getrsProviderARNs = lens _getrsProviderARNs (\ s a -> s{_getrsProviderARNs = a}) . _Default . _Coerce

-- | The name of the authorizer.
getrsName :: Lens' GetAuthorizerResponse (Maybe Text)
getrsName = lens _getrsName (\ s a -> s{_getrsName = a})

-- | The time to live (TTL), in seconds, of cached authorizer results. If it equals 0, authorization caching is disabled. If it is greater than 0, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
getrsAuthorizerResultTtlInSeconds :: Lens' GetAuthorizerResponse (Maybe Natural)
getrsAuthorizerResultTtlInSeconds = lens _getrsAuthorizerResultTtlInSeconds (\ s a -> s{_getrsAuthorizerResultTtlInSeconds = a}) . mapping _Nat

-- | The identity source for which authorization is requested. For the REQUEST authorizer, this is required when authorization caching is enabled. The value is a comma-separated string of one or more mapping expressions of the specified request parameters. For example, if an Auth header and a Name query string parameters are defined as identity sources, this value is method.request.header.Auth, method.request.querystring.Name. These parameters will be used to derive the authorization caching key and to perform runtime validation of the REQUEST authorizer by verifying all of the identity-related request parameters are present, not null, and non-empty. Only when this is true does the authorizer invoke the authorizer Lambda function, otherwise, it returns a 401 Unauthorized response without calling the Lambda function. The valid value is a string of comma-separated mapping expressions of the specified request parameters. When the authorization caching is not enabled, this property is optional.
getrsIdentitySource :: Lens' GetAuthorizerResponse [Text]
getrsIdentitySource = lens _getrsIdentitySource (\ s a -> s{_getrsIdentitySource = a}) . _Default . _Coerce

-- | The authorizer type. Currently the only valid value is REQUEST, for a Lambda function using incoming request parameters.
getrsAuthorizerType :: Lens' GetAuthorizerResponse (Maybe AuthorizerType)
getrsAuthorizerType = lens _getrsAuthorizerType (\ s a -> s{_getrsAuthorizerType = a})

-- | -- | The response status code.
getrsResponseStatus :: Lens' GetAuthorizerResponse Int
getrsResponseStatus = lens _getrsResponseStatus (\ s a -> s{_getrsResponseStatus = a})

instance NFData GetAuthorizerResponse where
