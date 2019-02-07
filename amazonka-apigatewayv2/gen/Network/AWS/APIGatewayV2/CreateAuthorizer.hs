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
-- Module      : Network.AWS.APIGatewayV2.CreateAuthorizer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Authorizer for an API.
--
--
module Network.AWS.APIGatewayV2.CreateAuthorizer
    (
    -- * Creating a Request
      createAuthorizer
    , CreateAuthorizer
    -- * Request Lenses
    , caaAuthorizerCredentialsARN
    , caaIdentityValidationExpression
    , caaProviderARNs
    , caaAuthorizerResultTtlInSeconds
    , caaAPIId
    , caaAuthorizerURI
    , caaAuthorizerType
    , caaIdentitySource
    , caaName

    -- * Destructuring the Response
    , createAuthorizerResponse
    , CreateAuthorizerResponse
    -- * Response Lenses
    , crsAuthorizerCredentialsARN
    , crsIdentityValidationExpression
    , crsAuthorizerURI
    , crsAuthorizerId
    , crsProviderARNs
    , crsName
    , crsAuthorizerResultTtlInSeconds
    , crsIdentitySource
    , crsAuthorizerType
    , crsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createAuthorizer' smart constructor.
data CreateAuthorizer = CreateAuthorizer'
  { _caaAuthorizerCredentialsARN :: !(Maybe Text)
  , _caaIdentityValidationExpression :: !(Maybe Text)
  , _caaProviderARNs :: !(Maybe [Text])
  , _caaAuthorizerResultTtlInSeconds :: !(Maybe Nat)
  , _caaAPIId :: !Text
  , _caaAuthorizerURI :: !Text
  , _caaAuthorizerType :: !AuthorizerType
  , _caaIdentitySource :: ![Text]
  , _caaName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAuthorizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caaAuthorizerCredentialsARN' - Specifies the required credentials as an IAM role for API Gateway to invoke the authorizer. To specify an IAM role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
--
-- * 'caaIdentityValidationExpression' - The validation expression does not apply to the REQUEST authorizer.
--
-- * 'caaProviderARNs' - For REQUEST authorizer, this is not defined.
--
-- * 'caaAuthorizerResultTtlInSeconds' - The time to live (TTL), in seconds, of cached authorizer results. If it equals 0, authorization caching is disabled. If it is greater than 0, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
--
-- * 'caaAPIId' - The API identifier.
--
-- * 'caaAuthorizerURI' - The authorizer's Uniform Resource Identifier (URI). For REQUEST authorizers, this must be a well-formed Lambda function URI, for example, arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations. In general, the URI has this form: arn:aws:apigateway:{region}:lambda:path/{service_api} , where {region} is the same as the region hosting the Lambda function, path indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial /. For Lambda functions, this is usually of the form /2015-03-31/functions/[FunctionARN]/invocations.
--
-- * 'caaAuthorizerType' - The authorizer type. Currently the only valid value is REQUEST, for a Lambda function using incoming request parameters.
--
-- * 'caaIdentitySource' - The identity source for which authorization is requested. For the REQUEST authorizer, this is required when authorization caching is enabled. The value is a comma-separated string of one or more mapping expressions of the specified request parameters. For example, if an Auth header and a Name query string parameters are defined as identity sources, this value is method.request.header.Auth, method.request.querystring.Name. These parameters will be used to derive the authorization caching key and to perform runtime validation of the REQUEST authorizer by verifying all of the identity-related request parameters are present, not null, and non-empty. Only when this is true does the authorizer invoke the authorizer Lambda function, otherwise, it returns a 401 Unauthorized response without calling the Lambda function. The valid value is a string of comma-separated mapping expressions of the specified request parameters. When the authorization caching is not enabled, this property is optional.
--
-- * 'caaName' - The name of the authorizer.
createAuthorizer
    :: Text -- ^ 'caaAPIId'
    -> Text -- ^ 'caaAuthorizerURI'
    -> AuthorizerType -- ^ 'caaAuthorizerType'
    -> Text -- ^ 'caaName'
    -> CreateAuthorizer
createAuthorizer pAPIId_ pAuthorizerURI_ pAuthorizerType_ pName_ =
  CreateAuthorizer'
    { _caaAuthorizerCredentialsARN = Nothing
    , _caaIdentityValidationExpression = Nothing
    , _caaProviderARNs = Nothing
    , _caaAuthorizerResultTtlInSeconds = Nothing
    , _caaAPIId = pAPIId_
    , _caaAuthorizerURI = pAuthorizerURI_
    , _caaAuthorizerType = pAuthorizerType_
    , _caaIdentitySource = mempty
    , _caaName = pName_
    }


-- | Specifies the required credentials as an IAM role for API Gateway to invoke the authorizer. To specify an IAM role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
caaAuthorizerCredentialsARN :: Lens' CreateAuthorizer (Maybe Text)
caaAuthorizerCredentialsARN = lens _caaAuthorizerCredentialsARN (\ s a -> s{_caaAuthorizerCredentialsARN = a})

-- | The validation expression does not apply to the REQUEST authorizer.
caaIdentityValidationExpression :: Lens' CreateAuthorizer (Maybe Text)
caaIdentityValidationExpression = lens _caaIdentityValidationExpression (\ s a -> s{_caaIdentityValidationExpression = a})

-- | For REQUEST authorizer, this is not defined.
caaProviderARNs :: Lens' CreateAuthorizer [Text]
caaProviderARNs = lens _caaProviderARNs (\ s a -> s{_caaProviderARNs = a}) . _Default . _Coerce

-- | The time to live (TTL), in seconds, of cached authorizer results. If it equals 0, authorization caching is disabled. If it is greater than 0, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
caaAuthorizerResultTtlInSeconds :: Lens' CreateAuthorizer (Maybe Natural)
caaAuthorizerResultTtlInSeconds = lens _caaAuthorizerResultTtlInSeconds (\ s a -> s{_caaAuthorizerResultTtlInSeconds = a}) . mapping _Nat

-- | The API identifier.
caaAPIId :: Lens' CreateAuthorizer Text
caaAPIId = lens _caaAPIId (\ s a -> s{_caaAPIId = a})

-- | The authorizer's Uniform Resource Identifier (URI). For REQUEST authorizers, this must be a well-formed Lambda function URI, for example, arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations. In general, the URI has this form: arn:aws:apigateway:{region}:lambda:path/{service_api} , where {region} is the same as the region hosting the Lambda function, path indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial /. For Lambda functions, this is usually of the form /2015-03-31/functions/[FunctionARN]/invocations.
caaAuthorizerURI :: Lens' CreateAuthorizer Text
caaAuthorizerURI = lens _caaAuthorizerURI (\ s a -> s{_caaAuthorizerURI = a})

-- | The authorizer type. Currently the only valid value is REQUEST, for a Lambda function using incoming request parameters.
caaAuthorizerType :: Lens' CreateAuthorizer AuthorizerType
caaAuthorizerType = lens _caaAuthorizerType (\ s a -> s{_caaAuthorizerType = a})

-- | The identity source for which authorization is requested. For the REQUEST authorizer, this is required when authorization caching is enabled. The value is a comma-separated string of one or more mapping expressions of the specified request parameters. For example, if an Auth header and a Name query string parameters are defined as identity sources, this value is method.request.header.Auth, method.request.querystring.Name. These parameters will be used to derive the authorization caching key and to perform runtime validation of the REQUEST authorizer by verifying all of the identity-related request parameters are present, not null, and non-empty. Only when this is true does the authorizer invoke the authorizer Lambda function, otherwise, it returns a 401 Unauthorized response without calling the Lambda function. The valid value is a string of comma-separated mapping expressions of the specified request parameters. When the authorization caching is not enabled, this property is optional.
caaIdentitySource :: Lens' CreateAuthorizer [Text]
caaIdentitySource = lens _caaIdentitySource (\ s a -> s{_caaIdentitySource = a}) . _Coerce

-- | The name of the authorizer.
caaName :: Lens' CreateAuthorizer Text
caaName = lens _caaName (\ s a -> s{_caaName = a})

instance AWSRequest CreateAuthorizer where
        type Rs CreateAuthorizer = CreateAuthorizerResponse
        request = postJSON apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 CreateAuthorizerResponse' <$>
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

instance Hashable CreateAuthorizer where

instance NFData CreateAuthorizer where

instance ToHeaders CreateAuthorizer where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateAuthorizer where
        toJSON CreateAuthorizer'{..}
          = object
              (catMaybes
                 [("authorizerCredentialsArn" .=) <$>
                    _caaAuthorizerCredentialsARN,
                  ("identityValidationExpression" .=) <$>
                    _caaIdentityValidationExpression,
                  ("providerArns" .=) <$> _caaProviderARNs,
                  ("authorizerResultTtlInSeconds" .=) <$>
                    _caaAuthorizerResultTtlInSeconds,
                  Just ("authorizerUri" .= _caaAuthorizerURI),
                  Just ("authorizerType" .= _caaAuthorizerType),
                  Just ("identitySource" .= _caaIdentitySource),
                  Just ("name" .= _caaName)])

instance ToPath CreateAuthorizer where
        toPath CreateAuthorizer'{..}
          = mconcat
              ["/v2/apis/", toBS _caaAPIId, "/authorizers"]

instance ToQuery CreateAuthorizer where
        toQuery = const mempty

-- | /See:/ 'createAuthorizerResponse' smart constructor.
data CreateAuthorizerResponse = CreateAuthorizerResponse'
  { _crsAuthorizerCredentialsARN :: !(Maybe Text)
  , _crsIdentityValidationExpression :: !(Maybe Text)
  , _crsAuthorizerURI :: !(Maybe Text)
  , _crsAuthorizerId :: !(Maybe Text)
  , _crsProviderARNs :: !(Maybe [Text])
  , _crsName :: !(Maybe Text)
  , _crsAuthorizerResultTtlInSeconds :: !(Maybe Nat)
  , _crsIdentitySource :: !(Maybe [Text])
  , _crsAuthorizerType :: !(Maybe AuthorizerType)
  , _crsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAuthorizerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsAuthorizerCredentialsARN' - Specifies the required credentials as an IAM role for API Gateway to invoke the authorizer. To specify an IAM role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
--
-- * 'crsIdentityValidationExpression' - The validation expression does not apply to the REQUEST authorizer.
--
-- * 'crsAuthorizerURI' - The authorizer's Uniform Resource Identifier (URI). ForREQUEST authorizers, this must be a well-formed Lambda function URI, for example, arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations. In general, the URI has this form: arn:aws:apigateway:{region}:lambda:path/{service_api} , where {region} is the same as the region hosting the Lambda function, path indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial /. For Lambda functions, this is usually of the form /2015-03-31/functions/[FunctionARN]/invocations.
--
-- * 'crsAuthorizerId' - The authorizer identifier.
--
-- * 'crsProviderARNs' - For REQUEST authorizer, this is not defined.
--
-- * 'crsName' - The name of the authorizer.
--
-- * 'crsAuthorizerResultTtlInSeconds' - The time to live (TTL), in seconds, of cached authorizer results. If it equals 0, authorization caching is disabled. If it is greater than 0, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
--
-- * 'crsIdentitySource' - The identity source for which authorization is requested. For the REQUEST authorizer, this is required when authorization caching is enabled. The value is a comma-separated string of one or more mapping expressions of the specified request parameters. For example, if an Auth header and a Name query string parameters are defined as identity sources, this value is method.request.header.Auth, method.request.querystring.Name. These parameters will be used to derive the authorization caching key and to perform runtime validation of the REQUEST authorizer by verifying all of the identity-related request parameters are present, not null, and non-empty. Only when this is true does the authorizer invoke the authorizer Lambda function, otherwise, it returns a 401 Unauthorized response without calling the Lambda function. The valid value is a string of comma-separated mapping expressions of the specified request parameters. When the authorization caching is not enabled, this property is optional.
--
-- * 'crsAuthorizerType' - The authorizer type. Currently the only valid value is REQUEST, for a Lambda function using incoming request parameters.
--
-- * 'crsResponseStatus' - -- | The response status code.
createAuthorizerResponse
    :: Int -- ^ 'crsResponseStatus'
    -> CreateAuthorizerResponse
createAuthorizerResponse pResponseStatus_ =
  CreateAuthorizerResponse'
    { _crsAuthorizerCredentialsARN = Nothing
    , _crsIdentityValidationExpression = Nothing
    , _crsAuthorizerURI = Nothing
    , _crsAuthorizerId = Nothing
    , _crsProviderARNs = Nothing
    , _crsName = Nothing
    , _crsAuthorizerResultTtlInSeconds = Nothing
    , _crsIdentitySource = Nothing
    , _crsAuthorizerType = Nothing
    , _crsResponseStatus = pResponseStatus_
    }


-- | Specifies the required credentials as an IAM role for API Gateway to invoke the authorizer. To specify an IAM role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
crsAuthorizerCredentialsARN :: Lens' CreateAuthorizerResponse (Maybe Text)
crsAuthorizerCredentialsARN = lens _crsAuthorizerCredentialsARN (\ s a -> s{_crsAuthorizerCredentialsARN = a})

-- | The validation expression does not apply to the REQUEST authorizer.
crsIdentityValidationExpression :: Lens' CreateAuthorizerResponse (Maybe Text)
crsIdentityValidationExpression = lens _crsIdentityValidationExpression (\ s a -> s{_crsIdentityValidationExpression = a})

-- | The authorizer's Uniform Resource Identifier (URI). ForREQUEST authorizers, this must be a well-formed Lambda function URI, for example, arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations. In general, the URI has this form: arn:aws:apigateway:{region}:lambda:path/{service_api} , where {region} is the same as the region hosting the Lambda function, path indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial /. For Lambda functions, this is usually of the form /2015-03-31/functions/[FunctionARN]/invocations.
crsAuthorizerURI :: Lens' CreateAuthorizerResponse (Maybe Text)
crsAuthorizerURI = lens _crsAuthorizerURI (\ s a -> s{_crsAuthorizerURI = a})

-- | The authorizer identifier.
crsAuthorizerId :: Lens' CreateAuthorizerResponse (Maybe Text)
crsAuthorizerId = lens _crsAuthorizerId (\ s a -> s{_crsAuthorizerId = a})

-- | For REQUEST authorizer, this is not defined.
crsProviderARNs :: Lens' CreateAuthorizerResponse [Text]
crsProviderARNs = lens _crsProviderARNs (\ s a -> s{_crsProviderARNs = a}) . _Default . _Coerce

-- | The name of the authorizer.
crsName :: Lens' CreateAuthorizerResponse (Maybe Text)
crsName = lens _crsName (\ s a -> s{_crsName = a})

-- | The time to live (TTL), in seconds, of cached authorizer results. If it equals 0, authorization caching is disabled. If it is greater than 0, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
crsAuthorizerResultTtlInSeconds :: Lens' CreateAuthorizerResponse (Maybe Natural)
crsAuthorizerResultTtlInSeconds = lens _crsAuthorizerResultTtlInSeconds (\ s a -> s{_crsAuthorizerResultTtlInSeconds = a}) . mapping _Nat

-- | The identity source for which authorization is requested. For the REQUEST authorizer, this is required when authorization caching is enabled. The value is a comma-separated string of one or more mapping expressions of the specified request parameters. For example, if an Auth header and a Name query string parameters are defined as identity sources, this value is method.request.header.Auth, method.request.querystring.Name. These parameters will be used to derive the authorization caching key and to perform runtime validation of the REQUEST authorizer by verifying all of the identity-related request parameters are present, not null, and non-empty. Only when this is true does the authorizer invoke the authorizer Lambda function, otherwise, it returns a 401 Unauthorized response without calling the Lambda function. The valid value is a string of comma-separated mapping expressions of the specified request parameters. When the authorization caching is not enabled, this property is optional.
crsIdentitySource :: Lens' CreateAuthorizerResponse [Text]
crsIdentitySource = lens _crsIdentitySource (\ s a -> s{_crsIdentitySource = a}) . _Default . _Coerce

-- | The authorizer type. Currently the only valid value is REQUEST, for a Lambda function using incoming request parameters.
crsAuthorizerType :: Lens' CreateAuthorizerResponse (Maybe AuthorizerType)
crsAuthorizerType = lens _crsAuthorizerType (\ s a -> s{_crsAuthorizerType = a})

-- | -- | The response status code.
crsResponseStatus :: Lens' CreateAuthorizerResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a})

instance NFData CreateAuthorizerResponse where
