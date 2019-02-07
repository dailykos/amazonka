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
-- Module      : Network.AWS.APIGatewayV2.UpdateAuthorizer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Authorizer.
--
--
module Network.AWS.APIGatewayV2.UpdateAuthorizer
    (
    -- * Creating a Request
      updateAuthorizer
    , UpdateAuthorizer
    -- * Request Lenses
    , uaAuthorizerCredentialsARN
    , uaIdentityValidationExpression
    , uaAuthorizerURI
    , uaProviderARNs
    , uaName
    , uaAuthorizerResultTtlInSeconds
    , uaIdentitySource
    , uaAuthorizerType
    , uaAuthorizerId
    , uaAPIId

    -- * Destructuring the Response
    , updateAuthorizerResponse
    , UpdateAuthorizerResponse
    -- * Response Lenses
    , uaarsAuthorizerCredentialsARN
    , uaarsIdentityValidationExpression
    , uaarsAuthorizerURI
    , uaarsAuthorizerId
    , uaarsProviderARNs
    , uaarsName
    , uaarsAuthorizerResultTtlInSeconds
    , uaarsIdentitySource
    , uaarsAuthorizerType
    , uaarsResponseStatus
    ) where

import Network.AWS.APIGatewayV2.Types
import Network.AWS.APIGatewayV2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateAuthorizer' smart constructor.
data UpdateAuthorizer = UpdateAuthorizer'
  { _uaAuthorizerCredentialsARN :: !(Maybe Text)
  , _uaIdentityValidationExpression :: !(Maybe Text)
  , _uaAuthorizerURI :: !(Maybe Text)
  , _uaProviderARNs :: !(Maybe [Text])
  , _uaName :: !(Maybe Text)
  , _uaAuthorizerResultTtlInSeconds :: !(Maybe Nat)
  , _uaIdentitySource :: !(Maybe [Text])
  , _uaAuthorizerType :: !(Maybe AuthorizerType)
  , _uaAuthorizerId :: !Text
  , _uaAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAuthorizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaAuthorizerCredentialsARN' - Specifies the required credentials as an IAM role for API Gateway to invoke the authorizer. To specify an IAM role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
--
-- * 'uaIdentityValidationExpression' - The validation expression does not apply to the REQUEST authorizer.
--
-- * 'uaAuthorizerURI' - The authorizer's Uniform Resource Identifier (URI). For REQUEST authorizers, this must be a well-formed Lambda function URI, for example, arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations. In general, the URI has this form: arn:aws:apigateway:{region}:lambda:path/{service_api} , where {region} is the same as the region hosting the Lambda function, path indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial /. For Lambda functions, this is usually of the form /2015-03-31/functions/[FunctionARN]/invocations.
--
-- * 'uaProviderARNs' - For REQUEST authorizer, this is not defined.
--
-- * 'uaName' - The name of the authorizer.
--
-- * 'uaAuthorizerResultTtlInSeconds' - The time to live (TTL), in seconds, of cached authorizer results. If it is zero, authorization caching is disabled. If it is greater than zero, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
--
-- * 'uaIdentitySource' - The identity source for which authorization is requested. For the REQUEST authorizer, this is required when authorization caching is enabled. The value is a comma-separated string of one or more mapping expressions of the specified request parameters. For example, if an Auth header, a Name query string parameter are defined as identity sources, this value is $method.request.header.Auth, $method.request.querystring.Name. These parameters will be used to derive the authorization caching key and to perform runtime validation of the REQUEST authorizer by verifying all of the identity-related request parameters are present, not null and non-empty. Only when this is true does the authorizer invoke the authorizer Lambda function, otherwise, it returns a 401 Unauthorized response without calling the Lambda function. The valid value is a string of comma-separated mapping expressions of the specified request parameters. When the authorization caching is not enabled, this property is optional.
--
-- * 'uaAuthorizerType' - The authorizer type. Currently the only valid value is REQUEST, for a Lambda function using incoming request parameters.
--
-- * 'uaAuthorizerId' - The authorizer identifier.
--
-- * 'uaAPIId' - The API identifier.
updateAuthorizer
    :: Text -- ^ 'uaAuthorizerId'
    -> Text -- ^ 'uaAPIId'
    -> UpdateAuthorizer
updateAuthorizer pAuthorizerId_ pAPIId_ =
  UpdateAuthorizer'
    { _uaAuthorizerCredentialsARN = Nothing
    , _uaIdentityValidationExpression = Nothing
    , _uaAuthorizerURI = Nothing
    , _uaProviderARNs = Nothing
    , _uaName = Nothing
    , _uaAuthorizerResultTtlInSeconds = Nothing
    , _uaIdentitySource = Nothing
    , _uaAuthorizerType = Nothing
    , _uaAuthorizerId = pAuthorizerId_
    , _uaAPIId = pAPIId_
    }


-- | Specifies the required credentials as an IAM role for API Gateway to invoke the authorizer. To specify an IAM role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
uaAuthorizerCredentialsARN :: Lens' UpdateAuthorizer (Maybe Text)
uaAuthorizerCredentialsARN = lens _uaAuthorizerCredentialsARN (\ s a -> s{_uaAuthorizerCredentialsARN = a})

-- | The validation expression does not apply to the REQUEST authorizer.
uaIdentityValidationExpression :: Lens' UpdateAuthorizer (Maybe Text)
uaIdentityValidationExpression = lens _uaIdentityValidationExpression (\ s a -> s{_uaIdentityValidationExpression = a})

-- | The authorizer's Uniform Resource Identifier (URI). For REQUEST authorizers, this must be a well-formed Lambda function URI, for example, arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations. In general, the URI has this form: arn:aws:apigateway:{region}:lambda:path/{service_api} , where {region} is the same as the region hosting the Lambda function, path indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial /. For Lambda functions, this is usually of the form /2015-03-31/functions/[FunctionARN]/invocations.
uaAuthorizerURI :: Lens' UpdateAuthorizer (Maybe Text)
uaAuthorizerURI = lens _uaAuthorizerURI (\ s a -> s{_uaAuthorizerURI = a})

-- | For REQUEST authorizer, this is not defined.
uaProviderARNs :: Lens' UpdateAuthorizer [Text]
uaProviderARNs = lens _uaProviderARNs (\ s a -> s{_uaProviderARNs = a}) . _Default . _Coerce

-- | The name of the authorizer.
uaName :: Lens' UpdateAuthorizer (Maybe Text)
uaName = lens _uaName (\ s a -> s{_uaName = a})

-- | The time to live (TTL), in seconds, of cached authorizer results. If it is zero, authorization caching is disabled. If it is greater than zero, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
uaAuthorizerResultTtlInSeconds :: Lens' UpdateAuthorizer (Maybe Natural)
uaAuthorizerResultTtlInSeconds = lens _uaAuthorizerResultTtlInSeconds (\ s a -> s{_uaAuthorizerResultTtlInSeconds = a}) . mapping _Nat

-- | The identity source for which authorization is requested. For the REQUEST authorizer, this is required when authorization caching is enabled. The value is a comma-separated string of one or more mapping expressions of the specified request parameters. For example, if an Auth header, a Name query string parameter are defined as identity sources, this value is $method.request.header.Auth, $method.request.querystring.Name. These parameters will be used to derive the authorization caching key and to perform runtime validation of the REQUEST authorizer by verifying all of the identity-related request parameters are present, not null and non-empty. Only when this is true does the authorizer invoke the authorizer Lambda function, otherwise, it returns a 401 Unauthorized response without calling the Lambda function. The valid value is a string of comma-separated mapping expressions of the specified request parameters. When the authorization caching is not enabled, this property is optional.
uaIdentitySource :: Lens' UpdateAuthorizer [Text]
uaIdentitySource = lens _uaIdentitySource (\ s a -> s{_uaIdentitySource = a}) . _Default . _Coerce

-- | The authorizer type. Currently the only valid value is REQUEST, for a Lambda function using incoming request parameters.
uaAuthorizerType :: Lens' UpdateAuthorizer (Maybe AuthorizerType)
uaAuthorizerType = lens _uaAuthorizerType (\ s a -> s{_uaAuthorizerType = a})

-- | The authorizer identifier.
uaAuthorizerId :: Lens' UpdateAuthorizer Text
uaAuthorizerId = lens _uaAuthorizerId (\ s a -> s{_uaAuthorizerId = a})

-- | The API identifier.
uaAPIId :: Lens' UpdateAuthorizer Text
uaAPIId = lens _uaAPIId (\ s a -> s{_uaAPIId = a})

instance AWSRequest UpdateAuthorizer where
        type Rs UpdateAuthorizer = UpdateAuthorizerResponse
        request = patchJSON apiGatewayV2
        response
          = receiveJSON
              (\ s h x ->
                 UpdateAuthorizerResponse' <$>
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

instance Hashable UpdateAuthorizer where

instance NFData UpdateAuthorizer where

instance ToHeaders UpdateAuthorizer where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateAuthorizer where
        toJSON UpdateAuthorizer'{..}
          = object
              (catMaybes
                 [("authorizerCredentialsArn" .=) <$>
                    _uaAuthorizerCredentialsARN,
                  ("identityValidationExpression" .=) <$>
                    _uaIdentityValidationExpression,
                  ("authorizerUri" .=) <$> _uaAuthorizerURI,
                  ("providerArns" .=) <$> _uaProviderARNs,
                  ("name" .=) <$> _uaName,
                  ("authorizerResultTtlInSeconds" .=) <$>
                    _uaAuthorizerResultTtlInSeconds,
                  ("identitySource" .=) <$> _uaIdentitySource,
                  ("authorizerType" .=) <$> _uaAuthorizerType])

instance ToPath UpdateAuthorizer where
        toPath UpdateAuthorizer'{..}
          = mconcat
              ["/v2/apis/", toBS _uaAPIId, "/authorizers/",
               toBS _uaAuthorizerId]

instance ToQuery UpdateAuthorizer where
        toQuery = const mempty

-- | /See:/ 'updateAuthorizerResponse' smart constructor.
data UpdateAuthorizerResponse = UpdateAuthorizerResponse'
  { _uaarsAuthorizerCredentialsARN :: !(Maybe Text)
  , _uaarsIdentityValidationExpression :: !(Maybe Text)
  , _uaarsAuthorizerURI :: !(Maybe Text)
  , _uaarsAuthorizerId :: !(Maybe Text)
  , _uaarsProviderARNs :: !(Maybe [Text])
  , _uaarsName :: !(Maybe Text)
  , _uaarsAuthorizerResultTtlInSeconds :: !(Maybe Nat)
  , _uaarsIdentitySource :: !(Maybe [Text])
  , _uaarsAuthorizerType :: !(Maybe AuthorizerType)
  , _uaarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAuthorizerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaarsAuthorizerCredentialsARN' - Specifies the required credentials as an IAM role for API Gateway to invoke the authorizer. To specify an IAM role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
--
-- * 'uaarsIdentityValidationExpression' - The validation expression does not apply to the REQUEST authorizer.
--
-- * 'uaarsAuthorizerURI' - The authorizer's Uniform Resource Identifier (URI). ForREQUEST authorizers, this must be a well-formed Lambda function URI, for example, arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations. In general, the URI has this form: arn:aws:apigateway:{region}:lambda:path/{service_api} , where {region} is the same as the region hosting the Lambda function, path indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial /. For Lambda functions, this is usually of the form /2015-03-31/functions/[FunctionARN]/invocations.
--
-- * 'uaarsAuthorizerId' - The authorizer identifier.
--
-- * 'uaarsProviderARNs' - For REQUEST authorizer, this is not defined.
--
-- * 'uaarsName' - The name of the authorizer.
--
-- * 'uaarsAuthorizerResultTtlInSeconds' - The time to live (TTL), in seconds, of cached authorizer results. If it equals 0, authorization caching is disabled. If it is greater than 0, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
--
-- * 'uaarsIdentitySource' - The identity source for which authorization is requested. For the REQUEST authorizer, this is required when authorization caching is enabled. The value is a comma-separated string of one or more mapping expressions of the specified request parameters. For example, if an Auth header and a Name query string parameters are defined as identity sources, this value is method.request.header.Auth, method.request.querystring.Name. These parameters will be used to derive the authorization caching key and to perform runtime validation of the REQUEST authorizer by verifying all of the identity-related request parameters are present, not null, and non-empty. Only when this is true does the authorizer invoke the authorizer Lambda function, otherwise, it returns a 401 Unauthorized response without calling the Lambda function. The valid value is a string of comma-separated mapping expressions of the specified request parameters. When the authorization caching is not enabled, this property is optional.
--
-- * 'uaarsAuthorizerType' - The authorizer type. Currently the only valid value is REQUEST, for a Lambda function using incoming request parameters.
--
-- * 'uaarsResponseStatus' - -- | The response status code.
updateAuthorizerResponse
    :: Int -- ^ 'uaarsResponseStatus'
    -> UpdateAuthorizerResponse
updateAuthorizerResponse pResponseStatus_ =
  UpdateAuthorizerResponse'
    { _uaarsAuthorizerCredentialsARN = Nothing
    , _uaarsIdentityValidationExpression = Nothing
    , _uaarsAuthorizerURI = Nothing
    , _uaarsAuthorizerId = Nothing
    , _uaarsProviderARNs = Nothing
    , _uaarsName = Nothing
    , _uaarsAuthorizerResultTtlInSeconds = Nothing
    , _uaarsIdentitySource = Nothing
    , _uaarsAuthorizerType = Nothing
    , _uaarsResponseStatus = pResponseStatus_
    }


-- | Specifies the required credentials as an IAM role for API Gateway to invoke the authorizer. To specify an IAM role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
uaarsAuthorizerCredentialsARN :: Lens' UpdateAuthorizerResponse (Maybe Text)
uaarsAuthorizerCredentialsARN = lens _uaarsAuthorizerCredentialsARN (\ s a -> s{_uaarsAuthorizerCredentialsARN = a})

-- | The validation expression does not apply to the REQUEST authorizer.
uaarsIdentityValidationExpression :: Lens' UpdateAuthorizerResponse (Maybe Text)
uaarsIdentityValidationExpression = lens _uaarsIdentityValidationExpression (\ s a -> s{_uaarsIdentityValidationExpression = a})

-- | The authorizer's Uniform Resource Identifier (URI). ForREQUEST authorizers, this must be a well-formed Lambda function URI, for example, arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations. In general, the URI has this form: arn:aws:apigateway:{region}:lambda:path/{service_api} , where {region} is the same as the region hosting the Lambda function, path indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial /. For Lambda functions, this is usually of the form /2015-03-31/functions/[FunctionARN]/invocations.
uaarsAuthorizerURI :: Lens' UpdateAuthorizerResponse (Maybe Text)
uaarsAuthorizerURI = lens _uaarsAuthorizerURI (\ s a -> s{_uaarsAuthorizerURI = a})

-- | The authorizer identifier.
uaarsAuthorizerId :: Lens' UpdateAuthorizerResponse (Maybe Text)
uaarsAuthorizerId = lens _uaarsAuthorizerId (\ s a -> s{_uaarsAuthorizerId = a})

-- | For REQUEST authorizer, this is not defined.
uaarsProviderARNs :: Lens' UpdateAuthorizerResponse [Text]
uaarsProviderARNs = lens _uaarsProviderARNs (\ s a -> s{_uaarsProviderARNs = a}) . _Default . _Coerce

-- | The name of the authorizer.
uaarsName :: Lens' UpdateAuthorizerResponse (Maybe Text)
uaarsName = lens _uaarsName (\ s a -> s{_uaarsName = a})

-- | The time to live (TTL), in seconds, of cached authorizer results. If it equals 0, authorization caching is disabled. If it is greater than 0, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
uaarsAuthorizerResultTtlInSeconds :: Lens' UpdateAuthorizerResponse (Maybe Natural)
uaarsAuthorizerResultTtlInSeconds = lens _uaarsAuthorizerResultTtlInSeconds (\ s a -> s{_uaarsAuthorizerResultTtlInSeconds = a}) . mapping _Nat

-- | The identity source for which authorization is requested. For the REQUEST authorizer, this is required when authorization caching is enabled. The value is a comma-separated string of one or more mapping expressions of the specified request parameters. For example, if an Auth header and a Name query string parameters are defined as identity sources, this value is method.request.header.Auth, method.request.querystring.Name. These parameters will be used to derive the authorization caching key and to perform runtime validation of the REQUEST authorizer by verifying all of the identity-related request parameters are present, not null, and non-empty. Only when this is true does the authorizer invoke the authorizer Lambda function, otherwise, it returns a 401 Unauthorized response without calling the Lambda function. The valid value is a string of comma-separated mapping expressions of the specified request parameters. When the authorization caching is not enabled, this property is optional.
uaarsIdentitySource :: Lens' UpdateAuthorizerResponse [Text]
uaarsIdentitySource = lens _uaarsIdentitySource (\ s a -> s{_uaarsIdentitySource = a}) . _Default . _Coerce

-- | The authorizer type. Currently the only valid value is REQUEST, for a Lambda function using incoming request parameters.
uaarsAuthorizerType :: Lens' UpdateAuthorizerResponse (Maybe AuthorizerType)
uaarsAuthorizerType = lens _uaarsAuthorizerType (\ s a -> s{_uaarsAuthorizerType = a})

-- | -- | The response status code.
uaarsResponseStatus :: Lens' UpdateAuthorizerResponse Int
uaarsResponseStatus = lens _uaarsResponseStatus (\ s a -> s{_uaarsResponseStatus = a})

instance NFData UpdateAuthorizerResponse where
