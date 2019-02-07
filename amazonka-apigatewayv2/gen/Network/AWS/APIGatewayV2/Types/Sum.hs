{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGatewayV2.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.APIGatewayV2.Types.Sum where

import Network.AWS.APIGatewayV2.Internal
import Network.AWS.Prelude

-- | The authorization type. Valid values are NONE for open access, AWS_IAM for using AWS IAM permissions, and CUSTOM for using a Lambda authorizer.
--
--
data AuthorizationType
  = AWSIAM
  | Custom
  | None
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AuthorizationType where
    parser = takeLowerText >>= \case
        "aws_iam" -> pure AWSIAM
        "custom" -> pure Custom
        "none" -> pure None
        e -> fromTextError $ "Failure parsing AuthorizationType from value: '" <> e
           <> "'. Accepted values: aws_iam, custom, none"

instance ToText AuthorizationType where
    toText = \case
        AWSIAM -> "AWS_IAM"
        Custom -> "CUSTOM"
        None -> "NONE"

instance Hashable     AuthorizationType
instance NFData       AuthorizationType
instance ToByteString AuthorizationType
instance ToQuery      AuthorizationType
instance ToHeader     AuthorizationType

instance ToJSON AuthorizationType where
    toJSON = toJSONText

instance FromJSON AuthorizationType where
    parseJSON = parseJSONText "AuthorizationType"

-- | The authorizer type. Currently the only valid value is REQUEST, for a Lambda function using incoming request parameters.
--
--
data AuthorizerType =
  Request
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AuthorizerType where
    parser = takeLowerText >>= \case
        "request" -> pure Request
        e -> fromTextError $ "Failure parsing AuthorizerType from value: '" <> e
           <> "'. Accepted values: request"

instance ToText AuthorizerType where
    toText = \case
        Request -> "REQUEST"

instance Hashable     AuthorizerType
instance NFData       AuthorizerType
instance ToByteString AuthorizerType
instance ToQuery      AuthorizerType
instance ToHeader     AuthorizerType

instance ToJSON AuthorizerType where
    toJSON = toJSONText

instance FromJSON AuthorizerType where
    parseJSON = parseJSONText "AuthorizerType"

-- | Represents a connection type.
--
--
data ConnectionType
  = Internet
  | VPCLink
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConnectionType where
    parser = takeLowerText >>= \case
        "internet" -> pure Internet
        "vpc_link" -> pure VPCLink
        e -> fromTextError $ "Failure parsing ConnectionType from value: '" <> e
           <> "'. Accepted values: internet, vpc_link"

instance ToText ConnectionType where
    toText = \case
        Internet -> "INTERNET"
        VPCLink -> "VPC_LINK"

instance Hashable     ConnectionType
instance NFData       ConnectionType
instance ToByteString ConnectionType
instance ToQuery      ConnectionType
instance ToHeader     ConnectionType

instance ToJSON ConnectionType where
    toJSON = toJSONText

instance FromJSON ConnectionType where
    parseJSON = parseJSONText "ConnectionType"

-- | Specifies how to handle response payload content type conversions.
--
--
data ContentHandlingStrategy
  = ConvertToBinary
  | ConvertToText
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ContentHandlingStrategy where
    parser = takeLowerText >>= \case
        "convert_to_binary" -> pure ConvertToBinary
        "convert_to_text" -> pure ConvertToText
        e -> fromTextError $ "Failure parsing ContentHandlingStrategy from value: '" <> e
           <> "'. Accepted values: convert_to_binary, convert_to_text"

instance ToText ContentHandlingStrategy where
    toText = \case
        ConvertToBinary -> "CONVERT_TO_BINARY"
        ConvertToText -> "CONVERT_TO_TEXT"

instance Hashable     ContentHandlingStrategy
instance NFData       ContentHandlingStrategy
instance ToByteString ContentHandlingStrategy
instance ToQuery      ContentHandlingStrategy
instance ToHeader     ContentHandlingStrategy

instance ToJSON ContentHandlingStrategy where
    toJSON = toJSONText

instance FromJSON ContentHandlingStrategy where
    parseJSON = parseJSONText "ContentHandlingStrategy"

-- | Represents a deployment status.
--
--
data DeploymentStatus
  = Deployed
  | Failed
  | Pending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeploymentStatus where
    parser = takeLowerText >>= \case
        "deployed" -> pure Deployed
        "failed" -> pure Failed
        "pending" -> pure Pending
        e -> fromTextError $ "Failure parsing DeploymentStatus from value: '" <> e
           <> "'. Accepted values: deployed, failed, pending"

instance ToText DeploymentStatus where
    toText = \case
        Deployed -> "DEPLOYED"
        Failed -> "FAILED"
        Pending -> "PENDING"

instance Hashable     DeploymentStatus
instance NFData       DeploymentStatus
instance ToByteString DeploymentStatus
instance ToQuery      DeploymentStatus
instance ToHeader     DeploymentStatus

instance FromJSON DeploymentStatus where
    parseJSON = parseJSONText "DeploymentStatus"

-- | Represents an endpoint type.
--
--
data EndpointType
  = Edge
  | Regional
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EndpointType where
    parser = takeLowerText >>= \case
        "edge" -> pure Edge
        "regional" -> pure Regional
        e -> fromTextError $ "Failure parsing EndpointType from value: '" <> e
           <> "'. Accepted values: edge, regional"

instance ToText EndpointType where
    toText = \case
        Edge -> "EDGE"
        Regional -> "REGIONAL"

instance Hashable     EndpointType
instance NFData       EndpointType
instance ToByteString EndpointType
instance ToQuery      EndpointType
instance ToHeader     EndpointType

instance ToJSON EndpointType where
    toJSON = toJSONText

instance FromJSON EndpointType where
    parseJSON = parseJSONText "EndpointType"

-- | Represents an API method integration type.
--
--
data IntegrationType
  = AWS
  | AWSProxy
  | HTTP
  | HTTPProxy
  | Mock
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText IntegrationType where
    parser = takeLowerText >>= \case
        "aws" -> pure AWS
        "aws_proxy" -> pure AWSProxy
        "http" -> pure HTTP
        "http_proxy" -> pure HTTPProxy
        "mock" -> pure Mock
        e -> fromTextError $ "Failure parsing IntegrationType from value: '" <> e
           <> "'. Accepted values: aws, aws_proxy, http, http_proxy, mock"

instance ToText IntegrationType where
    toText = \case
        AWS -> "AWS"
        AWSProxy -> "AWS_PROXY"
        HTTP -> "HTTP"
        HTTPProxy -> "HTTP_PROXY"
        Mock -> "MOCK"

instance Hashable     IntegrationType
instance NFData       IntegrationType
instance ToByteString IntegrationType
instance ToQuery      IntegrationType
instance ToHeader     IntegrationType

instance ToJSON IntegrationType where
    toJSON = toJSONText

instance FromJSON IntegrationType where
    parseJSON = parseJSONText "IntegrationType"

-- | The logging level.
--
--
data LoggingLevel
  = Error'
  | False'
  | Info
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LoggingLevel where
    parser = takeLowerText >>= \case
        "error" -> pure Error'
        "false" -> pure False'
        "info" -> pure Info
        e -> fromTextError $ "Failure parsing LoggingLevel from value: '" <> e
           <> "'. Accepted values: error, false, info"

instance ToText LoggingLevel where
    toText = \case
        Error' -> "ERROR"
        False' -> "false"
        Info -> "INFO"

instance Hashable     LoggingLevel
instance NFData       LoggingLevel
instance ToByteString LoggingLevel
instance ToQuery      LoggingLevel
instance ToHeader     LoggingLevel

instance ToJSON LoggingLevel where
    toJSON = toJSONText

instance FromJSON LoggingLevel where
    parseJSON = parseJSONText "LoggingLevel"

-- | Represents passthrough behavior for an integration response.
--
--
data PassthroughBehavior
  = Never
  | WhenNoMatch
  | WhenNoTemplates
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PassthroughBehavior where
    parser = takeLowerText >>= \case
        "never" -> pure Never
        "when_no_match" -> pure WhenNoMatch
        "when_no_templates" -> pure WhenNoTemplates
        e -> fromTextError $ "Failure parsing PassthroughBehavior from value: '" <> e
           <> "'. Accepted values: never, when_no_match, when_no_templates"

instance ToText PassthroughBehavior where
    toText = \case
        Never -> "NEVER"
        WhenNoMatch -> "WHEN_NO_MATCH"
        WhenNoTemplates -> "WHEN_NO_TEMPLATES"

instance Hashable     PassthroughBehavior
instance NFData       PassthroughBehavior
instance ToByteString PassthroughBehavior
instance ToQuery      PassthroughBehavior
instance ToHeader     PassthroughBehavior

instance ToJSON PassthroughBehavior where
    toJSON = toJSONText

instance FromJSON PassthroughBehavior where
    parseJSON = parseJSONText "PassthroughBehavior"

-- | 
data ProtocolType =
  Websocket
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProtocolType where
    parser = takeLowerText >>= \case
        "websocket" -> pure Websocket
        e -> fromTextError $ "Failure parsing ProtocolType from value: '" <> e
           <> "'. Accepted values: websocket"

instance ToText ProtocolType where
    toText = \case
        Websocket -> "WEBSOCKET"

instance Hashable     ProtocolType
instance NFData       ProtocolType
instance ToByteString ProtocolType
instance ToQuery      ProtocolType
instance ToHeader     ProtocolType

instance ToJSON ProtocolType where
    toJSON = toJSONText

instance FromJSON ProtocolType where
    parseJSON = parseJSONText "ProtocolType"
