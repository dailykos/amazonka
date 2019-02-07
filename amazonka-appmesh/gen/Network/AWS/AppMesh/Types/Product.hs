{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppMesh.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppMesh.Types.Product where

import Network.AWS.AppMesh.Internal
import Network.AWS.AppMesh.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The DNS service discovery information for your virtual node.
--
--
--
-- /See:/ 'dnsServiceDiscovery' smart constructor.
newtype DNSServiceDiscovery = DNSServiceDiscovery'
  { _dsdServiceName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DNSServiceDiscovery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsdServiceName' - The DNS service name for your virtual node.
dnsServiceDiscovery
    :: DNSServiceDiscovery
dnsServiceDiscovery = DNSServiceDiscovery' {_dsdServiceName = Nothing}


-- | The DNS service name for your virtual node.
dsdServiceName :: Lens' DNSServiceDiscovery (Maybe Text)
dsdServiceName = lens _dsdServiceName (\ s a -> s{_dsdServiceName = a})

instance FromJSON DNSServiceDiscovery where
        parseJSON
          = withObject "DNSServiceDiscovery"
              (\ x ->
                 DNSServiceDiscovery' <$> (x .:? "serviceName"))

instance Hashable DNSServiceDiscovery where

instance NFData DNSServiceDiscovery where

instance ToJSON DNSServiceDiscovery where
        toJSON DNSServiceDiscovery'{..}
          = object
              (catMaybes [("serviceName" .=) <$> _dsdServiceName])

-- | An object representing the HTTP routing specification for a route.
--
--
--
-- /See:/ 'hTTPRoute' smart constructor.
data HTTPRoute = HTTPRoute'
  { _httprAction :: !(Maybe HTTPRouteAction)
  , _httprMatch :: !(Maybe HTTPRouteMatch)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HTTPRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httprAction' - The action to take if a match is determined.
--
-- * 'httprMatch' - The criteria for determining an HTTP request match.
hTTPRoute
    :: HTTPRoute
hTTPRoute = HTTPRoute' {_httprAction = Nothing, _httprMatch = Nothing}


-- | The action to take if a match is determined.
httprAction :: Lens' HTTPRoute (Maybe HTTPRouteAction)
httprAction = lens _httprAction (\ s a -> s{_httprAction = a})

-- | The criteria for determining an HTTP request match.
httprMatch :: Lens' HTTPRoute (Maybe HTTPRouteMatch)
httprMatch = lens _httprMatch (\ s a -> s{_httprMatch = a})

instance FromJSON HTTPRoute where
        parseJSON
          = withObject "HTTPRoute"
              (\ x ->
                 HTTPRoute' <$> (x .:? "action") <*> (x .:? "match"))

instance Hashable HTTPRoute where

instance NFData HTTPRoute where

instance ToJSON HTTPRoute where
        toJSON HTTPRoute'{..}
          = object
              (catMaybes
                 [("action" .=) <$> _httprAction,
                  ("match" .=) <$> _httprMatch])

-- | An object representing the traffic distribution requirements for matched HTTP
--
--          requests.
--
--
-- /See:/ 'hTTPRouteAction' smart constructor.
newtype HTTPRouteAction = HTTPRouteAction'
  { _httpraWeightedTargets :: Maybe [WeightedTarget]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HTTPRouteAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httpraWeightedTargets' - The targets that traffic is routed to when a request matches the route. You can specify          one or more targets and their relative weights with which to distribute traffic.
hTTPRouteAction
    :: HTTPRouteAction
hTTPRouteAction = HTTPRouteAction' {_httpraWeightedTargets = Nothing}


-- | The targets that traffic is routed to when a request matches the route. You can specify          one or more targets and their relative weights with which to distribute traffic.
httpraWeightedTargets :: Lens' HTTPRouteAction [WeightedTarget]
httpraWeightedTargets = lens _httpraWeightedTargets (\ s a -> s{_httpraWeightedTargets = a}) . _Default . _Coerce

instance FromJSON HTTPRouteAction where
        parseJSON
          = withObject "HTTPRouteAction"
              (\ x ->
                 HTTPRouteAction' <$>
                   (x .:? "weightedTargets" .!= mempty))

instance Hashable HTTPRouteAction where

instance NFData HTTPRouteAction where

instance ToJSON HTTPRouteAction where
        toJSON HTTPRouteAction'{..}
          = object
              (catMaybes
                 [("weightedTargets" .=) <$> _httpraWeightedTargets])

-- | An object representing the requirements for a route to match HTTP requests for a virtual
--
--          router.
--
--
-- /See:/ 'hTTPRouteMatch' smart constructor.
newtype HTTPRouteMatch = HTTPRouteMatch'
  { _httprmPrefix :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HTTPRouteMatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httprmPrefix' - Specifies the path with which to match requests. This parameter must always start with             @/@ , which by itself matches all requests to the virtual router service name.          You can also match for path-based routing of requests. For example, if your virtual router          service name is @my-service.local@ , and you want the route to match requests to             @my-service.local/metrics@ , then your prefix should be          @/metrics@ .
hTTPRouteMatch
    :: HTTPRouteMatch
hTTPRouteMatch = HTTPRouteMatch' {_httprmPrefix = Nothing}


-- | Specifies the path with which to match requests. This parameter must always start with             @/@ , which by itself matches all requests to the virtual router service name.          You can also match for path-based routing of requests. For example, if your virtual router          service name is @my-service.local@ , and you want the route to match requests to             @my-service.local/metrics@ , then your prefix should be          @/metrics@ .
httprmPrefix :: Lens' HTTPRouteMatch (Maybe Text)
httprmPrefix = lens _httprmPrefix (\ s a -> s{_httprmPrefix = a})

instance FromJSON HTTPRouteMatch where
        parseJSON
          = withObject "HTTPRouteMatch"
              (\ x -> HTTPRouteMatch' <$> (x .:? "prefix"))

instance Hashable HTTPRouteMatch where

instance NFData HTTPRouteMatch where

instance ToJSON HTTPRouteMatch where
        toJSON HTTPRouteMatch'{..}
          = object
              (catMaybes [("prefix" .=) <$> _httprmPrefix])

-- | An object representing the health check policy for a virtual node's listener.
--
--
--
-- /See:/ 'healthCheckPolicy' smart constructor.
data HealthCheckPolicy = HealthCheckPolicy'
  { _hcpPath :: !(Maybe Text)
  , _hcpPort :: !(Maybe Nat)
  , _hcpHealthyThreshold :: !Nat
  , _hcpIntervalMillis :: !Nat
  , _hcpProtocol :: !PortProtocol
  , _hcpTimeoutMillis :: !Nat
  , _hcpUnhealthyThreshold :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HealthCheckPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hcpPath' - The destination path for the health check request. This is only required if the          specified protocol is HTTP; if the protocol is TCP, then this parameter is ignored.
--
-- * 'hcpPort' - The destination port for the health check request. This port must match the port defined          in the 'PortMapping' for the listener.
--
-- * 'hcpHealthyThreshold' - The number of consecutive successful health checks that must occur before declaring          listener healthy.
--
-- * 'hcpIntervalMillis' - The time period in milliseconds between each health check execution.
--
-- * 'hcpProtocol' - The protocol for the health check request.
--
-- * 'hcpTimeoutMillis' - The amount of time to wait when receiving a response from the health check, in          milliseconds.
--
-- * 'hcpUnhealthyThreshold' - The number of consecutive failed health checks that must occur before declaring a          virtual node unhealthy. 
healthCheckPolicy
    :: Natural -- ^ 'hcpHealthyThreshold'
    -> Natural -- ^ 'hcpIntervalMillis'
    -> PortProtocol -- ^ 'hcpProtocol'
    -> Natural -- ^ 'hcpTimeoutMillis'
    -> Natural -- ^ 'hcpUnhealthyThreshold'
    -> HealthCheckPolicy
healthCheckPolicy pHealthyThreshold_ pIntervalMillis_ pProtocol_ pTimeoutMillis_ pUnhealthyThreshold_ =
  HealthCheckPolicy'
    { _hcpPath = Nothing
    , _hcpPort = Nothing
    , _hcpHealthyThreshold = _Nat # pHealthyThreshold_
    , _hcpIntervalMillis = _Nat # pIntervalMillis_
    , _hcpProtocol = pProtocol_
    , _hcpTimeoutMillis = _Nat # pTimeoutMillis_
    , _hcpUnhealthyThreshold = _Nat # pUnhealthyThreshold_
    }


-- | The destination path for the health check request. This is only required if the          specified protocol is HTTP; if the protocol is TCP, then this parameter is ignored.
hcpPath :: Lens' HealthCheckPolicy (Maybe Text)
hcpPath = lens _hcpPath (\ s a -> s{_hcpPath = a})

-- | The destination port for the health check request. This port must match the port defined          in the 'PortMapping' for the listener.
hcpPort :: Lens' HealthCheckPolicy (Maybe Natural)
hcpPort = lens _hcpPort (\ s a -> s{_hcpPort = a}) . mapping _Nat

-- | The number of consecutive successful health checks that must occur before declaring          listener healthy.
hcpHealthyThreshold :: Lens' HealthCheckPolicy Natural
hcpHealthyThreshold = lens _hcpHealthyThreshold (\ s a -> s{_hcpHealthyThreshold = a}) . _Nat

-- | The time period in milliseconds between each health check execution.
hcpIntervalMillis :: Lens' HealthCheckPolicy Natural
hcpIntervalMillis = lens _hcpIntervalMillis (\ s a -> s{_hcpIntervalMillis = a}) . _Nat

-- | The protocol for the health check request.
hcpProtocol :: Lens' HealthCheckPolicy PortProtocol
hcpProtocol = lens _hcpProtocol (\ s a -> s{_hcpProtocol = a})

-- | The amount of time to wait when receiving a response from the health check, in          milliseconds.
hcpTimeoutMillis :: Lens' HealthCheckPolicy Natural
hcpTimeoutMillis = lens _hcpTimeoutMillis (\ s a -> s{_hcpTimeoutMillis = a}) . _Nat

-- | The number of consecutive failed health checks that must occur before declaring a          virtual node unhealthy. 
hcpUnhealthyThreshold :: Lens' HealthCheckPolicy Natural
hcpUnhealthyThreshold = lens _hcpUnhealthyThreshold (\ s a -> s{_hcpUnhealthyThreshold = a}) . _Nat

instance FromJSON HealthCheckPolicy where
        parseJSON
          = withObject "HealthCheckPolicy"
              (\ x ->
                 HealthCheckPolicy' <$>
                   (x .:? "path") <*> (x .:? "port") <*>
                     (x .: "healthyThreshold")
                     <*> (x .: "intervalMillis")
                     <*> (x .: "protocol")
                     <*> (x .: "timeoutMillis")
                     <*> (x .: "unhealthyThreshold"))

instance Hashable HealthCheckPolicy where

instance NFData HealthCheckPolicy where

instance ToJSON HealthCheckPolicy where
        toJSON HealthCheckPolicy'{..}
          = object
              (catMaybes
                 [("path" .=) <$> _hcpPath, ("port" .=) <$> _hcpPort,
                  Just ("healthyThreshold" .= _hcpHealthyThreshold),
                  Just ("intervalMillis" .= _hcpIntervalMillis),
                  Just ("protocol" .= _hcpProtocol),
                  Just ("timeoutMillis" .= _hcpTimeoutMillis),
                  Just
                    ("unhealthyThreshold" .= _hcpUnhealthyThreshold)])

-- | An object representing a listener for a virtual node.
--
--
--
-- /See:/ 'listener' smart constructor.
data Listener = Listener'
  { _lHealthCheck :: !(Maybe HealthCheckPolicy)
  , _lPortMapping :: !(Maybe PortMapping)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Listener' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lHealthCheck' - The health check information for the listener.
--
-- * 'lPortMapping' - The port mapping information for the listener.
listener
    :: Listener
listener = Listener' {_lHealthCheck = Nothing, _lPortMapping = Nothing}


-- | The health check information for the listener.
lHealthCheck :: Lens' Listener (Maybe HealthCheckPolicy)
lHealthCheck = lens _lHealthCheck (\ s a -> s{_lHealthCheck = a})

-- | The port mapping information for the listener.
lPortMapping :: Lens' Listener (Maybe PortMapping)
lPortMapping = lens _lPortMapping (\ s a -> s{_lPortMapping = a})

instance FromJSON Listener where
        parseJSON
          = withObject "Listener"
              (\ x ->
                 Listener' <$>
                   (x .:? "healthCheck") <*> (x .:? "portMapping"))

instance Hashable Listener where

instance NFData Listener where

instance ToJSON Listener where
        toJSON Listener'{..}
          = object
              (catMaybes
                 [("healthCheck" .=) <$> _lHealthCheck,
                  ("portMapping" .=) <$> _lPortMapping])

-- | An object representing a service mesh returned by a describe operation.
--
--
--
-- /See:/ 'meshData' smart constructor.
data MeshData = MeshData'
  { _mdStatus :: !(Maybe MeshStatus)
  , _mdMeshName :: !Text
  , _mdMetadata :: !ResourceMetadata
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MeshData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdStatus' - The status of the service mesh.
--
-- * 'mdMeshName' - The name of the service mesh.
--
-- * 'mdMetadata' - The associated metadata for the service mesh.
meshData
    :: Text -- ^ 'mdMeshName'
    -> ResourceMetadata -- ^ 'mdMetadata'
    -> MeshData
meshData pMeshName_ pMetadata_ =
  MeshData'
    {_mdStatus = Nothing, _mdMeshName = pMeshName_, _mdMetadata = pMetadata_}


-- | The status of the service mesh.
mdStatus :: Lens' MeshData (Maybe MeshStatus)
mdStatus = lens _mdStatus (\ s a -> s{_mdStatus = a})

-- | The name of the service mesh.
mdMeshName :: Lens' MeshData Text
mdMeshName = lens _mdMeshName (\ s a -> s{_mdMeshName = a})

-- | The associated metadata for the service mesh.
mdMetadata :: Lens' MeshData ResourceMetadata
mdMetadata = lens _mdMetadata (\ s a -> s{_mdMetadata = a})

instance FromJSON MeshData where
        parseJSON
          = withObject "MeshData"
              (\ x ->
                 MeshData' <$>
                   (x .:? "status") <*> (x .: "meshName") <*>
                     (x .: "metadata"))

instance Hashable MeshData where

instance NFData MeshData where

-- | An object representing a service mesh returned by a list operation.
--
--
--
-- /See:/ 'meshRef' smart constructor.
data MeshRef = MeshRef'
  { _mrMeshName :: !(Maybe Text)
  , _mrArn :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MeshRef' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrMeshName' - The name of the service mesh.
--
-- * 'mrArn' - The full Amazon Resource Name (ARN) of the service mesh.
meshRef
    :: MeshRef
meshRef = MeshRef' {_mrMeshName = Nothing, _mrArn = Nothing}


-- | The name of the service mesh.
mrMeshName :: Lens' MeshRef (Maybe Text)
mrMeshName = lens _mrMeshName (\ s a -> s{_mrMeshName = a})

-- | The full Amazon Resource Name (ARN) of the service mesh.
mrArn :: Lens' MeshRef (Maybe Text)
mrArn = lens _mrArn (\ s a -> s{_mrArn = a})

instance FromJSON MeshRef where
        parseJSON
          = withObject "MeshRef"
              (\ x ->
                 MeshRef' <$> (x .:? "meshName") <*> (x .:? "arn"))

instance Hashable MeshRef where

instance NFData MeshRef where

-- | An object representing the status of a service mesh.
--
--
--
-- /See:/ 'meshStatus' smart constructor.
newtype MeshStatus = MeshStatus'
  { _msStatus :: Maybe MeshStatusCode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MeshStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msStatus' - The current mesh status.
meshStatus
    :: MeshStatus
meshStatus = MeshStatus' {_msStatus = Nothing}


-- | The current mesh status.
msStatus :: Lens' MeshStatus (Maybe MeshStatusCode)
msStatus = lens _msStatus (\ s a -> s{_msStatus = a})

instance FromJSON MeshStatus where
        parseJSON
          = withObject "MeshStatus"
              (\ x -> MeshStatus' <$> (x .:? "status"))

instance Hashable MeshStatus where

instance NFData MeshStatus where

-- | An object representing a virtual node listener port mapping.
--
--
--
-- /See:/ 'portMapping' smart constructor.
data PortMapping = PortMapping'
  { _pmProtocol :: !(Maybe PortProtocol)
  , _pmPort :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PortMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmProtocol' - The protocol used for the port mapping.
--
-- * 'pmPort' - The port used for the port mapping.
portMapping
    :: PortMapping
portMapping = PortMapping' {_pmProtocol = Nothing, _pmPort = Nothing}


-- | The protocol used for the port mapping.
pmProtocol :: Lens' PortMapping (Maybe PortProtocol)
pmProtocol = lens _pmProtocol (\ s a -> s{_pmProtocol = a})

-- | The port used for the port mapping.
pmPort :: Lens' PortMapping (Maybe Natural)
pmPort = lens _pmPort (\ s a -> s{_pmPort = a}) . mapping _Nat

instance FromJSON PortMapping where
        parseJSON
          = withObject "PortMapping"
              (\ x ->
                 PortMapping' <$>
                   (x .:? "protocol") <*> (x .:? "port"))

instance Hashable PortMapping where

instance NFData PortMapping where

instance ToJSON PortMapping where
        toJSON PortMapping'{..}
          = object
              (catMaybes
                 [("protocol" .=) <$> _pmProtocol,
                  ("port" .=) <$> _pmPort])

-- | An object representing metadata for a resource.
--
--
--
-- /See:/ 'resourceMetadata' smart constructor.
data ResourceMetadata = ResourceMetadata'
  { _rmLastUpdatedAt :: !(Maybe POSIX)
  , _rmArn :: !(Maybe Text)
  , _rmCreatedAt :: !(Maybe POSIX)
  , _rmUid :: !(Maybe Text)
  , _rmVersion :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmLastUpdatedAt' - The Unix epoch timestamp in seconds for when the resource was last updated.
--
-- * 'rmArn' - The full Amazon Resource Name (ARN) for the resource.
--
-- * 'rmCreatedAt' - The Unix epoch timestamp in seconds for when the resource was created.
--
-- * 'rmUid' - The unique identifier for the resource.
--
-- * 'rmVersion' - The version of the resource. Resources are created at version 1, and this version is          incremented each time they are updated.
resourceMetadata
    :: ResourceMetadata
resourceMetadata =
  ResourceMetadata'
    { _rmLastUpdatedAt = Nothing
    , _rmArn = Nothing
    , _rmCreatedAt = Nothing
    , _rmUid = Nothing
    , _rmVersion = Nothing
    }


-- | The Unix epoch timestamp in seconds for when the resource was last updated.
rmLastUpdatedAt :: Lens' ResourceMetadata (Maybe UTCTime)
rmLastUpdatedAt = lens _rmLastUpdatedAt (\ s a -> s{_rmLastUpdatedAt = a}) . mapping _Time

-- | The full Amazon Resource Name (ARN) for the resource.
rmArn :: Lens' ResourceMetadata (Maybe Text)
rmArn = lens _rmArn (\ s a -> s{_rmArn = a})

-- | The Unix epoch timestamp in seconds for when the resource was created.
rmCreatedAt :: Lens' ResourceMetadata (Maybe UTCTime)
rmCreatedAt = lens _rmCreatedAt (\ s a -> s{_rmCreatedAt = a}) . mapping _Time

-- | The unique identifier for the resource.
rmUid :: Lens' ResourceMetadata (Maybe Text)
rmUid = lens _rmUid (\ s a -> s{_rmUid = a})

-- | The version of the resource. Resources are created at version 1, and this version is          incremented each time they are updated.
rmVersion :: Lens' ResourceMetadata (Maybe Integer)
rmVersion = lens _rmVersion (\ s a -> s{_rmVersion = a})

instance FromJSON ResourceMetadata where
        parseJSON
          = withObject "ResourceMetadata"
              (\ x ->
                 ResourceMetadata' <$>
                   (x .:? "lastUpdatedAt") <*> (x .:? "arn") <*>
                     (x .:? "createdAt")
                     <*> (x .:? "uid")
                     <*> (x .:? "version"))

instance Hashable ResourceMetadata where

instance NFData ResourceMetadata where

-- | An object representing a route returned by a describe operation.
--
--
--
-- /See:/ 'routeData' smart constructor.
data RouteData = RouteData'
  { _rdStatus :: !(Maybe RouteStatus)
  , _rdSpec :: !(Maybe RouteSpec)
  , _rdMetadata :: !(Maybe ResourceMetadata)
  , _rdMeshName :: !Text
  , _rdRouteName :: !Text
  , _rdVirtualRouterName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RouteData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdStatus' - The status of the route.
--
-- * 'rdSpec' - The specifications of the route.
--
-- * 'rdMetadata' - The associated metadata for the route.
--
-- * 'rdMeshName' - The name of the service mesh in which the route resides.
--
-- * 'rdRouteName' - The name of the route.
--
-- * 'rdVirtualRouterName' - The virtual router with which the route is associated.
routeData
    :: Text -- ^ 'rdMeshName'
    -> Text -- ^ 'rdRouteName'
    -> Text -- ^ 'rdVirtualRouterName'
    -> RouteData
routeData pMeshName_ pRouteName_ pVirtualRouterName_ =
  RouteData'
    { _rdStatus = Nothing
    , _rdSpec = Nothing
    , _rdMetadata = Nothing
    , _rdMeshName = pMeshName_
    , _rdRouteName = pRouteName_
    , _rdVirtualRouterName = pVirtualRouterName_
    }


-- | The status of the route.
rdStatus :: Lens' RouteData (Maybe RouteStatus)
rdStatus = lens _rdStatus (\ s a -> s{_rdStatus = a})

-- | The specifications of the route.
rdSpec :: Lens' RouteData (Maybe RouteSpec)
rdSpec = lens _rdSpec (\ s a -> s{_rdSpec = a})

-- | The associated metadata for the route.
rdMetadata :: Lens' RouteData (Maybe ResourceMetadata)
rdMetadata = lens _rdMetadata (\ s a -> s{_rdMetadata = a})

-- | The name of the service mesh in which the route resides.
rdMeshName :: Lens' RouteData Text
rdMeshName = lens _rdMeshName (\ s a -> s{_rdMeshName = a})

-- | The name of the route.
rdRouteName :: Lens' RouteData Text
rdRouteName = lens _rdRouteName (\ s a -> s{_rdRouteName = a})

-- | The virtual router with which the route is associated.
rdVirtualRouterName :: Lens' RouteData Text
rdVirtualRouterName = lens _rdVirtualRouterName (\ s a -> s{_rdVirtualRouterName = a})

instance FromJSON RouteData where
        parseJSON
          = withObject "RouteData"
              (\ x ->
                 RouteData' <$>
                   (x .:? "status") <*> (x .:? "spec") <*>
                     (x .:? "metadata")
                     <*> (x .: "meshName")
                     <*> (x .: "routeName")
                     <*> (x .: "virtualRouterName"))

instance Hashable RouteData where

instance NFData RouteData where

-- | An object representing a route returned by a list operation.
--
--
--
-- /See:/ 'routeRef' smart constructor.
data RouteRef = RouteRef'
  { _rrMeshName :: !(Maybe Text)
  , _rrArn :: !(Maybe Text)
  , _rrRouteName :: !(Maybe Text)
  , _rrVirtualRouterName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RouteRef' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrMeshName' - The name of the service mesh in which the route resides.
--
-- * 'rrArn' - The full Amazon Resource Name (ARN) for the route.
--
-- * 'rrRouteName' - The name of the route.
--
-- * 'rrVirtualRouterName' - The virtual router with which the route is associated.
routeRef
    :: RouteRef
routeRef =
  RouteRef'
    { _rrMeshName = Nothing
    , _rrArn = Nothing
    , _rrRouteName = Nothing
    , _rrVirtualRouterName = Nothing
    }


-- | The name of the service mesh in which the route resides.
rrMeshName :: Lens' RouteRef (Maybe Text)
rrMeshName = lens _rrMeshName (\ s a -> s{_rrMeshName = a})

-- | The full Amazon Resource Name (ARN) for the route.
rrArn :: Lens' RouteRef (Maybe Text)
rrArn = lens _rrArn (\ s a -> s{_rrArn = a})

-- | The name of the route.
rrRouteName :: Lens' RouteRef (Maybe Text)
rrRouteName = lens _rrRouteName (\ s a -> s{_rrRouteName = a})

-- | The virtual router with which the route is associated.
rrVirtualRouterName :: Lens' RouteRef (Maybe Text)
rrVirtualRouterName = lens _rrVirtualRouterName (\ s a -> s{_rrVirtualRouterName = a})

instance FromJSON RouteRef where
        parseJSON
          = withObject "RouteRef"
              (\ x ->
                 RouteRef' <$>
                   (x .:? "meshName") <*> (x .:? "arn") <*>
                     (x .:? "routeName")
                     <*> (x .:? "virtualRouterName"))

instance Hashable RouteRef where

instance NFData RouteRef where

-- | An object representing the specification of a route.
--
--
--
-- /See:/ 'routeSpec' smart constructor.
newtype RouteSpec = RouteSpec'
  { _rsHttpRoute :: Maybe HTTPRoute
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RouteSpec' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsHttpRoute' - The HTTP routing information for the route.
routeSpec
    :: RouteSpec
routeSpec = RouteSpec' {_rsHttpRoute = Nothing}


-- | The HTTP routing information for the route.
rsHttpRoute :: Lens' RouteSpec (Maybe HTTPRoute)
rsHttpRoute = lens _rsHttpRoute (\ s a -> s{_rsHttpRoute = a})

instance FromJSON RouteSpec where
        parseJSON
          = withObject "RouteSpec"
              (\ x -> RouteSpec' <$> (x .:? "httpRoute"))

instance Hashable RouteSpec where

instance NFData RouteSpec where

instance ToJSON RouteSpec where
        toJSON RouteSpec'{..}
          = object
              (catMaybes [("httpRoute" .=) <$> _rsHttpRoute])

-- | An object representing the current status of a route.
--
--
--
-- /See:/ 'routeStatus' smart constructor.
newtype RouteStatus = RouteStatus'
  { _rsStatus :: Maybe RouteStatusCode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RouteStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsStatus' - The current status for the route.
routeStatus
    :: RouteStatus
routeStatus = RouteStatus' {_rsStatus = Nothing}


-- | The current status for the route.
rsStatus :: Lens' RouteStatus (Maybe RouteStatusCode)
rsStatus = lens _rsStatus (\ s a -> s{_rsStatus = a})

instance FromJSON RouteStatus where
        parseJSON
          = withObject "RouteStatus"
              (\ x -> RouteStatus' <$> (x .:? "status"))

instance Hashable RouteStatus where

instance NFData RouteStatus where

-- | An object representing the service discovery information for a virtual node.
--
--
--
-- /See:/ 'serviceDiscovery' smart constructor.
newtype ServiceDiscovery = ServiceDiscovery'
  { _sdDns :: Maybe DNSServiceDiscovery
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServiceDiscovery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdDns' - Specifies the DNS service name for the virtual node.
serviceDiscovery
    :: ServiceDiscovery
serviceDiscovery = ServiceDiscovery' {_sdDns = Nothing}


-- | Specifies the DNS service name for the virtual node.
sdDns :: Lens' ServiceDiscovery (Maybe DNSServiceDiscovery)
sdDns = lens _sdDns (\ s a -> s{_sdDns = a})

instance FromJSON ServiceDiscovery where
        parseJSON
          = withObject "ServiceDiscovery"
              (\ x -> ServiceDiscovery' <$> (x .:? "dns"))

instance Hashable ServiceDiscovery where

instance NFData ServiceDiscovery where

instance ToJSON ServiceDiscovery where
        toJSON ServiceDiscovery'{..}
          = object (catMaybes [("dns" .=) <$> _sdDns])

-- | An object representing a virtual node returned by a describe operation.
--
--
--
-- /See:/ 'virtualNodeData' smart constructor.
data VirtualNodeData = VirtualNodeData'
  { _vndStatus :: !(Maybe VirtualNodeStatus)
  , _vndSpec :: !(Maybe VirtualNodeSpec)
  , _vndMetadata :: !(Maybe ResourceMetadata)
  , _vndMeshName :: !Text
  , _vndVirtualNodeName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VirtualNodeData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vndStatus' - The current status for the virtual node.
--
-- * 'vndSpec' - The specifications of the virtual node.
--
-- * 'vndMetadata' - The associated metadata for the virtual node.
--
-- * 'vndMeshName' - The name of the service mesh in which the virtual node resides.
--
-- * 'vndVirtualNodeName' - The name of the virtual node.
virtualNodeData
    :: Text -- ^ 'vndMeshName'
    -> Text -- ^ 'vndVirtualNodeName'
    -> VirtualNodeData
virtualNodeData pMeshName_ pVirtualNodeName_ =
  VirtualNodeData'
    { _vndStatus = Nothing
    , _vndSpec = Nothing
    , _vndMetadata = Nothing
    , _vndMeshName = pMeshName_
    , _vndVirtualNodeName = pVirtualNodeName_
    }


-- | The current status for the virtual node.
vndStatus :: Lens' VirtualNodeData (Maybe VirtualNodeStatus)
vndStatus = lens _vndStatus (\ s a -> s{_vndStatus = a})

-- | The specifications of the virtual node.
vndSpec :: Lens' VirtualNodeData (Maybe VirtualNodeSpec)
vndSpec = lens _vndSpec (\ s a -> s{_vndSpec = a})

-- | The associated metadata for the virtual node.
vndMetadata :: Lens' VirtualNodeData (Maybe ResourceMetadata)
vndMetadata = lens _vndMetadata (\ s a -> s{_vndMetadata = a})

-- | The name of the service mesh in which the virtual node resides.
vndMeshName :: Lens' VirtualNodeData Text
vndMeshName = lens _vndMeshName (\ s a -> s{_vndMeshName = a})

-- | The name of the virtual node.
vndVirtualNodeName :: Lens' VirtualNodeData Text
vndVirtualNodeName = lens _vndVirtualNodeName (\ s a -> s{_vndVirtualNodeName = a})

instance FromJSON VirtualNodeData where
        parseJSON
          = withObject "VirtualNodeData"
              (\ x ->
                 VirtualNodeData' <$>
                   (x .:? "status") <*> (x .:? "spec") <*>
                     (x .:? "metadata")
                     <*> (x .: "meshName")
                     <*> (x .: "virtualNodeName"))

instance Hashable VirtualNodeData where

instance NFData VirtualNodeData where

-- | An object representing a virtual node returned by a list operation.
--
--
--
-- /See:/ 'virtualNodeRef' smart constructor.
data VirtualNodeRef = VirtualNodeRef'
  { _vnrMeshName :: !(Maybe Text)
  , _vnrArn :: !(Maybe Text)
  , _vnrVirtualNodeName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VirtualNodeRef' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vnrMeshName' - The name of the service mesh in which the virtual node resides.
--
-- * 'vnrArn' - The full Amazon Resource Name (ARN) for the virtual node.
--
-- * 'vnrVirtualNodeName' - The name of the virtual node.
virtualNodeRef
    :: VirtualNodeRef
virtualNodeRef =
  VirtualNodeRef'
    {_vnrMeshName = Nothing, _vnrArn = Nothing, _vnrVirtualNodeName = Nothing}


-- | The name of the service mesh in which the virtual node resides.
vnrMeshName :: Lens' VirtualNodeRef (Maybe Text)
vnrMeshName = lens _vnrMeshName (\ s a -> s{_vnrMeshName = a})

-- | The full Amazon Resource Name (ARN) for the virtual node.
vnrArn :: Lens' VirtualNodeRef (Maybe Text)
vnrArn = lens _vnrArn (\ s a -> s{_vnrArn = a})

-- | The name of the virtual node.
vnrVirtualNodeName :: Lens' VirtualNodeRef (Maybe Text)
vnrVirtualNodeName = lens _vnrVirtualNodeName (\ s a -> s{_vnrVirtualNodeName = a})

instance FromJSON VirtualNodeRef where
        parseJSON
          = withObject "VirtualNodeRef"
              (\ x ->
                 VirtualNodeRef' <$>
                   (x .:? "meshName") <*> (x .:? "arn") <*>
                     (x .:? "virtualNodeName"))

instance Hashable VirtualNodeRef where

instance NFData VirtualNodeRef where

-- | An object representing the specification of a virtual node.
--
--
--
-- /See:/ 'virtualNodeSpec' smart constructor.
data VirtualNodeSpec = VirtualNodeSpec'
  { _vnsBackends :: !(Maybe [Text])
  , _vnsServiceDiscovery :: !(Maybe ServiceDiscovery)
  , _vnsListeners :: !(Maybe [Listener])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VirtualNodeSpec' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vnsBackends' - The backends to which the virtual node is expected to send outbound traffic.
--
-- * 'vnsServiceDiscovery' - The service discovery information for the virtual node.
--
-- * 'vnsListeners' - The listeners from which the virtual node is expected to receive inbound traffic.
virtualNodeSpec
    :: VirtualNodeSpec
virtualNodeSpec =
  VirtualNodeSpec'
    { _vnsBackends = Nothing
    , _vnsServiceDiscovery = Nothing
    , _vnsListeners = Nothing
    }


-- | The backends to which the virtual node is expected to send outbound traffic.
vnsBackends :: Lens' VirtualNodeSpec [Text]
vnsBackends = lens _vnsBackends (\ s a -> s{_vnsBackends = a}) . _Default . _Coerce

-- | The service discovery information for the virtual node.
vnsServiceDiscovery :: Lens' VirtualNodeSpec (Maybe ServiceDiscovery)
vnsServiceDiscovery = lens _vnsServiceDiscovery (\ s a -> s{_vnsServiceDiscovery = a})

-- | The listeners from which the virtual node is expected to receive inbound traffic.
vnsListeners :: Lens' VirtualNodeSpec [Listener]
vnsListeners = lens _vnsListeners (\ s a -> s{_vnsListeners = a}) . _Default . _Coerce

instance FromJSON VirtualNodeSpec where
        parseJSON
          = withObject "VirtualNodeSpec"
              (\ x ->
                 VirtualNodeSpec' <$>
                   (x .:? "backends" .!= mempty) <*>
                     (x .:? "serviceDiscovery")
                     <*> (x .:? "listeners" .!= mempty))

instance Hashable VirtualNodeSpec where

instance NFData VirtualNodeSpec where

instance ToJSON VirtualNodeSpec where
        toJSON VirtualNodeSpec'{..}
          = object
              (catMaybes
                 [("backends" .=) <$> _vnsBackends,
                  ("serviceDiscovery" .=) <$> _vnsServiceDiscovery,
                  ("listeners" .=) <$> _vnsListeners])

-- | An object representing the current status of the virtual node.
--
--
--
-- /See:/ 'virtualNodeStatus' smart constructor.
newtype VirtualNodeStatus = VirtualNodeStatus'
  { _vnsStatus :: Maybe VirtualNodeStatusCode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VirtualNodeStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vnsStatus' - The current status of the virtual node.
virtualNodeStatus
    :: VirtualNodeStatus
virtualNodeStatus = VirtualNodeStatus' {_vnsStatus = Nothing}


-- | The current status of the virtual node.
vnsStatus :: Lens' VirtualNodeStatus (Maybe VirtualNodeStatusCode)
vnsStatus = lens _vnsStatus (\ s a -> s{_vnsStatus = a})

instance FromJSON VirtualNodeStatus where
        parseJSON
          = withObject "VirtualNodeStatus"
              (\ x -> VirtualNodeStatus' <$> (x .:? "status"))

instance Hashable VirtualNodeStatus where

instance NFData VirtualNodeStatus where

-- | An object representing a virtual router returned by a describe operation.
--
--
--
-- /See:/ 'virtualRouterData' smart constructor.
data VirtualRouterData = VirtualRouterData'
  { _vrdStatus :: !(Maybe VirtualRouterStatus)
  , _vrdSpec :: !(Maybe VirtualRouterSpec)
  , _vrdMetadata :: !(Maybe ResourceMetadata)
  , _vrdMeshName :: !Text
  , _vrdVirtualRouterName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VirtualRouterData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vrdStatus' - The current status of the virtual router.
--
-- * 'vrdSpec' - The specifications of the virtual router.
--
-- * 'vrdMetadata' - The associated metadata for the virtual router.
--
-- * 'vrdMeshName' - The name of the service mesh in which the virtual router resides.
--
-- * 'vrdVirtualRouterName' - The name of the virtual router.
virtualRouterData
    :: Text -- ^ 'vrdMeshName'
    -> Text -- ^ 'vrdVirtualRouterName'
    -> VirtualRouterData
virtualRouterData pMeshName_ pVirtualRouterName_ =
  VirtualRouterData'
    { _vrdStatus = Nothing
    , _vrdSpec = Nothing
    , _vrdMetadata = Nothing
    , _vrdMeshName = pMeshName_
    , _vrdVirtualRouterName = pVirtualRouterName_
    }


-- | The current status of the virtual router.
vrdStatus :: Lens' VirtualRouterData (Maybe VirtualRouterStatus)
vrdStatus = lens _vrdStatus (\ s a -> s{_vrdStatus = a})

-- | The specifications of the virtual router.
vrdSpec :: Lens' VirtualRouterData (Maybe VirtualRouterSpec)
vrdSpec = lens _vrdSpec (\ s a -> s{_vrdSpec = a})

-- | The associated metadata for the virtual router.
vrdMetadata :: Lens' VirtualRouterData (Maybe ResourceMetadata)
vrdMetadata = lens _vrdMetadata (\ s a -> s{_vrdMetadata = a})

-- | The name of the service mesh in which the virtual router resides.
vrdMeshName :: Lens' VirtualRouterData Text
vrdMeshName = lens _vrdMeshName (\ s a -> s{_vrdMeshName = a})

-- | The name of the virtual router.
vrdVirtualRouterName :: Lens' VirtualRouterData Text
vrdVirtualRouterName = lens _vrdVirtualRouterName (\ s a -> s{_vrdVirtualRouterName = a})

instance FromJSON VirtualRouterData where
        parseJSON
          = withObject "VirtualRouterData"
              (\ x ->
                 VirtualRouterData' <$>
                   (x .:? "status") <*> (x .:? "spec") <*>
                     (x .:? "metadata")
                     <*> (x .: "meshName")
                     <*> (x .: "virtualRouterName"))

instance Hashable VirtualRouterData where

instance NFData VirtualRouterData where

-- | An object representing a virtual router returned by a list operation.
--
--
--
-- /See:/ 'virtualRouterRef' smart constructor.
data VirtualRouterRef = VirtualRouterRef'
  { _vrrMeshName :: !(Maybe Text)
  , _vrrArn :: !(Maybe Text)
  , _vrrVirtualRouterName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VirtualRouterRef' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vrrMeshName' - The name of the service mesh in which the virtual router resides.
--
-- * 'vrrArn' - The full Amazon Resource Name (ARN) for the virtual router.
--
-- * 'vrrVirtualRouterName' - The name of the virtual router.
virtualRouterRef
    :: VirtualRouterRef
virtualRouterRef =
  VirtualRouterRef'
    {_vrrMeshName = Nothing, _vrrArn = Nothing, _vrrVirtualRouterName = Nothing}


-- | The name of the service mesh in which the virtual router resides.
vrrMeshName :: Lens' VirtualRouterRef (Maybe Text)
vrrMeshName = lens _vrrMeshName (\ s a -> s{_vrrMeshName = a})

-- | The full Amazon Resource Name (ARN) for the virtual router.
vrrArn :: Lens' VirtualRouterRef (Maybe Text)
vrrArn = lens _vrrArn (\ s a -> s{_vrrArn = a})

-- | The name of the virtual router.
vrrVirtualRouterName :: Lens' VirtualRouterRef (Maybe Text)
vrrVirtualRouterName = lens _vrrVirtualRouterName (\ s a -> s{_vrrVirtualRouterName = a})

instance FromJSON VirtualRouterRef where
        parseJSON
          = withObject "VirtualRouterRef"
              (\ x ->
                 VirtualRouterRef' <$>
                   (x .:? "meshName") <*> (x .:? "arn") <*>
                     (x .:? "virtualRouterName"))

instance Hashable VirtualRouterRef where

instance NFData VirtualRouterRef where

-- | An object representing the specification of a virtual router.
--
--
--
-- /See:/ 'virtualRouterSpec' smart constructor.
newtype VirtualRouterSpec = VirtualRouterSpec'
  { _vrsServiceNames :: Maybe [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VirtualRouterSpec' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vrsServiceNames' - The service mesh service names to associate with the virtual router.
virtualRouterSpec
    :: VirtualRouterSpec
virtualRouterSpec = VirtualRouterSpec' {_vrsServiceNames = Nothing}


-- | The service mesh service names to associate with the virtual router.
vrsServiceNames :: Lens' VirtualRouterSpec [Text]
vrsServiceNames = lens _vrsServiceNames (\ s a -> s{_vrsServiceNames = a}) . _Default . _Coerce

instance FromJSON VirtualRouterSpec where
        parseJSON
          = withObject "VirtualRouterSpec"
              (\ x ->
                 VirtualRouterSpec' <$>
                   (x .:? "serviceNames" .!= mempty))

instance Hashable VirtualRouterSpec where

instance NFData VirtualRouterSpec where

instance ToJSON VirtualRouterSpec where
        toJSON VirtualRouterSpec'{..}
          = object
              (catMaybes
                 [("serviceNames" .=) <$> _vrsServiceNames])

-- | An object representing the status of a virtual router. 
--
--
--
-- /See:/ 'virtualRouterStatus' smart constructor.
newtype VirtualRouterStatus = VirtualRouterStatus'
  { _vrsStatus :: Maybe VirtualRouterStatusCode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VirtualRouterStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vrsStatus' - The current status of the virtual router.
virtualRouterStatus
    :: VirtualRouterStatus
virtualRouterStatus = VirtualRouterStatus' {_vrsStatus = Nothing}


-- | The current status of the virtual router.
vrsStatus :: Lens' VirtualRouterStatus (Maybe VirtualRouterStatusCode)
vrsStatus = lens _vrsStatus (\ s a -> s{_vrsStatus = a})

instance FromJSON VirtualRouterStatus where
        parseJSON
          = withObject "VirtualRouterStatus"
              (\ x -> VirtualRouterStatus' <$> (x .:? "status"))

instance Hashable VirtualRouterStatus where

instance NFData VirtualRouterStatus where

-- | An object representing a target and its relative weight. Traffic is distributed across
--
--          targets according to their relative weight. For example, a weighted target with a relative
--          weight of 50 receives five times as much traffic as one with a relative weight of
--          10.
--
--
-- /See:/ 'weightedTarget' smart constructor.
data WeightedTarget = WeightedTarget'
  { _wtWeight :: !(Maybe Nat)
  , _wtVirtualNode :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'WeightedTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wtWeight' - The relative weight of the weighted target.
--
-- * 'wtVirtualNode' - The virtual node to associate with the weighted target.
weightedTarget
    :: WeightedTarget
weightedTarget = WeightedTarget' {_wtWeight = Nothing, _wtVirtualNode = Nothing}


-- | The relative weight of the weighted target.
wtWeight :: Lens' WeightedTarget (Maybe Natural)
wtWeight = lens _wtWeight (\ s a -> s{_wtWeight = a}) . mapping _Nat

-- | The virtual node to associate with the weighted target.
wtVirtualNode :: Lens' WeightedTarget (Maybe Text)
wtVirtualNode = lens _wtVirtualNode (\ s a -> s{_wtVirtualNode = a})

instance FromJSON WeightedTarget where
        parseJSON
          = withObject "WeightedTarget"
              (\ x ->
                 WeightedTarget' <$>
                   (x .:? "weight") <*> (x .:? "virtualNode"))

instance Hashable WeightedTarget where

instance NFData WeightedTarget where

instance ToJSON WeightedTarget where
        toJSON WeightedTarget'{..}
          = object
              (catMaybes
                 [("weight" .=) <$> _wtWeight,
                  ("virtualNode" .=) <$> _wtVirtualNode])
