{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.APIGatewayV2
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.APIGatewayV2 where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.APIGatewayV2
import Test.AWS.APIGatewayV2.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateAPI $
--             createAPI
--
--         , requestGetDeployments $
--             getDeployments
--
--         , requestGetRouteResponses $
--             getRouteResponses
--
--         , requestGetDeployment $
--             getDeployment
--
--         , requestGetDomainNames $
--             getDomainNames
--
--         , requestGetModels $
--             getModels
--
--         , requestCreateIntegration $
--             createIntegration
--
--         , requestDeleteStage $
--             deleteStage
--
--         , requestUpdateStage $
--             updateStage
--
--         , requestCreateDeployment $
--             createDeployment
--
--         , requestDeleteRoute $
--             deleteRoute
--
--         , requestUpdateRoute $
--             updateRoute
--
--         , requestGetIntegrationResponses $
--             getIntegrationResponses
--
--         , requestGetIntegration $
--             getIntegration
--
--         , requestDeleteDeployment $
--             deleteDeployment
--
--         , requestUpdateDeployment $
--             updateDeployment
--
--         , requestDeleteRouteResponse $
--             deleteRouteResponse
--
--         , requestUpdateRouteResponse $
--             updateRouteResponse
--
--         , requestCreateModel $
--             createModel
--
--         , requestGetIntegrationResponse $
--             getIntegrationResponse
--
--         , requestCreateDomainName $
--             createDomainName
--
--         , requestDeleteModel $
--             deleteModel
--
--         , requestUpdateModel $
--             updateModel
--
--         , requestCreateRouteResponse $
--             createRouteResponse
--
--         , requestGetStages $
--             getStages
--
--         , requestGetModel $
--             getModel
--
--         , requestGetAPIMappings $
--             getAPIMappings
--
--         , requestCreateIntegrationResponse $
--             createIntegrationResponse
--
--         , requestGetDomainName $
--             getDomainName
--
--         , requestGetAuthorizers $
--             getAuthorizers
--
--         , requestGetRouteResponse $
--             getRouteResponse
--
--         , requestGetRoutes $
--             getRoutes
--
--         , requestDeleteIntegrationResponse $
--             deleteIntegrationResponse
--
--         , requestUpdateIntegrationResponse $
--             updateIntegrationResponse
--
--         , requestDeleteIntegration $
--             deleteIntegration
--
--         , requestUpdateIntegration $
--             updateIntegration
--
--         , requestGetRoute $
--             getRoute
--
--         , requestGetAuthorizer $
--             getAuthorizer
--
--         , requestGetStage $
--             getStage
--
--         , requestGetAPIMapping $
--             getAPIMapping
--
--         , requestGetAPIs $
--             getAPIs
--
--         , requestUpdateAPIMapping $
--             updateAPIMapping
--
--         , requestDeleteAPIMapping $
--             deleteAPIMapping
--
--         , requestCreateRoute $
--             createRoute
--
--         , requestCreateAuthorizer $
--             createAuthorizer
--
--         , requestUpdateAuthorizer $
--             updateAuthorizer
--
--         , requestDeleteAuthorizer $
--             deleteAuthorizer
--
--         , requestCreateAPIMapping $
--             createAPIMapping
--
--         , requestCreateStage $
--             createStage
--
--         , requestGetIntegrations $
--             getIntegrations
--
--         , requestUpdateDomainName $
--             updateDomainName
--
--         , requestDeleteDomainName $
--             deleteDomainName
--
--         , requestGetAPI $
--             getAPI
--
--         , requestDeleteAPI $
--             deleteAPI
--
--         , requestUpdateAPI $
--             updateAPI
--
--         , requestGetModelTemplate $
--             getModelTemplate
--
--           ]

--     , testGroup "response"
--         [ responseCreateAPI $
--             createAPIResponse
--
--         , responseGetDeployments $
--             getDeploymentsResponse
--
--         , responseGetRouteResponses $
--             getRouteResponsesResponse
--
--         , responseGetDeployment $
--             getDeploymentResponse
--
--         , responseGetDomainNames $
--             getDomainNamesResponse
--
--         , responseGetModels $
--             getModelsResponse
--
--         , responseCreateIntegration $
--             createIntegrationResponse'
--
--         , responseDeleteStage $
--             deleteStageResponse
--
--         , responseUpdateStage $
--             updateStageResponse
--
--         , responseCreateDeployment $
--             createDeploymentResponse
--
--         , responseDeleteRoute $
--             deleteRouteResponse'
--
--         , responseUpdateRoute $
--             updateRouteResponse'
--
--         , responseGetIntegrationResponses $
--             getIntegrationResponsesResponse
--
--         , responseGetIntegration $
--             getIntegrationResponse'
--
--         , responseDeleteDeployment $
--             deleteDeploymentResponse
--
--         , responseUpdateDeployment $
--             updateDeploymentResponse
--
--         , responseDeleteRouteResponse $
--             deleteRouteResponseResponse
--
--         , responseUpdateRouteResponse $
--             updateRouteResponseResponse
--
--         , responseCreateModel $
--             createModelResponse
--
--         , responseGetIntegrationResponse $
--             getIntegrationResponseResponse
--
--         , responseCreateDomainName $
--             createDomainNameResponse
--
--         , responseDeleteModel $
--             deleteModelResponse
--
--         , responseUpdateModel $
--             updateModelResponse
--
--         , responseCreateRouteResponse $
--             createRouteResponseResponse
--
--         , responseGetStages $
--             getStagesResponse
--
--         , responseGetModel $
--             getModelResponse
--
--         , responseGetAPIMappings $
--             getAPIMappingsResponse
--
--         , responseCreateIntegrationResponse $
--             createIntegrationResponseResponse
--
--         , responseGetDomainName $
--             getDomainNameResponse
--
--         , responseGetAuthorizers $
--             getAuthorizersResponse
--
--         , responseGetRouteResponse $
--             getRouteResponseResponse
--
--         , responseGetRoutes $
--             getRoutesResponse
--
--         , responseDeleteIntegrationResponse $
--             deleteIntegrationResponseResponse
--
--         , responseUpdateIntegrationResponse $
--             updateIntegrationResponseResponse
--
--         , responseDeleteIntegration $
--             deleteIntegrationResponse'
--
--         , responseUpdateIntegration $
--             updateIntegrationResponse'
--
--         , responseGetRoute $
--             getRouteResponse'
--
--         , responseGetAuthorizer $
--             getAuthorizerResponse
--
--         , responseGetStage $
--             getStageResponse
--
--         , responseGetAPIMapping $
--             getAPIMappingResponse
--
--         , responseGetAPIs $
--             getAPIsResponse
--
--         , responseUpdateAPIMapping $
--             updateAPIMappingResponse
--
--         , responseDeleteAPIMapping $
--             deleteAPIMappingResponse
--
--         , responseCreateRoute $
--             createRouteResponse'
--
--         , responseCreateAuthorizer $
--             createAuthorizerResponse
--
--         , responseUpdateAuthorizer $
--             updateAuthorizerResponse
--
--         , responseDeleteAuthorizer $
--             deleteAuthorizerResponse
--
--         , responseCreateAPIMapping $
--             createAPIMappingResponse
--
--         , responseCreateStage $
--             createStageResponse
--
--         , responseGetIntegrations $
--             getIntegrationsResponse
--
--         , responseUpdateDomainName $
--             updateDomainNameResponse
--
--         , responseDeleteDomainName $
--             deleteDomainNameResponse
--
--         , responseGetAPI $
--             getAPIResponse
--
--         , responseDeleteAPI $
--             deleteAPIResponse
--
--         , responseUpdateAPI $
--             updateAPIResponse
--
--         , responseGetModelTemplate $
--             getModelTemplateResponse
--
--           ]
--     ]

-- Requests

requestCreateAPI :: CreateAPI -> TestTree
requestCreateAPI = req
    "CreateAPI"
    "fixture/CreateAPI.yaml"

requestGetDeployments :: GetDeployments -> TestTree
requestGetDeployments = req
    "GetDeployments"
    "fixture/GetDeployments.yaml"

requestGetRouteResponses :: GetRouteResponses -> TestTree
requestGetRouteResponses = req
    "GetRouteResponses"
    "fixture/GetRouteResponses.yaml"

requestGetDeployment :: GetDeployment -> TestTree
requestGetDeployment = req
    "GetDeployment"
    "fixture/GetDeployment.yaml"

requestGetDomainNames :: GetDomainNames -> TestTree
requestGetDomainNames = req
    "GetDomainNames"
    "fixture/GetDomainNames.yaml"

requestGetModels :: GetModels -> TestTree
requestGetModels = req
    "GetModels"
    "fixture/GetModels.yaml"

requestCreateIntegration :: CreateIntegration -> TestTree
requestCreateIntegration = req
    "CreateIntegration"
    "fixture/CreateIntegration.yaml"

requestDeleteStage :: DeleteStage -> TestTree
requestDeleteStage = req
    "DeleteStage"
    "fixture/DeleteStage.yaml"

requestUpdateStage :: UpdateStage -> TestTree
requestUpdateStage = req
    "UpdateStage"
    "fixture/UpdateStage.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment = req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestDeleteRoute :: DeleteRoute -> TestTree
requestDeleteRoute = req
    "DeleteRoute"
    "fixture/DeleteRoute.yaml"

requestUpdateRoute :: UpdateRoute -> TestTree
requestUpdateRoute = req
    "UpdateRoute"
    "fixture/UpdateRoute.yaml"

requestGetIntegrationResponses :: GetIntegrationResponses -> TestTree
requestGetIntegrationResponses = req
    "GetIntegrationResponses"
    "fixture/GetIntegrationResponses.yaml"

requestGetIntegration :: GetIntegration -> TestTree
requestGetIntegration = req
    "GetIntegration"
    "fixture/GetIntegration.yaml"

requestDeleteDeployment :: DeleteDeployment -> TestTree
requestDeleteDeployment = req
    "DeleteDeployment"
    "fixture/DeleteDeployment.yaml"

requestUpdateDeployment :: UpdateDeployment -> TestTree
requestUpdateDeployment = req
    "UpdateDeployment"
    "fixture/UpdateDeployment.yaml"

requestDeleteRouteResponse :: DeleteRouteResponse -> TestTree
requestDeleteRouteResponse = req
    "DeleteRouteResponse"
    "fixture/DeleteRouteResponse.yaml"

requestUpdateRouteResponse :: UpdateRouteResponse -> TestTree
requestUpdateRouteResponse = req
    "UpdateRouteResponse"
    "fixture/UpdateRouteResponse.yaml"

requestCreateModel :: CreateModel -> TestTree
requestCreateModel = req
    "CreateModel"
    "fixture/CreateModel.yaml"

requestGetIntegrationResponse :: GetIntegrationResponse -> TestTree
requestGetIntegrationResponse = req
    "GetIntegrationResponse"
    "fixture/GetIntegrationResponse.yaml"

requestCreateDomainName :: CreateDomainName -> TestTree
requestCreateDomainName = req
    "CreateDomainName"
    "fixture/CreateDomainName.yaml"

requestDeleteModel :: DeleteModel -> TestTree
requestDeleteModel = req
    "DeleteModel"
    "fixture/DeleteModel.yaml"

requestUpdateModel :: UpdateModel -> TestTree
requestUpdateModel = req
    "UpdateModel"
    "fixture/UpdateModel.yaml"

requestCreateRouteResponse :: CreateRouteResponse -> TestTree
requestCreateRouteResponse = req
    "CreateRouteResponse"
    "fixture/CreateRouteResponse.yaml"

requestGetStages :: GetStages -> TestTree
requestGetStages = req
    "GetStages"
    "fixture/GetStages.yaml"

requestGetModel :: GetModel -> TestTree
requestGetModel = req
    "GetModel"
    "fixture/GetModel.yaml"

requestGetAPIMappings :: GetAPIMappings -> TestTree
requestGetAPIMappings = req
    "GetAPIMappings"
    "fixture/GetAPIMappings.yaml"

requestCreateIntegrationResponse :: CreateIntegrationResponse -> TestTree
requestCreateIntegrationResponse = req
    "CreateIntegrationResponse"
    "fixture/CreateIntegrationResponse.yaml"

requestGetDomainName :: GetDomainName -> TestTree
requestGetDomainName = req
    "GetDomainName"
    "fixture/GetDomainName.yaml"

requestGetAuthorizers :: GetAuthorizers -> TestTree
requestGetAuthorizers = req
    "GetAuthorizers"
    "fixture/GetAuthorizers.yaml"

requestGetRouteResponse :: GetRouteResponse -> TestTree
requestGetRouteResponse = req
    "GetRouteResponse"
    "fixture/GetRouteResponse.yaml"

requestGetRoutes :: GetRoutes -> TestTree
requestGetRoutes = req
    "GetRoutes"
    "fixture/GetRoutes.yaml"

requestDeleteIntegrationResponse :: DeleteIntegrationResponse -> TestTree
requestDeleteIntegrationResponse = req
    "DeleteIntegrationResponse"
    "fixture/DeleteIntegrationResponse.yaml"

requestUpdateIntegrationResponse :: UpdateIntegrationResponse -> TestTree
requestUpdateIntegrationResponse = req
    "UpdateIntegrationResponse"
    "fixture/UpdateIntegrationResponse.yaml"

requestDeleteIntegration :: DeleteIntegration -> TestTree
requestDeleteIntegration = req
    "DeleteIntegration"
    "fixture/DeleteIntegration.yaml"

requestUpdateIntegration :: UpdateIntegration -> TestTree
requestUpdateIntegration = req
    "UpdateIntegration"
    "fixture/UpdateIntegration.yaml"

requestGetRoute :: GetRoute -> TestTree
requestGetRoute = req
    "GetRoute"
    "fixture/GetRoute.yaml"

requestGetAuthorizer :: GetAuthorizer -> TestTree
requestGetAuthorizer = req
    "GetAuthorizer"
    "fixture/GetAuthorizer.yaml"

requestGetStage :: GetStage -> TestTree
requestGetStage = req
    "GetStage"
    "fixture/GetStage.yaml"

requestGetAPIMapping :: GetAPIMapping -> TestTree
requestGetAPIMapping = req
    "GetAPIMapping"
    "fixture/GetAPIMapping.yaml"

requestGetAPIs :: GetAPIs -> TestTree
requestGetAPIs = req
    "GetAPIs"
    "fixture/GetAPIs.yaml"

requestUpdateAPIMapping :: UpdateAPIMapping -> TestTree
requestUpdateAPIMapping = req
    "UpdateAPIMapping"
    "fixture/UpdateAPIMapping.yaml"

requestDeleteAPIMapping :: DeleteAPIMapping -> TestTree
requestDeleteAPIMapping = req
    "DeleteAPIMapping"
    "fixture/DeleteAPIMapping.yaml"

requestCreateRoute :: CreateRoute -> TestTree
requestCreateRoute = req
    "CreateRoute"
    "fixture/CreateRoute.yaml"

requestCreateAuthorizer :: CreateAuthorizer -> TestTree
requestCreateAuthorizer = req
    "CreateAuthorizer"
    "fixture/CreateAuthorizer.yaml"

requestUpdateAuthorizer :: UpdateAuthorizer -> TestTree
requestUpdateAuthorizer = req
    "UpdateAuthorizer"
    "fixture/UpdateAuthorizer.yaml"

requestDeleteAuthorizer :: DeleteAuthorizer -> TestTree
requestDeleteAuthorizer = req
    "DeleteAuthorizer"
    "fixture/DeleteAuthorizer.yaml"

requestCreateAPIMapping :: CreateAPIMapping -> TestTree
requestCreateAPIMapping = req
    "CreateAPIMapping"
    "fixture/CreateAPIMapping.yaml"

requestCreateStage :: CreateStage -> TestTree
requestCreateStage = req
    "CreateStage"
    "fixture/CreateStage.yaml"

requestGetIntegrations :: GetIntegrations -> TestTree
requestGetIntegrations = req
    "GetIntegrations"
    "fixture/GetIntegrations.yaml"

requestUpdateDomainName :: UpdateDomainName -> TestTree
requestUpdateDomainName = req
    "UpdateDomainName"
    "fixture/UpdateDomainName.yaml"

requestDeleteDomainName :: DeleteDomainName -> TestTree
requestDeleteDomainName = req
    "DeleteDomainName"
    "fixture/DeleteDomainName.yaml"

requestGetAPI :: GetAPI -> TestTree
requestGetAPI = req
    "GetAPI"
    "fixture/GetAPI.yaml"

requestDeleteAPI :: DeleteAPI -> TestTree
requestDeleteAPI = req
    "DeleteAPI"
    "fixture/DeleteAPI.yaml"

requestUpdateAPI :: UpdateAPI -> TestTree
requestUpdateAPI = req
    "UpdateAPI"
    "fixture/UpdateAPI.yaml"

requestGetModelTemplate :: GetModelTemplate -> TestTree
requestGetModelTemplate = req
    "GetModelTemplate"
    "fixture/GetModelTemplate.yaml"

-- Responses

responseCreateAPI :: CreateAPIResponse -> TestTree
responseCreateAPI = res
    "CreateAPIResponse"
    "fixture/CreateAPIResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy CreateAPI)

responseGetDeployments :: GetDeploymentsResponse -> TestTree
responseGetDeployments = res
    "GetDeploymentsResponse"
    "fixture/GetDeploymentsResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy GetDeployments)

responseGetRouteResponses :: GetRouteResponsesResponse -> TestTree
responseGetRouteResponses = res
    "GetRouteResponsesResponse"
    "fixture/GetRouteResponsesResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy GetRouteResponses)

responseGetDeployment :: GetDeploymentResponse -> TestTree
responseGetDeployment = res
    "GetDeploymentResponse"
    "fixture/GetDeploymentResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy GetDeployment)

responseGetDomainNames :: GetDomainNamesResponse -> TestTree
responseGetDomainNames = res
    "GetDomainNamesResponse"
    "fixture/GetDomainNamesResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy GetDomainNames)

responseGetModels :: GetModelsResponse -> TestTree
responseGetModels = res
    "GetModelsResponse"
    "fixture/GetModelsResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy GetModels)

responseCreateIntegration :: CreateIntegrationResponse' -> TestTree
responseCreateIntegration = res
    "CreateIntegrationResponse"
    "fixture/CreateIntegrationResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy CreateIntegration)

responseDeleteStage :: DeleteStageResponse -> TestTree
responseDeleteStage = res
    "DeleteStageResponse"
    "fixture/DeleteStageResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy DeleteStage)

responseUpdateStage :: UpdateStageResponse -> TestTree
responseUpdateStage = res
    "UpdateStageResponse"
    "fixture/UpdateStageResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy UpdateStage)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment = res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy CreateDeployment)

responseDeleteRoute :: DeleteRouteResponse' -> TestTree
responseDeleteRoute = res
    "DeleteRouteResponse"
    "fixture/DeleteRouteResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy DeleteRoute)

responseUpdateRoute :: UpdateRouteResponse' -> TestTree
responseUpdateRoute = res
    "UpdateRouteResponse"
    "fixture/UpdateRouteResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy UpdateRoute)

responseGetIntegrationResponses :: GetIntegrationResponsesResponse -> TestTree
responseGetIntegrationResponses = res
    "GetIntegrationResponsesResponse"
    "fixture/GetIntegrationResponsesResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy GetIntegrationResponses)

responseGetIntegration :: GetIntegrationResponse' -> TestTree
responseGetIntegration = res
    "GetIntegrationResponse"
    "fixture/GetIntegrationResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy GetIntegration)

responseDeleteDeployment :: DeleteDeploymentResponse -> TestTree
responseDeleteDeployment = res
    "DeleteDeploymentResponse"
    "fixture/DeleteDeploymentResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy DeleteDeployment)

responseUpdateDeployment :: UpdateDeploymentResponse -> TestTree
responseUpdateDeployment = res
    "UpdateDeploymentResponse"
    "fixture/UpdateDeploymentResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy UpdateDeployment)

responseDeleteRouteResponse :: DeleteRouteResponseResponse -> TestTree
responseDeleteRouteResponse = res
    "DeleteRouteResponseResponse"
    "fixture/DeleteRouteResponseResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy DeleteRouteResponse)

responseUpdateRouteResponse :: UpdateRouteResponseResponse -> TestTree
responseUpdateRouteResponse = res
    "UpdateRouteResponseResponse"
    "fixture/UpdateRouteResponseResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy UpdateRouteResponse)

responseCreateModel :: CreateModelResponse -> TestTree
responseCreateModel = res
    "CreateModelResponse"
    "fixture/CreateModelResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy CreateModel)

responseGetIntegrationResponse :: GetIntegrationResponseResponse -> TestTree
responseGetIntegrationResponse = res
    "GetIntegrationResponseResponse"
    "fixture/GetIntegrationResponseResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy GetIntegrationResponse)

responseCreateDomainName :: CreateDomainNameResponse -> TestTree
responseCreateDomainName = res
    "CreateDomainNameResponse"
    "fixture/CreateDomainNameResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy CreateDomainName)

responseDeleteModel :: DeleteModelResponse -> TestTree
responseDeleteModel = res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy DeleteModel)

responseUpdateModel :: UpdateModelResponse -> TestTree
responseUpdateModel = res
    "UpdateModelResponse"
    "fixture/UpdateModelResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy UpdateModel)

responseCreateRouteResponse :: CreateRouteResponseResponse -> TestTree
responseCreateRouteResponse = res
    "CreateRouteResponseResponse"
    "fixture/CreateRouteResponseResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy CreateRouteResponse)

responseGetStages :: GetStagesResponse -> TestTree
responseGetStages = res
    "GetStagesResponse"
    "fixture/GetStagesResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy GetStages)

responseGetModel :: GetModelResponse -> TestTree
responseGetModel = res
    "GetModelResponse"
    "fixture/GetModelResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy GetModel)

responseGetAPIMappings :: GetAPIMappingsResponse -> TestTree
responseGetAPIMappings = res
    "GetAPIMappingsResponse"
    "fixture/GetAPIMappingsResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy GetAPIMappings)

responseCreateIntegrationResponse :: CreateIntegrationResponseResponse -> TestTree
responseCreateIntegrationResponse = res
    "CreateIntegrationResponseResponse"
    "fixture/CreateIntegrationResponseResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy CreateIntegrationResponse)

responseGetDomainName :: GetDomainNameResponse -> TestTree
responseGetDomainName = res
    "GetDomainNameResponse"
    "fixture/GetDomainNameResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy GetDomainName)

responseGetAuthorizers :: GetAuthorizersResponse -> TestTree
responseGetAuthorizers = res
    "GetAuthorizersResponse"
    "fixture/GetAuthorizersResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy GetAuthorizers)

responseGetRouteResponse :: GetRouteResponseResponse -> TestTree
responseGetRouteResponse = res
    "GetRouteResponseResponse"
    "fixture/GetRouteResponseResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy GetRouteResponse)

responseGetRoutes :: GetRoutesResponse -> TestTree
responseGetRoutes = res
    "GetRoutesResponse"
    "fixture/GetRoutesResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy GetRoutes)

responseDeleteIntegrationResponse :: DeleteIntegrationResponseResponse -> TestTree
responseDeleteIntegrationResponse = res
    "DeleteIntegrationResponseResponse"
    "fixture/DeleteIntegrationResponseResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy DeleteIntegrationResponse)

responseUpdateIntegrationResponse :: UpdateIntegrationResponseResponse -> TestTree
responseUpdateIntegrationResponse = res
    "UpdateIntegrationResponseResponse"
    "fixture/UpdateIntegrationResponseResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy UpdateIntegrationResponse)

responseDeleteIntegration :: DeleteIntegrationResponse' -> TestTree
responseDeleteIntegration = res
    "DeleteIntegrationResponse"
    "fixture/DeleteIntegrationResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy DeleteIntegration)

responseUpdateIntegration :: UpdateIntegrationResponse' -> TestTree
responseUpdateIntegration = res
    "UpdateIntegrationResponse"
    "fixture/UpdateIntegrationResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy UpdateIntegration)

responseGetRoute :: GetRouteResponse' -> TestTree
responseGetRoute = res
    "GetRouteResponse"
    "fixture/GetRouteResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy GetRoute)

responseGetAuthorizer :: GetAuthorizerResponse -> TestTree
responseGetAuthorizer = res
    "GetAuthorizerResponse"
    "fixture/GetAuthorizerResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy GetAuthorizer)

responseGetStage :: GetStageResponse -> TestTree
responseGetStage = res
    "GetStageResponse"
    "fixture/GetStageResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy GetStage)

responseGetAPIMapping :: GetAPIMappingResponse -> TestTree
responseGetAPIMapping = res
    "GetAPIMappingResponse"
    "fixture/GetAPIMappingResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy GetAPIMapping)

responseGetAPIs :: GetAPIsResponse -> TestTree
responseGetAPIs = res
    "GetAPIsResponse"
    "fixture/GetAPIsResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy GetAPIs)

responseUpdateAPIMapping :: UpdateAPIMappingResponse -> TestTree
responseUpdateAPIMapping = res
    "UpdateAPIMappingResponse"
    "fixture/UpdateAPIMappingResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy UpdateAPIMapping)

responseDeleteAPIMapping :: DeleteAPIMappingResponse -> TestTree
responseDeleteAPIMapping = res
    "DeleteAPIMappingResponse"
    "fixture/DeleteAPIMappingResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy DeleteAPIMapping)

responseCreateRoute :: CreateRouteResponse' -> TestTree
responseCreateRoute = res
    "CreateRouteResponse"
    "fixture/CreateRouteResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy CreateRoute)

responseCreateAuthorizer :: CreateAuthorizerResponse -> TestTree
responseCreateAuthorizer = res
    "CreateAuthorizerResponse"
    "fixture/CreateAuthorizerResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy CreateAuthorizer)

responseUpdateAuthorizer :: UpdateAuthorizerResponse -> TestTree
responseUpdateAuthorizer = res
    "UpdateAuthorizerResponse"
    "fixture/UpdateAuthorizerResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy UpdateAuthorizer)

responseDeleteAuthorizer :: DeleteAuthorizerResponse -> TestTree
responseDeleteAuthorizer = res
    "DeleteAuthorizerResponse"
    "fixture/DeleteAuthorizerResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy DeleteAuthorizer)

responseCreateAPIMapping :: CreateAPIMappingResponse -> TestTree
responseCreateAPIMapping = res
    "CreateAPIMappingResponse"
    "fixture/CreateAPIMappingResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy CreateAPIMapping)

responseCreateStage :: CreateStageResponse -> TestTree
responseCreateStage = res
    "CreateStageResponse"
    "fixture/CreateStageResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy CreateStage)

responseGetIntegrations :: GetIntegrationsResponse -> TestTree
responseGetIntegrations = res
    "GetIntegrationsResponse"
    "fixture/GetIntegrationsResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy GetIntegrations)

responseUpdateDomainName :: UpdateDomainNameResponse -> TestTree
responseUpdateDomainName = res
    "UpdateDomainNameResponse"
    "fixture/UpdateDomainNameResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy UpdateDomainName)

responseDeleteDomainName :: DeleteDomainNameResponse -> TestTree
responseDeleteDomainName = res
    "DeleteDomainNameResponse"
    "fixture/DeleteDomainNameResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy DeleteDomainName)

responseGetAPI :: GetAPIResponse -> TestTree
responseGetAPI = res
    "GetAPIResponse"
    "fixture/GetAPIResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy GetAPI)

responseDeleteAPI :: DeleteAPIResponse -> TestTree
responseDeleteAPI = res
    "DeleteAPIResponse"
    "fixture/DeleteAPIResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy DeleteAPI)

responseUpdateAPI :: UpdateAPIResponse -> TestTree
responseUpdateAPI = res
    "UpdateAPIResponse"
    "fixture/UpdateAPIResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy UpdateAPI)

responseGetModelTemplate :: GetModelTemplateResponse -> TestTree
responseGetModelTemplate = res
    "GetModelTemplateResponse"
    "fixture/GetModelTemplateResponse.proto"
    apiGatewayV2
    (Proxy :: Proxy GetModelTemplate)
