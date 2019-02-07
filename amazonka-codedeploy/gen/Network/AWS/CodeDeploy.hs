{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS CodeDeploy__ 
--
-- AWS CodeDeploy is a deployment service that automates application deployments to Amazon EC2 instances, on-premises instances running in your own facility, serverless AWS Lambda functions, or applications in an Amazon ECS service.
--
-- You can deploy a nearly unlimited variety of application content, such as an updated Lambda function, updated applications in an Amazon ECS service, code, web and configuration files, executables, packages, scripts, multimedia files, and so on. AWS CodeDeploy can deploy application content stored in Amazon S3 buckets, GitHub repositories, or Bitbucket repositories. You do not need to make changes to your existing code before you can use AWS CodeDeploy.
--
-- AWS CodeDeploy makes it easier for you to rapidly release new features, helps you avoid downtime during application deployment, and handles the complexity of updating your applications, without many of the risks associated with error-prone manual deployments.
--
-- __AWS CodeDeploy Components__ 
--
-- Use the information in this guide to help you work with the following AWS CodeDeploy components:
--
--     * __Application__ : A name that uniquely identifies the application you want to deploy. AWS CodeDeploy uses this name, which functions as a container, to ensure the correct combination of revision, deployment configuration, and deployment group are referenced during a deployment.
--
--     * __Deployment group__ : A set of individual instances, CodeDeploy Lambda deployment configuration settings, or an Amazon ECS service and network details. A Lambda deployment group specifies how to route traffic to a new version of a Lambda function. An Amazon ECS deployment group specifies the service created in Amazon ECS to deploy, a load balancer, and a listener to reroute production traffic to an updated containerized application. An EC2/On-premises deployment group contains individually tagged instances, Amazon EC2 instances in Amazon EC2 Auto Scaling groups, or both. All deployment groups can specify optional trigger, alarm, and rollback settings.
--
--     * __Deployment configuration__ : A set of deployment rules and deployment success and failure conditions used by AWS CodeDeploy during a deployment.
--
--     * __Deployment__ : The process and the components used when updating a Lambda function, a containerized application in an Amazon ECS service, or of installing content on one or more instances. 
--
--     * __Application revisions__ : For an AWS Lambda deployment, this is an AppSpec file that specifies the Lambda function to be updated and one or more functions to validate deployment lifecycle events. For an Amazon ECS deployment, this is an AppSpec file that specifies the Amazon ECS task definition, container, and port where production traffic is rerouted. For an EC2/On-premises deployment, this is an archive file that contains source content—source code, webpages, executable files, and deployment scripts—along with an AppSpec file. Revisions are stored in Amazon S3 buckets or GitHub repositories. For Amazon S3, a revision is uniquely identified by its Amazon S3 object key and its ETag, version, or both. For GitHub, a revision is uniquely identified by its commit ID.
--
--
--
-- This guide also contains information to help you get details about the instances in your deployments, to make on-premises instances available for AWS CodeDeploy deployments, to get details about a Lambda function deployment, and to get details about Amazon ECS service deployments.
--
-- __AWS CodeDeploy Information Resources__ 
--
--     * <http://docs.aws.amazon.com/codedeploy/latest/userguide AWS CodeDeploy User Guide> 
--
--     * <http://docs.aws.amazon.com/codedeploy/latest/APIReference/ AWS CodeDeploy API Reference Guide> 
--
--     * <http://docs.aws.amazon.com/cli/latest/reference/deploy/index.html AWS CLI Reference for AWS CodeDeploy> 
--
--     * <https://forums.aws.amazon.com/forum.jspa?forumID=179 AWS CodeDeploy Developer Forum> 
--
--
--
module Network.AWS.CodeDeploy
    (
    -- * Service Configuration
      codeDeploy

    -- * Errors
    -- $errors

    -- ** LifecycleHookLimitExceededException
    , _LifecycleHookLimitExceededException

    -- ** InvalidTimeRangeException
    , _InvalidTimeRangeException

    -- ** InvalidComputePlatformException
    , _InvalidComputePlatformException

    -- ** InvalidTagException
    , _InvalidTagException

    -- ** InvalidFileExistsBehaviorException
    , _InvalidFileExistsBehaviorException

    -- ** InvalidAlarmConfigException
    , _InvalidAlarmConfigException

    -- ** InstanceNameAlreadyRegisteredException
    , _InstanceNameAlreadyRegisteredException

    -- ** IAMUserARNRequiredException
    , _IAMUserARNRequiredException

    -- ** InvalidDeploymentGroupNameException
    , _InvalidDeploymentGroupNameException

    -- ** InvalidInstanceTypeException
    , _InvalidInstanceTypeException

    -- ** IAMSessionARNAlreadyRegisteredException
    , _IAMSessionARNAlreadyRegisteredException

    -- ** InvalidTrafficRoutingConfigurationException
    , _InvalidTrafficRoutingConfigurationException

    -- ** DescriptionTooLongException
    , _DescriptionTooLongException

    -- ** InvalidIAMUserARNException
    , _InvalidIAMUserARNException

    -- ** InvalidOnPremisesTagCombinationException
    , _InvalidOnPremisesTagCombinationException

    -- ** DeploymentNotStartedException
    , _DeploymentNotStartedException

    -- ** DeploymentConfigLimitExceededException
    , _DeploymentConfigLimitExceededException

    -- ** RoleRequiredException
    , _RoleRequiredException

    -- ** InvalidLoadBalancerInfoException
    , _InvalidLoadBalancerInfoException

    -- ** InvalidBlueGreenDeploymentConfigurationException
    , _InvalidBlueGreenDeploymentConfigurationException

    -- ** InvalidRoleException
    , _InvalidRoleException

    -- ** DeploymentConfigAlreadyExistsException
    , _DeploymentConfigAlreadyExistsException

    -- ** InvalidTargetInstancesException
    , _InvalidTargetInstancesException

    -- ** DeploymentLimitExceededException
    , _DeploymentLimitExceededException

    -- ** IAMUserARNAlreadyRegisteredException
    , _IAMUserARNAlreadyRegisteredException

    -- ** InvalidIAMSessionARNException
    , _InvalidIAMSessionARNException

    -- ** InstanceLimitExceededException
    , _InstanceLimitExceededException

    -- ** InvalidLifecycleEventHookExecutionIdException
    , _InvalidLifecycleEventHookExecutionIdException

    -- ** InvalidDeploymentStyleException
    , _InvalidDeploymentStyleException

    -- ** InvalidTargetFilterNameException
    , _InvalidTargetFilterNameException

    -- ** DeploymentTargetListSizeExceededException
    , _DeploymentTargetListSizeExceededException

    -- ** InvalidDeployedStateFilterException
    , _InvalidDeployedStateFilterException

    -- ** InvalidAutoScalingGroupException
    , _InvalidAutoScalingGroupException

    -- ** InvalidApplicationNameException
    , _InvalidApplicationNameException

    -- ** GitHubAccountTokenDoesNotExistException
    , _GitHubAccountTokenDoesNotExistException

    -- ** ApplicationDoesNotExistException
    , _ApplicationDoesNotExistException

    -- ** InvalidMinimumHealthyHostValueException
    , _InvalidMinimumHealthyHostValueException

    -- ** UnsupportedActionForDeploymentTypeException
    , _UnsupportedActionForDeploymentTypeException

    -- ** ResourceValidationException
    , _ResourceValidationException

    -- ** InvalidGitHubAccountTokenException
    , _InvalidGitHubAccountTokenException

    -- ** InvalidEC2TagCombinationException
    , _InvalidEC2TagCombinationException

    -- ** InvalidLifecycleEventHookExecutionStatusException
    , _InvalidLifecycleEventHookExecutionStatusException

    -- ** AlarmsLimitExceededException
    , _AlarmsLimitExceededException

    -- ** OperationNotSupportedException
    , _OperationNotSupportedException

    -- ** DeploymentTargetDoesNotExistException
    , _DeploymentTargetDoesNotExistException

    -- ** InvalidTagFilterException
    , _InvalidTagFilterException

    -- ** InvalidTriggerConfigException
    , _InvalidTriggerConfigException

    -- ** InvalidDeploymentWaitTypeException
    , _InvalidDeploymentWaitTypeException

    -- ** InvalidIgnoreApplicationStopFailuresValueException
    , _InvalidIgnoreApplicationStopFailuresValueException

    -- ** InvalidUpdateOutdatedInstancesOnlyValueException
    , _InvalidUpdateOutdatedInstancesOnlyValueException

    -- ** TagRequiredException
    , _TagRequiredException

    -- ** DeploymentGroupNameRequiredException
    , _DeploymentGroupNameRequiredException

    -- ** BucketNameFilterRequiredException
    , _BucketNameFilterRequiredException

    -- ** DeploymentConfigDoesNotExistException
    , _DeploymentConfigDoesNotExistException

    -- ** InvalidBucketNameFilterException
    , _InvalidBucketNameFilterException

    -- ** DeploymentGroupAlreadyExistsException
    , _DeploymentGroupAlreadyExistsException

    -- ** InvalidSortByException
    , _InvalidSortByException

    -- ** RevisionDoesNotExistException
    , _RevisionDoesNotExistException

    -- ** InvalidTargetException
    , _InvalidTargetException

    -- ** DeploymentGroupLimitExceededException
    , _DeploymentGroupLimitExceededException

    -- ** DeploymentGroupDoesNotExistException
    , _DeploymentGroupDoesNotExistException

    -- ** ThrottlingException
    , _ThrottlingException

    -- ** InvalidDeploymentConfigNameException
    , _InvalidDeploymentConfigNameException

    -- ** DeploymentConfigNameRequiredException
    , _DeploymentConfigNameRequiredException

    -- ** DeploymentIdRequiredException
    , _DeploymentIdRequiredException

    -- ** InvalidInstanceIdException
    , _InvalidInstanceIdException

    -- ** DeploymentIsNotInReadyStateException
    , _DeploymentIsNotInReadyStateException

    -- ** InvalidTargetGroupPairException
    , _InvalidTargetGroupPairException

    -- ** InvalidNextTokenException
    , _InvalidNextTokenException

    -- ** InstanceIdRequiredException
    , _InstanceIdRequiredException

    -- ** InvalidDeploymentIdException
    , _InvalidDeploymentIdException

    -- ** InvalidSortOrderException
    , _InvalidSortOrderException

    -- ** InvalidAutoRollbackConfigException
    , _InvalidAutoRollbackConfigException

    -- ** DeploymentAlreadyCompletedException
    , _DeploymentAlreadyCompletedException

    -- ** ECSServiceMappingLimitExceededException
    , _ECSServiceMappingLimitExceededException

    -- ** DeploymentDoesNotExistException
    , _DeploymentDoesNotExistException

    -- ** BatchLimitExceededException
    , _BatchLimitExceededException

    -- ** InvalidRevisionException
    , _InvalidRevisionException

    -- ** RevisionRequiredException
    , _RevisionRequiredException

    -- ** InstanceDoesNotExistException
    , _InstanceDoesNotExistException

    -- ** DeploymentConfigInUseException
    , _DeploymentConfigInUseException

    -- ** InvalidInputException
    , _InvalidInputException

    -- ** InvalidEC2TagException
    , _InvalidEC2TagException

    -- ** InvalidInstanceNameException
    , _InvalidInstanceNameException

    -- ** InstanceNameRequiredException
    , _InstanceNameRequiredException

    -- ** MultipleIAMARNsProvidedException
    , _MultipleIAMARNsProvidedException

    -- ** TriggerTargetsLimitExceededException
    , _TriggerTargetsLimitExceededException

    -- ** InvalidDeploymentStatusException
    , _InvalidDeploymentStatusException

    -- ** InvalidRegistrationStatusException
    , _InvalidRegistrationStatusException

    -- ** ApplicationNameRequiredException
    , _ApplicationNameRequiredException

    -- ** InstanceNotRegisteredException
    , _InstanceNotRegisteredException

    -- ** ApplicationAlreadyExistsException
    , _ApplicationAlreadyExistsException

    -- ** InvalidInstanceStatusException
    , _InvalidInstanceStatusException

    -- ** InvalidDeploymentTargetIdException
    , _InvalidDeploymentTargetIdException

    -- ** TagLimitExceededException
    , _TagLimitExceededException

    -- ** ApplicationLimitExceededException
    , _ApplicationLimitExceededException

    -- ** TagSetListLimitExceededException
    , _TagSetListLimitExceededException

    -- ** InvalidOperationException
    , _InvalidOperationException

    -- ** DeploymentTargetIdRequiredException
    , _DeploymentTargetIdRequiredException

    -- ** GitHubAccountTokenNameRequiredException
    , _GitHubAccountTokenNameRequiredException

    -- ** InvalidECSServiceException
    , _InvalidECSServiceException

    -- ** InvalidDeploymentInstanceTypeException
    , _InvalidDeploymentInstanceTypeException

    -- ** IAMARNRequiredException
    , _IAMARNRequiredException

    -- ** InvalidGitHubAccountTokenNameException
    , _InvalidGitHubAccountTokenNameException

    -- ** LifecycleEventAlreadyCompletedException
    , _LifecycleEventAlreadyCompletedException

    -- ** InvalidKeyPrefixFilterException
    , _InvalidKeyPrefixFilterException

    -- ** DeploymentAlreadyStartedException
    , _DeploymentAlreadyStartedException

    -- * Waiters
    -- $waiters

    -- ** DeploymentSuccessful
    , deploymentSuccessful

    -- * Operations
    -- $operations

    -- ** RemoveTagsFromOnPremisesInstances 
    , module Network.AWS.CodeDeploy.RemoveTagsFromOnPremisesInstances

    -- ** BatchGetDeploymentGroups 
    , module Network.AWS.CodeDeploy.BatchGetDeploymentGroups

    -- ** DeleteDeploymentGroup 
    , module Network.AWS.CodeDeploy.DeleteDeploymentGroup

    -- ** UpdateDeploymentGroup 
    , module Network.AWS.CodeDeploy.UpdateDeploymentGroup

    -- ** ListOnPremisesInstances (Paginated)
    , module Network.AWS.CodeDeploy.ListOnPremisesInstances

    -- ** CreateDeploymentConfig 
    , module Network.AWS.CodeDeploy.CreateDeploymentConfig

    -- ** GetApplicationRevision 
    , module Network.AWS.CodeDeploy.GetApplicationRevision

    -- ** GetDeployment 
    , module Network.AWS.CodeDeploy.GetDeployment

    -- ** DeleteDeploymentConfig 
    , module Network.AWS.CodeDeploy.DeleteDeploymentConfig

    -- ** GetDeploymentConfig 
    , module Network.AWS.CodeDeploy.GetDeploymentConfig

    -- ** CreateDeployment 
    , module Network.AWS.CodeDeploy.CreateDeployment

    -- ** BatchGetApplicationRevisions 
    , module Network.AWS.CodeDeploy.BatchGetApplicationRevisions

    -- ** BatchGetDeployments 
    , module Network.AWS.CodeDeploy.BatchGetDeployments

    -- ** GetOnPremisesInstance 
    , module Network.AWS.CodeDeploy.GetOnPremisesInstance

    -- ** RegisterApplicationRevision 
    , module Network.AWS.CodeDeploy.RegisterApplicationRevision

    -- ** ContinueDeployment 
    , module Network.AWS.CodeDeploy.ContinueDeployment

    -- ** BatchGetApplications 
    , module Network.AWS.CodeDeploy.BatchGetApplications

    -- ** DeleteApplication 
    , module Network.AWS.CodeDeploy.DeleteApplication

    -- ** UpdateApplication 
    , module Network.AWS.CodeDeploy.UpdateApplication

    -- ** DeleteGitHubAccountToken 
    , module Network.AWS.CodeDeploy.DeleteGitHubAccountToken

    -- ** DeregisterOnPremisesInstance 
    , module Network.AWS.CodeDeploy.DeregisterOnPremisesInstance

    -- ** PutLifecycleEventHookExecutionStatus 
    , module Network.AWS.CodeDeploy.PutLifecycleEventHookExecutionStatus

    -- ** GetDeploymentTarget 
    , module Network.AWS.CodeDeploy.GetDeploymentTarget

    -- ** CreateApplication 
    , module Network.AWS.CodeDeploy.CreateApplication

    -- ** BatchGetDeploymentTargets 
    , module Network.AWS.CodeDeploy.BatchGetDeploymentTargets

    -- ** StopDeployment 
    , module Network.AWS.CodeDeploy.StopDeployment

    -- ** ListGitHubAccountTokenNames (Paginated)
    , module Network.AWS.CodeDeploy.ListGitHubAccountTokenNames

    -- ** GetApplication 
    , module Network.AWS.CodeDeploy.GetApplication

    -- ** ListDeploymentGroups (Paginated)
    , module Network.AWS.CodeDeploy.ListDeploymentGroups

    -- ** BatchGetOnPremisesInstances 
    , module Network.AWS.CodeDeploy.BatchGetOnPremisesInstances

    -- ** RegisterOnPremisesInstance 
    , module Network.AWS.CodeDeploy.RegisterOnPremisesInstance

    -- ** CreateDeploymentGroup 
    , module Network.AWS.CodeDeploy.CreateDeploymentGroup

    -- ** ListDeploymentConfigs (Paginated)
    , module Network.AWS.CodeDeploy.ListDeploymentConfigs

    -- ** GetDeploymentGroup 
    , module Network.AWS.CodeDeploy.GetDeploymentGroup

    -- ** ListDeployments (Paginated)
    , module Network.AWS.CodeDeploy.ListDeployments

    -- ** ListApplicationRevisions (Paginated)
    , module Network.AWS.CodeDeploy.ListApplicationRevisions

    -- ** ListApplications (Paginated)
    , module Network.AWS.CodeDeploy.ListApplications

    -- ** AddTagsToOnPremisesInstances 
    , module Network.AWS.CodeDeploy.AddTagsToOnPremisesInstances

    -- ** ListDeploymentTargets (Paginated)
    , module Network.AWS.CodeDeploy.ListDeploymentTargets

    -- * Types

    -- ** ApplicationRevisionSortBy
    , ApplicationRevisionSortBy (..)

    -- ** AutoRollbackEvent
    , AutoRollbackEvent (..)

    -- ** BundleType
    , BundleType (..)

    -- ** ComputePlatform
    , ComputePlatform (..)

    -- ** DeployErrorCode
    , DeployErrorCode (..)

    -- ** DeploymentCreator
    , DeploymentCreator (..)

    -- ** DeploymentOption
    , DeploymentOption (..)

    -- ** DeploymentReadyAction
    , DeploymentReadyAction (..)

    -- ** DeploymentStatus
    , DeploymentStatus (..)

    -- ** DeploymentTargetType
    , DeploymentTargetType (..)

    -- ** DeploymentType
    , DeploymentType (..)

    -- ** DeploymentWaitType
    , DeploymentWaitType (..)

    -- ** EC2TagFilterType
    , EC2TagFilterType (..)

    -- ** FileExistsBehavior
    , FileExistsBehavior (..)

    -- ** GreenFleetProvisioningAction
    , GreenFleetProvisioningAction (..)

    -- ** InstanceAction
    , InstanceAction (..)

    -- ** LifecycleErrorCode
    , LifecycleErrorCode (..)

    -- ** LifecycleEventStatus
    , LifecycleEventStatus (..)

    -- ** ListStateFilterAction
    , ListStateFilterAction (..)

    -- ** MinimumHealthyHostsType
    , MinimumHealthyHostsType (..)

    -- ** RegistrationStatus
    , RegistrationStatus (..)

    -- ** RevisionLocationType
    , RevisionLocationType (..)

    -- ** SortOrder
    , SortOrder (..)

    -- ** StopStatus
    , StopStatus (..)

    -- ** TagFilterType
    , TagFilterType (..)

    -- ** TargetFilterName
    , TargetFilterName (..)

    -- ** TargetLabel
    , TargetLabel (..)

    -- ** TargetStatus
    , TargetStatus (..)

    -- ** TrafficRoutingType
    , TrafficRoutingType (..)

    -- ** TriggerEventType
    , TriggerEventType (..)

    -- ** Alarm
    , Alarm
    , alarm
    , aName

    -- ** AlarmConfiguration
    , AlarmConfiguration
    , alarmConfiguration
    , acIgnorePollAlarmFailure
    , acEnabled
    , acAlarms

    -- ** AppSpecContent
    , AppSpecContent
    , appSpecContent
    , ascContent
    , ascSha256

    -- ** ApplicationInfo
    , ApplicationInfo
    , applicationInfo
    , aiLinkedToGitHub
    , aiComputePlatform
    , aiApplicationId
    , aiApplicationName
    , aiGitHubAccountName
    , aiCreateTime

    -- ** AutoRollbackConfiguration
    , AutoRollbackConfiguration
    , autoRollbackConfiguration
    , arcEnabled
    , arcEvents

    -- ** AutoScalingGroup
    , AutoScalingGroup
    , autoScalingGroup
    , asgHook
    , asgName

    -- ** BlueGreenDeploymentConfiguration
    , BlueGreenDeploymentConfiguration
    , blueGreenDeploymentConfiguration
    , bgdcDeploymentReadyOption
    , bgdcGreenFleetProvisioningOption
    , bgdcTerminateBlueInstancesOnDeploymentSuccess

    -- ** BlueInstanceTerminationOption
    , BlueInstanceTerminationOption
    , blueInstanceTerminationOption
    , bitoAction
    , bitoTerminationWaitTimeInMinutes

    -- ** DeploymentConfigInfo
    , DeploymentConfigInfo
    , deploymentConfigInfo
    , dciDeploymentConfigName
    , dciComputePlatform
    , dciMinimumHealthyHosts
    , dciTrafficRoutingConfig
    , dciDeploymentConfigId
    , dciCreateTime

    -- ** DeploymentGroupInfo
    , DeploymentGroupInfo
    , deploymentGroupInfo
    , dgiServiceRoleARN
    , dgiEc2TagSet
    , dgiDeploymentConfigName
    , dgiLastAttemptedDeployment
    , dgiOnPremisesTagSet
    , dgiComputePlatform
    , dgiTargetRevision
    , dgiEc2TagFilters
    , dgiEcsServices
    , dgiBlueGreenDeploymentConfiguration
    , dgiLoadBalancerInfo
    , dgiOnPremisesInstanceTagFilters
    , dgiLastSuccessfulDeployment
    , dgiApplicationName
    , dgiAlarmConfiguration
    , dgiTriggerConfigurations
    , dgiDeploymentGroupId
    , dgiAutoScalingGroups
    , dgiDeploymentStyle
    , dgiAutoRollbackConfiguration
    , dgiDeploymentGroupName

    -- ** DeploymentInfo
    , DeploymentInfo
    , deploymentInfo
    , diCreator
    , diStatus
    , diDeploymentId
    , diDeploymentConfigName
    , diComputePlatform
    , diPreviousRevision
    , diInstanceTerminationWaitTimeStarted
    , diDeploymentStatusMessages
    , diStartTime
    , diCompleteTime
    , diBlueGreenDeploymentConfiguration
    , diErrorInformation
    , diLoadBalancerInfo
    , diAdditionalDeploymentStatusInfo
    , diDeploymentOverview
    , diFileExistsBehavior
    , diApplicationName
    , diRollbackInfo
    , diTargetInstances
    , diRevision
    , diDescription
    , diDeploymentStyle
    , diCreateTime
    , diAutoRollbackConfiguration
    , diUpdateOutdatedInstancesOnly
    , diDeploymentGroupName
    , diIgnoreApplicationStopFailures

    -- ** DeploymentOverview
    , DeploymentOverview
    , deploymentOverview
    , doPending
    , doSkipped
    , doInProgress
    , doSucceeded
    , doReady
    , doFailed

    -- ** DeploymentReadyOption
    , DeploymentReadyOption
    , deploymentReadyOption
    , droActionOnTimeout
    , droWaitTimeInMinutes

    -- ** DeploymentStyle
    , DeploymentStyle
    , deploymentStyle
    , dsDeploymentOption
    , dsDeploymentType

    -- ** DeploymentTarget
    , DeploymentTarget
    , deploymentTarget
    , dtInstanceTarget
    , dtEcsTarget
    , dtDeploymentTargetType
    , dtLambdaTarget

    -- ** Diagnostics
    , Diagnostics
    , diagnostics
    , dLogTail
    , dErrorCode
    , dScriptName
    , dMessage

    -- ** EC2TagFilter
    , EC2TagFilter
    , ec2TagFilter
    , etfValue
    , etfKey
    , etfType

    -- ** EC2TagSet
    , EC2TagSet
    , ec2TagSet
    , etsEc2TagSetList

    -- ** ECSService
    , ECSService
    , eCSService
    , ecssServiceName
    , ecssClusterName

    -- ** ECSTarget
    , ECSTarget
    , eCSTarget
    , ecstTargetARN
    , ecstTargetId
    , ecstStatus
    , ecstDeploymentId
    , ecstLastUpdatedAt
    , ecstTaskSetsInfo
    , ecstLifecycleEvents

    -- ** ECSTaskSet
    , ECSTaskSet
    , eCSTaskSet
    , ecstsRunningCount
    , ecstsStatus
    , ecstsIdentifer
    , ecstsDesiredCount
    , ecstsPendingCount
    , ecstsTrafficWeight
    , ecstsTargetGroup
    , ecstsTaskSetLabel

    -- ** ELBInfo
    , ELBInfo
    , eLBInfo
    , elbiName

    -- ** ErrorInformation
    , ErrorInformation
    , errorInformation
    , eiCode
    , eiMessage

    -- ** GenericRevisionInfo
    , GenericRevisionInfo
    , genericRevisionInfo
    , griRegisterTime
    , griFirstUsedTime
    , griDeploymentGroups
    , griLastUsedTime
    , griDescription

    -- ** GitHubLocation
    , GitHubLocation
    , gitHubLocation
    , ghlCommitId
    , ghlRepository

    -- ** GreenFleetProvisioningOption
    , GreenFleetProvisioningOption
    , greenFleetProvisioningOption
    , gfpoAction

    -- ** InstanceInfo
    , InstanceInfo
    , instanceInfo
    , iiRegisterTime
    , iiInstanceARN
    , iiDeregisterTime
    , iiIamUserARN
    , iiInstanceName
    , iiIamSessionARN
    , iiTags

    -- ** InstanceTarget
    , InstanceTarget
    , instanceTarget
    , itTargetARN
    , itTargetId
    , itStatus
    , itDeploymentId
    , itInstanceLabel
    , itLastUpdatedAt
    , itLifecycleEvents

    -- ** LambdaTarget
    , LambdaTarget
    , lambdaTarget
    , ltTargetARN
    , ltTargetId
    , ltStatus
    , ltDeploymentId
    , ltLastUpdatedAt
    , ltLifecycleEvents

    -- ** LastDeploymentInfo
    , LastDeploymentInfo
    , lastDeploymentInfo
    , ldiStatus
    , ldiDeploymentId
    , ldiEndTime
    , ldiCreateTime

    -- ** LifecycleEvent
    , LifecycleEvent
    , lifecycleEvent
    , leStatus
    , leLifecycleEventName
    , leStartTime
    , leDiagnostics
    , leEndTime

    -- ** LoadBalancerInfo
    , LoadBalancerInfo
    , loadBalancerInfo
    , lbiElbInfoList
    , lbiTargetGroupInfoList
    , lbiTargetGroupPairInfoList

    -- ** MinimumHealthyHosts
    , MinimumHealthyHosts
    , minimumHealthyHosts
    , mhhValue
    , mhhType

    -- ** OnPremisesTagSet
    , OnPremisesTagSet
    , onPremisesTagSet
    , optsOnPremisesTagSetList

    -- ** RawString
    , RawString
    , rawString
    , rsContent
    , rsSha256

    -- ** RevisionInfo
    , RevisionInfo
    , revisionInfo
    , riGenericRevisionInfo
    , riRevisionLocation

    -- ** RevisionLocation
    , RevisionLocation
    , revisionLocation
    , rlString
    , rlRevisionType
    , rlS3Location
    , rlAppSpecContent
    , rlGitHubLocation

    -- ** RollbackInfo
    , RollbackInfo
    , rollbackInfo
    , riRollbackTriggeringDeploymentId
    , riRollbackMessage
    , riRollbackDeploymentId

    -- ** S3Location
    , S3Location
    , s3Location
    , slBundleType
    , slETag
    , slBucket
    , slKey
    , slVersion

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** TagFilter
    , TagFilter
    , tagFilter
    , tfValue
    , tfKey
    , tfType

    -- ** TargetGroupInfo
    , TargetGroupInfo
    , targetGroupInfo
    , tgiName

    -- ** TargetGroupPairInfo
    , TargetGroupPairInfo
    , targetGroupPairInfo
    , tgpiProdTrafficRoute
    , tgpiTestTrafficRoute
    , tgpiTargetGroups

    -- ** TargetInstances
    , TargetInstances
    , targetInstances
    , tiEc2TagSet
    , tiTagFilters
    , tiAutoScalingGroups

    -- ** TimeBasedCanary
    , TimeBasedCanary
    , timeBasedCanary
    , tbcCanaryInterval
    , tbcCanaryPercentage

    -- ** TimeBasedLinear
    , TimeBasedLinear
    , timeBasedLinear
    , tblLinearInterval
    , tblLinearPercentage

    -- ** TimeRange
    , TimeRange
    , timeRange
    , trStart
    , trEnd

    -- ** TrafficRoute
    , TrafficRoute
    , trafficRoute
    , trListenerARNs

    -- ** TrafficRoutingConfig
    , TrafficRoutingConfig
    , trafficRoutingConfig
    , trcTimeBasedCanary
    , trcTimeBasedLinear
    , trcType

    -- ** TriggerConfig
    , TriggerConfig
    , triggerConfig
    , tcTriggerName
    , tcTriggerEvents
    , tcTriggerTargetARN
    ) where

import Network.AWS.CodeDeploy.AddTagsToOnPremisesInstances
import Network.AWS.CodeDeploy.BatchGetApplicationRevisions
import Network.AWS.CodeDeploy.BatchGetApplications
import Network.AWS.CodeDeploy.BatchGetDeploymentGroups
import Network.AWS.CodeDeploy.BatchGetDeploymentTargets
import Network.AWS.CodeDeploy.BatchGetDeployments
import Network.AWS.CodeDeploy.BatchGetOnPremisesInstances
import Network.AWS.CodeDeploy.ContinueDeployment
import Network.AWS.CodeDeploy.CreateApplication
import Network.AWS.CodeDeploy.CreateDeployment
import Network.AWS.CodeDeploy.CreateDeploymentConfig
import Network.AWS.CodeDeploy.CreateDeploymentGroup
import Network.AWS.CodeDeploy.DeleteApplication
import Network.AWS.CodeDeploy.DeleteDeploymentConfig
import Network.AWS.CodeDeploy.DeleteDeploymentGroup
import Network.AWS.CodeDeploy.DeleteGitHubAccountToken
import Network.AWS.CodeDeploy.DeregisterOnPremisesInstance
import Network.AWS.CodeDeploy.GetApplication
import Network.AWS.CodeDeploy.GetApplicationRevision
import Network.AWS.CodeDeploy.GetDeployment
import Network.AWS.CodeDeploy.GetDeploymentConfig
import Network.AWS.CodeDeploy.GetDeploymentGroup
import Network.AWS.CodeDeploy.GetDeploymentTarget
import Network.AWS.CodeDeploy.GetOnPremisesInstance
import Network.AWS.CodeDeploy.ListApplicationRevisions
import Network.AWS.CodeDeploy.ListApplications
import Network.AWS.CodeDeploy.ListDeploymentConfigs
import Network.AWS.CodeDeploy.ListDeploymentGroups
import Network.AWS.CodeDeploy.ListDeploymentTargets
import Network.AWS.CodeDeploy.ListDeployments
import Network.AWS.CodeDeploy.ListGitHubAccountTokenNames
import Network.AWS.CodeDeploy.ListOnPremisesInstances
import Network.AWS.CodeDeploy.PutLifecycleEventHookExecutionStatus
import Network.AWS.CodeDeploy.RegisterApplicationRevision
import Network.AWS.CodeDeploy.RegisterOnPremisesInstance
import Network.AWS.CodeDeploy.RemoveTagsFromOnPremisesInstances
import Network.AWS.CodeDeploy.StopDeployment
import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.UpdateApplication
import Network.AWS.CodeDeploy.UpdateDeploymentGroup
import Network.AWS.CodeDeploy.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'CodeDeploy'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
