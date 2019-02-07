{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Chime
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Chime where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Chime
import Test.AWS.Chime.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestResetPersonalPIN $
--             resetPersonalPIN
--
--         , requestUpdateAccountSettings $
--             updateAccountSettings
--
--         , requestListUsers $
--             listUsers
--
--         , requestDeleteAccount $
--             deleteAccount
--
--         , requestUpdateAccount $
--             updateAccount
--
--         , requestListAccounts $
--             listAccounts
--
--         , requestLogoutUser $
--             logoutUser
--
--         , requestBatchUpdateUser $
--             batchUpdateUser
--
--         , requestBatchUnsuspendUser $
--             batchUnsuspendUser
--
--         , requestGetUser $
--             getUser
--
--         , requestBatchSuspendUser $
--             batchSuspendUser
--
--         , requestGetAccount $
--             getAccount
--
--         , requestGetAccountSettings $
--             getAccountSettings
--
--         , requestCreateAccount $
--             createAccount
--
--         , requestUpdateUser $
--             updateUser
--
--         , requestInviteUsers $
--             inviteUsers
--
--           ]

--     , testGroup "response"
--         [ responseResetPersonalPIN $
--             resetPersonalPINResponse
--
--         , responseUpdateAccountSettings $
--             updateAccountSettingsResponse
--
--         , responseListUsers $
--             listUsersResponse
--
--         , responseDeleteAccount $
--             deleteAccountResponse
--
--         , responseUpdateAccount $
--             updateAccountResponse
--
--         , responseListAccounts $
--             listAccountsResponse
--
--         , responseLogoutUser $
--             logoutUserResponse
--
--         , responseBatchUpdateUser $
--             batchUpdateUserResponse
--
--         , responseBatchUnsuspendUser $
--             batchUnsuspendUserResponse
--
--         , responseGetUser $
--             getUserResponse
--
--         , responseBatchSuspendUser $
--             batchSuspendUserResponse
--
--         , responseGetAccount $
--             getAccountResponse
--
--         , responseGetAccountSettings $
--             getAccountSettingsResponse
--
--         , responseCreateAccount $
--             createAccountResponse
--
--         , responseUpdateUser $
--             updateUserResponse
--
--         , responseInviteUsers $
--             inviteUsersResponse
--
--           ]
--     ]

-- Requests

requestResetPersonalPIN :: ResetPersonalPIN -> TestTree
requestResetPersonalPIN = req
    "ResetPersonalPIN"
    "fixture/ResetPersonalPIN.yaml"

requestUpdateAccountSettings :: UpdateAccountSettings -> TestTree
requestUpdateAccountSettings = req
    "UpdateAccountSettings"
    "fixture/UpdateAccountSettings.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers = req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestDeleteAccount :: DeleteAccount -> TestTree
requestDeleteAccount = req
    "DeleteAccount"
    "fixture/DeleteAccount.yaml"

requestUpdateAccount :: UpdateAccount -> TestTree
requestUpdateAccount = req
    "UpdateAccount"
    "fixture/UpdateAccount.yaml"

requestListAccounts :: ListAccounts -> TestTree
requestListAccounts = req
    "ListAccounts"
    "fixture/ListAccounts.yaml"

requestLogoutUser :: LogoutUser -> TestTree
requestLogoutUser = req
    "LogoutUser"
    "fixture/LogoutUser.yaml"

requestBatchUpdateUser :: BatchUpdateUser -> TestTree
requestBatchUpdateUser = req
    "BatchUpdateUser"
    "fixture/BatchUpdateUser.yaml"

requestBatchUnsuspendUser :: BatchUnsuspendUser -> TestTree
requestBatchUnsuspendUser = req
    "BatchUnsuspendUser"
    "fixture/BatchUnsuspendUser.yaml"

requestGetUser :: GetUser -> TestTree
requestGetUser = req
    "GetUser"
    "fixture/GetUser.yaml"

requestBatchSuspendUser :: BatchSuspendUser -> TestTree
requestBatchSuspendUser = req
    "BatchSuspendUser"
    "fixture/BatchSuspendUser.yaml"

requestGetAccount :: GetAccount -> TestTree
requestGetAccount = req
    "GetAccount"
    "fixture/GetAccount.yaml"

requestGetAccountSettings :: GetAccountSettings -> TestTree
requestGetAccountSettings = req
    "GetAccountSettings"
    "fixture/GetAccountSettings.yaml"

requestCreateAccount :: CreateAccount -> TestTree
requestCreateAccount = req
    "CreateAccount"
    "fixture/CreateAccount.yaml"

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser = req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

requestInviteUsers :: InviteUsers -> TestTree
requestInviteUsers = req
    "InviteUsers"
    "fixture/InviteUsers.yaml"

-- Responses

responseResetPersonalPIN :: ResetPersonalPINResponse -> TestTree
responseResetPersonalPIN = res
    "ResetPersonalPINResponse"
    "fixture/ResetPersonalPINResponse.proto"
    chime
    (Proxy :: Proxy ResetPersonalPIN)

responseUpdateAccountSettings :: UpdateAccountSettingsResponse -> TestTree
responseUpdateAccountSettings = res
    "UpdateAccountSettingsResponse"
    "fixture/UpdateAccountSettingsResponse.proto"
    chime
    (Proxy :: Proxy UpdateAccountSettings)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers = res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    chime
    (Proxy :: Proxy ListUsers)

responseDeleteAccount :: DeleteAccountResponse -> TestTree
responseDeleteAccount = res
    "DeleteAccountResponse"
    "fixture/DeleteAccountResponse.proto"
    chime
    (Proxy :: Proxy DeleteAccount)

responseUpdateAccount :: UpdateAccountResponse -> TestTree
responseUpdateAccount = res
    "UpdateAccountResponse"
    "fixture/UpdateAccountResponse.proto"
    chime
    (Proxy :: Proxy UpdateAccount)

responseListAccounts :: ListAccountsResponse -> TestTree
responseListAccounts = res
    "ListAccountsResponse"
    "fixture/ListAccountsResponse.proto"
    chime
    (Proxy :: Proxy ListAccounts)

responseLogoutUser :: LogoutUserResponse -> TestTree
responseLogoutUser = res
    "LogoutUserResponse"
    "fixture/LogoutUserResponse.proto"
    chime
    (Proxy :: Proxy LogoutUser)

responseBatchUpdateUser :: BatchUpdateUserResponse -> TestTree
responseBatchUpdateUser = res
    "BatchUpdateUserResponse"
    "fixture/BatchUpdateUserResponse.proto"
    chime
    (Proxy :: Proxy BatchUpdateUser)

responseBatchUnsuspendUser :: BatchUnsuspendUserResponse -> TestTree
responseBatchUnsuspendUser = res
    "BatchUnsuspendUserResponse"
    "fixture/BatchUnsuspendUserResponse.proto"
    chime
    (Proxy :: Proxy BatchUnsuspendUser)

responseGetUser :: GetUserResponse -> TestTree
responseGetUser = res
    "GetUserResponse"
    "fixture/GetUserResponse.proto"
    chime
    (Proxy :: Proxy GetUser)

responseBatchSuspendUser :: BatchSuspendUserResponse -> TestTree
responseBatchSuspendUser = res
    "BatchSuspendUserResponse"
    "fixture/BatchSuspendUserResponse.proto"
    chime
    (Proxy :: Proxy BatchSuspendUser)

responseGetAccount :: GetAccountResponse -> TestTree
responseGetAccount = res
    "GetAccountResponse"
    "fixture/GetAccountResponse.proto"
    chime
    (Proxy :: Proxy GetAccount)

responseGetAccountSettings :: GetAccountSettingsResponse -> TestTree
responseGetAccountSettings = res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    chime
    (Proxy :: Proxy GetAccountSettings)

responseCreateAccount :: CreateAccountResponse -> TestTree
responseCreateAccount = res
    "CreateAccountResponse"
    "fixture/CreateAccountResponse.proto"
    chime
    (Proxy :: Proxy CreateAccount)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser = res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    chime
    (Proxy :: Proxy UpdateUser)

responseInviteUsers :: InviteUsersResponse -> TestTree
responseInviteUsers = res
    "InviteUsersResponse"
    "fixture/InviteUsersResponse.proto"
    chime
    (Proxy :: Proxy InviteUsers)
