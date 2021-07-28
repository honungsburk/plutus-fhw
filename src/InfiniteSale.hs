{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module InfiniteSale where

import Control.Monad hiding (fmap)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Void (Void)
import Ledger hiding (singleton)
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value as Value
import Playground.Contract (ToSchema, ensureKnownCurrencies, printJson, printSchemas, stage)
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Plutus.Contract as Contract hiding (when)
import Plutus.Trace.Emulator as Emulator
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Text.Printf (printf)
import Wallet.Emulator.Wallet
import Prelude (IO, Semigroup (..), String, show)

-- https://plutus.readthedocs.io/en/latest/plutus/tutorials/basic-minting-policies.html

{-# INLINEABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy oref tn _ ctx =
  traceIfFalse "UTxO not consumed" hasUTxO
    && traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoForge info) of
      [(cs, tn', amt)] -> cs == ownCurrencySymbol ctx && tn' == tn && amt == 1
      _ -> False

policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tn =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||\oref' tn' -> Scripts.wrapMintingPolicy $ mkPolicy oref' tn'||])
      `PlutusTx.applyCode` PlutusTx.liftCode oref
      `PlutusTx.applyCode` PlutusTx.liftCode tn

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol $ policy oref tn

type NFTSchema = Endpoint "mint" TokenName

mint :: TokenName -> Contract w NFTSchema Text ()
mint tn = do
  pk <- Contract.ownPubKey
  utxos <- utxoAt (pubKeyAddress pk)
  case Map.keys utxos of
    [] -> Contract.logError @String "no utxo found"
    oref : _ -> do
      Contract.logInfo @String $ "Creating Constraints"
      let val = Value.singleton (curSymbol oref tn) tn 1
          lookups = Constraints.mintingPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
          tx = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
      Contract.logInfo @String $ "Submitting transaction"
      ledgerTx <- submitTxConstraintsWith @Void lookups tx
      Contract.logInfo @String $ "Awaiting confirmation"
      void $ awaitTxConfirmed $ txId ledgerTx
      Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >>= InfiniteSale.mint

mkSchemaDefinitions ''NFTSchema

mkKnownCurrencies []

test :: Prelude.IO ()
test = runEmulatorTraceIO $ do
  let tn = "ABC"
  h1 <- activateContractWallet (Wallet 1) endpoints
  h2 <- activateContractWallet (Wallet 2) endpoints
  callEndpoint @"mint" h1 tn
  callEndpoint @"mint" h2 tn
  void $ Emulator.waitNSlots 1
