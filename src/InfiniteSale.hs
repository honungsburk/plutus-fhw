{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
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
import qualified Prelude

--------------------------------------------------------------------------------
-- Infinite Sale
--------------------------------------------------------------------------------

data InfinitySaleParam = InfinitySaleParam
  { -- | The hash of the NFT minting script to use
    infMintingPolicy :: !CurrencySymbol,
    -- | The operator/owner control of the smart contract
    infOperator :: !PubKeyHash,
    -- | The NFT used to control the minting policy
    -- infFee :: !Integer,
    infNFT :: !AssetClass
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''InfinitySaleParam

{-# INLINEABLE infinitySaleTokenName #-}
infinitySaleTokenName :: TokenName
infinitySaleTokenName = TokenName "INFINITY SALE"

{-# INLINEABLE infinityAsset #-}
infinityAsset :: InfinitySaleParam -> AssetClass
infinityAsset = infNFT

newtype InfinitySaleDatum = InfinitySaleDatum {numberOfSales :: Integer}

{-# INLINEABLE infinityValue #-}
infinityValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe InfinitySaleDatum
infinityValue o f = do
  dh <- txOutDatum o
  Datum d <- f dh
  PlutusTx.fromData d

data InfinitySaleRedeemer
  = -- | Allows anyone to buy a freshly minted art piece
    Buy
  | -- | Called by the creator to handover the NFT and set the inital state
    Init
  | -- | Called by the creator to collect any fees
    CollectFees
  deriving (Prelude.Show, Prelude.Eq)

data InfinitySale

instance Scripts.ValidatorTypes InfinitySale where
  type DatumType InfinitySale = InfinitySaleDatum
  type RedeemerType InfinitySale = InfinitySaleRedeemer

{-# INLINEABLE mkInfinitySaleValidator #-}
mkInfinitySaleValidator :: InfinitySaleParam -> InfinitySaleDatum -> InfinitySaleRedeemer -> ScriptContext -> Bool
mkInfinitySaleValidator infSale state action ctx =
  --   traceIfFalse "token missing from input" inputHasToken
  --     && traceIfFalse "token missing from output" outputHasToken
  case action of
    Init ->
      traceIfFalse "Only the owner can init the script" (txSignedBy info $ infOperator infSale)
        && traceIfFalse "invalid output datum" validOutputDatum
    CollectFees ->
      traceIfFalse "Only the owner can collect fees" (txSignedBy info $ infOperator infSale)
  where
    --       Buy ->
    --         traceIfFalse "oracle value changed" (outputDatum == Just x)
    --           && traceIfFalse "fees not paid" feesPaid

    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Check that the NFT exists at the transaction
    hasToken :: TxOut -> Bool
    hasToken txOut = assetClassValueOf (txOutValue txOut) (infinityAsset infSale) == 1

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "NFT input missing"
      Just i -> txInInfoResolved i

    inputHasToken :: Bool
    inputHasToken = hasToken ownInput

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "expected exactly one NFT output"

    outputHasToken :: Bool
    outputHasToken = hasToken ownOutput

    outputDatum :: Maybe InfinitySaleDatum
    outputDatum = infinityValue ownOutput (`findDatum` info)

    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatum

--     feesPaid :: Bool
--     feesPaid =
--       let inVal = txOutValue ownInput
--           outVal = txOutValue ownOutput
--        in outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle))

infinityValidator :: InfinitySaleParam -> Scripts.TypedValidator InfinitySale
infinityValidator p =
  Scripts.mkTypedValidator @InfinitySale
    ($$(PlutusTx.compile [||mkInfinitySaleValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @InfinitySaleDatum @InfinitySaleRedeemer

validator :: InfinitySaleParam -> Validator
validator = Scripts.validatorScript . infinityValidator

valHash :: InfinitySaleParam -> Ledger.ValidatorHash
valHash = Scripts.validatorHash . infinityValidator

scrAddress :: InfinitySaleParam -> Ledger.Address
scrAddress = scriptAddress . validator

--------------------------------------------------------------------------------
-- Minting from NFT Policy
--------------------------------------------------------------------------------

{-# INLINEABLE mkPolicy #-}
mkPolicy ::
  -- | The NFT representing the generative art script
  AssetClass ->
  () ->
  ScriptContext ->
  Bool
mkPolicy ac _ ctx =
  traceIfFalse "NFT did not exist in output" hasNFT
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasNFT :: Bool
    hasNFT = any ((==) 1 . (flip assetClassValueOf ac . txOutValue . txInInfoResolved)) $ txInfoInputs info

policy :: AssetClass -> Scripts.MintingPolicy
policy ac =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode ac

curSymbol :: AssetClass -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

--------------------------------------------------------------------------------
-- Off chain code
--------------------------------------------------------------------------------

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
  let tn = "INFINITY"
  h1 <- activateContractWallet (Wallet 1) endpoints
  h2 <- activateContractWallet (Wallet 2) endpoints
  callEndpoint @"mint" h1 tn
  callEndpoint @"mint" h2 tn
  void $ Emulator.waitNSlots 1
