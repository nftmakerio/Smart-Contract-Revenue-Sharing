{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Canonical.RevenueSharing
  ( revenueSharing
  , Config
  , percentOwed
  ) where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import Ledger hiding (singleton)
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import qualified PlutusTx.AssocMap as A
import Ledger.Ada hiding (divide)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
type Config = A.Map PubKeyHash Integer

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------
{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = getLovelace . fromValue

{-# INLINABLE percentOwed #-}
percentOwed :: Value -> Integer -> Integer
percentOwed inVal pct = lovelaces inVal * pct `divide` 1000

-------------------------------------------------------------------------------
-- Validator
-------------------------------------------------------------------------------
{-
  For each address specified in the 'Config' the validator checks that they receive a
  percentage of the input equal or greater than the percentage specified
  in the 'Config'
-}
mkValidator :: Config -> Integer -> Integer -> ScriptContext -> Bool
mkValidator config _ _ ctx = traceIfFalse "Not all addresses were paid the correct amount" outputValid
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Iterate throught the Config Map and check that each
    -- address gets the correct percentage
    outputValid :: Bool
    outputValid = all paidPercentOwed . A.toList $ config

    -- For a given address and percentage pair, verify
    -- they received greater or equal to their percentage
    -- of the input.
    paidPercentOwed :: (PubKeyHash, Integer) -> Bool
    paidPercentOwed (addr, pct) =
      lovelaces (valuePaidTo info addr) >= percentOwed inValue pct

    -- The assets locked on the script
    inValue :: Value
    inValue = txOutValue ownInput

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "input missing"
      Just i -> txInInfoResolved i

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------
data RevenueSharing
instance Scripts.ValidatorTypes RevenueSharing where
    type instance DatumType RevenueSharing = Integer
    type instance RedeemerType RevenueSharing = Integer

typedValidator :: Config -> Scripts.TypedValidator RevenueSharing
typedValidator config = Scripts.mkTypedValidator @RevenueSharing
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode`
      PlutusTx.liftCode config)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @Integer

validator :: Config -> Validator
validator = Scripts.validatorScript . typedValidator

-------------------------------------------------------------------------------
-- Entry point
-------------------------------------------------------------------------------
revenueSharing :: Config -> PlutusScript PlutusScriptV1
revenueSharing
  = PlutusScriptSerialised
  . SBS.toShort
  . LB.toStrict
  . serialise
  . validator
