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
  ) where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import Ledger hiding (singleton)
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import PlutusTx.Ratio
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import qualified PlutusTx.AssocMap as A
import Ledger.Ada

type Config = A.Map PubKeyHash Integer

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = getLovelace . fromValue

{-# INLINABLE percentOwed #-}
percentOwed :: Value -> Integer -> Value
percentOwed inVal pct = lovelaceValueOf . truncate $ (lovelaces inVal * pct) % 1000

mkValidator :: Config -> () -> () -> ScriptContext -> Bool
mkValidator config _ _ ctx = traceIfFalse "Not all addresses were paid the correct amount" outputValid
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    outputValid :: Bool
    outputValid = all paidPercentOwed . A.toList $ config

    paidPercentOwed :: (PubKeyHash, Integer) -> Bool
    paidPercentOwed (addr, pct) =
      valuePaidTo info addr == percentOwed inValue pct

    inValue :: Value
    inValue = txOutValue ownInput

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "input missing"
      Just i -> txInInfoResolved i

data RevenueSharing
instance Scripts.ValidatorTypes RevenueSharing where
    type instance DatumType RevenueSharing = ()
    type instance RedeemerType RevenueSharing = ()

typedValidator :: Config -> Scripts.TypedValidator RevenueSharing
typedValidator config = Scripts.mkTypedValidator @RevenueSharing
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode`
      PlutusTx.liftCode config)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()

validator :: Config -> Validator
validator = Scripts.validatorScript . typedValidator

revenueSharing :: Config -> PlutusScript PlutusScriptV1
revenueSharing
  = PlutusScriptSerialised
  . SBS.toShort
  . LB.toStrict
  . serialise
  . validator
