{-
Author: Thang Tran
Project: Bworks, Paas
Plutus V2, support CIP-32 inline datum 
Validator description: Validate plutus POSIXTime from datum and redeemer with lesser, equal and greater logics.
How to use:
  1. lock some Ada to this contract with three dates. 
  2. unlock the locked UTXO with redeemer contain three dates which the first is lesser, the second equal and the last greaters than one in datum. 
  3. the transaction is validated only three date points matched above logics
This to demo how plutus works with POSIXTime which mostly use in real life.
-}


{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Paas.ValidatePosixDateTimePoint
  ( validateTimePointScriptV2
  , validateTimePointScriptShortBsV2
  ) where

import Prelude hiding (($), (&&), (==))
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Plutus.V2.Ledger.Api qualified as Plutus
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup ((<>)), unless, (.))
import Plutus.V2.Ledger.Contexts (ScriptContext, txSignedBy)
import Plutus.Script.Utils.Typed qualified as Scripts
import PlutusTx.Prelude qualified as P
import Plutus.V1.Ledger.Interval as Interval

data TimeValidateRedeemer
  = TimeValidateRedeemer
      {
       lesserTimeRedeemer :: Plutus.POSIXTime,
       equalTimeRedeemer :: Plutus.POSIXTime,
       greaterTimeRedeemer :: Plutus.POSIXTime 
      } deriving (Prelude.Eq, Show)

data TimeValidateDatum
  = TimeValidateDatum
       {
       lesserTimeDatum :: Plutus.POSIXTime,
       equalTimeDatum :: Plutus.POSIXTime,
       greaterTimeDatum :: Plutus.POSIXTime  
      } deriving (Prelude.Eq, Show)

PlutusTx.unstableMakeIsData ''TimeValidateRedeemer
PlutusTx.unstableMakeIsData ''TimeValidateDatum

{-# INLINABLE mkValidator #-}
mkValidator :: TimeValidateDatum -> TimeValidateRedeemer ->  ScriptContext -> Bool
mkValidator (TimeValidateDatum lesserTimeDatum equalTimeDatum greaterTimeDatum) (TimeValidateRedeemer lesserTimeRedeemer equalTimeRedeemer greaterTimeRedeemer) scriptContext = timeValidator
  where
    timeValidator = (lesserTimeDatum P.< lesserTimeRedeemer) P.&& (equalTimeDatum P.== equalTimeRedeemer) P.&& (greaterTimeDatum P.> greaterTimeRedeemer)


validator :: Plutus.Validator
validator = Plutus.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = Scripts.mkUntypedValidator mkValidator
     
script :: Plutus.Script
script = Plutus.unValidatorScript validator

validateTimePointScriptShortBsV2 :: SBS.ShortByteString
validateTimePointScriptShortBsV2 = SBS.toShort . LBS.toStrict $ serialise script

validateTimePointScriptV2 :: PlutusScript PlutusScriptV2
validateTimePointScriptV2 = PlutusScriptSerialised validateTimePointScriptShortBsV2