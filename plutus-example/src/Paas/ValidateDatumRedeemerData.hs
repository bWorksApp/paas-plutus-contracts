{-
Author: Thang Tran
Project: Bworks, Paas
Validator description: Validate plutus execution context timeRange lowerBound and upperBound by putting the value as integer in redeemer. 
Usage: To indentify valid time range (txInfoValidRange) lowerBound and upperBound.
How to use:
  1. lock some Ada to this contract with datum contains a string as bytes, a integer, a date as POSIXTime in milliseconds. 
  2. unlock the locked UTXO with redeemer contains a string as bytes, a integer, a date as POSIXTime in milliseconds.
  3. the transaction will be validated only if each of value in datum and redeemer are the equal
-}


{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Paas.ValidateDatumRedeemerData
  ( validateDatumRedeemerDataScriptV2
  , validateDatumRedeemerDataScriptShortBsV2
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


data Datum
  = Datum
       {
       datumString :: P.BuiltinByteString,
       datumInt:: P.Integer,
       datumDate:: Plutus.POSIXTime 
      } deriving (Prelude.Eq, Show)


data Redeemer
  = Redeemer
      {
       redeemerString :: P.BuiltinByteString,
       redeemerInt:: P.Integer,
       redeemerDate:: Plutus.POSIXTime    
      } deriving (Prelude.Eq, Show)

PlutusTx.unstableMakeIsData ''Datum
PlutusTx.unstableMakeIsData ''Redeemer

{-# INLINABLE mkValidator #-}
mkValidator :: Datum -> Redeemer ->  ScriptContext -> Bool
mkValidator (Datum datumString datumInt datumDate) (Redeemer redeemerString redeemerInt redeemerDate) scriptContext = (datumString P.== redeemerString) P.&& (datumInt P.== redeemerInt) P.&& (datumDate P.== redeemerDate)
 

validator :: Plutus.Validator
validator = Plutus.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = Scripts.mkUntypedValidator mkValidator
     
script :: Plutus.Script
script = Plutus.unValidatorScript validator

validateDatumRedeemerDataScriptShortBsV2 :: SBS.ShortByteString
validateDatumRedeemerDataScriptShortBsV2 = SBS.toShort . LBS.toStrict $ serialise script

validateDatumRedeemerDataScriptV2 :: PlutusScript PlutusScriptV2
validateDatumRedeemerDataScriptV2 = PlutusScriptSerialised validateDatumRedeemerDataScriptShortBsV2

{-
datum json file:
{
	"constructor": 0,
	"fields": [{
		"bytes":"313233"
	}]
}

redeemer json file:
{
	"constructor": 0,
	"fields": [{
		"bytes":"313233"
	}]
}
-}
