{-
Author: Thang Tran
Project: Bworks, Paas
Validator description: Validate plutus execution context timeRange lowerBound and upperBound by putting the value as integer in redeemer. 
Usage: To indentify valid time range (txInfoValidRange) lowerBound and upperBound.
How to use:
  1. lock some Ada to this contract with empty datum. 
  2. unlock the locked UTXO with redeemer contain an integer number be either below value and only one can be unlock. 
  Then we can reference the number in redeemer which can unlock the locked UTXO to indetify the bounds of valid context timerange (txInfoValidRange)
  to be either negative infinity, positive infinity, finite value:
    11  -> LowerBound NegInf and UpperBound NegInf
    12  -> LowerBound NegInf and UpperBound Finite value
    13  -> LowerBound NegInf and UpperBound PosInf
    14  -> LowerBound NegInf and UpperBound Nothing

    21  -> LowerBound Finite value and UpperBound NegInf 
    22  -> LowerBound Finite value and UpperBound Finite value
    23  -> LowerBound Finite value and UpperBound 
    24  -> LowerBound Finite value and UpperBound Nothing

    31  -> LowerBound PosInf and UpperBound NegInf
    32  -> LowerBound PosInf and UpperBound Finite value
    33  -> LowerBound PosInf and UpperBound PosInf
    34  -> LowerBound PosInf and UpperBound Nothing

    41  -> LowerBound Nothing and UpperBound NegInf
    42  -> LowerBound Nothing and UpperBound Finite value
    43  -> LowerBound Nothing and UpperBound PosInf
    44  -> LowerBound Nothing and UpperBound Nothing

  This is useful when we intend to develop the smart contract time validator with valid time range in execution context e.g value before, value fter, range contains.
-}


{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Paas.ValidateContextTxTimeRange
  ( validateContextTxTimeRangeScriptV2
  , validateContextTxTimeRangeScriptShortBsV2
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
import Prelude qualified as B

data UnlockWithTimeValidateRedeemer
  = UnlockWithTimeValidateRedeemer
      {
       number:: B.Integer
      } deriving (Prelude.Eq, Show)

data UnlockWithTimeValidateDatum
  = UnlockWithTimeValidateDatum
       { 
      } deriving (Prelude.Eq, Show)

PlutusTx.unstableMakeIsData ''UnlockWithTimeValidateRedeemer
PlutusTx.unstableMakeIsData ''UnlockWithTimeValidateDatum

{-# INLINABLE mkValidator #-}
mkValidator :: UnlockWithTimeValidateDatum -> UnlockWithTimeValidateRedeemer ->  ScriptContext -> Bool
mkValidator _ (UnlockWithTimeValidateRedeemer timeRangeIndicatorNumber) scriptContext = timeRangeIndicator P.== timeRangeIndicatorNumber
  where
    txInfo :: Plutus.TxInfo
    txInfo = Plutus.scriptContextTxInfo scriptContext
    txTimeRange :: Plutus.POSIXTimeRange
    txTimeRange = Plutus.txInfoValidRange txInfo

    -- extract lowerBound txTimeRange
    extractFiniteLower :: Interval Plutus.POSIXTime -> B.Integer
    extractFiniteLower interval = case ivFrom interval of
      LowerBound (NegInf ) _      -> 10
      LowerBound (Finite value) _ -> 20
      LowerBound (PosInf ) _      -> 30
      _                           -> 40

     -- extract upperBound txTimeRange
    extractFiniteUpper :: Interval Plutus.POSIXTime -> B.Integer
    extractFiniteUpper interval = case ivTo interval of
      UpperBound (NegInf ) _      -> 1
      UpperBound (Finite value) _ -> 2
      UpperBound (PosInf ) _      -> 3
      _                           -> 4

    timeRangeIndicator :: B.Integer
    timeRangeIndicator = (extractFiniteLower txTimeRange) P.+ (extractFiniteUpper txTimeRange)

validator :: Plutus.Validator
validator = Plutus.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = Scripts.mkUntypedValidator mkValidator
     
script :: Plutus.Script
script = Plutus.unValidatorScript validator

validateContextTxTimeRangeScriptShortBsV2 :: SBS.ShortByteString
validateContextTxTimeRangeScriptShortBsV2 = SBS.toShort . LBS.toStrict $ serialise script

validateContextTxTimeRangeScriptV2 :: PlutusScript PlutusScriptV2
validateContextTxTimeRangeScriptV2 = PlutusScriptSerialised validateContextTxTimeRangeScriptShortBsV2
