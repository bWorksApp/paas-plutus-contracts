{-
Author: Thang Tran
Project: Bworks, Paas
Validator description: Validate unlock time. If the unlock within time range defined in datum then transaction is validated.
Usage: Use to apply deadline
Notes: Support CIP-32 inline datum

This script just to test the LowerBound of txTimeRange to see if it has value or NegInf, PosInf
if can unlock then LowerBound is NegInf
-}


{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module TestedSamples.ValidateDateTime
  ( lowerBoundWithTimeValidateScriptV2
  , lowerBoundTimeValidateScriptShortBsV2
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
import Plutus.V1.Ledger.Time as V1Time

data UnlockWithTimeValidateRedeemer
  = UnlockWithTimeValidateRedeemer
      {
        time1 :: Plutus.POSIXTime 
      } deriving (Prelude.Eq, Show)

data UnlockWithTimeValidateDatum
  = UnlockWithTimeValidateDatum
       { 
        time :: Plutus.POSIXTime 
      } deriving (Prelude.Eq, Show)

PlutusTx.unstableMakeIsData ''UnlockWithTimeValidateRedeemer
PlutusTx.unstableMakeIsData ''UnlockWithTimeValidateDatum


{-# INLINABLE mkValidator #-}
--we will add validator logics here to verify the transaction is valid if it is signed by bWorks
mkValidator :: UnlockWithTimeValidateDatum -> UnlockWithTimeValidateRedeemer ->  ScriptContext -> Bool
mkValidator (UnlockWithTimeValidateDatum time) (UnlockWithTimeValidateRedeemer time1) scriptContext = (extractFiniteUpper txTimeRange) P.== Just (Plutus.POSIXTime 10)
 -- Interval.before time txTimeRange
  where
    x :: Plutus.POSIXTime
    y :: Plutus.POSIXTime
    x = Plutus.POSIXTime 10
    y = Plutus.POSIXTime 0
    timeRange:: Plutus.POSIXTimeRange
    timeRange = Interval.to time
    txInfo :: Plutus.TxInfo
    txInfo = Plutus.scriptContextTxInfo scriptContext
    txTimeRange :: Plutus.POSIXTimeRange
    txTimeRange = Plutus.txInfoValidRange txInfo
    extractFiniteUpper :: Interval Plutus.POSIXTime -> Maybe Plutus.POSIXTime
    extractFiniteUpper interval = case ivFrom interval of
      LowerBound (Finite value) _ -> Just (Plutus.POSIXTime 0)
      LowerBound (NegInf ) _      -> Just x
      LowerBound (PosInf ) _      -> Just y
     

    abc :: Maybe  Plutus.POSIXTime
    abc = extractFiniteUpper txTimeRange

validator :: Plutus.Validator
validator = Plutus.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = Scripts.mkUntypedValidator mkValidator
     
script :: Plutus.Script
script = Plutus.unValidatorScript validator

lowerBoundTimeValidateScriptShortBsV2 :: SBS.ShortByteString
lowerBoundTimeValidateScriptShortBsV2 = SBS.toShort . LBS.toStrict $ serialise script

lowerBoundWithTimeValidateScriptV2 :: PlutusScript PlutusScriptV2
lowerBoundWithTimeValidateScriptV2 = PlutusScriptSerialised lowerBoundTimeValidateScriptShortBsV2