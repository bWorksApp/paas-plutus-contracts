{-
Author: Thang Tran
Project: Bworks, Paas
Validator description: Validate timer. If the unlock after the time defined in datum then transaction is validated.
Usage: Use to apply deadline
Notes: Support CIP-32 inline datum

-}


{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Paas.ValidateDateTime
  ( unlockWithTimeValidateScriptV2
  , unlockWithTimeValidateScriptShortBsV2
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
import Ledger (CardanoAddress, CardanoTx, Language, SlotRange, TxOutRef (..), Versioned, toPlutusAddress)

data UnlockWithTimeValidateRedeemer
  = UnlockWithTimeValidateRedeemer
      { 
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
mkValidator (UnlockWithTimeValidateDatum time) (UnlockWithTimeValidateRedeemer ) scriptContext = 
  Plutus.txInfoValidRange txInfo `Interval.contains` timeRange
  where  
    timeRange:: Plutus.POSIXTimeRange
    timeRange = Interval.from time
    txInfo :: Plutus.TxInfo
    txInfo = Plutus.scriptContextTxInfo scriptContext
    txTimeRange :: Plutus.POSIXTimeRange
    txTimeRange = Plutus.txInfoValidRange txInfo

validator :: Plutus.Validator
validator = Plutus.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = Scripts.mkUntypedValidator mkValidator
     
script :: Plutus.Script
script = Plutus.unValidatorScript validator

unlockWithTimeValidateScriptShortBsV2 :: SBS.ShortByteString
unlockWithTimeValidateScriptShortBsV2 = SBS.toShort . LBS.toStrict $ serialise script

unlockWithTimeValidateScriptV2 :: PlutusScript PlutusScriptV2
unlockWithTimeValidateScriptV2 = PlutusScriptSerialised unlockWithTimeValidateScriptShortBsV2