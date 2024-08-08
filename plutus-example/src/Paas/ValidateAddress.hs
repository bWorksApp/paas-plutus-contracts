{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module TestedSamples.ValidateAddress
  ( validateAddressScriptShortBsV2
  , validateAddressScriptV2
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
import Plutus.V2.Ledger.Api qualified as V2
import Plutus.V2.Ledger.Contexts as V2
import Plutus.V1.Ledger.Address qualified as Address

data UnlockByEmpWithoutDeadLineRedeemer
  = UnlockByEmpWithoutDeadLineRedeemer
      { 
      } deriving (Prelude.Eq, Show)

data UnlockByEmpWithoutDeadLineDatum
  = UnlockByEmpWithoutDeadLineDatum
      {
      unlockSignature :: Plutus.PubKeyHash
      --unlockSignature :: [Plutus.PubKeyHash]
      } deriving (Prelude.Eq, Show)

PlutusTx.unstableMakeIsData ''UnlockByEmpWithoutDeadLineDatum
PlutusTx.unstableMakeIsData ''UnlockByEmpWithoutDeadLineRedeemer

{-# INLINABLE mkValidator #-}
-- validate receiver address to be the same with the one attached to datum
-- validate receiver address of unlock transaction.
-- get receiver address -> compare it with attached in datum 
mkValidator :: UnlockByEmpWithoutDeadLineDatum -> UnlockByEmpWithoutDeadLineRedeemer ->  ScriptContext -> Bool
mkValidator (UnlockByEmpWithoutDeadLineDatum unlockSignature) (UnlockByEmpWithoutDeadLineRedeemer {}) scriptContext = 
  outAddresses P.== P.map Address.pubKeyHashAddress [unlockSignature]
  --outAddresses P.== P.map Address.pubKeyHashAddress unlockSignature
  where  
    txInfo :: Plutus.TxInfo
    txInfo = Plutus.scriptContextTxInfo scriptContext
    txInfoOutputs :: [Plutus.TxOut]
    txInfoOutputs = Plutus.txInfoOutputs txInfo
    outAddresses = P.map V2.txOutAddress txInfoOutputs

validator :: Plutus.Validator
validator = Plutus.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = Scripts.mkUntypedValidator mkValidator
     
script :: Plutus.Script
script = Plutus.unValidatorScript validator

validateAddressScriptShortBsV2 :: SBS.ShortByteString
validateAddressScriptShortBsV2 = SBS.toShort . LBS.toStrict $ serialise script

validateAddressScriptV2 :: PlutusScript PlutusScriptV2
validateAddressScriptV2 = PlutusScriptSerialised validateAddressScriptShortBsV2