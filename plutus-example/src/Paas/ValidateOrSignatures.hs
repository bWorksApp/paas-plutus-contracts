{-
Author: Thang Tran
Project: Bworks, Paas
Validator description: Verfify sign wallet signature with public key hash attached to datum.
Usage: To assign a wallet to unlock a locked UTXO and only this wallet can unlock the locked UTXO
Notes: Support CIP-32 inline datum  
-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Paas.ValidateOrSignatures
  ( paasValidateOrSignaturesV2
  , paasValidateOrSignaturesBsV2
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

data Redeemer
  = Redeemer
      { 
      } deriving (Prelude.Eq, Show)

data Datum
  = Datum
      {
      firstSignature :: Plutus.PubKeyHash,
      secondSignature :: Plutus.PubKeyHash,
      thirdSignature :: Plutus.PubKeyHash   
      } deriving (Prelude.Eq, Show)

PlutusTx.unstableMakeIsData ''Datum
PlutusTx.unstableMakeIsData ''Redeemer

{-# INLINABLE mkValidator #-}
--if the unlock transaction was signed by a wallet with pkh in datum then unlock otherwise fail
mkValidator :: Datum -> Redeemer ->  ScriptContext -> Bool
mkValidator (Datum firstSignature secondSignature thirdSignature) (Redeemer {}) scriptContext = 
  (P.elem firstSignature signatures) P.|| (P.elem secondSignature signatures)  P.|| (P.elem thirdSignature signatures)
  where  
    txInfo :: Plutus.TxInfo
    txInfo = Plutus.scriptContextTxInfo scriptContext
    signatures :: [Plutus.PubKeyHash]
    signatures = Plutus.txInfoSignatories txInfo

validator :: Plutus.Validator
validator = Plutus.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = Scripts.mkUntypedValidator mkValidator
     
script :: Plutus.Script
script = Plutus.unValidatorScript validator

paasValidateOrSignaturesBsV2 :: SBS.ShortByteString
paasValidateOrSignaturesBsV2 = SBS.toShort . LBS.toStrict $ serialise script

paasValidateOrSignaturesV2 :: PlutusScript PlutusScriptV2
paasValidateOrSignaturesV2 = PlutusScriptSerialised paasValidateOrSignaturesBsV2
