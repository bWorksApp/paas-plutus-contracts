{-
Author: Thang tran
Projects: PAAS, Bworks
API docs
https://www.doitwithlovelace.io/haddock/plutus-ledger-api/html/Plutus-V2-Ledger-Contexts.html#t:TxOut

Description: validate the receiver address with public key hash attached in datum.

Logics:
- validate receiver address to be the same with the one attached to datum
- validate receiver address of unlock transaction.
- get receiver address from TxInfo -> compare it with attached in datum 

Notes:
Plutus V2, CIP 32
Worked with CLI and app wallets

-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Paas.ValidateAddress
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
import Plutus.V1.Ledger.Address qualified as V1Address

data ValidateAddressRedeemer
  = ValidateAddressRedeemer
      { 
      } deriving (Prelude.Eq, Show)

data ValidateAddressDatum
  = ValidateAddressDatum
      {
      unlockSignature :: Plutus.PubKeyHash
      } deriving (Prelude.Eq, Show)

PlutusTx.unstableMakeIsData ''ValidateAddressDatum
PlutusTx.unstableMakeIsData ''ValidateAddressRedeemer

{-# INLINABLE mkValidator #-}
mkValidator :: ValidateAddressDatum -> ValidateAddressRedeemer ->  ScriptContext -> Bool
mkValidator (ValidateAddressDatum unlockSignature) (ValidateAddressRedeemer {}) scriptContext = 
  outPKH P.== Just unlockSignature
  where 
    outAddress =  Address.pubKeyHashAddress unlockSignature
    txInfo :: Plutus.TxInfo
    txInfo = Plutus.scriptContextTxInfo scriptContext
    txInfoOutputs :: [Plutus.TxOut]
    txInfoOutputs = Plutus.txInfoOutputs txInfo
    outAddresses = P.map V2.txOutAddress txInfoOutputs
    txOutAddresses :: Address.Address
    txOutAddresses = P.head outAddresses
    outPKH :: Maybe Plutus.PubKeyHash
    outPKH = Address.toPubKeyHash txOutAddresses 


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


