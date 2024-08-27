{-
Author: Thang Tran
Project: Bworks, Paas
Validator description: Validate unlock sign wallet and receiver address with ones attached to datum. 
How to use:
  1. lock some Ada to this contract with datum contains public key hash of a wallet which will be used to sign the unlock transaction and public key hash of a wallet which will be use to receive locked ada. 
  2. unlock the locked UTXO with a wallet and to addres which with public key hash attached to datum.
  3. the transaction will be validated only if both the signed wallet and receive address are correct.
-}


{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Paas.ValidateSignatureAndReceiverWallet
  ( signatureAndAddressScriptV2
  , signatureAndAddressScriptShortBsV2
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
import Plutus.V1.Ledger.Address qualified as Address

data Datum
  = Datum
      {
       signPKH :: Plutus.PubKeyHash,
       receiverPKH :: Plutus.PubKeyHash
      } deriving (Prelude.Eq, Show)

data Redeemer
  = Redeemer
      {
      } deriving (Prelude.Eq, Show)

PlutusTx.unstableMakeIsData ''Datum
PlutusTx.unstableMakeIsData ''Redeemer

{-# INLINABLE mkValidator #-}
mkValidator :: Datum -> Redeemer ->  ScriptContext -> Bool
mkValidator (Datum signPKH receiverPKH) _ scriptContext = mustBySignedBy P.&& mustBeSendTo 
 where 
    txInfo :: Plutus.TxInfo
    txInfo = Plutus.scriptContextTxInfo scriptContext
    signatures :: [Plutus.PubKeyHash]
    signatures = Plutus.txInfoSignatories txInfo
    mustBySignedBy :: Bool
    mustBySignedBy = P.elem signPKH signatures
    txInfoOutputs = Plutus.txInfoOutputs txInfo
    outAddresses = P.map Plutus.txOutAddress txInfoOutputs
    outAddress :: Address.Address
    outAddress = P.head outAddresses
    outPKH :: Maybe Plutus.PubKeyHash
    outPKH = Address.toPubKeyHash outAddress
    mustBeSendTo = outPKH P.== Just receiverPKH

validator :: Plutus.Validator
validator = Plutus.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = Scripts.mkUntypedValidator mkValidator
     
script :: Plutus.Script
script = Plutus.unValidatorScript validator

signatureAndAddressScriptShortBsV2 :: SBS.ShortByteString
signatureAndAddressScriptShortBsV2 = SBS.toShort . LBS.toStrict $ serialise script

signatureAndAddressScriptV2 :: PlutusScript PlutusScriptV2
signatureAndAddressScriptV2 = PlutusScriptSerialised signatureAndAddressScriptShortBsV2
