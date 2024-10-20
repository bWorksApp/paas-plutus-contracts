{--
Author: p.Norvig
Projects: PAAS
The validator return true if redeemer contains 2024 paas string.
This use to mint asset via PAAS platform.
This worked with PAAS mint UI 
--}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Paas.MintingWithStringScript
  ( paasMintingWithStringScriptV2
  , paasMintingWithStringScriptV2ShortBs
  ) where

import Prelude hiding (($))
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Prelude hiding (($), (&&))
import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Plutus.Script.Utils.Typed as Scripts
import Plutus.V2.Ledger.Api qualified as V2
import Plutus.V2.Ledger.Contexts as V2
import PlutusTx qualified
import PlutusTx.Prelude qualified as P
import PlutusTx.Builtins
import PlutusTx.Prelude hiding (Semigroup (..), unless, (.))

data PaasMintRedeemer
  = PaasMintRedeemer
       { 
       redeemerString :: P.BuiltinByteString
      } deriving (Prelude.Eq, Show)

PlutusTx.unstableMakeIsData ''PaasMintRedeemer

{- HLINT ignore "Avoid lambda" -}
{-# INLINABLE mkPolicy #-}
mkPolicy :: PaasMintRedeemer -> V2.ScriptContext -> Bool
mkPolicy (PaasMintRedeemer redeemerString) _ctx = redeemerString P.== "paas"

policy :: V2.MintingPolicy
policy = V2.mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = Scripts.mkUntypedMintingPolicy mkPolicy

plutusScript :: V2.Script
plutusScript =
  V2.unMintingPolicyScript policy

validator :: V2.Validator
validator = V2.Validator plutusScript

scriptAsCbor :: LBS.ByteString
scriptAsCbor = serialise validator

paasMintingWithStringScriptV2 :: PlutusScript PlutusScriptV2
paasMintingWithStringScriptV2 = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict scriptAsCbor

paasMintingWithStringScriptV2ShortBs :: SBS.ShortByteString
paasMintingWithStringScriptV2ShortBs = SBS.toShort . LBS.toStrict $ scriptAsCbor