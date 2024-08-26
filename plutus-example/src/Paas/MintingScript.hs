{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

{-- 
The validator return true if redeemer contains 2024 integer number.
This use to mint asset via PAAS platform.
--}

module Paas.MintingScript
  ( paasMintingScriptV2
  , paasMintingScriptV2ShortBs
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
       number :: Integer
      } deriving (Prelude.Eq, Show)

PlutusTx.unstableMakeIsData ''PaasMintRedeemer


{- HLINT ignore "Avoid lambda" -}

{-# INLINABLE mkPolicy #-}
mkPolicy :: PaasMintRedeemer -> V2.ScriptContext -> Bool
mkPolicy (PaasMintRedeemer number) _ctx = number P.== 2024

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

paasMintingScriptV2 :: PlutusScript PlutusScriptV2
paasMintingScriptV2 = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict scriptAsCbor

paasMintingScriptV2ShortBs :: SBS.ShortByteString
paasMintingScriptV2ShortBs = SBS.toShort . LBS.toStrict $ scriptAsCbor
