
import Prelude

import Cardano.Api

import System.Directory
import System.FilePath.Posix ((</>))

import PlutusExample.PlutusVersion1.AlwaysFails (alwaysFailsScript)
import PlutusExample.PlutusVersion1.AlwaysSucceeds (alwaysSucceedsScript)
import PlutusExample.PlutusVersion1.CustomDatumRedeemerGuess
import PlutusExample.PlutusVersion1.DatumRedeemerGuess (guessScript, guessScriptStake)
import PlutusExample.PlutusVersion1.Loop (loopScript)
import PlutusExample.PlutusVersion1.MintingScript (apiExamplePlutusMintingScript)
import PlutusExample.PlutusVersion1.RedeemerContextScripts
import PlutusExample.PlutusVersion1.Sum (sumScript)

import PlutusExample.PlutusVersion2.EcdsaSecp256k1Loop (v2EcdsaLoopScript)
import PlutusExample.PlutusVersion2.MintingScript (v2mintingScript)
import PlutusExample.PlutusVersion2.RedeemerContextEquivalence (v2ScriptContextEquivalenceScript, v2mintEquivScript)
import PlutusExample.PlutusVersion2.RequireRedeemer (requireRedeemerScript)
import PlutusExample.PlutusVersion2.SchnorrSecp256k1Loop (v2SchnorrLoopScript)
import PlutusExample.PlutusVersion2.StakeScript (v2StakeScript)

--Paas
import Paas.ValidateAddress (validateAddressScriptV2)
import Paas.ValidateDateTime (unlockWithTimeValidateScriptV2)
import Paas.ValidateSignatureAppWallet (paasAppWalletScriptV2) 
import Paas.MintingScript (paasMintingScriptV2)
import Paas.ValidateContextTxTimeRange (validateContextTxTimeRangeScriptV2)
import Paas.ValidatePosixDateTimePoint (validateTimePointScriptV2)
import Paas.ValidateDatumRedeemerData (validateDatumRedeemerDataScriptV2)
import Paas.ValidateSignatureAndReceiverWallet (signatureAndAddressScriptV2)
import Paas.MintingWithStringScript (paasMintingWithStringScriptV2)
import Paas.ValidateOrSignatures (paasValidateOrSignaturesV2)

main :: IO ()
main = do
  let v1dir = "generated-plutus-scripts/v1"
      v2dir = "generated-plutus-scripts/v2"
      paasdir = "generated-plutus-scripts/paas"
  createDirectoryIfMissing True v1dir
  createDirectoryIfMissing True v2dir
  createDirectoryIfMissing True paasdir

  _ <- writeFileTextEnvelope (v1dir </> "always-fails.plutus") Nothing alwaysFailsScript
  _ <- writeFileTextEnvelope (v1dir </> "always-succeeds-spending.plutus") Nothing alwaysSucceedsScript
  _ <- writeFileTextEnvelope (v1dir </> "guess-42-datum-42-txin.plutus") Nothing guessScript
  _ <- writeFileTextEnvelope (v1dir </> "guess-42-stake.plutus") Nothing guessScriptStake
  _ <- writeFileTextEnvelope (v1dir </> "custom-guess-42-datum-42.plutus") Nothing customGuessScript
  _ <- writeFileTextEnvelope (v1dir </> "anyone-can-mint.plutus") Nothing apiExamplePlutusMintingScript
  _ <- writeFileTextEnvelope (v1dir </> "sum.plutus") Nothing sumScript
  _ <- writeFileTextEnvelope (v1dir </> "loop.plutus") Nothing loopScript
  _ <- writeFileTextEnvelope (v1dir </> "context-equivalance-test.plutus") Nothing scriptContextTextPayingScript
  _ <- writeFileTextEnvelope (v1dir </> "minting-context-equivalance-test.plutus") Nothing scriptContextTestMintingScript


  _ <- writeFileTextEnvelope (v2dir </> "required-redeemer.plutus") Nothing requireRedeemerScript
  _ <- writeFileTextEnvelope (v2dir </> "minting-script.plutus") Nothing v2mintingScript
  _ <- writeFileTextEnvelope (v2dir </> "stake-script.plutus") Nothing v2StakeScript
  _ <- writeFileTextEnvelope (v2dir </> "context-equivalence-test.plutus") Nothing v2ScriptContextEquivalenceScript
  _ <- writeFileTextEnvelope (v2dir </> "minting-context-equivalance-test.plutus") Nothing v2mintEquivScript
  _ <- writeFileTextEnvelope (v2dir </> "ecdsa-secp256k1-loop.plutus") Nothing v2EcdsaLoopScript
  _ <- writeFileTextEnvelope (v2dir </> "schnorr-secp256k1-loop.plutus") Nothing v2SchnorrLoopScript

--Paas
  _ <- writeFileTextEnvelope (paasdir </> "validate-address.plutus") Nothing validateAddressScriptV2
  _ <- writeFileTextEnvelope (paasdir </> "validate-time.plutus") Nothing unlockWithTimeValidateScriptV2
  _ <- writeFileTextEnvelope (paasdir </> "validate-signature.plutus") Nothing paasAppWalletScriptV2
  _ <- writeFileTextEnvelope (paasdir </> "minting-script.plutus") Nothing paasMintingScriptV2
  _ <- writeFileTextEnvelope (paasdir </> "validate-contextTx-timeRange.plutus") Nothing validateContextTxTimeRangeScriptV2
  _ <- writeFileTextEnvelope (paasdir </> "validate-time-points.plutus") Nothing validateTimePointScriptV2
  _ <- writeFileTextEnvelope (paasdir </> "validate-datum-redeemer-data.plutus") Nothing validateDatumRedeemerDataScriptV2
  _ <- writeFileTextEnvelope (paasdir </> "validate-signature-receiver-address.plutus") Nothing signatureAndAddressScriptV2
  _ <- writeFileTextEnvelope (paasdir </> "minting-script-with-string.plutus") Nothing paasMintingWithStringScriptV2
  _ <- writeFileTextEnvelope (paasdir </> "validate-or-signatures.plutus") Nothing paasValidateOrSignaturesV2
  
  return ()
