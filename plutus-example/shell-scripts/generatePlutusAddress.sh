#!/bin/zsh
#run script: zsh generatePlutusAddress.sh scriptName address
#walletName is fileName of a wallet in wallets folder e.g wallet01
SCRIPTNAME=$1
ADDRESS=$2

#preprod network
$CARDANO_CLI address build --payment-script-file ../generated-plutus-scripts/paas/${SCRIPTNAME}.plutus --testnet-magic 1 --out-file ../wallets/${ADDRESS}.addr

#mainnet: zsh generatePlutusAddress.sh bworksV2  bworksV2Mainnet

#mainnet: zsh generatePlutusAddressMainnet.sh bworksV2  bworksV2Mainnet
#$CARDANO_CLI address build --payment-script-file ../generated-plutus-scripts/paas/${SCRIPTNAME}.plutus --mainnet --out-file ../wallets/${ADDRESS}.addr
