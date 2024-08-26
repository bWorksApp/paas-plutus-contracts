#!/bin/zsh
#run script: zsh getWalletPkh.sh walletName
#walletName is fileName of a wallet in wallets folder e.g wallet01
#the command to query contract & wallet address is the same
#./getWalletPkh.sh wallet01
$CARDANO_CLI address key-hash --payment-verification-key-file ../wallets/$1.vkey