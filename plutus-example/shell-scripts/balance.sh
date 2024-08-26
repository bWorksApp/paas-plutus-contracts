#!/bin/zsh
#run script: zsh balance.sh walletName/scriptName
#walletName is fileName of a wallet in wallets folder e.g wallet01
#the command to query contract & wallet address is the same
#./balance.sh unlockByBWorksWithDeadLineScript
#./balance.sh wallet01
$CARDANO_CLI query utxo --address $(cat ../wallets/$1.addr) --testnet-magic $TESTNET_MAGIC
#mainnet
#$CARDANO_CLI query utxo --address $(cat ../wallets/$1.addr) --mainnet