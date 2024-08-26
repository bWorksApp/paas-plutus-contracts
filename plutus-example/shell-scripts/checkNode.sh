#!/bin/zsh
#run script: zsh checkNode.sh
#check node, cli version & status
$CARDANO_CLI version
$CARDANO_NODE version

#preprod
$CARDANO_CLI query tip --testnet-magic $TESTNET_MAGIC

#mainnet 
#$CARDANO_CLI query tip --mainnet