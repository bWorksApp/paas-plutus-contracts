#!/bin/zsh
#setup cardano environment variables
echo "export CARDANO_CLI=~/.local/bin/cardano-cli" >> ~/.zshrc
echo "export TESTNET_MAGIC=1" >> ~/.zshrc
echo "export TESTNET_MAGIC_NUM=1" >> ~/.zshrc

#reload environment variables
source ~/.zshrc

#verify the environment variables
echo $TESTNET_MAGIC
echo $CARDANO_CLI

#setup environment for preprod or mainnet
#Testnet / Preprod: NetworkMagic: 1, Mainnet / Production: NetworkMagic: 764824073
#export CARDANO_NODE_SOCKET_PATH=~/cardano-src/cardano-node/mainnet-db/node.socket
export CARDANO_NODE_SOCKET_PATH=~/cardano-src/cardano-node/testnet-db/node.socket