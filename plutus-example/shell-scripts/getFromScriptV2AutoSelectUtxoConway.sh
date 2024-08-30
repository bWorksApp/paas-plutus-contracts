#!/bin/zsh
#this script is similar to getFromScriptV2 except it call different functions from functions.sh 
#to auto select collatel utxo & curent locked utxo (the utxo need to be unlock)
#zsh getFromScriptV2AutoSelectUtxoConway scriptName redeemerJsonFile payCollatelWalletName receiveWalletAddress scriptTxHash
#zsh getFromScriptV2AutoSelectUtxoConway.sh validate-signature  redeemer.json wallet01 addr_test1vrytgm4p7dckfpdjehpm3w263rsgh0tujtjue3eej5rh2lsczr8qs e7426079219d9ec87a4b813eff0872b8eea5aaffda94571a719f05fcd3868b9d

source ./functions.sh

SCRIPT_NAME=${1}
SCRIPT_FILE=../generated-plutus-scripts/paas/${SCRIPT_NAME}.plutus
REDEEMER_JSON_FILE=${2}
PAY_COLLATEL_WALLET=${3}
SIGNING_WALLET=${3}
RECEIVER_ADDR=${4}
SCRIPT_TXHASH=${5}
SCRIPT_ADDRESS=$(${CARDANO_CLI} address build --payment-script-file ${SCRIPT_FILE} --testnet-magic ${TESTNET_MAGIC_NUM})
echo ${SCRIPT_ADDRESS} > ../wallets/${SCRIPT_NAME}.addr

section "Get Script UTxO"
getScriptUtxo ${SCRIPT_NAME} ${SCRIPT_TXHASH} 
SCRIPT_UTXO=${SELECTED_UTXO}
LOCKED_ASSET_VALUE=${SELECTED_UTXO_LOVELACE}

section "Select Collateral UTxO"
getCollatelUtxo wallet01
COLLATERAL_TX=${SELECTED_UTXO}

$CARDANO_CLI query protocol-parameters --testnet-magic ${TESTNET_MAGIC_NUM} > params.json

removeTxFiles

echo TEST VARIABLES
echo SIGNING_WALLET ${SIGNING_WALLET}
echo COLLATERAL_TX ${COLLATERAL_TX}
echo RECEIVER_ADDR ${RECEIVER_ADDR}
echo SCRIPT_FILE ${SCRIPT_FILE}
echo SCRIPT_UTXO ${SCRIPT_UTXO}
echo LOCKED_ASSET_VALUE ${LOCKED_ASSET_VALUE} 


$CARDANO_CLI conway transaction build \
--required-signer ../wallets/${SIGNING_WALLET}.skey \
--testnet-magic ${TESTNET_MAGIC_NUM} \
--tx-in ${SCRIPT_UTXO} \
--tx-in-script-file ${SCRIPT_FILE} \
--tx-in-inline-datum-present \
--tx-in-redeemer-file ${REDEEMER_JSON_FILE} \
--tx-in-collateral ${COLLATERAL_TX} \
--change-address ${RECEIVER_ADDR} \
--out-file tx.build


$CARDANO_CLI transaction sign \
--tx-body-file tx.build \
--signing-key-file ../wallets/${SIGNING_WALLET}.skey \
--testnet-magic ${TESTNET_MAGIC_NUM} \
--out-file tx.signed \

$CARDANO_CLI transaction submit --tx-file tx.signed --testnet-magic ${TESTNET_MAGIC_NUM}

#print txHash
TX_HASH=$($CARDANO_CLI transaction txid --tx-file tx.signed) 
DATE=$(date)
echo "Summited TxHash:" ${TX_HASH} "Date:" ${DATE}