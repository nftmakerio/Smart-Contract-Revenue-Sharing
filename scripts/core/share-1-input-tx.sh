set -eux

baseDir="scripts"

# arguments:
#   script utxo
#   wallet address file
#   signing key file

bodyFile=temp/share-tx-body.01
outFile=temp/share-tx.01
validatorFile="$baseDir/$BLOCKCHAIN_PREFIX/revenue-sharing.plutus"
scriptHash=$(cat $baseDir/$BLOCKCHAIN_PREFIX/revenue-sharing.addr)
scriptUtxo=$1
collateralUtxo=$2
senderAddr=$3
fiftyAddr=$4
thirtythreethreeAddr=$5
sixteensevenAddr=$6
signingKey=$7
fiftyAmount="$8"
thirtythreethreeAmount="$9"
sixteensevenAmount="${10}"

echo "bodyFile: $bodyFile"
echo "outFile: $outFile"
echo "validatorFile: $validatorFile"
echo "signing key file: $signingKey"
echo "scriptHash $scriptHash"
echo

cardano-cli transaction build \
    --alonzo-era \
    $BLOCKCHAIN \
    --tx-in $utxoScript \
    --tx-in-script-file $validatorFile \
    --tx-in $collateralUtxo \
    --tx-in-collateral $collateralUtxo \
    --required-signer $signingKey \
    --tx-out "$fiftyAddr + $fiftyAmount lovelace" \
    --tx-out "$thirtythreethreeAddr + $thirtythreethreeAmount lovelace" \
    --tx-out "$sixteensevenAddr + $sixteensevenAmount lovelace" \
    --change-address $senderAddr \
    --protocol-params-file $baseDir/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
    --out-file $bodyFile

echo "saved transaction to $bodyFile"

cardano-cli transaction sign \
   --tx-body-file $bodyFile \
   --signing-key-file $signingKey \
   $BLOCKCHAIN \
   --out-file $outFile

echo "signed transaction and saved as $outFile"

cardano-cli transaction submit \
  $BLOCKCHAIN \
  --tx-file $outFile

echo "submitted transaction"

echo
