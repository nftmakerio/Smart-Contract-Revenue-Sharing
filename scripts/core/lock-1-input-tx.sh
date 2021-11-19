set -eux

baseDir="scripts"

# arguments:
#   input utxo
#   wallet address file
#   signing key file

$baseDir/hash-script.sh

bodyFile=temp/lock-tx-body.01
outFile=temp/lock-tx.01
validatorFile="$baseDir/$BLOCKCHAIN_PREFIX/revenue-sharing.plutus"
scriptHash=$(cat $baseDir/$BLOCKCHAIN_PREFIX/revenue-sharing.addr)

utxo="$1"
walletAddr="$2"
signingKey="$3"
value="$4"
datumHash="$(cardano-cli transaction hash-script-data --script-data-value 12)"

echo "bodyFile: $bodyFile"
echo "outFile: $outFile"
echo "validatorFile: $validatorFile"
echo "scriptHash $scriptHash"
echo "utxo: $utxo"
echo "walletAddress: $walletAddr"
echo "signing key file: $signingKey"
echo "value: $value"
echo


cardano-cli transaction build \
    --alonzo-era \
    $BLOCKCHAIN \
    --tx-in $utxo \
    --tx-out "$scriptHash + $value" \
    --tx-out-datum-hash "$datumHash" \
    --change-address $walletAddr \
    --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
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
