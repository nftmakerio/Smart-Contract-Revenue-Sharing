set -eux

baseDir="scripts"

# arguments:
#   input utxo
#   wallet address file
#   signing key file
#   policyId
#   token name

utxoScript=$1
utxoBuyer=$2
bodyFile=buy-tx-body.01
outFile=buy-tx.01
nftValidatorFile="$baseDir/buy-cancel.plutus"
nftPolicyId=$7
value="1 $nftPolicyId.$8"
buyerAddr=$3
sellerAddr=$4
marketplaceAddr=$5
signingKey=$6
sellerAmount=$9
marketPlaceAmount="${10}"
scriptHash=$(cat $baseDir/buy-cancel.addr)
datumFile="$baseDir/buy-cancel-datum.json"
# datumFile="$baseDir/buy-cancel-datum-minimum-price.json"
# datumFile="$baseDir/buy-cancel-datum-minimum-fee.json"
redeemerFile="$baseDir/buy-redeemer.json"

echo "utxoScript: $utxoScript"
echo "utxoBuyer: $utxoBuyer"
echo "bodyFile: $bodyFile"
echo "outFile: $outFile"
echo "nftValidatorFile: $nftValidatorFile"
echo "nftPolicyId: $nftPolicyId"
echo "value: $value"
echo "buyerAddress: $buyerAddr"
echo "sellerAddress: $sellerAddr"
echo "marketplaceAddr: $marketplaceAddr"
echo "signing key file: signingKey"
echo "scriptHash $scriptHash"
echo "sellerAmount $sellerAmount"
echo "marketPlaceAmount $marketPlaceAmount"
echo

echo "querying protocol parameters"
$baseDir/mainnet-query-protocol-parameters.sh

echo

cardano-cli transaction build \
    --alonzo-era \
    $BLOCKCHAIN \
    --tx-in $utxoBuyer \
    --tx-in $utxoScript \
    --tx-in-script-file $nftValidatorFile \
    --tx-in-datum-file $datumFile \
    --tx-in-redeemer-file $redeemerFile \
    --required-signer $signingKey \
    --tx-in-collateral $utxoBuyer \
    --tx-out "$sellerAddr + $sellerAmount lovelace" \
    --tx-out "$buyerAddr + 1758582 lovelace" \
    --tx-out "$marketplaceAddr + $marketPlaceAmount lovelace" \
    --change-address $buyerAddr \
    --protocol-params-file mainnet-protocol-parameters.json \
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
