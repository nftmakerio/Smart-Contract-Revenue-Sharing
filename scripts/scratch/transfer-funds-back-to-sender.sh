set -eux

bodyFile=temp/consolidate-tx-body.01
outFile=temp/consolidate-tx.01

fiftySigningKey=~/$BLOCKCHAIN_PREFIX/fifty.skey
sixteensevenSigningKey=~/$BLOCKCHAIN_PREFIX/sixteenseven.skey
thirtythreethreeSigningKey=~/$BLOCKCHAIN_PREFIX/thirtythreethree.skey

cardano-cli transaction build \
  --alonzo-era \
  $BLOCKCHAIN \
  --tx-in 30cc31d95bd555ab4d962f7c16e97ed16bd309906a102b9b9251a9bc1d5be6b2#1 \
  --tx-in 30cc31d95bd555ab4d962f7c16e97ed16bd309906a102b9b9251a9bc1d5be6b2#2 \
  --tx-in 30cc31d95bd555ab4d962f7c16e97ed16bd309906a102b9b9251a9bc1d5be6b2#3 \
  --change-address $(cat ~/$BLOCKCHAIN_PREFIX/sender.addr) \
  --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
  --out-file $bodyFile

echo "saved transaction to $bodyFile"

cardano-cli transaction sign \
   --tx-body-file $bodyFile \
   --signing-key-file $fiftySigningKey \
   --signing-key-file $sixteensevenSigningKey \
   --signing-key-file $thirtythreethreeSigningKey \
   $BLOCKCHAIN \
   --out-file $outFile

echo "signed transaction and saved as $outFile"

cardano-cli transaction submit \
 $BLOCKCHAIN \
 --tx-file $outFile

echo "submitted transaction"
