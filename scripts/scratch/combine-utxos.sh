set -eux

bodyFile=temp/consolidate-tx-body.01
outFile=temp/consolidate-tx.01

cardano-cli transaction build \
  --alonzo-era \
  $BLOCKCHAIN \
  --tx-in 23576e992b36755f4da74b3b7ed9c2a6aef5615a9adb5f1b2b42ff6135120081#0 \
  --tx-in 30cc31d95bd555ab4d962f7c16e97ed16bd309906a102b9b9251a9bc1d5be6b2#0 \
  --tx-in 4b84f95d6c1a1912f20f160757138e4c53db173e5250d498ce000577a6ab3d0b#0 \
  --change-address $(cat ~/$BLOCKCHAIN_PREFIX/sender.addr) \
  --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
  --out-file $bodyFile

cardano-cli transaction sign \
   --tx-body-file $bodyFile \
   --signing-key-file ~/$BLOCKCHAIN_PREFIX/sender.skey\
   $BLOCKCHAIN \
   --out-file $outFile


cardano-cli transaction submit \
 $BLOCKCHAIN \
 --tx-file $outFile
