cardano-cli address build \
  --payment-script-file scripts/$BLOCKCHAIN_PREFIX/revenue-sharing.plutus \
  $BLOCKCHAIN \
  --out-file scripts/$BLOCKCHAIN_PREFIX/revenue-sharing.addr
