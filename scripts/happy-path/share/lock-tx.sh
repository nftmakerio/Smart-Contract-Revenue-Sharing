set -eux

scripts/core/lock-1-input-tx.sh $1 \
  $(cat ~/$BLOCKCHAIN_PREFIX/sender.addr) \
  ~/$BLOCKCHAIN_PREFIX/sender.skey \
  "1724100 lovelace"
