set -eux

scripts/core/share-1-input-tx.sh $1 $2 \
  $(cat ~/$BLOCKCHAIN_PREFIX/sender.addr) \
  $(cat ~/$BLOCKCHAIN_PREFIX/fifty.addr) \
  $(cat ~/$BLOCKCHAIN_PREFIX/thirtythreethree.addr) \
  $(cat ~/$BLOCKCHAIN_PREFIX/sixteenseven.addr) \
  ~/$BLOCKCHAIN_PREFIX/sender.skey \
  862050 \
  574125 \
  287924
