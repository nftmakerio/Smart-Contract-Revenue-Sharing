set -eux

scripts/core/share-1-input-tx.sh $1 $2 \
  $(cat ~/$BLOCKCHAIN_PREFIX/sender.addr) \
  $(cat ~/$BLOCKCHAIN_PREFIX/fifty.addr) \
  $(cat ~/$BLOCKCHAIN_PREFIX/thirtythreethree.addr) \
  $(cat ~/$BLOCKCHAIN_PREFIX/sixteenseven.addr) \
  ~/$BLOCKCHAIN_PREFIX/sender.skey \
  3348200 \
  2496501 \
  1151698
