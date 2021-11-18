set -eux
./scripts/wallets/make-wallet.sh $1
./scripts/wallets/make-pkh.sh $1
