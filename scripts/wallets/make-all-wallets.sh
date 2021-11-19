set -eux
mkdir -p scripts/temp/
mkdir -p ~/$BLOCKCHAIN_PREFIX
mkdir -p scripts/$BLOCKCHAIN_PREFIX
mkdir -p temp
./scripts/wallets/make-wallet-and-pkh.sh sender
./scripts/wallets/make-wallet-and-pkh.sh fifty
./scripts/wallets/make-wallet-and-pkh.sh thirtythreethree
./scripts/wallets/make-wallet-and-pkh.sh sixteenseven
