set -eux
mkdir -p scripts/temp/
mkdir -p ~/$BLOCKCHAIN_PREFIX
./scripts/wallets/make-wallet-and-pkh.sh sender
./scripts/wallets/make-wallet-and-pkh.sh fifty
./scripts/wallets/make-wallet-and-pkh.sh thirtythreethree
./scripts/wallets/make-wallet-and-pkh.sh sixteenseven
