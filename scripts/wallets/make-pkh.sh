set -eux

cardano-cli address key-hash --payment-verification-key-file ~/$BLOCKCHAIN_PREFIX/$1.vkey \
 > scripts/$BLOCKCHAIN_PREFIX/$1-pkh.txt
