# Revenue Split Smart Contract

This smart contract validates Ada is split correctly between multiple addresses.

During compilation, a list of address and percentage pairs are passed in. When validating the disbursement transaction, the smart contractor ensures each address receives the correct percentage that was locked at the script address.

## Building

To build, run:

```bash
$ cabal build
```

A `shell.nix` is also providing for nix users.

## Installing the Smart Contract Generator

This library includes an executable to build the smart contracts. To install it to a specific directory, run:

```
$ cabal install exe:create-revenue-split-sc --install-method=copy --installdir=YOUR_INSTALLATION_DIR
```

Where `YOUR_INSTALLATION_DIR` is a directory of your choosing.

## Compiling the Smart Contracts

Every unique revenue split requires a custom smart contract. To compile a smart contract, use the provided executable `create-revenue-split-sc`.

Running `create-revenue-split-sc --help` gives:

```bash
Usage: create-revenue-split-sc (--to <public-key-hash>:<pct>) --output FILE
  Create a smart contract for revenue sharing

Available options:
  --to <public-key-hash>:<pct>
                           Address to send to and the percent to send. Can
                           appear multiple times. Percentages are times 10, e.g.
                           2.5% would be 25. Percentages must add up to 100%
                           (i.e. 1000).
  --output FILE            Where to write the script.
  -h,--help                Show this help text
```

To create a smart contract, you must provide several public key hashes and percentages and percentages. The percentages are specified as integers as 10, so 2.5% would be 25.

For example, the following splits revenue between three parties, where the first receives 50% the second 45% and the third receives 5%:

```bash
$ create-revenue-split-sc \
    --to 7d719f67ba959af1ad21e636d037ddd4399e1188e8d9b405ba89296f:500 \
    --to 09ea313c856b8efae22332fa73d503b73cf2c848d66dfc246f25c3a0:450 \
    --to 94e9ee57b311a5d0bcb9c112c1800571b8698d25fa77aba1039905f0:50 \
    --output SC_FILEPATH
```

Where `SC_FILEPATH` is the location you would like to store the .plutus cbor.

After the .plutus file is written, one must create a script address using the `cardano-cli`:

```bash
$ cardano-cli address build \
  --payment-script-file revenue-split.plutus --mainnet \
  --out-file revenue-split.addr
```

## Example Transactions

To split funds, Ada must be sent to the script address created in the step above.

Here is an example transaction:

```bash
cardano-cli transaction build \
    --alonzo-era \
    --mainnet \
    --tx-in 2a27d27eb9d32a3c98276eb65fbeba4d0e134679726f7af78521c403de08311e#0 \
    --tx-out "$(cat revenue-split.addr) + 10000000 lovelace" \
    --tx-out-datum-hash "$(cardano-cli transaction hash-script-data --script-data-value 12)" \
    --change-address addr1v85teypffelqjaa92t6s363qhzcfkdplcfeh6e0pr9k48mc20wq09 \
    --protocol-params-file protocol-parameters.json \
    --out-file locking-tx-body.txt
```

A couple of things to point out. First, we are sending Ada to the address we created earlier, e.g. `revenue-split.addr`. Next, we included a required datum hash. The datum is ignored by the script, so the exact value we are hashing does not matter. However, it must be a valid hash, and the datum needs to be passed into the unlocking transaction.

Here is an example of unlocking transaction corresponding to above locking transaction:

```bash
cardano-cli transaction build \
    --alonzo-era \
    --mainnet \
    --tx-in 2b81720f2cb268ff0827ba6f5858e7ce82e1fc4a14f4c8effcafa389acaad55b#1 \
    --tx-in-script-file revenue-split.plutus \
    --tx-in-datum-value 12 \
    --tx-in-redeemer-value 0 \
    --tx-in bedfb6a1729598dc5af08d29ebf0e7b3c73a86db4a2dc5316fe6fd7873f64946#0 \
    --required-signer ~/keys/splitter.skey \
    --tx-in-collateral bedfb6a1729598dc5af08d29ebf0e7b3c73a86db4a2dc5316fe6fd7873f64946#0 \
    --tx-out "addr1v8dg6hwygkphs4x0f3uwqx0jyywcarvhaquf0f2pzamf2ac7nzw0f + 5000000 lovelace" \
    --tx-out "addr1v9ptfru625resx2rnw4csqfz0y99lecem97a4vqfnhhvk7qen3w2m + 4500000 lovelace" \
    --tx-out "addr1v85teypffelqjaa92t6s363qhzcfkdplcfeh6e0pr9k48mc20wq09 + 500000 lovelace" \
    --change-address addr1vyzwagqvqhd4q5swq67e60fm7dcrtcal4t96z0gea39zrgqjjtcvh \
    --protocol-params-file protocol-parameters.json \
    --out-file unlocking-tx-body.txt
```

A few things to note. We are passing in the same datum we hashed for the locking transaction, e.g. `--tx-in-datum-value 12`. Another thing to note, we have an extra input UTxO to cover the transaction costs. Additionally, we are passing in a redeemer of `0`. This is arbitrary, any integer would work here.

Similar example transactions can be found in the `scripts` folder, which is used for testing, as described below.

## Testing

Here's how to use these scripts:


## General

When in a shell, before running anything below, source the env vars for file for either mainnet or testnet, depending on which you're testing on.

For testnet

```
$ source scripts/envars/testnet-env.envvars
```

For mainnet

```
$ source scripts/envars/mainnet-env.envvars
```

## Init (only done once)

First create the wallets, get the protocol parameters, compile the plutus, and create the script address

```
$ ./scripts/wallets/make-all-wallets.sh
$ ./scripts/query-protocol-parameters.sh
$ ./scripts/compile.sh
$ ./scripts/hash-script.sh
```

## Make sure the `sender` has funds

If you just created the wallets, find the sender address (it will be different then the example value below).

```
$ cat ~/$BLOCKCHAIN_PREFIX/sender.addr
addr_test1vz2wnmjhkvg6t59uh8q39svqq4cms6vdyha802apqwvstuq80a88a
```

If you're testing on the mainnet, you'll need to send some Ada to that address from your wallet (or have someone else send it).

If you're testing on the testnet, you can go to the faucet <https://testnets.cardano.org/en/testnets/cardano/tools/faucet/> and send Ada to that address.

Wait a bit and check that the funds are available

```
$ ./scripts/query/sender.sh
++ scripts/query/find-utxo.sh sender
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
6595d9126749b6df408578be6cdeef1ef22f17ebab057a3b198d18501d0a4d96     0        1000000000 lovelace + TxOutDatumHashNone
```

If you don't see any transactions, wait a bit longer and try again.

Once we have wallets and the sender has funds, we're ready for testing.

## Test happy path

### Locking transaction

First, we need to create the locking transaction. Before we can do that, we need to find a UTxO, for the sender, that we can use.
We do that by running the `./scripts/query/sender.sh` script.

```
$ ./scripts/query/sender.sh
++ scripts/query/find-utxo.sh sender
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
6595d9126749b6df408578be6cdeef1ef22f17ebab057a3b198d18501d0a4d96     0        1000000000 lovelace + TxOutDatumHashNone
```

From this, we can see there is a single transaction with a single output. The UTxO address is thus `6595d9126749b6df408578be6cdeef1ef22f17ebab057a3b198d18501d0a4d96#0`.

Now we're ready to run the locking transaction.

```
$ ./scripts/happy-path/lock-tx.sh 6595d9126749b6df408578be6cdeef1ef22f17ebab057a3b198d18501d0a4d96#0
```

Assuming you didn't see any errors, you can now look for the UTxO for the script (which is needed in the next step)

```
$ ./scripts/query/sc.sh
+++ cat scripts/testnet/revenue-sharing.addr
++ cardano-cli query utxo --address addr_test1wq952y8g67s3v5kwk3n5kqc2q5uzumkf7djrh5gfmz95lns7wuzrk --testnet-magic 1097911063
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a     1        1724100 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "5e9d8bac576e8604e7c3526025bc146f5fa178173e3a5592d122687bd785b520"
```

### Unlocking transaction

For creating the transaction to share the ada sent, we need two UTxOs: the first is the script UTxO, as found at the end of the previous section (`cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a#1`).

The second is a UTxO to cover fees and collateral. We use the same UTxO for both. Find a UTxO to use from the sender

```
$ ./scripts/query/sender.sh
++ scripts/query/find-utxo.sh sender
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a     0        998110179 lovelace + TxOutDatumHashNone
```

Given the above, we'll use the UTxO `cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a#0` in this example.

Now we can create the revenue sharing transaction which runs the validator

```
$ ./scripts/happy-path/share-tx.sh cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a#1 cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a#0
```

Assuming everything went well, and you give it a little time, you should be able to query the script address (`./scripts/query/sc.sh`) and see the UTxO you used is gone. You should also be able to check the other addresses have received the funds

```
$ ./scripts/query/fifty.sh && ./scripts/query/thirtythreethree.sh && ./scripts/query/sixteenseven.sh
++ scripts/query/find-utxo.sh fifty
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
9d3d154d005054894695b08e97f08ef9e065a87d6cb3742e9d5cba355d4afecc     1        3448200 lovelace + TxOutDatumNone
++ scripts/query/find-utxo.sh thirtythreethree
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
9d3d154d005054894695b08e97f08ef9e065a87d6cb3742e9d5cba355d4afecc     2        2296501 lovelace + TxOutDatumNone
++ scripts/query/find-utxo.sh sixteenseven
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
9d3d154d005054894695b08e97f08ef9e065a87d6cb3742e9d5cba355d4afecc     3        1151698 lovelace + TxOutDatumNone
```

## Negative Test

To demonstrate that the validator fails when the split is invalid follow the instructions for locking funds above.

After the funds are at the script address query the senders address for a UTxO to cover the fees:

```
$ ./scripts/query/sender.sh
++ scripts/query/find-utxo.sh sender
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a     0        998110179 lovelace + TxOutDatumHashNone
```

Next, find the UTxO for the funds at the script address:

```
$ ./scripts/query/sc.sh
+++ cat scripts/testnet/revenue-sharing.addr
++ cardano-cli query utxo --address addr_test1wq952y8g67s3v5kwk3n5kqc2q5uzumkf7djrh5gfmz95lns7wuzrk --testnet-magic 1097911063
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a     1        1724100 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "5e9d8bac576e8604e7c3526025bc146f5fa178173e3a5592d122687bd785b520"
```

Finally execute the negative, which should fail:

```
./scripts/failure-cases/invalid-split.sh cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a#1 cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a#0
...
...
Command failed: transaction build  Error: The following scripts have execution failures:
the script for transaction input 1 (in the order of the TxIds) failed with:
The Plutus script evaluation failed: An error has occurred:  User error:
The provided Plutus code called 'error'.
Script debugging logs: Not all addresses were paid the correct amount
PT5
```
