# Revenue Split Smart Contract

This smart contract validates Ada is split correctly between multiple address.

During compliation, a list of address and percentage pairs are passed in. When validating the disembursement transaction, the smart contractor ensures each address receives the correct percentage that was locked at the script address.

## Testing

Here's how to use these scripts:


## General

When in a shell, before running anything below, source the env vars for file for either mainnet or testnet, depending on which you're testing on.

For testnet

```
$ source testnet-env.envvars
```

For mainnet

```
$ source mainnet-env.envvars
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

If you're testing on the mainnet, you'll need to send some ada to that address from your wallet (or have someone else send it).

If you're testing on the testnet, you can go to the faucet <https://testnets.cardano.org/en/testnets/cardano/tools/faucet/> and send ada to that address.

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

First, we need to create the locking transaction. Before we can do that, we need to find a utxo, for the sender, that we can use.
We do that by running the `./scripts/query/sender.sh` script.

```
$ ./scripts/query/sender.sh
++ scripts/query/find-utxo.sh sender
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
6595d9126749b6df408578be6cdeef1ef22f17ebab057a3b198d18501d0a4d96     0        1000000000 lovelace + TxOutDatumHashNone
```

From this, we can see there is a single transaction with a single output. The utxo address is thus `6595d9126749b6df408578be6cdeef1ef22f17ebab057a3b198d18501d0a4d96#0`.

Now we're ready to run the locking transaction.

```
$ ./scripts/happy-path/share/lock-tx.sh 6595d9126749b6df408578be6cdeef1ef22f17ebab057a3b198d18501d0a4d96#0
```

Assuming you didn't see any errors, you can now look for the utxo for the script (which is needed in the next step)

```
$ ./scripts/query/sc.sh
+++ cat scripts/testnet/revenue-sharing.addr
++ cardano-cli query utxo --address addr_test1wq952y8g67s3v5kwk3n5kqc2q5uzumkf7djrh5gfmz95lns7wuzrk --testnet-magic 1097911063
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a     1        1724100 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "5e9d8bac576e8604e7c3526025bc146f5fa178173e3a5592d122687bd785b520"
```

### Unlocking transaction

For creating the transaction to share the ada sent we need two utxos: the first is the script utxo, as found at the end of the previous section (`cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a#1`).

The second is a utxo to cover fees and collateral. We use the same utxo for both. Find a utxo to use from the sender

```
$ ./scripts/query/sender.sh
++ scripts/query/find-utxo.sh sender
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a     0        998110179 lovelace + TxOutDatumHashNone
```

Given the above, we'll use the utxo `cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a#0` in this example.

Now we can create the revenue sharing transaction which runs the validator

```
$ ./scripts/happy-path/share/share-tx.sh cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a#1 cd9a08c297353218b532bf110091ebaa4623d4775af6819029340911068a6b0a#0
```

Assuming everything went well, and you give it a little time, you should be able to query the script address (`./scripts/query/sc.sh`) and see the utxo you used is gone. You should also be able to check the other addresses have received the funds

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
