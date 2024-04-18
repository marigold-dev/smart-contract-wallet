# Smart contract wallet

A prototype of a smart contract wallet.

## Deployer contract

A deployer contract helper is available in `src/deployer.mligo`. This small smart contract assumes
that the main contract has been deployed with:

```
ligo compile contract src/main.mligo > sc-wallet.tz
```
