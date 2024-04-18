#import "ligo-breathalyzer/lib/lib.mligo" "B"
#import "../src/deployer.mligo" "Deployer"
#import "../src/main.mligo" "SC"

let suite = B.Model.suite "Suite for deployer contract" [
  B.Model.case
    "deploys"
    "deploys a smart contract wallet and registers the address for the correct owner"
    (fun level ->
      let (_, (actor, owner, _)) = B.Context.init_default () in
      let contract =
        B.Contract.originate
          level
          "deployer"
          (contract_of Deployer)
          Deployer.empty_storage
          0tez
      in
      B.Result.reduce [
        B.Context.call_as
          actor
          contract
          (Deploy owner.address);
        let storage = B.Contract.storage_of contract in
        B.Assert.is_some_and
          "the wallet address should have been registered"
          (fun address ->
            let wallet_contract = (Tezos.get_contract address : (SC parameter_of) contract) in
            B.Assert.succeeds)
          (Big_map.find_opt owner.address storage)
      ])
]
