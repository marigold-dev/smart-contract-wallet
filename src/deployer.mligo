#import "./main.mligo" "SC"

type storage = (address, address) big_map

let empty_storage : storage = Big_map.empty

[@entry]
let deploy (owner: address) (storage: storage) : operation list * storage =
  let (initial_storage: SC.storage) = {
    owner = owner;
    conditions = Big_map.empty
  }
  in
  let contract_op, address =
    [%create_contract_of_file "../sc-wallet.tz"]
    None
    0tez
    initial_storage
  in
  [contract_op], (Big_map.update owner (Some address) storage)
