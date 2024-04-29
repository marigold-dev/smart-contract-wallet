#import "ligo-breathalyzer/lib/lib.mligo" "B"
#import "../ligo-fa/lib/main.mligo" "FA2"
#import "../src/main.mligo" "SC"
#include "./common.mligo"

(* TODO:
- tester si on peut autoriser un opérateur en douce
- tester si on peut autoriser un opérateur depuis le SC wallet et faire le transfert en douce
- tester si on peut forcer une séquence à commencer et finir par update operator *)

module FA2 = FA2.MultiAsset

let dummy_token_data (token_id : nat) : FA2.TZIP12.tokenMetadataData =
  let dummy_token_info = (Map.empty : (string, bytes) map) in
  {token_id=token_id; token_info=dummy_token_info}

let initial_storage owner =
  let empty = FA2.empty_storage in
  let token_metadata = Big_map.literal [
      (1n, dummy_token_data(1n));
      (2n, dummy_token_data(2n));
      (3n, dummy_token_data(3n));
  ]
  in
  {
    empty with
      token_metadata = token_metadata;
      ledger = Big_map.add (owner, 1n) 10n empty.ledger;
  }

let originate_fa2 owner level =
  B.Contract.originate
    level
    "fa2_contract"
    (contract_of FA2)
    (initial_storage owner)
    0tez

(* Spending limit for 100_000 tokens *)
let spending_limit (fa2_address : address) : SC.user_condition =
  fun (ser_ops, storage : bytes * bytes) ->
    let counter = (assume storage : nat) in
    let ops = (assume ser_ops : bytes list) in
    let head, hearts, tail = split_list ops in
    let head : FA2 parameter_of = Update_operators [(assume head : FA2.TZIP12.unit_update)] in
    let tail : FA2 parameter_of = Update_operators [(assume tail : FA2.TZIP12.unit_update)] in
    let hearts : FA2 parameter_of = Transfer (List.map (fun x -> (assume x : FA2.TZIP12.transfer_from)) hearts) in
    let operations =
      List.map
        (fun entrypoint ->
         let contract = (Tezos.get_contract fa2_address : FA2 parameter_of contract) in
         Tezos.Next.Operation.transaction entrypoint 0tez contract)
        [head; hearts; tail]
    in
    operations, Bytes.pack counter

let suite = B.Model.suite "Advanced tests for conditions on FA2 contracts" [

  B.Model.case
    ""
    ""
    (fun level ->
      let owner, other, wallet = init level in
      let fa2 = originate_fa2 owner.address level in
      let fa2_address = fa2.originated_address in
      let init_storage = Bytes.pack 0n in
      let session = new_session () in
      let time = Tezos.get_now () + 100 in
      let transfer : FA2.TZIP12.transfer  = [({
          from_=wallet.originated_address;
          txs=[{to_=other.address;amount=50000n;token_id=0n}]
      })]
      in
      let sequence : bytes list = [
        Bytes.pack
          (Add_operator {
            owner = wallet.originated_address;
            operator = wallet.originated_address;
            token_id = 0n
          });
        Bytes.pack transfer;
        Bytes.pack
          (Remove_operator {
            owner = wallet.originated_address;
            operator = wallet.originated_address;
            token_id = 0n
          });
        ]
      in
      let serialized_transfer = Bytes.pack (sequence) in
      let signature = Test.Next.Crypto.sign session.sk serialized_transfer in
      B.Result.reduce [
        B.Context.call_as
          owner
          wallet
          (Set_condition (session.pk, spending_limit fa2_address, init_storage, time));
        B.Context.call_as
          other
          wallet
          (Relay_check (session.pk, signature, serialized_transfer));
        B.Assert.is_true "true" true
      ])
]

