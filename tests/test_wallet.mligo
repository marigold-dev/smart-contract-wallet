#import "ligo-breathalyzer/lib/lib.mligo" "B"
#import "../src/main.mligo" "Main"
#include "./test_contracts.mligo"
#include "./common.mligo"

let lambda1 : Main.user_condition = fun (_, x) -> [], x
let lambda2 : Main.user_condition = user_condition
let lambda3 : Main.user_condition =
  fun (packed_op, storage) ->
    let i = (assume storage : int) in
    if i < 3 then
      let ops = (assume packed_op : unit Main.serialized_ops) in
      let ops = List.map Main.transaction ops in
      ops, Bytes.pack (i+1)
    else
      failwith "This has been called too many times"

let suite = B.Model.suite "Suite for Ligo wallet sessions" [
  B.Model.case
    "set_condition"
    "registers a condition and a session key"
    (fun level ->
      let owner, _other, sc_wallet = init level in
      let session = new_session () in
      B.Result.reduce [
        let time = Tezos.get_now () + 100 in
        B.Context.call_as
          owner
          sc_wallet
          (Set_condition (session.pk, lambda1, 0x, time));
        let storage = B.Contract.storage_of sc_wallet in
        B.Assert.is_true
          "the session has been registered"
          (Option.is_some (Big_map.find_opt session.pk storage.conditions))
      ]);

  B.Model.case
    "set_condition_with_signature"
    "registers a condition and a session key when correctly signed"
    (fun level ->
      let owner, other, sc_wallet = init level in
      let session = new_session () in
      let time = Tezos.get_now () + 100 in
      let packed_data = Bytes.pack (session.pk, lambda1, 0x, time) in
      let signature = Test.Next.Crypto.sign owner.secret packed_data in
      B.Result.reduce [
        B.Context.call_as
          other
          sc_wallet
          (Set_condition_with_signature (owner.key, packed_data, signature));
        let storage = B.Contract.storage_of sc_wallet in
        B.Assert.is_true
          "the session has been registered"
          (Option.is_some (Big_map.find_opt session.pk storage.conditions))
      ]);

  B.Model.case
    "set_condition_with_signature"
    "fails when the key provided does not match the owner's"
    (fun level ->
      let owner, other, sc_wallet = init level in
      let session = new_session () in
      let time = Tezos.get_now () + 100 in
      let packed_data = Bytes.pack (session.pk, lambda1, 0x, time) in
      let signature = Test.Next.Crypto.sign owner.secret packed_data in
      B.Result.reduce [
        B.Expect.fail_with_message
          "Wrong signature"
          (B.Context.call_as
            other
            sc_wallet
            (Set_condition_with_signature (other.key, packed_data, signature)));
      ]);

  B.Model.case
    "set_condition_with_signature"
    "fails when the packed data is not signed by the owner"
    (fun level ->
      let owner, other, sc_wallet = init level in
      let session = new_session () in
      let time = Tezos.get_now () + 100 in
      let packed_data = Bytes.pack (session.pk, lambda1, 0x, time) in
      let signature = Test.Next.Crypto.sign other.secret packed_data in
      B.Result.reduce [
        B.Expect.fail_with_message
          "Wrong signature"
          (B.Context.call_as
            other
            sc_wallet
            (Set_condition_with_signature (owner.key, packed_data, signature)));
      ]);

  B.Model.case
    "relay_check"
    "succeeds in calling another contract when the condition succeeds"
    (fun level ->
      let owner, _other, sc_wallet = init level in
      let example_contract = originate_example level in
      let session = new_session () in
      let byts = make_bytes1 example_contract.originated_contract in
      let signature = Test.Next.Crypto.sign session.sk byts in
      B.Result.reduce [
        let time = Tezos.get_now () + 100 in
        B.Context.call_as
          owner
          sc_wallet
          (Set_condition (session.pk, lambda2, 0x, time));
        B.Context.call_as
          owner
          sc_wallet
          (Relay_check (session.pk, signature, byts));
        let storage = B.Contract.storage_of example_contract in
        B.Assert.is_equal "storage has been modified" storage 1
      ]);

  B.Model.case
    "relay_check"
    "successfully updates the storage when the condition returns a new storage"
    (fun level ->
      let owner, _other, sc_wallet = init level in
      let empty_ops = Bytes.pack ([] : (address * (TestContract1 parameter_of) * nat) list) in
      let session = new_session () in
      let signature = Test.Next.Crypto.sign session.sk empty_ops in
      B.Result.reduce [
        let time = Tezos.get_now () + 100 in
        B.Context.call_as
          owner
          sc_wallet
          (Set_condition (session.pk, lambda3, (Bytes.pack 0), time));
        B.Context.call_as
          owner
          sc_wallet
          (Relay_check (session.pk, signature, empty_ops));
        let storage = B.Contract.storage_of sc_wallet in
        B.Assert.is_some_and
          "condition storage has been modified"
          (fun (_, condition_storage, _) ->
            B.Assert.is_equal "equal to 1" condition_storage (Bytes.pack 1))
          (Big_map.find_opt session.pk storage.conditions)
      ]);

  B.Model.case
    "relay_check"
    "fails when the session expired"
    (fun level ->
      let owner, _other, sc_wallet = init level in
      let example_contract = originate_example level in
      let session = new_session () in
      let byts = make_bytes1 example_contract.originated_contract in
      let signature = Test.Next.Crypto.sign session.sk byts in
      B.Result.reduce [
        let expiration_time = Tezos.get_now () + 100 in
        B.Context.call_as
          owner
          sc_wallet
          (Set_condition (session.pk, lambda2, 0x, expiration_time));
        B.Context.wait_for 100n;
        B.Expect.fail_with_message
          "This session has expired"
          (B.Context.call_as
            owner
            sc_wallet
            (Relay_check (session.pk, signature, byts)));
        let storage = B.Contract.storage_of example_contract in
        B.Assert.is_equal "storage has not been modified" storage 0
      ]);


  B.Model.case
    "relay_direct"
    "succeeds when called by the owner of the wallet"
    (fun level ->
      let owner, _other, sc_wallet = init level in
      let example_contract = originate_example level in
      let addr = Tezos.address example_contract.originated_contract in
      B.Result.reduce [
        B.Context.call_as
          owner
          sc_wallet
          (Relay_direct
            (fun () ->
              let entry = Bar 5 in
              let (contract: (TestContract2 parameter_of) contract) =
                Tezos.get_contract addr
              in
              [Tezos.Next.Operation.transaction entry 0tez contract]
            ));
        let storage = B.Contract.storage_of example_contract in
        B.Assert.is_equal "storage has been modified" storage (-1)
      ]);

  B.Model.case
    "relay_direct"
    "fails when called by another user"
    (fun level ->
      let _owner, other, sc_wallet = init level in
      let example_contract = originate_example level in
      let addr = Tezos.address example_contract.originated_contract in
      B.Result.reduce [
        B.Expect.fail_with_message
          "Not the owner"
          (B.Context.call_as
            other
            sc_wallet
            (Relay_direct
              (fun () ->
                let entry = Bar 5 in
                let (contract: (TestContract2 parameter_of) contract) =
                  Tezos.get_contract addr
                in
                [Tezos.Next.Operation.transaction entry 0tez contract]
              )))
      ]);
]

