#import "ligo-breathalyzer/lib/lib.mligo" "B"
#import "../src/main.mligo" "Main"

let originate_empty level owner =
  let empty_storage: Main.storage = {
    owner = owner;
    registry = Big_map.empty;
  }
  in
  B.Contract.originate
    level
    "session_contract"
    (contract_of Main)
    empty_storage
    (0tez)

let init level =
  let (_, (owner, other, _)) = B.Context.init_default () in
  let contract = originate_empty level owner.address in
  owner, other, contract

let lambda1 : Main.user_filter = fun _ -> []

module Ex2 = struct
  module Contract = struct
    type storage = int

    [@entry]
    let foo (_: string) storage: operation list * storage =
      [], storage + 1

    [@entry]
    let bar (_: int) storage: operation list * storage =
      [], storage - 1
  end

  type entrypoints = Contract parameter_of

  let hd (type a) (xs: a list) =
    match xs with
      | [] -> failwith "List.hd"
      | x::_xs -> x

  let foo s = Foo s

  let user_condition bytes =
    match (Bytes.unpack bytes: (address * entrypoints * nat) list option) with
      | None -> failwith "big mistake"
      | Some serialized_ops ->
        let (addr, entry, n) = hd serialized_ops in
        let contract = (Tezos.get_contract addr: entrypoints contract) in
        [Tezos.transaction entry (n * 1mutez) contract]

  let user_direct addr () =
    let contract = (Tezos.get_contract addr: entrypoints contract) in
    [Tezos.transaction (Bar 5) 0tez contract]

  let make_bytes1 (contract: entrypoints contract) =
    let address = Tezos.address contract in
    Bytes.pack [(address, Foo "foo", 0n)]

  (* FIXME not sure why this is needed but I couldn't do it outside of the module *)
  let call_contract originated_address : operation list =
    let entry = Bar 5 in
    let (contract : entrypoints contract) = Tezos.get_contract originated_address in
    [Tezos.transaction entry 0tez contract]

  let originate_example level =
    let empty_storage: Contract.storage = 0 in
    B.Contract.originate
      level
      "example_contract"
      (contract_of Contract)
      empty_storage
      (0tez)
end

let lambda2 = Ex2.user_condition

(* Remove this when Test.Next.Account.new is fixed *)
let new (() : unit) =
    let (sk, pk) = [%external ("TEST_NEW_ACCOUNT", ())] in
    let addr = [%michelson ({| { HASH_KEY ; IMPLICIT_ACCOUNT ; ADDRESS } |} pk : address)] in
    { addr ; pk ; sk }

let suite = B.Model.suite "Suite for Ligo wallet sessions" [
  B.Model.case
    "set_filter"
    "registers a filter and a session key"
    (fun level ->
      let owner, other, sc_wallet = init level in
      let session = new () in
      B.Result.reduce [
        let time = Tezos.get_now () + 100 in
        B.Context.call_as
          owner
          sc_wallet
          (Set_filter (session.pk, lambda1, time));
        let storage = B.Contract.storage_of sc_wallet in
        B.Assert.is_true
          "the session has been registered"
          (Option.is_some (Big_map.find_opt session.pk storage.registry))
      ]);

  B.Model.case
    "set_filter_with_signature"
    "registers a filter and a session key when correctly signed"
    (fun level ->
      let owner, other, sc_wallet = init level in
      let session = new () in
      let time = Tezos.get_now () + 100 in
      let packed_data = Bytes.pack (session.pk, lambda1, time) in
      let signature = Test.Next.Crypto.sign owner.secret packed_data in
      B.Result.reduce [
        B.Context.call_as
          other
          sc_wallet
          (Set_filter_with_signature (owner.key, packed_data, signature));
        let storage = B.Contract.storage_of sc_wallet in
        B.Assert.is_true
          "the session has been registered"
          (Option.is_some (Big_map.find_opt session.pk storage.registry))
      ]);

  B.Model.case
    "set_filter_with_signature"
    "fails when the key provided does not match the owner's"
    (fun level ->
      let owner, other, sc_wallet = init level in
      let session = new () in
      let time = Tezos.get_now () + 100 in
      let packed_data = Bytes.pack (session.pk, lambda1, time) in
      let signature = Test.Next.Crypto.sign owner.secret packed_data in
      B.Result.reduce [
        B.Expect.fail_with_message
          "Wrong signature"
          (B.Context.call_as
            other
            sc_wallet
            (Set_filter_with_signature (other.key, packed_data, signature)));
      ]);

  B.Model.case
    "set_filter_with_signature"
    "fails when the packed data is not signed by the owner"
    (fun level ->
      let owner, other, sc_wallet = init level in
      let session = new () in
      let time = Tezos.get_now () + 100 in
      let packed_data = Bytes.pack (session.pk, lambda1, time) in
      let signature = Test.Next.Crypto.sign other.secret packed_data in
      B.Result.reduce [
        B.Expect.fail_with_message
          "Wrong signature"
          (B.Context.call_as
            other
            sc_wallet
            (Set_filter_with_signature (owner.key, packed_data, signature)));
      ]);

  B.Model.case
    "relay_check"
    "succeeds in calling another contract when the condition is right"
    (fun level ->
      let owner, other, sc_wallet = init level in
      let example_contract = Ex2.originate_example level in
      let session = new () in
      let byts = Ex2.make_bytes1 example_contract.originated_contract in
      let signature = Test.Next.Crypto.sign session.sk byts in
      B.Result.reduce [
        let time = Tezos.get_now () + 100 in
        B.Context.call_as
          owner
          sc_wallet
          (Set_filter (session.pk, lambda2, time));
        B.Context.call_as
          owner
          sc_wallet
          (Relay_check (session.pk, signature, byts));
        let storage = B.Contract.storage_of example_contract in
        B.Assert.is_equal "storage has been modified" storage 1
      ]);

  B.Model.case
    "relay_check"
    "fails when the session expired"
    (fun level ->
      let owner, other, sc_wallet = init level in
      let example_contract = Ex2.originate_example level in
      let session = new () in
      let byts = Ex2.make_bytes1 example_contract.originated_contract in
      let signature = Test.Next.Crypto.sign session.sk byts in
      B.Result.reduce [
        let expiration_time = Tezos.get_now () + 100 in
        B.Context.call_as
          owner
          sc_wallet
          (Set_filter (session.pk, lambda2, expiration_time));
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
      let owner, other, sc_wallet = init level in
      let example_contract = Ex2.originate_example level in
      B.Result.reduce [
        B.Context.call_as
          owner
          sc_wallet
          (Relay_direct (Ex2.user_direct example_contract.originated_address));
        let storage = B.Contract.storage_of example_contract in
        B.Assert.is_equal "storage has been modified" storage (-1)
      ]);

]

let () =
  B.Model.run_suites Trace [
    suite
  ]
