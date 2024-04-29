(* POC smart contract wallet
 *
 * Design considerations:
 * - only one implicit account per SC wallet, as some target contracts could rely on SENDER
 * - the application is expected to generate a key for the duration of the session to prevent
 *   attacks from outsider or from the gas station
 * - this is still different from just using a session based on a temporary key, as the SC
 *   wallet is persistent
 * - several keys can be used at the same time, with different conditions, so that the SC
 *   wallet is shared between different dapps
*)

(* User conditions, expect bytes, as we don't know beforehand the type
   of the entrypoints of other contracts. *)

module Condition_storage = struct
  type t = bytes
end

type user_condition = bytes * Condition_storage.t -> operation list * Condition_storage.t

(* Direct actions, sent by the user to the wallet *)
type user_action = unit -> operation list

type 'a serialized_op = address * 'a * nat
type 'a serialized_ops = 'a serialized_op list

let transaction (type a) (op: (address * a * nat)) =
  let (address, entrypoint, amount) = op in
  let contract = (Tezos.get_contract address: a contract) in
  Tezos.Next.Operation.transaction entrypoint (amount * 1mutez) contract

type storage = {
  owner: address;
  conditions: (key, user_condition * Condition_storage.t * timestamp) big_map;
}

let empty_storage owner : storage = {
  owner = owner;
  conditions = Big_map.empty;
}

[@inline]
let assert_owner (storage: storage) =   (* TODO check if annotation mandatory *)
  let sender = Tezos.get_sender () in
  if sender <> storage.owner then
    failwith "Not the owner"

(* Laurent please put this in the stdlib *)
[@inline]
let key_to_address (pk: key): address =
  Tezos.address (Tezos.implicit_account (Crypto.hash_key pk))

[@entry]
let set_condition
  (key, condition, initial_storage, t: key * user_condition * Condition_storage.t * timestamp)
  (storage: storage): operation list * storage =
  let _ = assert_owner storage in
  let storage = {
    storage with
      conditions = Big_map.add key (condition, initial_storage, t) storage.conditions;
  }
  in
  [], storage

(* This entrypoint may be called by a different sender than the owner, hence
   the session key must be part of the signed bytes *)
[@entry]
let set_condition_with_signature
  (owner_key, packed_condition, s: key * bytes * signature)
  (storage: storage): operation list * storage =
  match (Bytes.unpack packed_condition: (key * user_condition * Condition_storage.t * timestamp) option) with
    | None -> failwith "Wrong bytes"
    | Some (session_key, condition, initial_storage, t) ->
      if not (Crypto.check owner_key s packed_condition) then
        failwith "Wrong signature"
      else if key_to_address owner_key <> storage.owner then
        failwith "The key does not match the owner"
      else
        let storage = {
          storage with
            conditions = Big_map.add session_key (condition, initial_storage, t) storage.conditions;
        }
        in
        [], storage

(* Check that serialized operations have been correctly signed by a session key
 and that they are valid with respect to a predefined condition. *)
[@entry]
let relay_check
  (key, signature, packed_ops: key * signature * bytes)
  (storage: storage): operation list * storage =
  match Big_map.find_opt key storage.conditions with
    | None -> failwith "Session not found"
    | Some (condition, condition_storage, t) ->
      if Tezos.get_now () > t then
        failwith "This session has expired"
      else if not (Crypto.check key signature packed_ops) then
        failwith "Wrong signature"
      else
        let ops, new_storage = condition (packed_ops, condition_storage) in
        let storage = {
          storage with
            conditions = Big_map.update key (Some (condition, new_storage, t)) storage.conditions
        }
        in
        ops, storage

(* Directly relay a lambda from the user. *)
[@entry]
let relay_direct (user_action: user_action) (storage: storage): operation list * storage =
  let _ = assert_owner storage in
  user_action (), storage
