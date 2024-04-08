
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

(* User filters, or conditions, expect bytes, as we don't know beforehand the type
 of the entrypoints of other contracts. *)
(* TODO: extendable conditions, storage *)
type user_filter = bytes -> operation list
type user_action = unit -> operation list

type storage = {
  owner: address;
  registry: (key, user_filter * timestamp) big_map;
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

(* REMOVE ME? *)
[@inline]
let deserialize (packed_ops: bytes) : operation list =
  match (Bytes.unpack packed_ops: user_action option) with
    | None -> failwith "Wrong serialized operations"
    | Some ops_f -> ops_f ()

[@entry]
let set_filter
  (k, f, t: key * user_filter * timestamp)
  (storage: storage): operation list * storage =
  let _ = assert_owner storage in
  let storage = {
    storage with
      registry = Big_map.add k (f, t) storage.registry;
  }
  in
  [], storage

[@entry]
let set_filter_with_signature
  (registry_key, owner_key, packed_filter, s: key * key * bytes * signature)
  (storage: storage): operation list * storage =
  match (Bytes.unpack packed_filter: (user_filter * timestamp) option) with
    | None -> failwith "Wrong bytes"
    | Some (f, t) ->
      if not (Crypto.check owner_key s packed_filter) then
        failwith "Wrong signature"
      else if key_to_address owner_key <> storage.owner then
        failwith "The key does not match the owner"
      else
        let storage = {
          storage with
            registry = Big_map.add registry_key (f, t) storage.registry;
        }
        in
        [], storage

(* Check that serialized operations have been correctly signed by a session key
 and that they are valid with respect to a predefined condition. *)
[@entry]
let relay_check
  (key, signature, packed_ops: key * signature * bytes)
  (storage: storage): operation list * storage =
  match Big_map.find_opt key storage.registry with
    | None -> failwith "Session not found"
    | Some (f, t) ->
      if Tezos.get_now () > t then
        failwith "This session has expired"
      else if not (Crypto.check key signature packed_ops) then
        failwith "Wrong signature"
      else
        let ops = f packed_ops in
        ops, storage

(* Directly relay a lambda from the user. *)
[@entry]
let relay_direct (user_action: user_action) (storage: storage): operation list * storage =
  if Tezos.get_sender () <> storage.owner then
    failwith "Only the smart contract owner can call this entrypoint"
  else
    user_action (), storage
