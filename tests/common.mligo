#import "ligo-breathalyzer/lib/lib.mligo" "B"
#import "../src/main.mligo" "Main"

let originate_empty level owner =
  B.Contract.originate
    level
    "session_contract"
    (contract_of Main)
    (Main.empty_storage owner)
    (0tez)

let init level =
  let (_, (owner, other, _)) = B.Context.init_default () in
  let contract = originate_empty level owner.address in
  owner, other, contract

let assume (type a) bytes : a = match (Bytes.unpack bytes : a option) with
  | None -> failwith "Assertion error: wrong type"
  | Some x -> x

(* Remove this when Test.Next.Account.new is fixed *)
let new_session (() : unit) =
    let (sk, pk) = [%external ("TEST_NEW_ACCOUNT", ())] in
    let addr = [%michelson ({| { HASH_KEY ; IMPLICIT_ACCOUNT ; ADDRESS } |} pk : address)] in
    { addr ; pk ; sk }

