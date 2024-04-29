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

let rev (type a) (xs : a list) : a list =
  let rec rev (type a) (xs : a list) (acc : a list) : a list =
    match xs with
      | [] -> []
      | y::ys -> rev ys (y :: acc)
  in
  rev xs []

let split_list (type a) (xs : a list) =
  let rec aux (type a) (xs : a list) (acc : a list) : a list * a =
    match xs with
      | [y] -> rev acc, y
      | y::ys -> aux ys (y :: acc)
      | [] -> failwith "The list has no last element"
  in match xs with
    | [] -> failwith "The list has no first element"
    | head::rest ->
      let heart, tail = aux rest [] in
      (head, heart, tail)

let rec concat (type a) (xs : a list) (ys : a list) : a list =
  match xs with
    | [] -> ys
    | x::xs -> x :: concat xs ys
