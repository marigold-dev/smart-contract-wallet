#import "ligo-breathalyzer/lib/lib.mligo" "B"
#import "../src/main.mligo" "Main"
#include "./common.mligo"

module TestContract1 = struct
  type storage = unit

  [@entry]
  let a () s : operation list * storage =
    [], s
end

module TestContract2 = struct
  type storage = int

  [@entry]
  let foo (_: string) storage: operation list * storage =
    [], storage + 1

  [@entry]
  let bar (_: int) storage: operation list * storage =
    [], storage - 1
end

type entrypoints = TestContract2 parameter_of

let hd (type a) (xs: a list) =
  match xs with
    | [] -> failwith "List.hd"
    | x::_xs -> x

let foo s = Foo s

let user_condition bytes (storage: bytes) =
  let serialized_ops = (assume bytes : entrypoints Main.serialized_ops) in
  let ops = List.map Main.transaction serialized_ops in
  ops, storage

let make_bytes1 (contract: entrypoints contract) =
  let address = Tezos.address contract in
  Bytes.pack [(address, Foo "foo", 0n)]

let originate_example level =
  let empty_storage: TestContract2.storage = 0 in
  B.Contract.originate
    level
    "example_contract"
    (contract_of TestContract2)
    empty_storage
    (0tez)


