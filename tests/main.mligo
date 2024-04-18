#import "ligo-breathalyzer/lib/lib.mligo" "B"
#import "./test_wallet.mligo" "Wallet"

let () =
  B.Model.run_suites Trace [
    Wallet.suite
  ]
