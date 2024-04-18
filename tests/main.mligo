#import "ligo-breathalyzer/lib/lib.mligo" "B"
#import "./test_wallet.mligo" "Wallet"
#import "./test_deployer.mligo" "Deployer"

let () =
  B.Model.run_suites Trace [
    Wallet.suite ;
    Deployer.suite
  ]
