#import "ligo-breathalyzer/lib/lib.mligo" "B"
#import "./test_wallet.mligo" "Wallet"
#import "./test_deployer.mligo" "Deployer"
#import "./test_fa2.mligo" "FA2"

let () =
  B.Model.run_suites Trace [
    Wallet.suite ;
    Deployer.suite ;
    FA2.suite
  ]
