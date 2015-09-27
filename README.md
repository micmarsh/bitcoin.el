# Bitcoin For Emacs

A wrapper around `bitcoin-cli` commands for use from emacs.

## Usage
Not in any kind of package manager yet.

In your `.emacs. file
```elisp
(load "path/to/bitcoin.el")

(setq btc-network "testnet")
;; can also change network with command below
```

## Commands

### Wallet
* `btc-balance` prints and copies to kill ring the current balance of the wallet
* `btc-address` prints and copies an address from the wallet
* `btc-send` prompts for an address and amount (in satoshis), sends to address, copies transaction id to kill ring

### Network
* `btc-network` print the current network (test or main)
* `btc-mainnet` set network to main
* `btc-testnet` set network to testnet

