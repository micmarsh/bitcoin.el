(require 'json)

(defcustom btc-network "testnet"
  "String of the current bitcoin network to use, mainnet or testnet. Defaults to test. NOTE: electrum doesn't have a testnet"
  :type 'string :group 'bitcoin)

(defcustom btc-client "electrum"
  "String of the current bitcoin cli to utilize"
  :type 'string :group 'bitcoin)

(defvar btc-units "satoshis")

(defconst satoshi-per-btc 100000000.0)

(defun addresses-command (network client)
  (cond ((string= "bitcoind" client)
         (format "bitcoin-cli -%s getaddressesbyaccount ''" network))
        ((string= "electrum" client)
         "electrum listaddresses")))

(defun addresses ()
  (let ((command (addresses-command btc-network btc-client)))
    (json-read-from-string (shell-command-to-string command))))

(defun first-address ()
  (elt (addresses) 0))

(defun balance-response (network client)
  (cond ((string= "bitcoind" client)
         (shell-command-to-string (format "bitcoin-cli -%s getbalance" network)))
        ((string= "electrum" client)
         (gethash "confirmed"
                  (let ((json-object-type 'hash-table))
                    (json-read-from-string (shell-command-to-string "electrum getbalance")))))))

(defun balance ()
  (balance-response btc-network btc-client))

(defun send-command (network client address amount)
  (cond ((string= "bitcoind" client)
         (format "bitcoin-cli -%s sendfrom '' %s %.8f" btc-network address amount))
        ((string= "electrum" client)
         (format "electrum payto %s %.8f" address amount))))

(defmacro defun-out (name body)
  `(defun ,name ()
     (interactive)
     (let ((result ,body))
       (kill-new result)
       (message result))))

;; Public network reading and mutation

(defun btc-network ()
  (interactive)
  (message btc-network))

(defun btc-client ()
  (interactive)
  (message btc-client))

(defun btc-electrum ()
  (interactive)
  (setq btc-client "electrum"))

(defun btc-bitcoind ()
  (interactive)
  (setq btc-client "bitcoind"))

(defun btc-testnet ()
  (interactive)
  (setq btc-network "testnet"))

(defun btc-mainnet ()
  (interactive)
  (setq btc-network "mainnet"))

;; Public wallet interface

(defun-out btc-balance (balance))

(defun-out btc-address (first-address))

(defun btc-send (address amount)
  (interactive "sEnter address to send to: \nnEnter amount to send: ")
  (let* ((amount (if (string= btc-units "satoshis") (/ amount satoshi-per-btc) amount))
         (command (send-command btc-network btc-client address amount))
         (tx (shell-command-to-string command)))
    (kill-new tx)
    (message (format "Sent %.8f BTC, see tx %s" amount tx))))
