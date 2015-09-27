(require 'json)

(defcustom btc-network "testnet"
  "String of the current bitcoin network to use, mainnet or testnet. Defaults to test."
  :type 'string :group 'bitcoin)

(defvar btc-units "satoshis")

(defconst satoshi-per-btc 100000000.0)

(defun addresses ()
  (let ((command (format "bitcoin-cli -%s getaddressesbyaccount ''" btc-network)))
    (json-read-from-string (shell-command-to-string command))))

(defun first-address ()
  (elt (addresses) 0))

(defun balance ()
  (shell-command-to-string (format "bitcoin-cli -%s getbalance" btc-network)))

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

(defun btc-testnet ()
  (interactive)
  (setq btc-network "testnet"))

(defun btc-mainnet ()
  (interactive)
  (setq btc-network "mainnet"))

;; Public wallet interface

(defun-out btc-balance (balance))

(defun-out btc-first-address (first-address))

(defun btc-send (address amount)
  (interactive "sEnter address to send to: \nnEnter amount to send: ")
  (let* ((amount (if (string= btc-units "satoshis") (/ amount satoshi-per-btc) amount))
         (command (format "bitcoin-cli -%s sendfrom '' %s %.8f" btc-network address amount))
         (tx (shell-command-to-string command)))
    (kill-new tx)
    (message (format "Sent %.8f BTC, see tx %s" amount tx))))
