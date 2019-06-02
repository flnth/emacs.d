;; modules/feature/dap/config.el    -*- lexical-binding: t; -*-

;; ---------------------------------------------------------
;; this requires vscode and an extension to be installed:
;;    https://github.com/WebFreak001/code-debug
;;
;; (TODO: actually, is it possible to just store the content somewhere and NOT
;; install vscode?)
;;
;; (dap-register-debug-template "GDB::Run"
;; 							 (list :type "gdb"
;; 								   :request "launch"
;; 								   :name "GDB::Run"
;; 								   :gdbpath "/usr/bin/gdb"
;; 								   :target nil
;; 								   :printCalls t
;; 								   :valuesFormatting "prettyPrinters"
;; 								   :showDevDebugOutput nil
;; 								   :autorun (list "target remote /dev/ttyb")
;; 								   :cwd nil))

;;  this is for launching, might also need separate one for attaching, maybe,
;;  for remote debugging. Or just call "target remote" in the autorun above.

;; check here for package:
;; https://github.com/WebFreak001/code-debug/blob/master/package.json#L72

