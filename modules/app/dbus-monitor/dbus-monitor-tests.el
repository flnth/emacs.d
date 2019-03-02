;;; dbus-monitor-tests.el                             -*- lexical-binding: t -*-
;;
;; Filename: dbus-monitor-tests.el
;;

(eval-and-compile
  (require 'ert nil t)
  (require 'ert "lib/ert"))

(require 'app/dbus-monitor/dbus-monitor)

;; test:
;;   - register 2 services
;;   - test all 4 possibilities
;;   - see that what should happen, happens. Maybe that is not entirely testable though...

(ert-deftest dbus-monitor-test/a ()
  (setf +dbus-monitor--services nil)
  (+dbus-monitor-add-service (+dbus-monitor-service-create
							  :name "net.pydbus.ClientServerExample"
							  :executable "/home/fthevissen/cloud/41    Code/python/dbus/qt_server.py"
							  :supervise t
							  :policy 'keep-alive
							  ))

  (let ((SS (car +dbus-monitor--services)))
	(setq S SS)

	;; ensure service is not running
	(+dbus-monitor-service-stop SS)
	(sit-for 0.3)
	(should (equal nil (+dbus-monitor-all-running-p)))

	;; start service
	(+dbus-monitor-service-start SS)
	(sit-for 0.3)
	(should (equal t (+dbus-monitor-all-running-p)))

	;; stop service
	(+dbus-monitor-service-stop SS)
	)

  ;; stop service


  )


(progn
	(ert "dbus-monitor-test")
	nil)

;; (ert "whist")
