;; modules/app/dbus-monitor/config.el -*- lexical-binding: t; -*-

(use-package app/dbus-monitor/dbus-monitor
  :after (dbus)
  :commands (+dbus-monitor-show)
  ;; :bind (:map evil-motion-state-map
  ;; 			  ("" . acc-ido-switch-buffer)
  ;; 			  )
  :config

  (progn
	(+dbus-monitor-add-service
	 (+dbus-monitor-service-create :name "net.pydbus.ClientServerExample"))
	)

  )



