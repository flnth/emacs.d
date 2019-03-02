;;; dbus-interface.el                                  -*- lexical-binding: t -*-
;;
;; Filename: dbus-interface.el
;;
;; Copyright (C) 2018 Florian Thevißen
;;
;; Description: A simple dbus interface for emacs
;; Author: Florian Thevißen <mail@florian-thevissen.de>
;; Keywords: lisp, tools
;; Created: Sun Oct 28 17:15:36 2018 (+0100)
;; Version: 0.1.0
;; Package-Required: TODO
;; URL: TODO
;;
;; -----------------------------------------------------------------------------
;;; Commentary
;;
;; Custom tools for accessing files/buffers/commands, and performing actions on
;; them.
;;
;; -----------------------------------------------------------------------------

(message "loading dbus-interface ...")

(setq +dbus-top-introspect-string
  "<node name='/'>
  	<interface name='org.freedesktop.DBus.Introspectable'>
  		<method name='Introspect'>
  			<arg name='xml_data' type='s' direction='out'/>
  		</method>
  	</interface>
  	<interface name='org.test'>
  		<method name='testfun'>
  			<arg name='' direction='out' type='s' />
  		</method>
  	</interface>
  	<interface name='org.test2'>
  	</interface>
  </node>")

(defun +dbus-add-interface-spec (spec)
  "Declares xml interface SPEC in the form of
'<interface name='iname'>
	<method name='fun'>
		<arg name=''>...
	</method>
 </interface>'
to the top-level emacs dbus interface."

  ;; parse spec
  ;; parse +dbus-top-introspect-string
  ;; insert spec into it
  ;; write +dbus-top-introspect-string back out
  )

;; (setq parsed
;; 	  (with-temp-buffer
;; 		(insert (s-replace "\n" "" s))
;; 		(xml-parse-region (point-min) (point-max))))


;; (require 'xml-to-string)
;; (set ss (s-replace "\"" "'" (xml-to-string parsed)))





(message "... done!")
(provide 'access)
