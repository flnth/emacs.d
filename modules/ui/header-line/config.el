;; modules/ui/header-line/config.el -*- lexical-binding: t; -*-

(message "ui/header-line/config.el ...")

;; ⚠ ⏽                      
;;         
;; ⛁🗎       
;;         
;; https://github.com/Malabarba/spinner.el ? :)

;; magit --------------------------------------
;;  -> show current path?
;; git-commit-post-finish-hook (!),
;; (advice-add 'magit-checkout :after #'revert-buffer)
;; hook into git's post-checkout hook :)


(use-package ui/header-line/+header-line
  :after (feature/compilation/pjson s f projectile aquamacs-tabbar)

  ;; ... stuff in +header-line ...

  :config
  ;; (setq tabbar-default-header-line-format-function #'+header-line-format)
  )






