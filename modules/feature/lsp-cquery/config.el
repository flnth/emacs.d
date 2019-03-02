;; modules/ui/lsp-cquery/config.el    -*- lexical-binding: t; -*-


(use-package cquery
  :ensure t
  :config

  (setq
   cquery-executable "/home/fthevissen/software/local/bin/cquery"
   cquery-cache-dir "/home/fthevissen/software/.cquery_cache"
   cquery-extra-init-params '(:index (:comments 2)
									 :cacheFormat "msgpack")
   )

  )
