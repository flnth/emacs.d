;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path nil
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(restclient
     ruby
     ocaml
     java
     scala
     ;; helm
     (ivy :Variables
          ivy-height 6
          ivy-use-selectable-prompt t
          ivy-use-virtual-buffers t
          ivy-virtual-abbreviate 'full
          ivy-wrap t
          )
     rust
     racket
     sml
     (haskell :variables haskell-enable-hindent-style "johan-tibell")
     finance
     html
     octave
     calendar
     vimscript
     ;; sr-speedbar
     csv
     ;; org
     nlinum
     ;; vim-powerline
     evil-snipe
     dash
     python
     yaml
     markdown
     sql
     ipython-notebook
     python
     ;; notmuch
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion
      (haskell :variables haskell-completion-backend 'intero)
      :variables
      auto-completion-return-key-behavior nil
      auto-completion-enable-help-tooltip t
      auto-completion-tab-key-behavior 'complete
      auto-completion-complete-with-key-sequence nil
      auto-completion-complete-with-key-sequence-delay 0.5
      auto-completion-private-snippets-directory nil
      :disabled-for org git)
     better-defaults
     emacs-lisp
     git
     markdown
     (org :variables
          org-enable-github-support t)
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom
     ;;        shell-default-shell 'eshell
     ;;        shell-enable-smart-eshell)  ;
     ;; spell-checking
     ;; syntax-checking
     version-control
     ;;eyebrowse
     javascript
     java
     jabber
     pdf-tools
     ;; realgud
     ;; (mu4e   :variables
     ;;         mu4e-installation-path "/home/fthevissen/software/mu/mu4e")
     (c-c++ :variables
            c-c++-enable-clang-support nil
            c-c++-default-mode-for-headers 'c++-mode)
     ;;gnus
     cscope
     fasd
     colors
     ;; spacemacs-cmake-ide
     ;; semantic
     emms
     (latex :variables
            latex-enable-folding t)
     ;; (elfeed :variables
     ;;         rmh-elfeed-org-files (list (concat (getenv "DIR_ORG") "/.rss.org")))
     ;; window-purpose
	 spacemacs-purpose
     ;; (gtags :variables gtags-enable-by-default nil)
	 common-lisp
	 treemacs
	 plantuml
	 (erc :variables
		  erc-enable-sasl-auth t)
	 jabber
	 lua
	 ;; lsp
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      yascroll
                                      evil-textobj-column
                                      evil-text-object-python
                                      highlight-quoted
                                      rtags
                                      flycheck
                                      ;; smart-mode-line
                                      ;; smart-mode-line-powerline-theme
                                      all-the-icons
                                      mode-icons
                                      ;; flycheck-pos-tip      ;; pos-tip not working :(
                                      ;; simpleclip
                                      ;; xclip
                                      ;;drag-stuff
                                      ;;outorg
                                      ;;outshine
                                      ;;emamux
                                      ob-ipython
                                      org-plus-contrib
                                      sml-mode
                                      sml-mode
                                      muse
                                      visual-fill-column
                                      ;; offlineimap
                                      outshine
                                      navi-mode
                                      python-cell
                                      elfeed
                                      elfeed-org
                                      elfeed-web
									  nav-flash
									  posframe
									  ivy-posframe
									  company-childframe
									  multi-compile
									  cmake-mode
									  helpful
									  modern-cpp-font-lock
									  helm-org-rifle
									  beacon
									  highlight-indent-guides
									  face-explorer
									  morlock
									  shadchen
									  treepy
									  page-break-lines
									  smart-jump
									  ag
									  paren-face
									  znc
									  sr-speedbar
									  ercn
									  org-fancy-priorities
									  org-super-agenda
									  highlight-defined
									  calfw-org
									  vertigo
									  anki-editor
									  ansible
									  feebleline
									  framesize
									  mu4e-conversation
									  mu4e-maildirs-extension
									  use-package
									  qml-mode
									  ansible
									  lsp-mode
									  lsp-ui
									  company-lsp
									  eglot
									  smartparens
									  find-file-in-project
									  pydoc
									  filladapt
									  magit-todos
									  zoom
									  request-deferred
									  org-attach-screenshot
									  helm-projectile
									  ccls
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
                                    org-pomodoro
                                    ;; org-bullets
                                    )
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only)
  )

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacsinitialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive lis
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the lastest
   ;; version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil
   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)   leuven,flatland,jbeans,hc-zenburn,darktooth!
   dotspacemacs-themes '(flatland)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("CodeNewRoman NF"
                               :size 15
                               :weight light
                               :style Book
                               :slant normal)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'any
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"
   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil
   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

;;  (require 'use-package)
;;  (require 'auth-source)
  ;; (auth-source-search :max 1 :host "irc.freenode.net")

  (message "\n----- loading custom configuration -------------------------------")


(add-to-list 'load-path (getenv "DIR_EMACSD"))
(add-to-list 'load-path (concat (getenv "DIR_EMACSD") "/modules"))

(let ((default-directory (getenv "DIR_EMACSD")))
  (normal-top-level-add-subdirs-to-load-path)
  )

(require 's)
(require 'dash)
(require 'window-purpose)
;;
;;  ;; -- core
(load "core/paths/config")
(load "core/utils/config")
(load "core/general/config")
(load "core/on-exit/config")
(load "core/on-startup/config")
(load "core/spacemacs/config")
;;
;;  ;; -- editor
(load "editor/code-folding/config")
(load "editor/general/config")
(load "editor/horizontal-rulers/config")
(load "editor/multiple-cursors/config")
(load "editor/scrolling/config")
;;
;;  ;; -- feature
(load "feature/general/config")
(load "feature/chat/config")
(load "feature/compilation/config")
(load "feature/diff/config")
(load "feature/eshell/config")
(load "feature/git-gutter/config")
(load "feature/helm/config")
(load "feature/history/config")
(load "feature/magit/config")
(load "feature/config-json/config")
(load "feature/porg/config")
(load "feature/org/config")
(load "feature/org-agenda/config")
(load "feature/printing/config")
(load "feature/redmine/config")
(load "feature/cmake/config")
(load "feature/plantuml/config")
 ;; -- ipc
 ;; (load "ipc/dbus-interface/config")
(load "feature/pm/config")
  ;; (require 'lsp-mode)
(load "feature/lsp/config")
  ;; (load "feature/lsp-supervisor/config")

;; -- lang
(load "lang/cc/config")
(load "lang/haskell/config")
(load "lang/java/config")
(load "lang/lisp/config")
 (load "lang/markup/config")
 (load "lang/python/config")
 (load "lang/web/config")
 (load "lang/debug-mode/config")
 (load "lang/ert/config")
;;
;;  ;; -- mail
;;  (load "mail/config")
;;
;;  ;; -- ui
(load "ui/access/config")
(load "ui/code-navigation/config")
(load "ui/completion/config")
(load "ui/evil/config")
(load "ui/general/config")
(load "ui/tabbar/config")
(load "ui/theme/config")
(load "ui/tmux/config")
(load "ui/tty/config")
(load "ui/tty-colors/config")
(load "ui/window-management/config")
(load "ui/window-navigation/config")
(load "ui/treemacs/config")
(load "ui/header-line/config")
(load "ui/prettify/config")
 (load "ui/dired/config")
;;
;;  ;; -- app
 (load "app/elfeed/config")
 (spacemacs/toggle-truncate-lines-on)
;;
) ;; ------------------------------------------------------------------------------
  (message "... modules loaded.")
  (message "--------------------------------------------------------------------\n")

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-emphasis-regexp "'\\(\\(.\\|\\\\n\\)*?\\)'")
 '(Info-fontify-angle-bracketed-flag t)
 '(Info-fontify-emphasis-flag t)
 '(Info-fontify-quotations-flag t)
 '(Man-notify-method 'pushy)
 '(Man-width 90)
 '(ac-auto-show-menu nil)
 '(ac-auto-start 1 t)
 '(ahs-edit-mode-off-hook nil)
 '(ahs-edit-mode-on-hook nil)
 '(ansi-color-names-vector
   ["#26292c" "#ac443d" "#83a748" "#fbcb41" "#afc4db" "#dc8cc3" "#93e0e3" "#f8f8f8"])
 '(anzu-cons-mode-line-p nil)
 '(anzu-mode-line-update-function 'spacemacs/anzu-update-mode-line)
 '(auto-compile-mode-line-counter t)
 '(auto-fill-inhibit-regexp nil)
 '(avy-all-windows t)
 '(avy-background nil)
 '(avy-style 'at-full)
 '(avy-timeout-seconds 0.2)
 '(beacon-blink-delay 0.005)
 '(beacon-blink-duration 0.025)
 '(beacon-blink-when-focused t)
 '(beacon-blink-when-point-moves-vertically nil)
 '(beacon-blink-when-window-scrolls nil)
 '(beacon-color "#ff0000")
 '(beacon-dont-blink-major-modes
   '(t magit-popup-mode inf-ruby-mode mu4e-headers-mode gnus-summary-mode gnus-group-mode))
 '(beacon-mode nil)
 '(beacon-size 30)
 '(bug-reference-bug-regexp "\\( #\\)\\([0-9]+\\(?:#[0-9]+\\)?\\)")
 '(c-basic-offset 4)
 '(c-default-style '((other . "bsd") (java-mode . "java") (awk-mode . "awk")))
 '(c-indent-comments-syntactically-p 'set-from-style)
 '(c-offsets-alist
   '((inline-open . +)
	 (inline-close . 0)
	 (func-decl-cont . 0)
	 (member-init-cont . fn--c++-member-init-cont)
	 (inher-cont . +)
	 (substatement . 0)
	 (substatement-open . 0)
	 (arglist-intro . ++)
	 (arglist-cont . 0)
	 (arglist-cont-nonempty +cc-arglist-cont-nonempty-align-if-args ++)
	 (innamespace . 0)
	 (template-args-cont . fn--c++-template-args-cont-align)))
 '(calendar-christian-all-holidays-flag nil)
 '(calendar-date-style 'iso)
 '(calendar-mark-diary-entries-flag t)
 '(calendar-mark-holidays-flag t)
 '(cmake-ide-build-dir "/home/fthevissen/work/cmake_example_project_build")
 '(cmake-ide-make-command "make -j8")
 '(comint-process-echoes nil)
 '(comint-scroll-show-maximum-output nil)
 '(comint-use-prompt-regexp nil)
 '(common-lisp-style-default nil)
 '(company-auto-complete t)
 '(company-backends
   '(company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
				  (company-dabbrev-code company-gtags company-etags company-keywords)
				  company-oddmuse company-dabbrev company-elisp))
 '(company-idle-delay 2)
 '(company-lua-interpreter 'lua52)
 '(company-quickhelp-delay 3)
 '(company-selection-wrap-around t)
 '(company-show-numbers nil)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-offset-display 'lines)
 '(compilation-always-kill t)
 '(compilation-auto-jump-to-first-error nil)
 '(compilation-context-lines 3)
 '(compilation-environment '("TERM=screen-256color"))
 '(compilation-scroll-output t)
 '(compilation-skip-threshold 2)
 '(compilation-window-height nil)
 '(confirm-nonexistent-file-or-buffer nil)
 '(counsel-describe-function-function 'helpful-function)
 '(counsel-describe-variable-function 'helpful-variable)
 '(counsel-org-headline-display-priority t)
 '(counsel-projectile-rg-initial-input '(ivy-thing-at-point))
 '(counsel-projectile-sort-buffers nil)
 '(counsel-projectile-sort-projects t)
 '(counsel-rg-base-command "rg -S --line-number --no-heading --color never %s .")
 '(custom-safe-themes
   '("b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "760441ae217f2b41f67b8c205bb0288507b21c0cff7d45d48bd3c26718dd65d7" "a2e7b508533d46b701ad3b055e7c708323fb110b6676a8be458a758dd8f24e27" default))
 '(delete-selection-mode t)
 '(desktop-save-mode t)
 '(dired-async-mode nil)
 '(dired-listing-switches "-haltg --group-directories-first --time-style=long-iso")
 '(display-buffer-alist
   '((popwin:display-buffer-condition popwin:display-buffer-action)
	 ("*Async Shell Command*" display-buffer-no-window
	  (nil))))
 '(display-buffer-base-action '((display-buffer-use-some-window)))
 '(display-time-24hr-format nil)
 '(display-time-default-load-average nil)
 '(display-time-mail-face nil)
 '(display-time-mode nil)
 '(dumb-jump-force-searcher 'rg)
 '(ediff-highlight-all-diffs t)
 '(ediff-use-faces t)
 '(electric-indent-mode t)
 '(elfeed-db-directory "/home/fthevissen/cloud/82    Notes/org/.elfeed")
 '(elfeed-goodies/entry-pane-position 'bottom)
 '(elfeed-goodies/entry-pane-size 0.65)
 '(elfeed-goodies/show-mode-padding 10)
 '(elfeed-goodies/switch-to-entry nil)
 '(enable-recursive-minibuffers t)
 '(erc-auto-query nil)
 '(erc-hide-list '("JOIN" "NICK" "PART" "QUIT" "MODE"))
 '(erc-hl-nicks-mode t)
 '(erc-modules
   '(log ring scrolltobottom image youtube completion hl-nicks netsplit fill button match track readonly networks ring autojoin noncommands irccontrols move-to-prompt stamp menu list ercn))
 '(erc-nickserv-identify-mode nil)
 '(erc-prompt-for-nickserv-password nil)
 '(erc-query-display 'buffer)
 '(erc-scrolltobottom-mode t)
 '(erc-services-mode nil)
 '(erc-track-exclude '("*irc-freenode*" "*irc-OFTC*" "&pidgin"))
 '(erc-track-exclude-server-buffer t)
 '(erc-track-shorten-cutoff 10)
 '(erc-track-shorten-function nil)
 '(erc-track-shorten-start 5)
 '(erc-track-switch-direction 'newest)
 '(erc-track-visibility 'selected-visible)
 '(eshell-variable-aliases-list
   '(("COLUMNS"
	  (lambda
		(indices)
		(-
		 (window-width)
		 15))
	  t)
	 ("LINES"
	  (lambda
		(indices)
		(window-height))
	  t)
	 ("_"
	  (lambda
		(indices)
		(if
			(not indices)
			(car
			 (last eshell-last-arguments))
		  (eshell-apply-indices eshell-last-arguments indices)))
	  nil)
	 ("?" eshell-last-command-status nil)
	 ("$" eshell-last-command-result nil)
	 ("0" eshell-command-name nil)
	 ("1"
	  (lambda
		(indices)
		(nth 0 eshell-command-arguments))
	  nil)
	 ("2"
	  (lambda
		(indices)
		(nth 1 eshell-command-arguments))
	  nil)
	 ("3"
	  (lambda
		(indices)
		(nth 2 eshell-command-arguments))
	  nil)
	 ("4"
	  (lambda
		(indices)
		(nth 3 eshell-command-arguments))
	  nil)
	 ("5"
	  (lambda
		(indices)
		(nth 4 eshell-command-arguments))
	  nil)
	 ("6"
	  (lambda
		(indices)
		(nth 5 eshell-command-arguments))
	  nil)
	 ("7"
	  (lambda
		(indices)
		(nth 6 eshell-command-arguments))
	  nil)
	 ("8"
	  (lambda
		(indices)
		(nth 7 eshell-command-arguments))
	  nil)
	 ("9"
	  (lambda
		(indices)
		(nth 8 eshell-command-arguments))
	  nil)
	 ("*"
	  (lambda
		(indices)
		(if
			(not indices)
			eshell-command-arguments
		  (eshell-apply-indices eshell-command-arguments indices)))
	  nil)))
 '(eval-sexp-fu-flash-error-duration 0.25)
 '(evil-emacs-state-modes
   '(org-brain-visualize-mode bbdb-mode biblio-selection-mode bookmark-bmenu-mode bookmark-edit-annotation-mode browse-kill-ring-mode bzr-annotate-mode calc-mode cfw:calendar-mode completion-list-mode custom-theme-choose-mode delicious-search-mode desktop-menu-blist-mode desktop-menu-mode dvc-bookmarks-mode dvc-diff-mode dvc-info-buffer-mode dvc-log-buffer-mode dvc-revlist-mode dvc-revlog-mode dvc-status-mode dvc-tips-mode ediff-meta-mode efs-mode Electric-buffer-menu-mode emms-browser-mode emms-mark-mode emms-metaplaylist-mode emms-playlist-mode ess-help-mode etags-select-mode fj-mode gc-issues-mode gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode gdb-locals-mode gdb-memory-mode gdb-registers-mode gdb-threads-mode gist-list-mode gnus-article-mode gnus-browse-mode gnus-group-mode gnus-server-mode gnus-summary-mode google-maps-static-mode jde-javadoc-checker-report-mode magit-popup-mode magit-popup-sequence-mode magit-branch-manager-mode magit-commit-mode magit-key-mode magit-rebase-mode magit-wazzup-mode mh-folder-mode monky-mode pdf-outline-buffer-mode pdf-view-mode proced-mode rcirc-mode rebase-mode recentf-dialog-mode reftex-select-bib-mode reftex-select-label-mode reftex-toc-mode slime-thread-control-mode sr-buttons-mode sr-mode sr-tree-mode sr-virtual-mode tetris-mode tla-annotate-mode tla-archive-list-mode tla-bconfig-mode tla-bookmarks-mode tla-branch-list-mode tla-browse-mode tla-category-list-mode tla-changelog-mode tla-follow-symlinks-mode tla-inventory-file-mode tla-inventory-mode tla-lint-mode tla-logs-mode tla-revision-list-mode tla-revlog-mode tla-tree-lint-mode tla-version-list-mode twittering-mode urlview-mode vm-mode vm-summary-mode w3m-mode wab-compilation-mode xgit-annotate-mode xgit-changelog-mode xgit-diff-mode xgit-revlog-mode xhg-annotate-mode xhg-log-mode xhg-mode xhg-mq-mode xhg-mq-sub-mode xhg-status-extra-mode))
 '(evil-ex-interactive-search-highlight 'selected-window)
 '(evil-ex-search-interactive t)
 '(evil-ex-substitute-highlight-all t)
 '(evil-jumps-max-length 10)
 '(evil-lisp-state-global t)
 '(evil-lisp-state-major-modes '(emacs-lisp-mode racket-mode))
 '(evil-lookup-func 'helpful-at-point)
 '(evil-magit-use-z-for-folds t)
 '(evil-magit-want-horizontal-movement nil)
 '(evil-mode-line-format 'before)
 '(evil-search-highlight-string-min-len 3)
 '(evil-surround-pairs-alist
   '((40 "(" . ")")
	 (91 "[ " . " ]")
	 (123 "{ " . " }")
	 (41 "(" . ")")
	 (93 "[" . "]")
	 (125 "{" . "}")
	 (35 "#{" . "}")
	 (98 "(" . ")")
	 (66 "{" . "}")
	 (62 "<" . ">")
	 (116 . evil-surround-read-tag)
	 (60 . evil-surround-read-tag)
	 (102 . evil-surround-function)))
 '(evil-want-C-i-jump nil)
 '(evil-want-C-u-scroll t)
 '(evil-want-Y-yank-to-eol nil)
 '(explicit-shell-file-name nil)
 '(eyebrowse-mode-line-right-delimiter "]")
 '(eyebrowse-mode-line-style 'hide)
 '(eyebrowse-new-workspace t)
 '(fci-rule-color "gray22")
 '(feebleline-show-directory nil)
 '(feebleline-show-linenum nil)
 '(fill-nobreak-predicate nil)
 '(fill-prefix nil)
 '(filladapt-token-conversion-table
   '((citation-> . exact)
	 (supercite-citation . exact)
	 (lisp-comment . exact)
	 (sh-comment . exact)
	 (postscript-comment . exact)
	 (c++-comment . exact)
	 (cc-multiline-continuation . exact)
	 (texinfo-comment . exact)
	 (bullet . spaces)
	 (space . exact)
	 (end-of-line . exact)))
 '(filladapt-token-match-table
   '((citation-> citation->)
	 (supercite-citation supercite-citation)
	 (lisp-comment lisp-comment)
	 (sh-comment sh-comment)
	 (postscript-comment postscript-comment)
	 (c++-comment c++-comment)
	 (cc-multiline-continuation cc-multiline-continuation)
	 (texinfo-comment texinfo-comment)
	 (bullet)
	 (space bullet space)
	 (beginning-of-line beginning-of-line)))
 '(filladapt-token-table
   '(("^" beginning-of-line)
	 (">+" citation->)
	 ("\\(\\w\\|[0-9]\\)[^'`\"< 	
]*>[ 	]*" supercite-citation)
	 (";+" lisp-comment)
	 ("#+" sh-comment)
	 ("%+" postscript-comment)
	 ("///*" c++-comment)
	 ("*" cc-multiline-continuation)
	 ("@c[ 	]" texinfo-comment)
	 ("@comment[ 	]" texinfo-comment)
	 ("\\\\item[ 	]" bullet)
	 ("[0-9]+\\.[ 	]" bullet)
	 ("[0-9]+\\(\\.[0-9]+\\)+[ 	]" bullet)
	 ("[A-Za-z]\\.[ 	]" bullet)
	 ("(?[0-9]+)[ 	]" bullet)
	 ("(?[A-Za-z])[ 	]" bullet)
	 ("[0-9]+[A-Za-z]\\.[ 	]" bullet)
	 ("(?[0-9]+[A-Za-z])[ 	]" bullet)
	 ("[-~*+]+[ 	]" bullet)
	 ("o[ 	]" bullet)
	 ("[ 	]+" space)
	 ("$" end-of-line)))
 '(form-feed-line-width t)
 '(frame-font-increment 5 t)
 '(fringe-mode '(8 . 10) nil (fringe))
 '(ggtags-enable-navigation-keys nil)
 '(ggtags-mode-sticky nil)
 '(git-commit-fill-column nil)
 '(git-commit-setup-hook
   '(git-commit-save-message git-commit-setup-changelog-support git-commit-propertize-diff with-editor-usage-message))
 '(git-commit-summary-max-length 80)
 '(git-gutter+-modified-sign ".")
 '(git-gutter-fr+-side 'left-fringe)
 '(git-gutter-fr:side 'left-fringe t)
 '(git-gutter:ask-p nil)
 '(git-gutter:visual-line t)
 '(global-highlight-parentheses-mode t)
 '(global-hl-line-mode nil)
 '(global-mark-ring-max 2)
 '(global-page-break-lines-mode t nil (page-break-lines))
 '(global-paren-face-mode t)
 '(global-vi-tilde-fringe-mode nil)
 '(gnus-cloud-method nil)
 '(gnus-posting-styles '(("gnus-outlook-style")))
 '(gnus-select-method
   '(nnmaildir "rwth"
			   (directory "~/.mail/rwth/INBOX")
			   (directory-files nnheader-directory-files-safe)
			   (get-new-mail nil)))
 '(go-back-boring-buffer-regexp-list
   '("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*Minibuf" "\\*Ibuffer\\*" "\\*Help\\*" "\\*RTags\\*"))
 '(golden-ratio-exclude-buffer-names
   '(" *which-key*" "*LV*" " *NeoTree*" "*Python*" "*trepan3k*"))
 '(haskell-indentation-layout-offset 4)
 '(haskell-indentation-left-offset 4)
 '(haskell-indentation-starter-offset 4)
 '(haskell-interactive-prompt "λ> ")
 '(haskell-interactive-prompt2 "λ| ")
 '(haskell-process-show-debug-tips nil)
 '(haskell-process-suggest-hayoo-imports t)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-use-presentation-mode t)
 '(hdefd-highlight-type 'fns-and-vars)
 '(helm-always-two-windows nil)
 '(helm-autoresize-max-height 35)
 '(helm-autoresize-min-height 35)
 '(helm-autoresize-mode nil)
 '(helm-completing-read-handlers-alist
   '((describe-function . helm-completing-read-symbols)
	 (describe-variable . helm-completing-read-symbols)
	 (describe-symbol . helm-completing-read-symbols)
	 (debug-on-entry . helm-completing-read-symbols)
	 (find-function . helm-completing-read-symbols)
	 (disassemble . helm-completing-read-symbols)
	 (trace-function . helm-completing-read-symbols)
	 (trace-function-foreground . helm-completing-read-symbols)
	 (trace-function-background . helm-completing-read-symbols)
	 (find-tag . helm-completing-read-with-cands-in-buffer)
	 (org-capture . helm-org-completing-read-tags)
	 (org-set-tags . helm-org-completing-read-tags)
	 (ffap-alternate-file)
	 (tmm-menubar)
	 (find-file)
	 (execute-extended-command)))
 '(helm-dash-browser-func 'eww-browse-url)
 '(helm-dash-docsets-path "/home/fthevissen/.local/share/Zeal/Zeal/docsets/")
 '(helm-display-buffer-default-height nil)
 '(helm-display-buffer-default-width nil)
 '(helm-display-buffer-height 20)
 '(helm-display-buffer-reuse-frame nil)
 '(helm-display-header-line nil)
 '(helm-display-source-at-screen-top nil)
 '(helm-echo-input-in-header-line t)
 '(helm-full-frame nil)
 '(helm-grep-ag-command
   "rg --vimgrep --color=always --no-heading --smart-case %s %s %s")
 '(helm-grep-default-command
   "rg --vimgrep --color=always --no-heading --smart-case %s %s %s
")
 '(helm-grep-default-recurse-command
   "rg --vimgrep --color=always --no-heading --smart-case %s %s %s
")
 '(helm-lisp-fuzzy-completion t)
 '(helm-mode nil)
 '(helm-scroll-amount nil)
 '(helm-split-window-inside-p t)
 '(helm-swoop-split-with-multiple-windows t t)
 '(helm-turn-on-show-completion nil)
 '(helm-use-frame-when-more-than-two-windows nil)
 '(helm-use-undecorated-frame-option nil)
 '(helpful-max-buffers 1)
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-indent-guides-method 'column)
 '(hl-paren-attributes nil)
 '(hl-paren-background-colors '("#171717"))
 '(hl-paren-colors '("firebrick1"))
 '(hl-paren-delay 0.01)
 '(hl-paren-highlight-adjacent t)
 '(hl-todo-keyword-faces
   '(("HOLD" . "#d0bf8f")
	 ("TODO" . org-todo)
	 ("NEXT" . "#dca3a3")
	 ("THEM" . "#dc8cc3")
	 ("PROG" . "#7cb8bb")
	 ("OKAY" . "#7cb8bb")
	 ("DONT" . "#5f7f5f")
	 ("FAIL" . "#8c5353")
	 ("DONE" . org-done)
	 ("NOTE" . "#d0bf8f")
	 ("KLUDGE" . "#d0bf8f")
	 ("HACK" . "#d0bf8f")
	 ("FIXME" . "#cc9393")
	 ("XXX" . "#cc9393")
	 ("XXXX" . "#cc9393")
	 ("???" . "#cc9393")))
 '(hlt-use-overlays-flag t)
 '(hs-hide-comments-when-hiding-all nil)
 '(htmlize-output-type 'inline-css)
 '(ido-max-window-height 5)
 '(ielm-prompt "ELISP> ")
 '(indent-tabs-mode t)
 '(indicate-buffer-boundaries nil)
 '(indicate-empty-lines nil)
 '(initial-buffer-choice t)
 '(ivy-count-format "%-4d ")
 '(ivy-display-function nil)
 '(ivy-display-style nil)
 '(ivy-dynamic-exhibit-delay-ms 10)
 '(ivy-fixed-height-minibuffer t)
 '(ivy-format-function 'ivy-format-function-default)
 '(ivy-height 6)
 '(ivy-height-alist
   '((counsel-evil-registers . 5)
	 (counsel-yank-pop . 5)
	 (counsel-git-log . 4)
	 (counsel--generic . 7)
	 (counsel-ag . 20)
	 (counsel-el . 7)))
 '(ivy-sort-functions-alist
   '((counsel-projectile-switch-project . +access--sort-projects-binary-predicate)
	 (read-file-name-internal . ivy-sort-file-function-default)
	 (internal-complete-buffer)
	 (ivy-completion-in-region)
	 (counsel-git-grep-function)
	 (Man-goto-section)
	 (org-refile)
	 (t . ivy-string<)
	 (persp-kill-buffer)
	 (persp-remove-buffer)
	 (persp-add-buffer)
	 (persp-switch)
	 (persp-window-switch)
	 (persp-frame-switch)))
 '(jit-lock-chunk-size 1500)
 '(keyboard-coding-system 'utf-8-unix)
 '(line-number-mode nil)
 '(line-spacing 0.0)
 '(linum-format "%4d ")
 '(lisp-align-keywords-in-calls nil)
 '(lisp-body-indent 2)
 '(lisp-extra-font-lock-quoted-face nil)
 '(lisp-extra-font-lock-quoted-function-face nil)
 '(lisp-indent-function 'lisp-indent-function)
 '(lisp-indent-offset nil)
 '(lisp-lambda-list-keyword-alignment nil)
 '(lisp-lambda-list-keyword-parameter-alignment t)
 '(lisp-lambda-list-keyword-parameter-indentation 2)
 '(lisp-loop-indent-body-forms-relative-to-loop-start t)
 '(lisp-loop-indent-forms-like-keywords nil)
 '(lisp-tag-body-indentation 3)
 '(lisp-tag-indentation 1)
 '(lispy-insert-space-after-wrap nil)
 '(lispy-no-permanent-semantic t)
 '(lispy-no-space t)
 '(lispy-safe-actions-ignore-comments t)
 '(lispy-safe-actions-no-pull-delimiters-into-comments nil)
 '(lispy-safe-copy t)
 '(ls-lisp-dirs-first t)
 '(lsp-clients-clangd-args '("-header-insertion-decorators=0" "-index"))
 '(lsp-report-if-no-buffer nil)
 '(lsp-ui-doc-border "
#ff0000")
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-doc-header t)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-max-height 40)
 '(lsp-ui-doc-position 'top)
 '(lsp-ui-doc-use-childframe t)
 '(lsp-ui-peek-always-show t)
 '(lsp-ui-peek-enable t)
 '(lsp-ui-peek-fontify 'on-demand)
 '(lsp-ui-sideline-delay 1.2)
 '(lsp-ui-sideline-enable nil)
 '(magit-auto-revert-mode t)
 '(magit-blame-heading-format "%H %-20a %C %s")
 '(magit-blame-styles
   '((headings
	  (heading-format . "%-20a %C %s
"))
	 (margin
	  (margin-format " %s%f" " %C %a" " %H")
	  (margin-width . 100)
	  (margin-face . magit-blame-margin)
	  (margin-body-face magit-blame-dimmed))
	 (highlight
	  (highlight-face . magit-blame-highlight))
	 (lines
	  (show-lines . t))))
 '(magit-bury-buffer-function 'magit-mode-quit-window)
 '(magit-completing-read-function 'helm--completing-read-default)
 '(magit-diff-highlight-indentation nil)
 '(magit-diff-highlight-trailing nil)
 '(magit-diff-paint-whitespace nil)
 '(magit-diff-section-arguments '("--no-ext-diff"))
 '(magit-display-buffer-function 'fn-magit-display-buffer-function)
 '(magit-ellipsis 9662)
 '(magit-log-arguments '("-n32"))
 '(magit-log-revision-headers-format "%+b
Author:    %aN <%aE>
Committer: %cN <%cE>
")
 '(magit-log-section-arguments '("-n32"))
 '(magit-log-select-arguments '("-n32"))
 '(magit-reflog-margin '(t age magit-log-margin-width t 18))
 '(magit-refresh-status-buffer t)
 '(magit-revision-headers-format "%cN <%cE>
%cd
")
 '(magit-revision-insert-related-refs nil)
 '(magit-status-headers-hook
   '(magit-insert-error-header magit-insert-diff-filter-header magit-insert-head-branch-header magit-insert-tags-header))
 '(magit-status-sections-hook
   '(magit-insert-status-headers magit-insert-merge-log magit-insert-rebase-sequence magit-insert-am-sequence magit-insert-sequencer-sequence magit-insert-bisect-output magit-insert-bisect-rest magit-insert-bisect-log magit-insert-unstaged-changes magit-insert-staged-changes magit-todos--insert-todos magit-insert-unpulled-from-upstream magit-insert-unpulled-from-pushremote magit-insert-unpushed-to-pushremote magit-insert-untracked-files))
 '(mail-citation-hook nil)
 '(mail-from-style 'parens)
 '(mail-header-separator "--text follows this line--")
 '(mail-send-hook nil)
 '(mark-ring-max 2)
 '(markdown-asymmetric-header nil)
 '(markdown-coding-system 'utf-8)
 '(maximum-scroll-margin 0.125)
 '(mc/always-run-for-all nil)
 '(message-citation-line-function 'message-insert-formatted-citation-line)
 '(message-cite-function 'sc-cite-original)
 '(message-cite-style 'message-cite-style-outlook)
 '(message-default-headers "Cc: 
")
 '(message-fill-column 100)
 '(message-from-style 'angles)
 '(message-hidden-headers
   '("^References:" "^Face:" "^X-Face:" "^X-Draft-From:" "^In-Reply-To:" "^Fcc:"))
 '(message-mail-user-agent t)
 '(message-required-headers '((optional . References) From))
 '(message-send-hook '(outlook-style--call-muse-for-message))
 '(message-send-mail-hook nil)
 '(message-setup-hook nil)
 '(mm-body-charset-encoding-alist
   '((iso-2022-jp . 7bit)
	 (iso-2022-jp-2 . 7bit)
	 (utf-16 . base64)
	 (utf-16be . base64)
	 (utf-16le . base64)
	 (iso-8859-1 . 8bit)
	 (utf-8 . 8bit)))
 '(mm-coding-system-priorities '(utf-8))
 '(mm-content-transfer-encoding-defaults
   '(("text/html" 8bit)
	 ("multipart/related" 8bit)
	 ("multipart*" 8bit)
	 ("text/x-patch" 8bit)
	 ("text/.*" qp-or-base64)
	 ("message/rfc822" 8bit)
	 ("application/emacs-lisp" qp-or-base64)
	 ("application/x-emacs-lisp" qp-or-base64)
	 ("application/x-patch" qp-or-base64)
	 (".*" base64)))
 '(mm-text-html-renderer 'shr)
 '(mode-line-format nil)
 '(modern-c++-operators '("..." "=" "+=" "-=" "*=" "!=" ">>" "<<" ">>=" "<<="))
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control))))
 '(mouse-yank-at-point t)
 '(mu4e-bookmarks
   '(("flag:unread AND NOT flag:trashed" "unread" 117)
	 ("date:today..now" "today" 116)
	 ("date:7d..now" "last7" 119)))
 '(mu4e-compose-cite-function 'fn--mail-cite-function)
 '(mu4e-drafts-folder "/Drafts")
 '(mu4e-get-mail-command "true")
 '(mu4e-headers-date-format "%d/%m/%y %H:%M")
 '(mu4e-headers-fields '((:subject . 60) (:from . 35) (:date . 16)))
 '(mu4e-headers-visible-flags '(flagged new passed replied attach))
 '(mu4e-index-update-in-background nil)
 '(mu4e-maildir "/home/fthevissen/.mail")
 '(mu4e-maildirs-extension-action-text nil)
 '(mu4e-maildirs-extension-bookmark-hl-regex "%u")
 '(mu4e-maildirs-extension-default-collapse-level 2)
 '(mu4e-maildirs-extension-maildir-format "%i%p %n|%u")
 '(mu4e-maildirs-extension-maildir-hl-regex "%u")
 '(mu4e-maildirs-extension-title nil)
 '(mu4e-maildirs-extension-use-maildirs t)
 '(mu4e-sent-folder "/Sent")
 '(mu4e-trash-folder "/Trash")
 '(mu4e-use-fancy-chars t)
 '(muse-html-encoding-default 'utf-8)
 '(muse-html-markup-tags
   '(("class" t t t muse-html-class-tag)
	 ("div" t t t muse-html-div-tag)
	 ("src" t t nil muse-html-src-tag)
	 ("srcc" t t nil muse-html-src-tag)))
 '(muse-html-meta-content-encoding 'utf-8)
 '(muse-html-style-sheet
   "<style type=\"text/css\">
body {
  background: color; color: white;
  margin-left: 3%; margin-right: 7%;
}

p { margin-top: 1% }
p.verse { margin-left: 3% }

.example { margin-left: 3% }

h2 {
  margin-top: 25px;
  margin-bottom: 0px;
}
h3 { margin-bottom: 0px; }
 </style>")
 '(neo-cwd-line-style 'button)
 '(neo-mode-line-type 'neotree t)
 '(neo-show-updir-line t t)
 '(neo-theme 'arrow)
 '(neo-window-width 35 t)
 '(next-error-highlight 3)
 '(next-error-highlight-no-select 3)
 '(next-screen-context-lines 0)
 '(nlinum-format "%4d " t)
 '(nlinum-relative-redisplay-delay 0 t)
 '(offlineimap-enable-mode-line-p nil)
 '(offlineimap-event-hooks nil)
 '(org-M-RET-may-split-line '((default)))
 '(org-agenda-custom-commands nil)
 '(org-agenda-fontify-priorities 'cookies)
 '(org-agenda-include-diary t)
 '(org-agenda-prefix-format
   '((agenda . " %i c %?-22t% s")
	 (todo . " %i c %(concat \"[\"(org-format-outline-path (org-get-outline-path))\"]  \")")
	 (tags . " %i %(org-get-category)")
	 (search . " %i")))
 '(org-agenda-restore-windows-after-quit nil)
 '(org-agenda-sticky nil)
 '(org-agenda-tags-column 'auto)
 '(org-agenda-todo-keyword-format "%-6s")
 '(org-agenda-view-columns-initially nil)
 '(org-agenda-window-setup 'current-window)
 '(org-archive-file-header-format "")
 '(org-archive-location "%s_archive.org::datetree/")
 '(org-archive-save-context-info '(time category olpath))
 '(org-ascii-list-margin 0)
 '(org-blank-before-new-entry '((heading . t) (plain-list-item)))
 '(org-bullets-bullet-list '("•" "▪" "▸" "·" "·"))
 '(org-clock-in-resume t)
 '(org-clock-persist t)
 '(org-columns-default-format "%TODO %50ITEM %3PRIORITY %TAGS")
 '(org-confirm-babel-evaluate nil)
 '(org-default-priority 52)
 '(org-download-image-dir "./images")
 '(org-fontify-done-headline t)
 '(org-format-latex-header
   "\\documentclass[leqno]{article}
\\usepackage[usenames]{color}
[PACKAGES]
[DEFAULT-PACKAGES]
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-8cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}")
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
				 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-hide-leading-stars t)
 '(org-highest-priority 49)
 '(org-html-htmlize-output-type 'css)
 '(org-list-indent-offset 2)
 '(org-lowest-priority 50)
 '(org-mark-ring-length 2)
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m org-drill))
 '(org-outline-path-complete-in-steps nil)
 '(org-preview-latex-default-process 'dvipng)
 '(org-priority-faces
   '((49 . org-priority-face-one)
	 (50 . org-priority-face-two)))
 '(org-refile-allow-creating-parent-nodes 'confirm)
 '(org-refile-targets '((org-agenda-files :maxlevel . 2)))
 '(org-refile-use-cache nil)
 '(org-refile-use-outline-path 'file)
 '(org-special-ctrl-a/e t)
 '(org-special-ctrl-k t)
 '(org-src-block-faces nil)
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation t)
 '(org-startup-folded 'content)
 '(org-startup-indented t)
 '(org-super-agenda-fontify-whole-header-line t)
 '(org-super-agenda-mode t)
 '(org-tags-column -80)
 '(org-todo-keyword-faces '(("WAITING" . org-waiting-face) ("CANCELED" . "#d75f00")))
 '(org-todo-keywords '((sequence "TODO" "|" "DONE" "WAITING" "CANCELED")))
 '(outlook-style-blockquotes-css
   "line-height: 120%; border-left: 4px solid #aaa; padding: 0em 0em 0em 0.5em; margin: 1.2em 4em 0em 0.5em; display: block; quotes: none; color:#33f;")
 '(outlook-style-body-css
   "color: black; font-family: Helvetica, sans-serif; font-size: 14;")
 '(outlook-style-code-css
   "background: #e8e8e8; padding-left: 2px; padding-right: 2px;")
 '(outlook-style-format-helper-location "/home/fthevissen/system/mail/format/format_mail.py")
 '(outlook-style-header-data-css
   "font-family: Calibri, sans-serif; font-size: 11pt; color: #606060; padding: 0px 0px 0px 3pt;")
 '(outlook-style-header-title-css
   "font-family: Calibri, sans-serif; font-size: 11pt; color: #505050; font-weight:bold; margin: 0cm; margin-bottom: .0001pt;")
 '(outlook-style-pre-transform-function 'fn-wrap-citations)
 '(outlook-style-pre-transform-hook '(fn-wrap-citations))
 '(outlook-style-quoted-header-rule-css
   "border-top: 0px solid #9c9e9c;border-bottom: 1px solid #dddddd;")
 '(outlook-style-src-css
   "color: white; border: 3pt solid #b0b0b0; background-color: #1f1f1f; padding: 5pt; font-family: monospace; font-size: 90%; overflow: auto;")
 '(outshine-fontify-whole-heading-line nil)
 '(outshine-outline-regexp-outcommented-p t)
 '(outshine-preserve-delimiter-whitespace t)
 '(outshine-startup-folded-p nil)
 '(overflow-newline-into-fringe nil)
 '(package-selected-packages
   '(lsp-clangd cycle-quotes org-attach-screenshot zoom magit-todos emr lsp lsp-python-ms pydoc python-info daemons filladapt python-mode flymake-python-pyflakes flycheck-mypy eglot company-lsp elscreen-separate-buffer-list lsp-ui lsp-mode find-file-in-project mu4e-conversation loccur framesize feebleline minibuffer-line lua-mode ansible-doc ansible auto-dim-other-buffers clojure-mode anki-editor origami vimish-fold evil-fringe-mark calfw-org tabbar-ruler tabbar highlight-defined highlight-function-calls org-super-agenda peep-dired org-fancy-priorities restclient org-caldav ercn remark-mode znc plantuml-mode ag smart-jump paren-face lispyville page-break-lines form-feed treepy morlock shadchen vertigo face-explorer highlight-indent-guides beacon helm-org-rifle indent-tools modern-cpp-font-lock helpful multi-compile dired-ranger ranger company-childframe ivy-posframe posframe nav-flash smooth-scroll python-cell flycheck-haskell dante org-category-capture goto-chg rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby utop tuareg caml ocp-indent merlin ensime sbt-mode scala-mode wgrep smex ivy-hydra counsel-projectile counsel-dash counsel swiper ivy racket-mode faceup toml-mode racer cargo rust-mode fsharp-mode navi-mode outshine spaceline-all-the-icons intero hlint-refactor hindent helm-hoogle haskell-snippets company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode offlineimap ob-sml ledger-mode messages-are-flowing visual-fill-column mu4e-maildirs-extension mu4e-alert ht muse spinner hydra projectile diminish bind-key packed memoize font-lock+ avy powerline highlight iedit smartparens bind-map evil undo-tree helm helm-core f s winum unfill fuzzy sml-mode web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data org-gcal request-deferred deferred calfw google-maps elfeed-web elfeed-org elfeed-goodies ace-jump-mode noflet elfeed vimrc-mode dactyl-mode spotify ob-ipython company-auctex auctex emms diff-hl mode-icons zoom-window zoom-frm all-the-icons spaceline smart-mode-line-powerline-theme smart-mode-line sr-speedbar ecb flycheck-tip flycheck-pos-tip traad stickyfunc-enhance srefactor flycheck rtags company-irony irony cmake-ide levenshtein helm-cscope xcscope csv-mode dash async ox-gfm org-projectile org-present org-pomodoro org-download toc-org orgit org org-plus-contrib org-bullets rainbow-mode vdiff highlight-quoted evil-text-object-python Theme1_-theme yascroll evil-textobj-column names simpleclip nlinum-relative nlinum zeal-at-point yapfify yaml-mode xterm-color web-beautify sql-indent smeargle shell-pop realgud test-simple loc-changes load-relative rainbow-identifiers pyvenv pytest pyenv-mode py-isort pip-requirements pdf-tools tablist alert log4e gntp mwim multi-term mmm-mode markdown-toc markdown-mode magit-gitflow livid-mode skewer-mode simple-httpd live-py-mode json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc jabber fsm hy-mode htmlize helm-pydoc helm-gitignore helm-dash helm-company helm-c-yasnippet gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fasd evil-magit magit magit-popup git-commit with-editor eshell-z eshell-prompt-extras esh-help ein websocket disaster cython-mode company-tern dash-functional tern company-statistics company-quickhelp pos-tip company-emacs-eclim eclim company-c-headers company-anaconda company color-identifiers-mode coffee-mode cmake-mode clang-format auto-yasnippet yasnippet anaconda-mode pythonic ac-ispell auto-complete zonokai-theme zenburn-theme zen-and-art-theme ws-butler window-numbering which-key volatile-highlights vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacemacs-theme spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme restart-emacs request rainbow-delimiters railscasts-theme quelpa purple-haze-theme professional-theme popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pastels-on-dark-theme paradox organic-green-theme open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme neotree naquadah-theme mustang-theme move-text monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme macrostep lush-theme lorem-ipsum linum-relative link-hint light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme info+ indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gandalf-theme flx-ido flatui-theme flatland-theme firebelly-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu espresso-theme elisp-slime-nav dumb-jump dracula-theme django-theme define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme clean-aindent-mode cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-highlight-symbol auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line))
 '(paradox-github-token t)
 '(paren-face-modes
   '(lisp-mode emacs-lisp-mode lisp-interaction-mode ielm-mode scheme-mode inferior-scheme-mode clojure-mode cider-repl-mode nrepl-mode arc-mode inferior-arc-mode c++-mode))
 '(paren-face-regexp "[][(){}]")
 '(pdf-view-continuous t)
 '(pdf-view-display-size 'fit-height)
 '(persp-emacsclient-init-frame-behaviour-override nil)
 '(popwin-mode t)
 '(popwin:adjust-other-windows t)
 '(popwin:special-display-config
   '(("*Pp Macroexpand Output*" :regexp nil :width 80 :position right :noselect t :dedicated t :stick t)
	 ("^*eshell.*$" :regexp t :height 0.28 :position bottom :dedicated t :stick t)
	 (inferior-python-mode :regexp nil :height 0.28 :position bottom :dedicated t :stick t)
	 (intero-repl-mode :regexp nil :height 0.28 :position bottom :dedicated t :stick t)
	 ("*slime-repl sbcl*" :regexp nil :height 0.28 :position bottom :dedicated t :stick t)
	 ("*Warnings*" :regexp nil :noselect t)
	 ("*ielm*" :regexp nil :height 0.28 :position bottom :dedicated t :stick t)
	 ("*Occur*" :regexp nil :height 0.2 :position bottom)
	 ("*pdb*" :regexp nil :height 0.2 :position bottom)
	 ("*rake-compilation*" :height 0.4 :position bottom :noselect t :dedicated t :stick t)
	 ("*rspec-compilation*" :height 0.4 :position bottom :noselect t :dedicated t :stick t)
	 ("^*WoMan.+*$" :regexp t :position bottom)
	 ("*nosetests*" :position bottom :noselect nil :dedicated t :stick t)
	 ("*grep*" :position bottom :noselect nil :dedicated t :stick t)
	 ("*ert*" :position bottom :noselect nil :dedicated t :stick t)
	 (" *undo-tree*" :height 0.4 :position bottom :noselect nil :dedicated t :stick t)
	 ("*Async Shell Command*" :position bottom :noselect nil :dedicated t :stick t)
	 ("*Shell Command Output*" :position bottom :noselect nil :dedicated t :stick t)
	 ("*compilation*" :height 0.382 :position bottom :noselect t :dedicated t :stick t)
	 ("*Help*" :height 0.4 :position bottom :noselect t :dedicated t :stick t)
	 ("*xref*" :regexp nil :height 0.25 :position bottom :dedicated t)
	 ("*hoogle*" :regexp nil :height 0.25 :position bottom :dedicated t)
	 ("*elfeed-entry*" :regexp nil :height 0.64 :position bottom :dedicated t)
	 (helpful-mode :regexp nil :height 0.36 :position bottom :noselect nil :dedicated t :stick t)
	 ("*ert*" :regexp nil :height 0.2 :position top :noselect t :dedicated nil :stick t)))
 '(powerline-buffer-size-suffix t)
 '(powerline-default-separator 'arrow)
 '(powerline-display-hud t)
 '(powerline-display-mule-info nil)
 '(powerline-gui-use-vcs-glyph t)
 '(powerline-height nil)
 '(powerline-text-scale-factor nil)
 '(prettify-symbols-unprettify-at-point nil)
 '(projectile-buffers-filter-function 'projectile-buffers-with-file)
 '(projectile-completion-system 'ivy)
 '(projectile-enable-caching t)
 '(projectile-indexing-method 'hybrid)
 '(projectile-project-root-files
   '("rebar.config" "project.clj" "build.boot" "deps.edn" "SConstruct" "pom.xml" "build.sbt" "gradlew" "build.gradle" ".ensime" "Gemfile" "tox.ini" "composer.json" "Cargo.toml" "mix.exs" "info.rkt" "DESCRIPTION" "TAGS" "GTAGS" "configure.in" "configure.ac" "cscope.out"))
 '(projectile-require-project-root nil)
 '(projectile-sort-order 'recently-active)
 '(pulse-iterations 5)
 '(purpose-user-mode-purposes
   '((helpful-mode . helpful)
	 (inferior-python-mode . terminal)))
 '(purpose-user-regexp-purposes
   '(("*ielm*" . terminal)
	 ("*slime-repl sbcl*" . terminal)
	 ("*intero*repl*" . terminal)
	 ("*Python*" . terminal)))
 '(purpose-x-popwin-buffer-names '("*Shell Command Output*"))
 '(purpose-x-popwin-major-modes
   '(help-mode compilation-mode occur-mode inferior-python-mode helpful-mode))
 '(purpose-x-popwin-position 'bottom)
 '(python-indent-def-block-scale 0)
 '(python-indent-guess-indent-offset nil)
 '(python-indent-guess-indent-offset-verbose nil)
 '(python-indent-offset 4)
 '(python-shell-completion-native-enable nil)
 '(python-shell-interpreter-args "--simple-prompt --no-banner")
 '(python-shell-prompt-detect-failure-warning nil)
 '(racket-pretty-lambda t)
 '(ranger-preview-delay 1000.0)
 '(realgud:pdb-command-name (concat (getenv "DIR_ANACONDA") "/lib/python3.5/pdb.py"))
 '(recentf-exclude
   '("/git-rebase-todo\\'" "/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'" "COMMIT_EDITMSG\\'" "/home/fthevissen/system/.emacs.d/.cache/"))
 '(safe-local-variable-values
   '((Readtable . GLISP)
	 (Package . SGML)
	 (Package . LALR)
	 (Syntax . Common-Lisp)
	 (org-src-fontify-natively . t)
	 (org-user-property-inheritance . t)))
 '(save-interprogram-paste-before-kill t)
 '(sc-attribs-postselect-hook '(fn-sc-attribution-post-selection-hook))
 '(sc-auto-fill-region-p nil)
 '(sc-blank-lines-after-headers 1)
 '(sc-citation-delimiter "> ")
 '(sc-citation-delimiter-regexp "[>]+")
 '(sc-citation-leader "")
 '(sc-citation-separator " ")
 '(sc-cite-blank-lines-p nil)
 '(sc-confirm-always-p nil)
 '(sc-default-attribution "")
 '(sc-electric-references-p nil)
 '(sc-fixup-whitespace-p nil)
 '(sc-nested-citation-p t)
 '(sc-preferred-attribution-list '(""))
 '(sc-preferred-header-style 0)
 '(sc-reference-tag-string ">")
 '(scalable-fonts-allowed t)
 '(scroll-margin 12)
 '(select-enable-clipboard t)
 '(select-enable-primary nil)
 '(send-mail-function 'smtpmail-send-it)
 '(shell-file-name "zsh")
 '(show-paren-delay 0.0)
 '(show-paren-highlight-openparen nil)
 '(show-paren-mode nil)
 '(show-paren-style 'parenthesis)
 '(show-paren-when-point-in-periphery nil)
 '(show-paren-when-point-inside-paren nil)
 '(show-smartparens-global-mode nil)
 '(show-trailing-whitespace nil)
 '(shr-color-visible-distance-min 40)
 '(shr-color-visible-luminance-min 80)
 '(shr-use-colors nil)
 '(shr-width 80)
 '(size-indication-mode nil)
 '(sml-font-lock-symbols t)
 '(sml/show-frame-identification nil)
 '(smooth-scroll-mode t)
 '(smooth-scroll/vscroll-step-size 7)
 '(smtpmail-smtp-server "mail.rwth-aachen.de")
 '(smtpmail-smtp-service 587)
 '(sp-autodelete-closing-pair t)
 '(sp-autodelete-opening-pair t)
 '(sp-escape-quotes-after-insert nil)
 '(sp-escape-wrapped-region nil)
 '(sp-show-pair-from-inside nil)
 '(spaceline-helm-mode t)
 '(spaceline-info-mode t)
 '(split-height-threshold 0)
 '(split-width-threshold 40)
 '(sublimity-map-max-fraction 0.6)
 '(sublimity-map-size 45)
 '(sublimity-map-text-scale -7)
 '(tab-width 4)
 '(tabbar-background-color "#161616")
 '(tabbar-cycle-scope 'tabs)
 '(tabbar-mode t nil (tabbar))
 '(tabbar-separator '(1))
 '(tabbar-show-key-bindings nil)
 '(tabbar-use-images nil)
 '(text-scale-mode-step 1.1)
 '(tramp-syntax 'default nil (tramp))
 '(treemacs-file-follow-delay 0.5)
 '(treemacs-width 42)
 '(underline-minimum-offset 0)
 '(vc-annotate-background "nil")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#ff0000")
	 (40 . "#ff4a52")
	 (60 . "#f6aa11")
	 (80 . "#f1e94b")
	 (100 . "#f5f080")
	 (120 . "#f6f080")
	 (140 . "#41a83e")
	 (160 . "#40b83e")
	 (180 . "#b6d877")
	 (200 . "#b7d877")
	 (220 . "#b8d977")
	 (240 . "#b9d977")
	 (260 . "#93e0e3")
	 (280 . "#72aaca")
	 (300 . "#8996a8")
	 (320 . "#afc4db")
	 (340 . "#cfe2f2")
	 (360 . "#dc8cc3")))
 '(vc-annotate-very-old-color "#dc8cc3")
 '(visual-line-fringe-indicators '(nil nil))
 '(whitespace-style
   '(indentation::tab indentation::space indentation big-indent space-after-tab::tab space-after-tab::space space-after-tab space-before-tab::tab space-before-tab::space space-before-tab space-mark tab-mark))
 '(window-divider-default-bottom-width 1)
 '(window-divider-default-places t)
 '(window-divider-default-right-width 1)
 '(window-min-height 8)
 '(winum-auto-assign-0-to-minibuffer nil)
 '(word-wrap t)
 '(x-select-enable-clipboard-manager t)
 '(x-select-request-type nil)
 '(x-stretch-cursor t)
 '(x-underline-at-descent-line nil)
 '(x-use-underline-position-properties nil)
 '(xclip-program "xclip")
 '(xclip-use-pbcopy&paste nil)
 '(xref-marker-ring-length 2)
 '(xterm-mouse-mode t)
 '(yank-pop-change-selection t)
 '(yascroll:delay-to-hide 0.3)
 '(yascroll:disabled-modes '(org-mode elfeed-search-mode elfeed-show-mode))
 '(yascroll:scroll-bar '(right-fringe text-area))
 '(znc-servers
   '(("h2712310.stratoserver.net" 5000 t
	  ((OFTC "fthevissen/OFTC" "")
	   (freenode "fthevissen/freenode" "")))))
 '(zoom-ignored-buffer-names '("*ielm*"))
 '(zoom-ignored-major-modes '(inferior-emacs-lisp-mode))
 '(zoom-size '(0.618 . 0.618)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#161616" :foreground "#f8f8f8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 146 :width extra-expanded :foundry "CNR " :family "CodeNewRoman NF"))))
 '(+magit-current-line-face ((t (:inverse-video t))))
 '(Info-quoted ((t (:foreground "gray30"))))
 '(Man-overstrike ((t (:foreground "#ffaf5f"))))
 '(Man-underline ((t (:foreground "#875fff"))))
 '(ac-candidate-face ((t (:background "#e0e0e0" :foreground "blue"))))
 '(ac-gtags-selection-face ((t (:background "sandybrown" :foreground "black"))))
 '(acc-rg-filename-face ((t (:foreground "red"))))
 '(acc-rg-line-number-face ((t (:foreground "#337ebe"))))
 '(acc-rg-path-face ((t (:foreground "gray35"))))
 '(ahs-definition-face ((t (:background "black" :foreground "moccasin" :underline t))))
 '(ahs-edit-mode-face ((t (:background "black" :foreground "White"))))
 '(ahs-face ((t (:background "gray25" :foreground "#cbe39c"))))
 '(ansible::task-label-face ((t (:inherit font-lock-function-name-face))))
 '(auto-dim-other-buffers-face ((t (:background "color-233"))))
 '(avy-goto-char-timer-face ((t (:background "red3" :foreground "#000000"))))
 '(avy-lead-face ((t (:background "red3" :foreground "#000000"))))
 '(avy-lead-face-0 ((t (:background "red2" :foreground "#000000"))))
 '(avy-lead-face-1 ((t (:background "green" :foreground "#000000"))))
 '(avy-lead-face-2 ((t (:background "#00af00" :foreground "#000000"))))
 '(beacon-fallback-background ((t (:background "white"))))
 '(bold ((t (:weight bold))))
 '(border ((t (:background "gray20"))))
 '(button ((t (:foreground "steelblue3" :underline t))))
 '(c-nonbreakable-space-face ((t (:background "#f1e94b" :foreground "black"))) t)
 '(comint-highlight-input ((t (:foreground "white"))))
 '(comint-highlight-prompt ((t (:foreground "#98bd5e"))))
 '(company-preview ((t (:background "#161616"))))
 '(company-preview-common ((t (:inherit company-preview :foreground "gray30"))))
 '(company-preview-search ((t (:inherit company-preview :foreground "gray30"))))
 '(company-scrollbar-bg ((t nil)))
 '(company-scrollbar-fg ((t nil)))
 '(company-tooltip ((t (:background "gray14" :foreground "#dfcfc1"))))
 '(company-tooltip-annotation ((t (:foreground "#72aaca"))))
 '(company-tooltip-common ((t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :foreground "#f2c585"))))
 '(company-tooltip-mouse ((t (:inherit highlight))))
 '(company-tooltip-search ((t (:inherit highlight))))
 '(company-tooltip-search-selection ((t (:inherit highlight))))
 '(company-tooltip-selection ((t (:inherit company-tooltip-common-selection :background "gray20"))))
 '(compilation-info ((t (:foreground "#cf7c43" :underline nil :slant italic))))
 '(compilation-line-number ((t (:foreground "#337ebe"))))
 '(compilation-mode-line-exit ((t (:foreground "brightgreen" :weight bold))))
 '(compilation-mode-line-fail ((t (:foreground "#ac443f"))))
 '(compilation-mode-line-run ((t (:foreground "brightyellow" :weight bold))))
 '(completions-annotations ((t (:inherit italic))))
 '(completions-first-difference ((t (:inherit bold))))
 '(counsel-outline-default ((t (:inherit minibuffer-prompt))))
 '(custom-face-tag ((t (:foreground "#cf7c43"))))
 '(custom-group-tag ((t (:inherit variable-pitch :foreground "#5f87d7" :underline t :weight bold))))
 '(custom-state ((t (:foreground "#98bd5e"))))
 '(custom-variable-tag ((t (:foreground "#cf7c43"))))
 '(describe-variable-value ((t (:foreground "#337ebe"))))
 '(diff-added ((t (:background "#171717" :foreground "#83a748"))))
 '(diff-context ((t (:foreground "gray40"))))
 '(diff-file-header ((t (:background "#161616" :foreground "#636ccb" :weight bold))))
 '(diff-header ((t (:background "#161616" :foreground "gray40"))))
 '(diff-hunk-header ((t (:foreground "#636ccb"))))
 '(diff-removed ((t (:foreground "#ac443f"))))
 '(dired-directory ((t (:foreground "#98bd5e"))))
 '(diredp-dir-heading ((t (:background "#171717" :foreground "#171717"))))
 '(diredp-dir-name ((t (:foreground "#98bd5e"))))
 '(diredp-file-name ((t (:foreground "#dfcfc1"))))
 '(diredp-file-suffix ((t (:foreground "#72aaca"))))
 '(diredp-ignored-file-name ((t (:foreground "grey31"))))
 '(diredp-symlink ((t (:foreground "#98bd5e"))))
 '(ediff-current-diff-A ((t (:background "#ac443f" :foreground "#f8f8f8"))))
 '(ediff-current-diff-B ((t (:background "#678439" :foreground "#f8f8f8"))))
 '(ediff-current-diff-C ((t (:background "gray15" :foreground "#f8f8f8"))))
 '(ediff-even-diff-A ((t (:background "grey15"))))
 '(ediff-even-diff-B ((t (:background "grey15"))))
 '(ediff-even-diff-C ((t (:background "grey15"))))
 '(ediff-fine-diff-A ((t (:background "brightred" :foreground "#f8f8f8"))))
 '(ediff-fine-diff-Ancestor ((t nil)))
 '(ediff-fine-diff-B ((t (:background "#5faf00" :foreground "#f8f8f8" :weight bold))))
 '(ediff-fine-diff-C ((t (:background "#005faf" :foreground "tan" :weight normal))))
 '(ediff-odd-diff-A ((t (:background "grey7"))))
 '(ediff-odd-diff-B ((t (:background "grey7"))))
 '(ediff-odd-diff-C ((t (:background "grey7"))))
 '(ein:cell-input-area ((t (:background "gray14"))))
 '(ein:cell-input-prompt ((t (:inherit header-line :overline t :underline t))))
 '(elfeed-search-date-face ((t (:foreground "#337ebe"))))
 '(elfeed-search-feed-face ((t (:foreground "#98bd5e"))))
 '(elfeed-search-filter-face ((t (:inherit mode-line-buffer-id :inverse-video t))))
 '(elfeed-search-last-update-face ((t (:inverse-video t))))
 '(elfeed-search-tag-face ((t (:foreground "#8c8ac2" :slant italic))))
 '(elfeed-search-title-face ((t (:foreground "grey42"))))
 '(elfeed-search-unread-count-face ((t (:foreground "#cc6666" :inverse-video t))))
 '(elfeed-search-unread-title-face ((t (:foreground "#cf7c43"))))
 '(erc-default-face ((t (:foreground "brightwhite"))))
 '(erc-input-face ((t (:foreground "tan"))))
 '(erc-my-nick-face ((t (:foreground "#8c8ac2" :weight normal))))
 '(erc-my-nick-prefix-face ((t (:inherit erc-nick-default-face :weight normal))))
 '(erc-nick-default-face ((t (:foreground "#f6f080" :weight normal))))
 '(erc-nick-prefix-face ((t (:inherit erc-nick-default-face))))
 '(erc-prompt-face ((t (:background "#000000" :foreground "#f6aa11" :inverse-video t :weight bold))))
 '(error ((t (:foreground "#ff0000"))))
 '(escape-glyph ((t (:foreground "#87cefa" :weight bold))))
 '(eshell-ls-directory ((t (:foreground "#98bd5e" :weight normal))))
 '(eshell-ls-executable ((t (:foreground "#ac443f" :weight normal))))
 '(eval-sexp-fu-flash ((t (:background "RoyalBlue4" :foreground "white" :weight bold))))
 '(eval-sexp-fu-flash-error ((t (:background "red" :foreground "black" :weight bold))))
 '(evil-ex-lazy-highlight ((t (:foreground "#ffd75f" :inverse-video t))))
 '(evil-ex-search ((t (:foreground "#cc6666" :inverse-video t))))
 '(evil-ex-substitute-matches ((t (:foreground "#98bd5e" :inverse-video t))))
 '(evil-ex-substitute-replacement ((t (:foreground "#ac443f" :inverse-video t))))
 '(evil-search-highlight-persist-highlight-face ((t (:foreground "#f1e94b"))))
 '(eww-form-textarea ((t (:box 1))))
 '(feebleline-asterisk-face ((t (:foreground "grey9"))))
 '(feebleline-bufname-face ((t (:foreground "grey9"))))
 '(flycheck-error ((((type x)) (:underline (:color "#ff4a52" :style wave))) (((type tty)) (:underline "royal blue"))))
 '(flycheck-warning ((t (:underline (:color "#f6aa11" :style wave) :weight bold))))
 '(fn-comp-buffer-error-highlight ((t (:inverse-video t))))
 '(font-latex-italic-face ((t (:inherit italic :foreground "#cf7c43"))))
 '(font-latex-sectioning-5-face ((t (:foreground "#98bd5e" :underline t :slant italic))))
 '(font-latex-sedate-face ((t (:foreground "#d7af5f"))))
 '(font-lock-builtin-face ((t (:foreground "#af8788" :weight normal))))
 '(font-lock-comment-delimiter-face ((t (:foreground "burlywood4" :slant normal :weight normal))))
 '(font-lock-comment-face ((t (:foreground "#0087d7" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#8886c6" :slant italic))))
 '(font-lock-doc-face ((t (:foreground "grey45"))))
 '(font-lock-function-call-face ((t (:foreground "#af87af"))))
 '(font-lock-function-name-face ((t (:foreground "tan1"))))
 '(font-lock-keyword-face ((t (:foreground "#648fb9" :weight normal))))
 '(font-lock-method-call-face ((t (:foreground "indianred"))))
 '(font-lock-negation-char-face ((t (:foreground "#f6f080" :weight bold))))
 '(font-lock-preprocessor-face ((t (:foreground "medium purple"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#a52a2a" :weight normal))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#1c86ee" :weight normal))))
 '(font-lock-string-face ((t (:foreground "#98bd5e"))))
 '(font-lock-type-face ((t (:foreground "#83a748" :slant italic))))
 '(font-lock-variable-name-face ((t (:foreground "#d0bf8f"))))
 '(font-lock-warning-face ((t (:foreground "#fbcb41"))))
 '(form-feed-line ((t (:foreground "green" :strike-through t :slant italic :weight normal :width normal))))
 '(fringe ((t (:background "#161616" :foreground "#5f87af"))))
 '(git-commit-comment-action ((t (:foreground "#c6ab94"))))
 '(git-commit-comment-branch-local ((t (:foreground "brightred" :inverse-video t))))
 '(git-commit-comment-branch-remote ((t (:foreground "red"))))
 '(git-commit-keyword ((t (:inherit ## :inverse-video t :foreground "#cc6666" :Underline t))))
 '(git-commit-nonempty-second-line ((t (:foreground "grey30"))))
 '(git-commit-summary ((t (:foreground "#cc6666" :underline t))))
 '(git-gutter:added ((t (:background "#000000" :foreground "#00ff00" :inverse-video t :weight normal))))
 '(git-gutter:deleted ((t (:background "#000000" :foreground "#ff0000" :inverse-video t :weight normal))))
 '(git-gutter:modified ((t (:background "#000000" :foreground "#d75fff" :inverse-video t :weight bold))))
 '(git-rebase-comment-hash ((t (:inherit git-rebase-hash))))
 '(git-timemachine-minibuffer-author-face ((t (:foreground "orange"))))
 '(git-timemachine-minibuffer-detail-face ((t (:foreground "yellow"))))
 '(gnus-cite-1 ((t (:foreground "#cf7c43"))))
 '(gnus-cite-2 ((t (:foreground "#83a748"))))
 '(gnus-cite-4 ((t (:foreground "#cc6666"))))
 '(gnus-cite-5 ((t (:foreground "#8c8ac2"))))
 '(gnus-cite-6 ((t (:foreground "#337ebe"))))
 '(gnus-cite-7 ((t (:foreground "#ad875f"))))
 '(haskell-constructor-face ((t (:foreground "indianred" :slant italic))))
 '(haskell-literate-comment-face ((t (:foreground "#afaf87"))))
 '(hdefd-functions ((t (:foreground "#eecbad"))))
 '(hdefd-variables ((t (:foreground "#cd69c9"))))
 '(header-line ((t (:background "gray10" :foreground "tan1" :box nil))))
 '(header-line-buffer-name-face ((t (:inherit header-line-face :foreground "#ff0000" :underline t :weight normal))))
 '(header-line-code-completion-face ((t (:inherit header-line-face :foreground "#376737"))))
 '(header-line-directory-face ((t (:inherit header-line-face :foreground "#676767"))))
 '(header-line-face ((t (:background "gray10" :height 1.0))))
 '(header-line-highlight ((t nil)))
 '(header-line-project-face ((t (:inherit header-line-face :foreground "#ffa54f"))))
 '(header-line-warning-face ((t (:inherit header-line :foreground "tan1"))))
 '(helm-buffer-file ((t (:foreground "#bcbcbc"))))
 '(helm-buffer-process ((t (:foreground "#5f87af"))))
 '(helm-buffer-size ((t (:foreground "grey35"))))
 '(helm-ff-file ((t (:foreground "azure3"))))
 '(helm-ff-symlink ((t (:foreground "#cf7c43"))))
 '(helm-grep-file ((t (:foreground "blue" :underline t))))
 '(helm-match ((t (:foreground "#f2c585"))))
 '(helm-non-file-buffer ((t (:foreground "#87afd7"))))
 '(helm-org-rifle-separator ((t (:foreground "purple"))))
 '(helm-source-header ((t (:foreground "#5f87d7" :box nil :underline t :weight bold))))
 '(helpful-heading ((t (:foreground "#cf7c43"))))
 '(hi-yellow ((t (:inherit evil-ex-lazy-highlight))))
 '(highlight-defined-function-name-face ((t (:foreground "color-181"))))
 '(highlight-defined-macro-name-face ((t (:inherit highlight-defined-function-name-face))))
 '(highlight-function-calls-face ((t (:foreground "color-139"))))
 '(highlight-indent-guides-character-face ((t (:foreground "blue"))) t)
 '(highlight-indent-guides-even-face ((t (:foreground "red"))) t)
 '(highlight-indent-guides-stack-even-face ((t (:foreground "cyan"))) t)
 '(highlight-indent-guides-stack-odd-face ((t (:foreground "green"))) t)
 '(highlight-indent-guides-top-even-face ((t (:foreground "red"))) t)
 '(highlight-indent-guides-top-odd-face ((t (:foreground "blue"))) t)
 '(highlight-numbers-number ((t (:foreground "indianred"))))
 '(highlight-quoted-symbol ((t (:foreground "indian red"))))
 '(hl-line ((t nil)))
 '(hl-paren-face ((t nil)) t)
 '(hl-todo ((t (:foreground "#cc6666" :weight bold))))
 '(holiday ((t (:foreground "brightred"))))
 '(ido-vertical-first-match-face ((t (:inherit font-lock-variable-name-face))))
 '(info-emphasis ((t (:foreground "#cf7c43" :slant italic))))
 '(info-function-ref-item ((t (:foreground "#6dbeb3" :inverse-video t :underline t :slant oblique))))
 '(info-macro-ref-item ((t (:foreground "Yellow" :inverse-video t :underline t :slant oblique))))
 '(info-reference-item ((t (:foreground "#fbcb41" :inverse-video nil :underline t))))
 '(info-special-form-ref-item ((t (:foreground "Yellow" :inverse-video t :underline t :slant italic))))
 '(info-syntax-class-item ((t (:foreground "#FFFF9B9BFFFF" :inverse-video t :underline t :slant italic))))
 '(info-title-1 ((t (:foreground "#fbcb41" :weight bold))))
 '(info-title-2 ((t (:foreground "#98bd5e" :weight bold :height 1.6))))
 '(info-title-3 ((t (:foreground "#98bd5e" :weight bold :height 1.4))))
 '(info-title-4 ((t (:foreground "#98bd5e" :weight bold :height 1.3))))
 '(info-user-option-ref-item ((t (:foreground "#ac443f" :inverse-video t :underline t :slant oblique))))
 '(info-variable-ref-item ((t (:foreground "Orange" :inverse-video t :underline t :slant oblique))))
 '(info-xref-visited ((t (:inherit info-xref))))
 '(internal-border ((t nil)))
 '(isearch ((t (:background "#f1e94b" :foreground "grey10" :weight bold))))
 '(italic ((t (:foreground "tomato" :slant italic))))
 '(ivy-current-match ((t (:background "#ac443f" :foreground "#161616" :inverse-video nil))))
 '(ivy-grep-info ((t (:inherit compilation-info))))
 '(ivy-minibuffer-match-face-2 ((t (:background "gold2" :foreground "gray11" :weight bold))))
 '(ivy-minibuffer-match-face-3 ((t (:background "steel blue" :foreground "gray11" :weight bold))))
 '(ivy-minibuffer-match-face-4 ((t (:background "#ffbbff" :foreground "gray11" :weight bold))))
 '(ivy-minibuffer-match-highlight ((t (:foreground "#fbcb41"))))
 '(ivy-modified-buffer ((t (:foreground "#cf7c43"))))
 '(ivy-modified-outside-buffer ((t (:inherit default))))
 '(ivy-prompt-match ((t (:inherit ivy-current-match))))
 '(lazy-highlight ((t (:background "#202325" :foreground "#f1e94b" :weight semi-light))))
 '(line-number ((t (:inherit (shadow default) :background "gray15"))))
 '(line-number-current-line ((t (:inherit line-number :foreground "peru"))))
 '(link ((t (:foreground "#5f87d7" :underline t :weight bold))))
 '(linum ((t (:background "#1f1f1d" :foreground "#3c3f42"))))
 '(lisp-extra-font-lock-backquote ((t (:foreground "peru"))))
 '(lisp-extra-font-lock-quoted ((t (:foreground "#c6ab94"))))
 '(lisp-extra-font-lock-special-variable-name ((t (:foreground "#cd69c9"))))
 '(lisp-function-face ((t (:inherit font-lock-function-name-face :overline "gray20"))))
 '(lisp-variable-name-face ((t (:foreground "#ffd700"))))
 '(lisp-warning-face ((t (:foreground "#ff4500"))))
 '(lispy-face-hint ((t (:background "gray15" :foreground "#fff3bc"))))
 '(lsp-face-highlight-read ((t (:background "black"))))
 '(lsp-ui-doc-background ((t (:background "grey15"))))
 '(lsp-ui-doc-header ((t (:background "#454dd0" :foreground "#ffffff"))))
 '(lsp-ui-peek-peek ((t (:background "#212121" :foreground "white"))))
 '(lsp-ui-sideline-current-symbol ((t (:foreground "IndianRed3" :box (:line-width -1 :color "white") :weight normal :height 0.9))))
 '(lsp-ui-sideline-global ((t (:background "#262626"))))
 '(lsp-ui-sideline-symbol ((t (:foreground "grey" :box (:line-width -1 :color "grey") :height 0.9))))
 '(lsp-ui-sideline-symbol-info ((t (:foreground "#b2b2b2" :slant italic :height 0.9))))
 '(lv-separator ((t (:background "grey30"))))
 '(magit-blame-date ((t (:inherit magit-blame-heading :foreground "#98bd5e"))) t)
 '(magit-blame-heading ((t (:background "grey15" :foreground "#5f87af"))))
 '(magit-blame-summary ((t (:inherit magit-blame-heading :foreground "#c6ab94"))) t)
 '(magit-branch-current ((t (:inherit magit-branch-local :weight bold))))
 '(magit-branch-local ((t (:foreground "#cc6666" :inverse-video t :slant italic :weight bold :height 1.3))))
 '(magit-branch-remote ((t (:foreground "#98bd5e" :box (:line-width 1 :color "#98bd5e") :slant italic :weight bold))))
 '(magit-diff-added ((t (:background "#171717" :foreground "#83a748"))))
 '(magit-diff-added-highlight ((t (:background "#171717" :foreground "#83a748"))))
 '(magit-diff-base ((t (:foreground "#aaaa11"))))
 '(magit-diff-base-highlight ((t (:foreground "#aaaa11"))))
 '(magit-diff-context ((t (:foreground "grey50"))))
 '(magit-diff-context-highlight ((t (:foreground "grey50"))))
 '(magit-diff-file-heading ((t (:foreground "tan"))))
 '(magit-diff-file-heading-highlight ((t nil)))
 '(magit-diff-file-heading-selection ((t nil)))
 '(magit-diff-hunk-heading ((t (:background "grey15" :foreground "#8c8ac2" :underline t))))
 '(magit-diff-hunk-heading-highlight ((t (:background "grey15" :foreground "#8c8ac2" :underline t))))
 '(magit-diff-hunk-region ((t (:weight bold))))
 '(magit-diff-removed ((t (:background "#191919" :foreground "#ac443f"))))
 '(magit-diff-removed-highlight ((t (:background "#191919" :foreground "#ac443f"))))
 '(magit-diffstat-added ((t (:foreground "#678439"))))
 '(magit-diffstat-removed ((t (:foreground "#ac443f"))))
 '(magit-dimmed ((t (:foreground "grey50"))))
 '(magit-filename ((t (:foreground "#cc6666" :inverse-video t :weight normal))))
 '(magit-hash ((t (:foreground "#ff8737" :slant italic))))
 '(magit-keyword ((t (:foreground "#98bd5e"))))
 '(magit-log-author ((t (:foreground "#0087ff" :slant normal))))
 '(magit-log-date ((t (:foreground "#98bd5e" :slant normal))))
 '(magit-log-graph ((t nil)))
 '(magit-reflog-cherry-pick ((t (:foreground "#678439"))))
 '(magit-reflog-commit ((t (:foreground "#678439"))))
 '(magit-reflog-merge ((t (:foreground "#678439"))))
 '(magit-section-heading ((t (:foreground "#5f87d7" :height 1.1))))
 '(magit-section-highlight ((t nil)))
 '(magit-section-secondary-heading ((t (:foreground "#8c8ac2"))))
 '(magit-sequence-stop ((t (:foreground "green"))))
 '(magit-tag ((t (:foreground "#8c8ac2"))))
 '(markdown-code-face ((t (:inherit fixed-pitch :background "#212121"))))
 '(markdown-header-face-1 ((t (:foreground "#fbcb41"))))
 '(markdown-header-face-2 ((t (:foreground "#98bd5e"))))
 '(markdown-inline-code-face ((t (:inherit font-lock-constant-face :foreground "#cf7c43"))))
 '(markdown-pre-face ((t (:foreground "brightblue"))))
 '(message-header-cc ((t (:foreground "#8c8af8" :weight normal))))
 '(message-header-name ((t (:foreground "#98bd5e" :underline t))))
 '(message-header-newsgroups ((t (:foreground "#f6f080" :weight normal))))
 '(message-header-other ((t (:foreground "#337ebe"))))
 '(message-header-subject ((t (:foreground "#f2c585" :weight normal))))
 '(message-header-to ((t (:foreground "#af8787" :weight normal))))
 '(message-mml ((t (:foreground "#f6f080" :weight normal))))
 '(minibuffer-prompt ((t (:foreground "#ffa54f"))))
 '(mmm-default-submode-face ((t nil)))
 '(mode-line ((t (:background "#161616" :foreground "wheat4" :box nil :underline t))))
 '(mode-line-buffer-id ((t (:foreground "#f2c585" :slant italic :weight normal))))
 '(mode-line-buffer-id-inactive ((t (:foreground "#f2c585" :box nil :slant italic))))
 '(mode-line-emphasis ((t nil)))
 '(mode-line-highlight ((t nil)))
 '(mode-line-inactive ((t (:background "#161616" :foreground "AntiqueWhite4" :box nil :underline t))))
 '(mu4e-cited-1-face ((t (:foreground "#cf7c43" :slant normal))))
 '(mu4e-cited-2-face ((t (:foreground "#83a748" :slant normal))))
 '(mu4e-cited-3-face ((t (:foreground "#72aaca" :slant normal))))
 '(mu4e-cited-4-face ((t (:foreground "#cc6666" :slant normal))))
 '(mu4e-cited-5-face ((t (:foreground "#8c8ac2" :slant normal))))
 '(mu4e-cited-6-face ((t (:foreground "#337ebe" :slant normal))))
 '(mu4e-cited-7-face ((t (:foreground "#ad875f" :slant normal))))
 '(mu4e-flagged-face ((t (:foreground "brightred" :weight bold))))
 '(mu4e-header-face ((t (:inherit default :foreground "white"))))
 '(mu4e-header-highlight-face ((t (:background "#000000" :inverse-video t))))
 '(mu4e-link-face ((t (:inherit link :slant italic))))
 '(mu4e-maildirs-extension-maildir-face ((t (:inherit mu4e-header-face :foreground "#5f87d7"))))
 '(mu4e-maildirs-extension-maildir-hl-face ((t (:inherit mu4e-unread-face :foreground "#ff0000"))))
 '(mu4e-maildirs-extension-maildir-root-face ((t (:foreground "tan1" :underline t))))
 '(mu4e-replied-face ((t (:foreground "green"))))
 '(mu4e-trashed-face ((t (:foreground "#585858" :strike-through t))))
 '(my-mode-foo-face ((t (:foreground "green"))) t)
 '(neo-banner-face ((t (:foreground "#161616" :weight bold))) t)
 '(neo-button-face ((t (:underline nil))) t)
 '(neo-dir-link-face ((t (:foreground "#337ebe"))) t)
 '(neo-expand-btn-face ((t (:foreground "gray30"))) t)
 '(neo-file-link-face ((t (:foreground "#d7d7d7"))) t)
 '(neo-header-face ((t (:foreground "gray50"))) t)
 '(neo-root-dir-face ((t (:foreground "burlywood" :weight bold))) t)
 '(neo-vc-added-face ((t (:foreground "LightGreen"))) t)
 '(neo-vc-conflict-face ((t (:foreground "Red1"))) t)
 '(neo-vc-default-face ((t (:foreground "White"))) t)
 '(neo-vc-edited-face ((t (:foreground "Magenta"))) t)
 '(neo-vc-ignored-face ((t (:foreground "DarkGrey"))) t)
 '(neo-vc-missing-face ((t (:foreground "Red1"))) t)
 '(neo-vc-needs-merge-face ((t (:foreground "Red1"))) t)
 '(neo-vc-needs-update-face ((t (:underline t))) t)
 '(neo-vc-removed-face ((t (:strike-through t))) t)
 '(neo-vc-unlocked-changes-face ((t (:background "Blue" :foreground "Red"))) t)
 '(neo-vc-up-to-date-face ((t (:foreground "LightGray"))) t)
 '(neo-vc-user-face ((t (:foreground "Red" :slant italic))) t)
 '(next-error ((t (:inherit pulse-highlight-face))))
 '(org-agenda-calendar-event ((t (:foreground "red"))))
 '(org-agenda-calendar-sexp ((t (:foreground "blue"))))
 '(org-agenda-clocking ((t (:foreground "red"))))
 '(org-agenda-column-dateline ((t (:foreground "yellow"))))
 '(org-agenda-date ((t (:foreground "#98bd5e"))))
 '(org-agenda-date-today ((t (:foreground "dodger blue" :slant italic :weight normal))))
 '(org-agenda-date-weekend ((t (:foreground "gray40" :slant italic :weight normal))))
 '(org-agenda-diary ((t (:foreground "white"))))
 '(org-agenda-filter-category ((t (:foreground "tan"))))
 '(org-agenda-structure ((t (:foreground "#5f87d7" :underline t))))
 '(org-archived ((t (:foreground "#3a3a3a" :weight bold))))
 '(org-block ((t (:inherit shadow :background "#161616"))))
 '(org-block-begin-line ((t (:background "#1d1a1a" :foreground "grey25"))))
 '(org-checkbox ((t (:background "#171717" :foreground "#8b8378" :box nil))))
 '(org-code ((t (:foreground "IndianRed3"))))
 '(org-column ((t (:background "#1f2124"))))
 '(org-column-title ((t (:background "#1f2124" :foreground "brightmagenta" :underline t :weight bold))))
 '(org-date ((t (:foreground "#6c6c6c" :underline t))))
 '(org-document-info ((t (:foreground "cornflower blue"))))
 '(org-document-info-keyword ((t (:foreground "grey25"))))
 '(org-document-title ((t (:foreground "#6495ed" :weight bold :height 1.5))))
 '(org-done ((t (:foreground "#83a748"))))
 '(org-ellipsis ((t (:foreground "#ff0000"))))
 '(org-footnote ((t (:foreground "cyan4" :underline t))))
 '(org-headline-done ((t (:foreground "grey30"))))
 '(org-hide ((t (:inherit default :background "#161616" :foreground "#161616"))))
 '(org-indent ((t (:background "#161616" :foreground "#161616"))))
 '(org-kbd ((t (:background "#303030" :foreground "LemonChiffon1" :box (:line-width 2 :style released-button)))))
 '(org-level-1 ((t (:foreground "#fbcb41" :underline t :height 1.0))))
 '(org-level-2 ((t (:foreground "#98bd5e"))))
 '(org-level-3 ((t (:foreground "#5361be"))))
 '(org-level-4 ((t (:foreground "#fbcb41"))))
 '(org-link ((t (:foreground "DodgerBlue3" :underline t))))
 '(org-list-dt ((t (:foreground "burlywood"))))
 '(org-meta-line ((t (:inherit font-lock-comment-face :foreground "gray47"))))
 '(org-priority ((t (:foreground "color-166"))))
 '(org-priority-face-one ((t (:foreground "#ff0000"))))
 '(org-priority-face-two ((t (:foreground "#ffff00" :inverse-video nil))))
 '(org-property-value ((t (:foreground "gray35"))) t)
 '(org-quote ((t (:foreground "grey25"))))
 '(org-special-keyword ((t (:foreground "#4e4e4e" :weight normal))))
 '(org-table ((t (:foreground "#646482"))))
 '(org-tag ((t (:foreground "gray40" :slant italic))))
 '(org-todo ((t (:foreground "#cc6666" :weight bold))))
 '(org-verbatim ((t (:foreground "#d7af87"))))
 '(org-waiting-face ((t (:foreground "#d7af87" :weight bold))))
 '(outline-1 ((t (:foreground "#f6aa11" :underline t :slant italic))))
 '(outline-2 ((t (:foreground "#b9d977" :underline t :slant italic))))
 '(outline-3 ((t (:foreground "#5361be" :underline nil))))
 '(outshine-level-1 ((t (:inherit outline-1 :underline t))))
 '(outshine-level-2 ((t (:inherit outline-2 :underline t :slant italic))))
 '(outshine-level-3 ((t (:inherit outline-3 :underline t :slant italic))))
 '(outshine-level-4 ((t (:inherit outline-4 :underline t :slant italic))))
 '(persp-face-lighter-buffer-not-in-persp ((t (:background "#F00" :foreground "#00F"))))
 '(powerline-active0 ((t (:inherit mode-line :background "gray20" :box nil))))
 '(powerline-active1 ((t (:inherit mode-line :background "gray20" :box nil))))
 '(powerline-active2 ((t (:inherit mode-line :background "gray20" :box nil))))
 '(powerline-inactive0 ((t (:inherit mode-line-inactive :background "gray15"))))
 '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "gray15"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "gray15"))))
 '(pulse-highlight-face ((t (:background "#cc6666" :foreground "#161616"))))
 '(pulse-highlight-start-face ((t (:background "#cc6666" :foreground "#161616"))))
 '(quick-peek-background-face ((t (:inherit default :background "gray15"))))
 '(quick-peek-border-face ((t (:background "red
" :box (:line-width 2 :color "grey75" :style released-button) :height 0.1))))
 '(quick-peek-padding-face ((t (:height 0.1))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#d7875f"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#fbcb41"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#98bd5e"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#5893ab"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#af8787"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "plum3"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#cab189"))))
 '(rst-level-1 ((t nil)))
 '(rst-level-2 ((t nil)))
 '(rst-level-3 ((t nil)))
 '(rst-level-4 ((t nil)))
 '(rst-level-5 ((t nil)))
 '(rst-level-6 ((t nil)))
 '(secondary-selection ((t (:background "gray18"))))
 '(shadow ((t (:foreground "#6c6c6c"))))
 '(show-paren-match ((t (:background "grey7" :foreground "firebrick1" :weight bold))))
 '(show-paren-mismatch ((t (:background "#00000" :foreground "#d03620" :inverse-video t :weight bold))))
 '(smerge-lower ((t (:background "grey19"))))
 '(smerge-markers ((t (:background "brown" :foreground "tan"))))
 '(smerge-refined-added ((t (:inherit smerge-refined-change :foreground "#678439"))))
 '(smerge-refined-changed ((t (:background "grey23"))))
 '(smerge-refined-removed ((t (:inherit smerge-refined-change :foreground "#ac443f"))))
 '(smerge-upper ((t (:background "gray19"))))
 '(sp-pair-overlay-face ((t (:inherit lazy-highlight :weight semi-light))))
 '(sp-show-pair-enclosing ((t (:inherit highlight))))
 '(sp-show-pair-match-face ((t (:background "pink" :weight semi-light))))
 '(sp-show-pair-mismatch-face ((t (:background "#161616" :foreground "#d03620" :weight bold))))
 '(sp-wrap-overlay-face ((t nil)))
 '(spaceline-evil-normal ((t (:background "DarkGoldenrod2" :foreground "#000000" :inherit 'mode-line))))
 '(spacemacs-emacs-face ((t (:inherit 'mode-line :background "SkyBlue2" :foreground "#000000"))))
 '(spacemacs-evilified-face ((t (:inherit 'mode-line :background "#ffaf00" :foreground "#000000"))))
 '(spacemacs-hybrid-face ((t (:inherit 'mode-line :background "SkyBlue2" :foreground "#000000"))))
 '(spacemacs-iedit-face ((t (:inherit 'mode-line :background "firebrick1" :foreground "#000000"))))
 '(spacemacs-iedit-insert-face ((t (:inherit 'mode-line :background "firebrick1" :foreground "#000000"))))
 '(spacemacs-insert-face ((t (:inherit 'mode-line :background "chartreuse3" :foreground "#000000"))))
 '(spacemacs-lisp-face ((t (:inherit 'mode-line :background "HotPink1" :foreground "#000000"))))
 '(spacemacs-micro-state-header-face ((t (:background "DarkGoldenrod2" :foreground "#000000" :box (:line-width -1 :color (plist-get (face-attribute 'mode-line :box) :color)) :weight bold))))
 '(spacemacs-motion-face ((t (:inherit 'mode-line :background "plum3" :foreground "#000000"))))
 '(spacemacs-normal-face ((t (:inherit 'mode-line :background "DarkGoldenrod2" :foreground "#000000"))))
 '(spacemacs-replace-face ((t (:inherit 'mode-line :background "chocolate" :foreground "#000000"))))
 '(spacemacs-visual-face ((t (:inherit 'mode-line :background "gray" :foreground "#000000"))))
 '(tabbar-button ((t (:background "#26292c" :foreground "#f8f8f8" :box nil))))
 '(tabbar-button-highlight ((t (:inherit tabbar-default))))
 '(tabbar-default ((t (:inherit nil :background "gray20" :foreground "gray80"))))
 '(tabbar-highlight ((t (:inherit tabbar-default :background "gray20"))))
 '(tabbar-key-binding ((t (:foreground "#005faf"))))
 '(tabbar-modified ((t (:inherit tabbar-button :underline t))))
 '(tabbar-selected ((t (:inherit tabbar-default :background "gray20" :foreground "#337ebe" :box nil))))
 '(tabbar-selected-highlight ((t (:inherit tabbar-selected))))
 '(tabbar-selected-modified ((t (:inherit tabbar-selected))))
 '(tabbar-separator ((t (:inherit tabbar-default :background "#1c1c1c" :foreground "brightgreen"))))
 '(tabbar-unselected ((t (:inherit tabbar-default :background "gray10" :foreground "gray50" :box nil))))
 '(tabbar-unselected-modified ((t (:inherit tabbar-unselected :underline nil))))
 '(tooltip ((t (:inherit variable-pitch :background "lightyellow" :foreground "black"))))
 '(trailing-whitespace ((t (:background "#0037d0" :foreground "black" :inverse-video nil))))
 '(treemacs-directory-face ((t (:inherit font-lock-function-name-face :height 1.0))))
 '(treemacs-file-face ((t (:inherit default))))
 '(treemacs-root-face ((t (:foreground "#cc6666" :underline t :height 1.0))))
 '(variable-pitch ((t (:family "Sans Serif"))))
 '(vertical-border ((t (:foreground "wheat4"))))
 '(warning ((t (:foreground "#f6aa11"))))
 '(whitespace-indentation ((t (:background "grey16" :foreground "#ff4a52"))))
 '(whitespace-newline ((t (:background "green" :foreground "black"))))
 '(whitespace-space ((t (:background "#161616" :foreground "brightyellow"))))
 '(whitespace-tab ((t (:foreground "red"))))
 '(whitespace-trailing ((t (:background "red"))))
 '(widget-field ((t (:background "#303030" :foreground "#337ebe"))))
 '(window-divider ((t (:background "#161616" :foreground "gray36"))))
 '(window-divider-first-pixel ((t (:foreground "gray18"))))
 '(window-divider-last-pixel ((t (:foreground "gray18"))))
 '(window-highlight-focused-window ((t (:background "#666666"))) t)
 '(yascroll:thumb-fringe ((t (:background "#3e4249" :foreground "#3e4249"))))
 '(yascroll:thumb-text-area ((t (:background "#3e4249")))))
)
