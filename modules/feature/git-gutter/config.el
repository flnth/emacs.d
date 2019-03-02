;; modules/feature/git-gutter/config.el    -*- lexical-binding: t; -*-


;; TODO:  make ordinary hydra out of this
(spacemacs|define-transient-state fn-vcs
  :title "VCS Transient State (modded)"
  :doc "
 Hunk Commands^^^^^^                          Magit Commands
----------------------------^^^^^^            ------------------------------------------
 [_n_]^^^^      next hunk                      [_w_/_u_]^^    stage/unstage in current file
 [_N_/_p_]^^    previous hunk                  [_c_/_C_]^^    quick commit/commit with popup
 [_x_/_s_/_._/_h_]  revert/stage/commit/show     [_f_/_F_/_P_]  fetch/pull/push popup
 [_t_]^^^^      toggle diff signs              [_l_/_D_]^^    log/diff popup"
  :on-enter (spacemacs/vcs-enable-margin)
  :on-exit (let ((inhibit-message t)) (fn-disable-git-gutter))
  :bindings
  ("c" +magit-quick-commit :exit t)
  ("." fn-magit-stage-commit :exit t)
  ("d" magit-ediff-popup :exit t)
  ("D" magit-diff-unstaged :exit t)
  ("F" magit-pull-popup :exit t)
  ("P" magit-push-popup :exit t)
  ("C" magit-commit-popup :exit t)
  ("f" magit-fetch-popup :exit t)
  ("l" magit-log-popup :exit t)
  ("u" magit-unstage-file)
  ("w" magit-stage-file)
  ("n" spacemacs/vcs-next-hunk)
  ("N" spacemacs/vcs-previous-hunk)
  ("p" spacemacs/vcs-previous-hunk)
  ("r" spacemacs/vcs-revert-hunk)
  ("x" spacemacs/vcs-revert-hunk)
  ("s" spacemacs/vcs-stage-hunk)
  ("h" spacemacs/vcs-show-hunk)
  ("t" spacemacs/toggle-version-control-margin)
  ("q" fn-disable-git-gutter :exit t))

;;; git-gutter
(use-package git-gutter
  :commands (spacemacs/fn-vcs-transient-state/body)

  :init
  ;; git-gutter+ crashes sometimes, do use git-gutter instead
  (setq version-control-diff-tool 'git-gutter)
  (spacemacs/set-leader-keys "g." #'(lambda () (interactive)
									  (let ((inhibit-message t))
										(spacemacs/fn-vcs-transient-state/body))))

  :config
  (global-git-gutter-mode -1)
  (global-git-gutter+-mode -1)
  ;; fix for prev/next hunk:
  (defun git-gutter:search-near-diff-index (diffinfos is-reverse)
	(cl-loop with current-line = (line-number-at-pos)
			 with cmp-fn = (if is-reverse #'> #'<)
			 for diffinfo in (if is-reverse (reverse diffinfos) diffinfos)
			 for index in (number-sequence 0 (- (length diffinfos) 1))
			 for start-line = (git-gutter-hunk-start-line diffinfo)
			 when (funcall cmp-fn current-line start-line)
			 return (if is-reverse
						(1- (- (length diffinfos) index))
					  index)))

  (defun fn-disable-git-gutter ()
	(interactive)
	(let ((current-prefix-arg '(0))
		  (inhibit-message t))
	  ;; (call-interactively 'git-gutter+-mode)
	  (call-interactively 'git-gutter-mode)))

  (defun fn-magit-stage-commit ()
	(interactive)
	(spacemacs/vcs-stage-hunk)
	(magit-commit)))

;;; git-gutter+
(use-package git-gutter+
  :config
  (global-git-gutter+-mode -1)
  (run-with-timer 10 nil #'(lambda ()
							 (global-git-gutter+-mode -1)
							 )))
