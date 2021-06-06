;;;; semicolon leader key
;; inspiration: https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-evil.el

(package-initialize)
(require 'custom-evil-vim-wrapper)

;; =====================================
;; evil-vim defaults
;; =====================================

;; -------------------------------------
;; org-mode
;; -------------------------------------

(add-hook 'org-mode-hook
      (lambda ()
	(setq evil-auto-indent 'nil)))

;; =====================================
;; custom normal state maps
;; =====================================

(define-key evil-normal-state-map "gf" 'helm-find-files)

;; =====================================
;; ";" leader key maps
;; =====================================

(general-create-definer my-comma-leader-def
  :prefix ";"
  :states '(normal visual))

(my-comma-leader-def
  "'" 'evilnc-comment-operator
  ;; window
  ";" 'other-window
  "w" 'evil-switch-to-windows-last-buffer
  "b" 'switch-to-buffer
  "q" 'quit-window
  ;; comments
  "cc" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cy" 'evilnc-copy-and-comment-lines
  "cs" 'comment-dwim			     ; this type of comment
  "ct" 'evilnc-comment-or-uncomment-html-tag ; evil-nerd-commenter v3.3.0 required
  "cb" 'rebox-cycle
  ;; c shortcuts
  "cg" 'ff-find-other-file
  ;; slime
  "lb" 'slime-eval-buffer
  "ld" 'slime-eval-defun
  "lr" 'slime-eval-region
  "ls" 'slime-eval-last-expression
  ;; emacs lisp
  "eb" 'eval-buffer
  "ed" 'eval-defun
  "er" 'eval-region
  "es" 'eval-last-sexp
  ;; indent
  "ir" 'indent-region
  ;; org-mode
  "ox" 'org-toggle-checkbox
  "oe" 'org-babel-execute-src-block-maybe
  "ot" 'org-babel-tangle
  "oi" 'org-clock-in
  "oo" 'org-clock-out
  "ol" 'org-toggle-latex-fragment
  "op" 'org-pomodoro
  "ot" 'my-org-toggle-blocks		; toggle all src, etc blocks in current buffer
  "o'" 'org-edit-special		; to edit babel src blocks in another buffer
  "ose" 'org-edit-special		; to edit babel src blocks in another buffer
  "oso" 'org-edit-src-exit
  "ouc" 'org-update-checkbox-count
  "oud" 'org-update-all-dblocks
  "oh" 'helm-org-in-buffer-headings

  ; "ol" 'worklog-quick-start

  ;; python-mode
  "pg" 'jedi:goto-definition
  "peb" 'python-shell-send-buffer
  "per" 'python-shell-send-region

;;   "bf" 'beginning-of-defun
;;   "bu" 'backward-up-list
;;   "bb" (lambda () (interactive) (switch-to-buffer nil)) ; to previous buffer
;;   "ef" 'end-of-defun
;;   "m" 'evil-set-marker
;;   "em" 'shellcop-erase-buffer
;;   "eb" 'eval-buffer
;;   "sc" 'scratch
;;   "ee" 'eval-expression
;;   "aa" 'copy-to-x-clipboard ; used frequently
;;   "aw" 'ace-swap-window
;;   "af" 'ace-maximize-window
;;   "ac" 'aya-create
;;   "pp" 'paste-from-x-clipboard ; used frequently
;;   "bs" '(lambda () (interactive) (goto-char (car (my-create-range t))))
;;   "es" '(lambda () (interactive) (goto-char (1- (cdr (my-create-range t)))))
;;   "vj" 'my-validate-json-or-js-expression
;;   "kc" 'kill-ring-to-clipboard
;;   "fn" 'cp-filename-of-current-buffer
;;   "fp" 'cp-fullpath-of-current-buffer
;;   "dj" 'dired-jump ;; open the dired from current file
;;   "xo" 'ace-window
;;   "ff" 'my-toggle-full-window ;; I use WIN+F in i3
;;   "ip" 'find-file-in-project
;;   "tt" 'find-file-in-current-directory
;;   "jj" 'find-file-in-project-at-point
;;   "kk" 'find-file-in-project-by-selected
;;   "kn" 'find-file-with-similar-name ; ffip v5.3.1
;;   "fd" 'find-directory-in-project-by-selected
;;   "trm" 'get-term
;;   "tff" 'toggle-frame-fullscreen
;;   "tfm" 'toggle-frame-maximized
;;   "ti" 'fastdef-insert
;;   "th" 'fastdef-insert-from-history
;;   "ci" 'evilnc-comment-or-uncomment-lines
;;   "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
;;   "cc" 'evilnc-copy-and-comment-lines
;;   "cp" 'my-evilnc-comment-or-uncomment-paragraphs
;;   "ct" 'evilnc-comment-or-uncomment-html-tag ; evil-nerd-commenter v3.3.0 required
;;   "ic" 'my-imenu-comments
;;   ;; {{ window move
;;   "wh" 'evil-window-left
;;   "wl" 'evil-window-right
;;   "wk" 'evil-window-up
;;   "wj" 'evil-window-down
;;   ;; }}
;;   "rv" 'my-rename-thing-at-point
;;   "nm" 'js2hl-add-namespace-to-thing-at-point
;;   "rb" 'evilmr-replace-in-buffer
;;   "ts" 'evilmr-tag-selected-region ;; recommended
;;   "rt" 'counsel-etags-recent-tag
;;   "ft" 'counsel-etags-find-tag
;;   "yy" 'counsel-browse-kill-ring
;;   "cf" 'counsel-grep ; grep current buffer
;;   "gf" 'counsel-git ; find file
;;   "gg" 'my-counsel-git-grep ; quickest grep should be easy to press
;;   "gd" 'ffip-show-diff-by-description ;find-file-in-project 5.3.0+
;;   "gt" 'my-evil-goto-definition ; "gt" is occupied by evil
;;   "gl" 'my-git-log-trace-definition ; find history of a function or range
;;   "sh" 'my-select-from-search-text-history
;;   "rjs" 'run-js
;;   "jsr" 'js-comint-send-region
;;   "jsb" 'my-js-clear-send-buffer
;;   "kb" 'kill-buffer-and-window ;; "k" is preserved to replace "C-g"
;;   "ls" 'highlight-symbol
;;   "lq" 'highlight-symbol-query-replace
;;   "ln" 'highlight-symbol-nav-mode ; use M-n/M-p to navigation between symbols
;;   "ii" 'my-imenu-or-list-tag-in-current-file
;;   ;; @see https://github.com/pidu/git-timemachine
;;   ;; p: previous; n: next; w:hash; W:complete hash; g:nth version; q:quit
;;   "tm" 'my-git-timemachine
;;   ;; toggle overview,  @see http://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/
;;   "op" 'compile
;;   "c$" 'org-archive-subtree ; `C-c $'
;;   ;; org-do-demote/org-do-premote support selected region
;;   "c<" 'org-do-promote ; `C-c C-<'
;;   "c>" 'org-do-demote ; `C-c C->'
;;   "cam" 'org-tags-view ; `C-c a m': search items in org-file-apps by tag
;;   "cxi" 'org-clock-in ; `C-c C-x C-i'
;;   "cxo" 'org-clock-out ; `C-c C-x C-o'
;;   "cxr" 'org-clock-report ; `C-c C-x C-r'
;;   "qq" 'my-multi-purpose-grep
;;   "dd" 'counsel-etags-grep-current-directory
;;   "rr" 'my-counsel-recentf
;;   "da" 'diff-region-tag-selected-as-a
;;   "db" 'diff-region-compare-with-b
;;   "di" 'evilmi-delete-items
;;   "si" 'evilmi-select-items
;;   "jb" 'my-beautfiy-code
;;   "jp" 'my-print-json-path
;;   ;; {{ @see http://ergoemacs.org/emacs/emacs_pinky_2020.html
;;   ;; `keyfreq-show' proved sub-window operations happen most.
;;   "x0" 'delete-window
;;   "x1" 'delete-other-windows
;;   "x2" 'split-window-vertically
;;   "x3" 'split-window-horizontally
;;   "xq" 'delete-window
;;   "xa" 'split-window-vertically
;;   "xd" 'split-window-horizontally
;;   "s0" 'delete-window
;;   "s1" 'delete-other-windows
;;   "s2" 'split-window-vertically
;;   "s3" 'split-window-horizontally
;;   "sq" 'delete-window
;;   "sa" 'split-window-vertically
;;   "sd" 'split-window-horizontally
;;   "oo" 'delete-other-windows
;;   ;; }}
;;   "xr" 'rotate-windows
;;   "xt" 'toggle-two-split-window
;;   "uu" 'my-transient-winner-undo
;;   "fs" 'ffip-save-ivy-last
;;   "fr" 'ivy-resume
;;   "ss" 'my-swiper
;;   "fb" '(lambda ()
;;           (interactive)
;;           (my-ensure 'wucuo)
;;           (let* ((wucuo-flyspell-start-mode "normal"))
;;             (wucuo-spell-check-internal)))
;;   "fe" 'flyspell-goto-next-error
;;   "fa" 'flyspell-auto-correct-word
;;   "lb" 'langtool-check-buffer
;;   "ll" 'langtool-goto-next-error
;;   "pe" 'lazyflymake-goto-prev-error
;;   "ne" 'lazyflymake-goto-next-error
;;   "og" 'org-agenda

;;   "otl" 'org-toggle-link-display
;;   "oa" '(lambda ()
;;           (interactive)
;;           (my-ensure 'org)
;;           (counsel-org-agenda-headlines))
;;   "ar" 'align-regexp
;;   "wrn" 'httpd-restart-now
;;   "wrd" 'httpd-restart-at-default-directory
;;   "bk" 'buf-move-up
;;   "bj" 'buf-move-down
;;   "bh" 'buf-move-left
;;   "bl" 'buf-move-right
;;   "0" 'winum-select-window-0-or-10
;;   "1" 'winum-select-window-1
;;   "2" 'winum-select-window-2
;;   "3" 'winum-select-window-3
;;   "4" 'winum-select-window-4
;;   "5" 'winum-select-window-5
;;   "6" 'winum-select-window-6
;;   "7" 'winum-select-window-7
;;   "8" 'winum-select-window-8
;;   "9" 'winum-select-window-9
;;   "xm" 'counsel-M-x
;;   "xx" 'er/expand-region
;;   "xf" 'counsel-find-file
;;   "xb" 'ivy-switch-buffer-by-pinyin
;;   "xh" 'mark-whole-buffer
;;   "xk" 'kill-buffer
;;   "xs" 'save-buffer
;;   "xc" 'my-switch-to-shell
;;   "xz" 'my-switch-to-shell
;;   "vf" 'vc-rename-file-and-buffer
;;   "vc" 'vc-copy-file-and-rename-buffer
;;   "xv" 'vc-next-action ; 'C-x v v' in original
;;   "va" 'git-add-current-file
;;   "vk" 'git-checkout-current-file
;;   "vg" 'vc-annotate ; 'C-x v g' in original
;;   "vv" 'vc-msg-show
;;   "v=" 'git-gutter:popup-hunk
;;   "hh" 'cliphist-paste-item
;;   "yu" 'cliphist-select-item
;;   "ih" 'my-goto-git-gutter ; use ivy-mode
;;   "ir" 'ivy-resume
;;   "ww" 'narrow-or-widen-dwim
;;   "ycr" 'my-yas-reload-all
;;   "wf" 'popup-which-function
)

(provide 'custom-evil-vim)
