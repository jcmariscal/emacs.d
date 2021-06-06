;; Add your custom functions here

;; (defun something
;;    (do-something))

(package-initialize)
(load-library "./nip.el.gpg")


;; =====================================
;; MAGIT
;; =====================================

;; use evil-mode for editing commit message in magit
;; source: https://emacs.stackexchange.com/questions/14008/default-magit-commit-state-in-evil
(add-hook 'with-editor-mode-hook 'evil-insert-state)

;; =====================================
;; Appearance customizations
;; =====================================

;; wrap lines
;; https://emacs.stackexchange.com/questions/19629/word-wrap-line-option-by-default
(global-visual-line-mode t)


;;;; =======================
;;;; COMPANY-MODE
;;;; =======================

;;;; =======================
;;;; UNICODE SUPPORT
;;;; =======================
;;Fuente: https://www.enmimaquinafunciona.com/pregunta/54947/como-hacer-que-emacs-aceptan-utf-8-desde-el-teclado
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-language-environment 'UTF-8)
; prefer utf-8 for language settings
(set-default-coding-systems 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(prefer-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8-unix)
(setq default-file-name-coding-system 'utf-8-unix)
(setq default-keyboard-coding-system 'utf-8-unix)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(setq default-sendmail-coding-system 'utf-8-unix)
(setq default-terminal-coding-system 'utf-8-unix)

;;;; =======================
;;;; HEFTZNER LOGIN
;;;; =======================

;; source: https://stackoverflow.com/questions/20624024/what-is-the-best-way-to-open-remote-files-with-emacs-and-ssh
;; (setq url-hetzner "/ssh:root@000.000.00.00:/root/")
;; /method:user@host#port:filename
;; https://www.gnu.org/software/tramp/
;; depends on: nip.el.gpg
(defun login-hetz ()
    (interactive)
    (find-file-literally url-hetzner))

;;;; ========================
;;;; PERFORMANCE ENHANCEMENTS
;;;; ========================

;; disable linum-mode
;; https://www.emacswiki.org/emacs/LineNumbers
(global-display-line-numbers-mode)
(global-linum-mode 0) (add-hook 'term-mode-hook (lambda () (linum-mode -1)))

;; ===========================
;; C / C++
;; ===========================
;; https://stackoverflow.com/questions/663588/emacs-c-mode-incorrect-indentation
;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/Built_002din-Styles.html
;; https://www.emacswiki.org/emacs/IndentingC

(setq c-default-style '((java-mode . "java")
			(awk-mode . "awk")
			(other . "linux")
			(c-mode . "linux")
			(c++-mode . "stroustrup"))
      c-basic-offset 4)

(defun my-c++-mode-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0)
  (company-mode 1))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(defun my-c-mode-common-hook ()
 ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
 (c-set-offset 'substatement-open 0)
 ;; other customizations can go here

 (setq c++-tab-always-indent t)
 (setq c-basic-offset 4)                  ;; Default is 2
 (setq c-indent-level 4)                  ;; Default is 2

 (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
 (setq tab-width 4)
 (setq indent-tabs-mode t)  ; use spaces only if nil
 (company-mode 1)
 )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; irony mode: https://github.com/Sarcasm/irony-mode
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; END C/ C++ ========================================================

;; =======================================
;; evil mode
;; =======================================

;; pre-requisite: install evil from package manager
  (require `evil)
  (evil-mode 1)
   (defun my-move-key (keymap-from keymap-to key)
     "Moves key binding from one keymap to another, deleting from the old location. "
     (define-key keymap-to key (lookup-key keymap-from key))
     (define-key keymap-from key nil))
   (my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
   (my-move-key evil-motion-state-map evil-normal-state-map " ")

;; =======================================
;; SLIME defaults
;; =======================================
;; Set your lisp system and, optionally, some contribs
(setq inferior-lisp-program "/usr/bin/clisp")
(setq slime-contribs '(slime-fancy))

;;https://github.com/purcell/ac-slime
(add-hook 'slime-mode-hook 'auto-complete-mode)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;;;; =======================================
;;;; kill all buffers except current one
;;;; =======================================
(defun kill-other-buffers ()
      "Kill all other buffers."
      (interactive)
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;;;; ===============
;;;; Dokuwiki module
;;;; ===============

;; need to enable remote API system
(require 'dokuwiki)
;; (setq dokuwiki-xml-rpc-url url-dokuwiki)
;;(setq dokuwiki-xml-rpc-url "https://www.tostareweb.com")
(setq dokuwiki-xml-rpc-url "http://tostareweb.com:9093/lib/exe/xmlrpc.php")
;;(setq dokuwiki-xml-rpc-url "http://localhost:9091/lib/exe/xmlrpc.php")
;;(setq dokuwiki-xml-rpc-url "http://34.203.212.15/lib/exe/xmlrpc.php")
(setq dokuwiki-login-user-name "jc")

;; set dokuwiki major mode to .dwiki and .dokuwiki files
;; todo: fix bug
(add-to-list 'auto-mode-alist '("\\.dwiki\\'" . dokuwiki-mode))
(add-to-list 'auto-mode-alist '("\\.dokuwiki\\'" . dokuwiki-mode))

;; ================================
;; emacs -nw autocompletion in helm
;; ================================
(require 'cl)
;;(define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
;;(define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
;;(define-key helm-map (kbd "C-z") #'helm-select-action)

;; =====================
;; TRANSPARENCY
;; =====================
;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; set transparency parameters and font
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))
(set-face-attribute 'default nil :background "black"
		    ;:foreground "white" :font "IBM Plex Mono" :weight 'normal :width 'normal :height 120)
		    :foreground "white" :font "Pragmata Pro Mono" :weight 'normal :width 'normal :height 120)
		    ;:foreground "white" :font "Fira Code Retina" :weight 'normal :width 'normal :height 120)

(provide 'base-functions)
