;; Add your custom functions here

;; (defun something
;;    (do-something))

(package-initialize)

;;;; =======================
;;;; COMPANY-MODE
;;;; =======================
;; disable company-mode in org
(setq company-global-modes '(not org-mode))

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
(load-library "./nip.el.gpg")
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

;;;; ======================================
;;;; ORG-REF
;;;; SOURCE:
;;;; =======================================

;; wrap lines
(global-visual-line-mode 1)

;; ;; setup org-ref

;; turn off if slow in large files
;; (setq org-ref-show-broken-links t)

 (setq reftex-default-bibliography '("~/org-ref/references.bib"))
 (setq org-ref-bibliography-notes "~/org-ref/notes.org"
       org-ref-default-citation-link "parencite"
       org-ref-notes-directory "~/org-ref/"
       org-ref-default-bibliography '("~/org-ref/references.bib")
       org-ref-pdf-directory "~/org-ref/bibtex-pdfs/")

 (unless (file-exists-p org-ref-pdf-directory)
   (make-directory org-ref-pdf-directory t))

;; use helm-bibtex
(setq bibtex-completion-bibliography "~/org-ref/references.bib"
      bibtex-completion-library-path "~/org-ref/bibtex-pdfs"
      bibtex-completion-notes-path "~/org-ref/bibtex-notes")

(setq helm-bibtex-bibliography "~/org-ref/references.bib" ;; where your references are stored
      helm-bibtex-library-path "~/org-ref/" ;; where your pdfs etc are stored
      helm-bibtex-notes-path "~/org-ref/notes.org" ;; where your notes are stored
      bibtex-completion-bibliography "~/org-ref/references.bib" ;; writing completion
      bibtex-completion-notes-path "~/org-ref/notes.org")

;; open pdf with system pdf viewer (works on mac)
(setq bibtex-completion-pdf-open-function
  (lambda (fpath)
    (start-process "open" "*open*" "open" fpath)))

;; org-mode latex export minted source environment
;; https://emacs.stackexchange.com/questions/27982/export-code-blocks-in-org-mode-with-minted-environment
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; break-lines
;; source: https://emacs.stackexchange.com/questions/33010/how-to-word-wrap-within-code-blocks
(setq org-latex-listings-options '(("breaklines" "true")))
(setq org-latex-minted-options '(("breaklines" "true")
                                 ("breakanywhere" "true")))

;; get rid of temp files latex makes on export:https://nickgeorge.net/science/org_ref_setup/
(setq org-latex-logfiles-extensions (quote ("lof" "lot" "tex" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "pygtex" "pygstyle")))

;; Some org-mode customization
(setq org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-src-preserve-indentation t)


(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

(setq org-latex-default-packages-alist
      (-remove-item
       '("" "hyperref" nil)
       org-latex-default-packages-alist))

;; Append new packages
(add-to-list 'org-latex-default-packages-alist '("" "natbib" "") t)
(add-to-list 'org-latex-default-packages-alist
	     '("linktocpage,pdfstartview=FitH,colorlinks,
linkcolor=blue,anchorcolor=blue,
citecolor=blue,filecolor=blue,menucolor=blue,urlcolor=blue"
	       "hyperref" nil)
	     t)

;; ORG-MODE increase size of font in math formulas
;; source: https://emacs.stackexchange.com/questions/19880/font-size-control-of-latex-previews-in-org-files

(setq org-format-latex-options (plist-put org-format-latex-options :scale 3.0))

;; set agenda files folder and regex recursively (to see tags and agenda items)
;; source: https://stackoverflow.com/questions/24966333/emacs-org-mode-tags-not-found
(setq org-agenda-files (append
			(list "~/")
			(directory-files-recursively "~/0s4ts1d3/" "\\.org$")))
(setq org-agenda-file-regexp "\\`[^.].*\\.org\\|.todo\\'")

;; break emacs
;; (setq org-ref-completion-library 'org-ref-ivy-cite)
;; (require 'ox-manuscript)
;; (setq org-latex-pdf-process 'ox-manuscript-latex-pdf-process)

(setq org-ref-completion-library 'org-ref-helm)
(require 'org-ref)
(require 'org-id)
(require 'org-ref-wos)
(require 'org-ref-scopus)
(require 'org-ref-pubmed)
(require 'dash)


;;;; =============
;;;; ORG-POMODORO
;;;; =============

;; source: https://github.com/marcinkoziej/org-pomodoro/issues/89
(defun my-org-pomodoro-resume-after-break ()
  "Resume Org Pomodoro after break is finished."
  (org-clock-goto)
  (org-pomodoro))
(setq org-pomodoro-length 25)
(setq org-pomodoro-short-break-length 5)
(setq org-pomodoro-long-break-length 10)
(add-hook 'org-pomodoro-break-finished-hook #'my-org-pomodoro-resume-after-break)

;;;; =============
;; org-babel
;;;; =============
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (C . t)
   (calc . t)
   (latex . t)
   (java . t)
   (ruby . t)
   (lisp . t)
   (scheme . t)
   (shell . t)
   (sqlite . t)
   (js . t)
   (restclient . t)))

;;;; ===========
;;;; org-bullets
;;;; ===========
;; note: slow package
;; source: https://github.com/sabof/org-bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; javascript support
(require 'ob-js)
(add-to-list 'org-babel-load-languages '(js . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("js" . "js"))

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

;;;; =======================================
;;;; org-mode insert source after selecting text
;;;; =======================================
;; author zck: https://emacs.stackexchange.com/questions/5740/how-to-quickly-format-selected-code-in-org-mode
(defun org-wrap-source ()
  " wraps selected text with SRC tags in org-mode"
  (interactive)
  (let ((start (min (point) (mark)))
        (end (max (point) (mark))))
    (goto-char end)
    (unless (bolp)
      (newline))
    (insert "#+END_SRC\n")
    (goto-char start)
    (unless (bolp)
      (newline))
    (insert "#+BEGIN_SRC\n")))

;; (define-key global-mode-map (kbd "C - c o") 'org-wrap-source)
(global-set-key (kbd "C-c c p p") 'org-wrap-source) ; C-c c p p

;;;; ===============
;;;; Dokuwiki module
;;;; ===============

;; need to enable remote API system
(require 'dokuwiki)
(setq dokuwiki-xml-rpc-url url-dokuwiki)
(setq dokuwiki-login-user-name "jc")

;; set dokuwiki major mode to .dwiki and .dokuwiki files
;; todo: fix bug
(add-to-list 'auto-mode-alist '("\\.dwiki\\'" . dokuwiki-mode))
(add-to-list 'auto-mode-alist '("\\.dokuwiki\\'" . dokuwiki-mode))

;; emacs -nw autocompletion in helm
;; ================================
(require 'cl)
;;(define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
;;(define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
;;(define-key helm-map (kbd "C-z") #'helm-select-action)

;; TRANSPARENCY
;; =====================
;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; set transparency parameters and font
(set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(alpha 85 85))
(set-face-attribute 'default nil :background "black"
		    ;:foreground "white" :font "IBM Plex Mono" :weight 'normal :width 'normal :height 120)
		    :foreground "white" :font "Pragmata Pro Mono" :weight 'normal :width 'normal :height 120)
		    ;:foreground "white" :font "Fira Code Retina" :weight 'normal :width 'normal :height 120)

(provide 'base-functions)
