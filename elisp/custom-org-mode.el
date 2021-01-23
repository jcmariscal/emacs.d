(load-library "./nip.el.gpg")

;; =====================================
;; ORG-MODE
;; =====================================

;; disable company-mode in org
(setq company-global-modes '(not org-mode))

;; Some org-mode customization
(setq org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-src-preserve-indentation t)


;; increase size of font in math formulas
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

;; =====================================
;; ORG PUBLISHING
;; =====================================

;; depends on nip.el.gpg
(require 'ox-publish)
(setq org-publish-project-alist
      (list
	`("project-notes-helper-org-files"
	 ;:base-directory "~/foo/notes/"
	 :base-directory ,my-project-notes-base-directory ;uses variable my-project-notes-base-directory from encrypted nip.el.gpg
	 :base-extension "org"
	 ;:publishing-directory "~/foo/notes/_exported_html"
	 :publishing-directory ,my-project-notes-publishing-directory
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :headline-levels 4             ; Just the default for this project.
	 :auto-preamble t
	 )

	`("project-notes-helper-static-cs"
	 ;:base-directory "~/foo/notes/"
	 :base-directory ,my-project-notes-cs-directory
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|js"
	 ;:publishing-directory "~/foo/notes/_exported_html/"
	 :publishing-directory ,my-project-notes-publishing-directory
	 :recursive t
	 :publishing-function org-publish-attachment
	 )

	`("project-notes-helper-static-trading"
	 ;:base-directory "~/foo/notes/"
	 :base-directory ,my-project-notes-trading-directory
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|js"
	 ;:publishing-directory "~/foo/notes/_exported_html/"
	 :publishing-directory ,my-project-notes-publishing-directory
	 :recursive t
	 :publishing-function org-publish-attachment
	 )

	'("project-notes" :components ("project-notes-helper-org-files"
				       "project-notes-helper-static-cs"
				       "project-notes-helper-static-trading"))

	))

;; ======================================
;; ORG-REF
;; SOURCE:
;; =======================================

;; ;; setup org-ref

;; turn off if slow in large files
;; (setq org-ref-show-broken-links t)

 (setq org-ref-bibliography-notes "~/org-ref/notes.org"
       org-ref-default-citation-link "parencite"
       org-ref-notes-directory "~/org-ref/"
       org-ref-default-bibliography '("~/org-ref/references.bib")
       org-ref-pdf-directory "~/org-ref/bibtex-pdfs/")

 (unless (file-exists-p org-ref-pdf-directory)
   (make-directory org-ref-pdf-directory t))


;; -------------------------------------
;; REFTEX
;; -------------------------------------

 (setq reftex-default-bibliography '("~/org-ref/references.bib"))

;; -------------------------------------
;; BIBTEX and HELM-BIBTEX
;; -------------------------------------

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

(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

;; -------------------------------------
;; ORG-LATEX
;; -------------------------------------

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

;; =============
;; ORG-POMODORO
;; =============

;; source: https://github.com/marcinkoziej/org-pomodoro/issues/89
(defun my-org-pomodoro-resume-after-break ()
  "Resume Org Pomodoro after break is finished."
  (org-clock-goto)
  (org-pomodoro))
(setq org-pomodoro-length 25)
(setq org-pomodoro-short-break-length 5)
(setq org-pomodoro-long-break-length 10)
(add-hook 'org-pomodoro-break-finished-hook #'my-org-pomodoro-resume-after-break)

;; =====================================
;; ORG-BABEL
;; =====================================
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

;; javascript support
(require 'ob-js)
(add-to-list 'org-babel-load-languages '(js . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("js" . "js"))


;;;; ===========
;;;; org-bullets
;;;; ===========
;; note: slow package
;; source: https://github.com/sabof/org-bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;;; =======================================
;;;; ORG-MODE custom functions
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

(require 'org-ref)
(require 'org-id)
(require 'org-ref-wos)
(require 'org-ref-scopus)
(require 'org-ref-pubmed)
(require 'dash)
(provide 'custom-org-mode)
