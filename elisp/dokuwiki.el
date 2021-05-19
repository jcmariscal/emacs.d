;;; dokuwiki.el --- Edit Remote DokuWiki Pages Using XML-RPC

;; Copyright (C) 2017 Juan Karlo Licudine
;; Copyright (C) 2020 J. C. Mariscal Melgar

;; Author: Juan Karlo Licudine <accidentalrebel@gmail.com>
;; URL: http://www.github.com/accidentalrebel/emacs-dokuwiki
;; Version: 1.0.0
;; Keywords: convenience
;; Package-Requires: ((emacs "24.3") (xml-rpc "1.6.8"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides a way to edit a remote Dokuwiki wiki on Emacs.  Uses Dokuwiki's XML-RPC API.

;; Usage:
;; (require 'dokuwiki) ;; unless installed as a package

;;; License:

;; This program is free software; you can redistributfe it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'xml-rpc)
(require 'auth-source)

(defgroup dokuwiki nil
  "Edit remote Dokuwiki pages using XML-RPC"
 :group 'dokuwiki)

(defcustom dokuwiki-xml-rpc-url ""
  "The url pointing to the \"xmlrpc.php\" file in the wiki to be accessed."
  :group 'dokuwiki
  :type 'string)

(defcustom dokuwiki-login-user-name ""
  "The user name to use when logging in to the wiki."
  :group 'dokuwiki
  :type 'string)

(defvar dokuwiki--has-successfully-logged-in nil
  "A variable that is set to true once successfully logged in to a wiki.")

;;;###autoload
(defun dokuwiki-login ()
  "Connects to the dokuwiki."
  (interactive)
  (let* ((xml-rpc-url (dokuwiki--get-xml-rpc-url))
         (credentials (dokuwiki--credentials))
         (login-user-name (plist-get credentials :user))
         (login-password (plist-get credentials :password)))
    (if (not (xml-rpc-method-call xml-rpc-url 'dokuwiki.login login-user-name login-password))
	(error "Login unsuccessful! Check if your dokuwiki-xml-rpc-url or login credentials are correct!")
      (message "Login successful!")
      (setq dokuwiki--has-successfully-logged-in t))))

(defun dokuwiki-open-page (page-name-or-url)
  "Opens a page from the wiki.
PAGE-NAME-OR-URL: The page id or url to open.
To open a page in a particular namespace add the namespace name before
the page-name.  For example, \"namespace:wiki-page\" to open the
\"wiki-page\" page inside the \"namespace\" namespace.
If the specified page does not exist, it creates a new page once the
buffer is saved."
  (interactive "sEnter page name: ")
  (if (not dokuwiki--has-successfully-logged-in)
      (user-error "Login first before opening a page")
    (let* ((page-name (car (last (split-string page-name-or-url "/"))))
	  (page-content (xml-rpc-method-call dokuwiki-xml-rpc-url 'wiki.getPage page-name)))
      (message "Page name is \"%s\"" page-name)
      (if (not page-content)
	  (message "Page not found in wiki. Creating a new buffer with page name \"%s\"" page-name)
	(message "Page exists. Creating buffer for existing page \"%s\"" page-name))
      (get-buffer-create (concat page-name ".dwiki"))
      (switch-to-buffer (concat page-name ".dwiki"))
      ; set major mode as dokuwiki-mode
      (dokuwiki-mode)
      (erase-buffer)
      (when page-content
	(insert page-content)))))

(defun dokuwiki-save-page ()
  "Save the current buffer as a page in the wiki.
Uses the buffer name as the page name.  A buffer of \"wiki-page.dwiki\"
is saved as \"wikiurl.com/wiki-page\".  On the other hand, a buffer of
\"namespace:wiki-page.dwiki\" is saved as \"wikiurl.com/namespace:wiki-page\""
  (interactive)
  (if (not dokuwiki--has-successfully-logged-in)
      (user-error "Login first before saving a page")
    (if (not (string-match-p ".dwiki" (buffer-name)))
	(error "The current buffer is not a .dwiki buffer")
      (let ((page-name (replace-regexp-in-string ".dwiki" "" (buffer-name))))
	(if (not (y-or-n-p (concat "Do you want to save the page \"" page-name "\"?")))
	    (message "Cancelled saving of the page."))
	 (let* ((summary (read-string "Summary: "))
		(minor (y-or-n-p "Is this a minor change? "))
		(save-success (xml-rpc-method-call dokuwiki-xml-rpc-url 'wiki.putPage page-name (buffer-string) `(("sum" . ,summary) ("minor" . ,minor)))))
	   (if save-success
	       (message "Saving successful with summary %s and minor of %s." summary minor)
	     (error "Saving unsuccessful!")))))))

; temporary funcion to ask whether the user wants to login or not
(defun dokuwiki-l ()
  "Connects to the dokuwiki."
  (interactive)
  (if (not (y-or-n-p (concat "Do you want to login?")))
      (message "not-logging-in")
    (dokuwiki-login)))

; save-fast function
(defun dokuwiki-sf ()
  "dokuwiki-save-fast: Save current buffer as page in the wiki without interaction"
  (dokuwiki-l)
  (interactive)
  (if (not dokuwiki--has-successfully-logged-in)
      (user-error "Login first before saving a page")
    (if (not (string-match-p ".dwiki" (buffer-name)))
	(error "The current buffer is not a .dwiki buffer")
      ;; page-name removes .dwiki extension
      (let ((page-name (replace-regexp-in-string ".dwiki" "" (buffer-name))))
	 (let* ((summary "minor change")
		(minor t)
		(save-success (xml-rpc-method-call dokuwiki-xml-rpc-url 'wiki.putPage page-name (buffer-string) `(("sum" . ,summary) ("minor" . ,minor)))))
	   (if save-success
	       (message "Saving successful with summary %s and minor of %s." summary minor)
	     (error "Saving unsuccessful!")))))))

;; save and list pages
(defun dokuwiki-lps ()
  "List pages and saves current buffer, logins if not logged in already"
  (dokuwiki-l)
  (interactive)
  (if (not dokuwiki--has-successfully-logged-in)
      (user-error "Login first before saving a page")
    (if (not (string-match-p ".dwiki" (buffer-name)))
	(error "The current buffer is not a .dwiki buffer")
      (let ((page-name (replace-regexp-in-string ".dwiki" "" (buffer-name))))
	(let* ((summary "minor change")
	       (minor t)
	       (save-success (xml-rpc-method-call dokuwiki-xml-rpc-url 'wiki.putPage page-name (buffer-string) `(("sum" . ,summary) ("minor" . ,minor)))))
	  (if save-success
	      (message "Saving successful with summary %s and minor of %s." summary minor)
	    (error "Saving unsuccessful!"))))))
  (if (not dokuwiki--has-successfully-logged-in)
      (user-error "Login first before listing the pages")
    (let ((page-detail-list (xml-rpc-method-call dokuwiki-xml-rpc-url 'wiki.getAllPages))
	  (wiki-title (dokuwiki-get-wiki-title))
	  (page-list ()))
      (dolist (page-detail page-detail-list)
	(push (cdr (assoc "id" page-detail)) page-list)
	)
      (dokuwiki-open-page (completing-read "Select a page to open: " page-list)))))

(defun dokuwiki-ll ()
  "login and list pages"
  (dokuwiki-sa)
  (interactive)
  (if (not dokuwiki--has-successfully-logged-in)
      (user-error "Login first before listing the pages")
    (let ((page-detail-list (xml-rpc-method-call dokuwiki-xml-rpc-url 'wiki.getAllPages))
	  (wiki-title (dokuwiki-get-wiki-title))
	  (page-list ()))
      (dolist (page-detail page-detail-list)
	(push (cdr (assoc "id" page-detail)) page-list)
	)
      (dokuwiki-open-page (completing-read "Select a page to open: " page-list)))))

; todo: save all buffers
(defun dokuwiki-sa ()
  "Saves all opened dokuwiki buffers"
  (dokuwiki-l)
  (interactive)
  ;; (save-buffer-matches string) -> list of matched
  ;; creates a list of matches in elisp buffer list
  ;; eg. (save-buffer-matches "dwiki") -> ("foo.dwiki" "foo2.dwiki")
  ;; inline function:https://www.gnu.org/software/emacs/manual/html_node/cl/Function-Bindings.html
  (cl-flet ((save-buffer-matches (my-string-to-match)
				 (remove nil (mapcar #'(lambda (x)
							 (if (string-match my-string-to-match x)
							     x))
						     (mapcar #'buffer-name (buffer-list))))))
    ;; body
    (let ((dokuwiki-buffers (save-buffer-matches "dwiki")))
      (mapcar #'(lambda (doku-buffer)
		  ;; see: https://www.gnu.org/software/emacs/manual/html_node/elisp/Current-Buffer.html#Current-Buffer
		  ;; goes into another buffer virtually
		  (save-current-buffer
		    (set-buffer doku-buffer)
		    ;; remove dwiki extension
		    (let ((page-name (replace-regexp-in-string ".dwiki" "" (buffer-name))))
		      ;; upload modification to dokuwiki server
		      (let* ((summary "minor change")
			     (minor t)
			     (save-success (xml-rpc-method-call dokuwiki-xml-rpc-url 'wiki.putPage page-name (buffer-string) `(("sum" . ,summary) ("minor" . ,minor))))
			     )
			;; messages
			(if save-success
			    (message "%s saving successful." (buffer-name) summary minor)
			  (error "Saving unsuccessful!"))))))


	      (save-buffer-matches "dwiki")))))

(defun dokuwiki-get-wiki-title ()
  "Gets the title of the current wiki."
  (interactive)
  (if (not dokuwiki--has-successfully-logged-in)
      (user-error "Login first before getting the wiki title")
    (let ((dokuwiki-title (xml-rpc-method-call dokuwiki-xml-rpc-url 'dokuwiki.getTitle)))
      (message "The title of the wiki is \"%s\"" dokuwiki-title))))

(defun dokuwiki-list-pages ()
  "Show a selectable list containing pages from the current wiki."
  (dokuwiki-login)
  (interactive)
  (if (not dokuwiki--has-successfully-logged-in)
      (user-error "Login first before listing the pages")
    (let ((page-detail-list (xml-rpc-method-call dokuwiki-xml-rpc-url 'wiki.getAllPages))
	  (wiki-title (dokuwiki-get-wiki-title))
	  (page-list ()))
      (dolist (page-detail page-detail-list)
	(push (cdr (assoc "id" page-detail)) page-list)
	)
      (dokuwiki-open-page (completing-read "Select a page to open: " page-list)))))


;; Helpers
(defun dokuwiki--credentials ()
  "Read dokuwiki credentials either from auth source or from the user input."
  (let ((auth-source-credentials (nth 0 (auth-source-search :max 1 :host (dokuwiki--get-xml-rpc-url) :require '(:user :secret)))))
    (if auth-source-credentials
        (let* ((user (plist-get auth-source-credentials :user))
               (password-raw (plist-get auth-source-credentials :secret))
               (password (if (functionp password-raw) (funcall password-raw) password-raw)))
          (list :user user :password password))
      (let ((user (dokuwiki--get-login-user-name))
            (password (read-passwd "Enter password: ")))
        (list :user user :password password)))))

(defun dokuwiki--get-xml-rpc-url ()
  "Gets the xml-rpc to be used for logging in."
  (if (not (string= dokuwiki-xml-rpc-url ""))
      dokuwiki-xml-rpc-url
    (let ((xml-rpc-url (read-string "Enter wiki URL: ")))
      (message "The entered wiki url is \"%s\"." xml-rpc-url)
      xml-rpc-url)))

(defun dokuwiki--get-login-user-name ()
  "Gets the login user name to be used for logging in."
  (if (not (string= dokuwiki-login-user-name ""))
      dokuwiki-login-user-name
    (let ((login-name (read-string "Enter login user name: ")))
      (message "The entered login user name is \"%s\"." login-name)
      login-name)))

;;; dokuwiki-mode.el --- Major mode for DokuWiki document

;; Copyright (C)  2013-2017 Tsunenobu Kai

;; Author: Tsunenobu Kai <kai2nenobu@gmail.com>
;; URL: https://github.com/kai2nenobu/emacs-dokuwiki-mode
;; Version: 0.1.1
;; Keywords: hypermedia text DokuWiki

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defgroup dokuwiki-mode nil
  "Major mode for DokuWiki document."
  :group 'text
  :group 'dokuwiki
  :tag "DokuWiki"
  :link '(url-link "https://www.dokuwiki.org/dokuwiki"))

(defvar dokuwiki-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") 'outline-next-visible-heading)
    (define-key map (kbd "C-c C-p") 'outline-previous-visible-heading)
    (define-key map (kbd "C-c C-f") 'outline-forward-same-level)
    (define-key map (kbd "C-c C-b") 'outline-backward-same-level)
    (define-key map (kbd "C-c C-u") 'outline-up-heading)
    (define-key map (kbd "C-c C-@") 'outline-mark-subtree)
    map)
  "Keymap for the `dokuwiki-mode'.")

(defvar dokuwiki-smiley-list
  '("8-)" "8-O" ":-(" ":-)" "=) " ":-/" ":-\\" ":-?" ":-D" ":-P" ":-O"
    ":-X" ":-|" ";-)" "^_^" ":?:" ":!:" "LOL" "FIXME" "DELETEME")
  "Smiley list in DokuWiki.")

(defvar dokuwiki-outline-regexp " ?\\(=\\{2,6\\}\\)"
  "Regexp which indicates headline in DokuWiki.
See also `outline-regexp'.")

;;;; Faces
(defface dokuwiki-box '((t (:box t)))
  "Face enabled box property")

(defface dokuwiki-code '((t (:inherit shadow)))
  "DokuWiki face for code."
  :group 'dokuwiki)

(defface dokuwiki-list '((t (:inherit font-lock-type-face)))
  "DokuWiki face for list."
  :group 'dokuwiki)

(defface dokuwiki-verbatim '((t (:inherit shadow)))
  "DokuWiki face for text as is."
  :group 'dokuwiki)

(defface dokuwiki-footnote '((t (:inherit font-lock-builtin-face)))
  "DokuWiki face for footnote."
  :group 'dokuwiki)

(defface dokuwiki-headline-1 '((t (:inherit outline-1)))
  "DokuWiki face for level 1 headline."
  :group 'dokuwiki)

(defface dokuwiki-headline-2 '((t (:inherit outline-2)))
  "DokuWiki face for level 2 headline."
  :group 'dokuwiki)

(defface dokuwiki-headline-3 '((t (:inherit outline-3)))
  "DokuWiki face for level 3 headline."
  :group 'dokuwiki)

(defface dokuwiki-headline-4 '((t (:inherit outline-4)))
  "DokuWiki face for level 4 headline."
  :group 'dokuwiki)

(defface dokuwiki-headline-5 '((t (:inherit outline-5)))
  "DokuWiki face for level 5 headline."
  :group 'dokuwiki)

(defface dokuwiki-link '((t (:inherit link)))
  "DokuWiki face for link."
  :group 'dokuwiki)

(defface dokuwiki-image '((t (:inherit font-lock-variable-name-face)))
  "DokuWiki face for image."
  :group 'dokuwiki)

(defface dokuwiki-table '((t (:inherit font-lock-function-name-face)))
  "DokuWiki face for table."
  :group 'dokuwiki)

(defface dokuwiki-smiley '((t (:inherit font-lock-constant-face)))
  "DokuWiki face for smiley."
  :group 'dokuwiki)

(defvar dokuwiki-font-lock-keywords
  `(
   ;; bold
   ("\\*\\*.+?\\*\\*" (0 'bold append))
   ;; italic
   ("//.+?//" . (0 'italic append))
   ;; underline
   ("__.+?__" . (0 'underline append))
   ;; monospace
   ("''\\(.+?\\)''" (0 'dokuwiki-code append) (1 'dokuwiki-box append))
   ;; verbatim
   ("%%.+?%%" (0 'dokuwiki-code t))
   ;; footnote
   ("((.+?))" (0 'dokuwiki-footnote))
   ;; headline
   (" ?======.+======[ \t]*$" (0 'dokuwiki-headline-1))
   (" ?=====.+=====[ \t]*$" (0 'dokuwiki-headline-2))
   (" ?====.+====[ \t]*$" (0 'dokuwiki-headline-3))
   (" ?===.+===[ \t]*$" (0 'dokuwiki-headline-4))
   (" ?==.+==[ \t]*$" (0 'dokuwiki-headline-5))
   ;; link
   ("\\[\\[[^|]+?\\(?:\\(|\\)\\(.*?\\)\\)?\\]\\]"
    (0 'dokuwiki-link) (1 'dokuwiki-code t t)
    (2 'font-lock-string-face t t) (2 'underline append t))
   ("https?://\\(\\([-_.!~*'()a-zA-Z0-9;?:@&=+$,%#]+\\)/?\\)+" (0 'dokuwiki-link))
   ;; image
   ("{{[^|]+?\\(|\\(.*?\\)\\)?}}"
    (0 'dokuwiki-image t)
    (1 'dokuwiki-code t t) (2 'font-lock-string-face t t))
   ;; table
   ("^[ \t]*[|^].*$" (0 'dokuwiki-table))
   ;; linebreak
   ("\\\\\\\\\\s-+" (0 'dokuwiki-code t))
   ;; list
   ("^\\(?: \\{2\\}\\|[\t]\\)[ \t]*" "\\([-*]\\).*$" nil nil (1 'dokuwiki-list))
   ;; code block
   ("^\\(?: \\{2\\}\\|[\t]\\)[ \t]*" dokuwiki-code-block-search
     nil nil (0 'dokuwiki-code t))
   ;; smiley
   ,@(mapcar #'(lambda (smiley)
                 (list (concat "\\W\\(" (regexp-quote smiley) "\\)\\W")
                       1 'dokuwiki-smiley))
             dokuwiki-smiley-list)
   ))

(defun dokuwiki-code-block-search (limit)
  (if (not (looking-at "[-*]"))
      (re-search-forward ".*$" limit t)))

(defun dokuwiki-outline-level ()
  "Compute a header's nesting level in `dokuwiki-mode'.
See also `outline-level'."
  (when (looking-at outline-regexp)
    (let ((const 7)
          (headline (match-string 1)))
      (- const (length headline)))))

;;;; Work with `outline-magic'
(eval-after-load "outline-magic"
  '(progn
     (define-key dokuwiki-mode-map (kbd "TAB") 'outline-cycle)
     (define-key dokuwiki-mode-map (kbd "<S-tab>")
       '(lambda () (interactive) (outline-cycle '(4))))
     (define-key dokuwiki-mode-map (kbd "<M-S-right>") 'outline-demote)
     (define-key dokuwiki-mode-map (kbd "<M-S-left>") 'outline-promote)
     (define-key dokuwiki-mode-map (kbd "<M-up>") 'outline-move-subtree-up)
     (define-key dokuwiki-mode-map (kbd "<M-down>") 'outline-move-subtree-down)
     (add-hook 'dokuwiki-mode-hook 'dokuwiki-outline-magic-hook)
     ;; Enable outline-magic features in `dokuwiki-mode' buffers
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (eq major-mode 'dokuwiki-mode) (dokuwiki-outline-magic-hook))))
     ))

(defun dokuwiki-outline-magic-hook ()
  "Hook to configure `outline-magic'."
  (set (make-local-variable 'outline-promotion-headings)
       '(("======" . 1) ("=====" . 2) ("====" . 3) ("===" . 4) ("==" . 5)))
  (set (make-local-variable 'outline-cycle-emulate-tab) t))

;;;###autoload
(define-derived-mode dokuwiki-mode text-mode "DokuWiki"
  "Major mode for DokuWiki document."
  (set (make-local-variable 'font-lock-defaults)
       '(dokuwiki-font-lock-keywords
         nil nil ((?_ . "w")) nil))
  (set (make-local-variable 'outline-regexp) dokuwiki-outline-regexp)
  (set (make-local-variable 'outline-level) 'dokuwiki-outline-level)
  (outline-minor-mode 1)
  )

(provide 'dokuwiki-mode)
;; ___________end dokuwiki-mode_______________________

;;; dokuwiki-mode.el ends here
;; -*- coding: utf-8 -*-
;; @dokuwiki-mode syntax highlighting
;; https://github.com/larsjsol/dokuwiki-mode.el
;;
;; Lars Jørgen Solberg <supersolberg@gmail.com> 2013
;;
;;
;; (defvar dokuwiki-mode-hook nil)
;;
;; (defvar dokuwiki-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     map)
;;   "Keymap for dokuwiki-mode")
;;
;; (defvar dokuwiki-mode-syntax-table
;;   (let ((dokuwiki-mode-syntax-table (make-syntax-table)))
;;     dokuwiki-mode-syntax-table)
;;   "Syntax table for dokuwiki-mode")
;;
;;
;; (defconst dokuwiki-mode-font-lock-verbatim
;;   (let (
;;         (ident "\\(^ +[^* -].*$\\)")
;;         (percent "\\(%%.*?%%\\)")
;;         (nowiki "\\(<nowiki>\\(\n\\|.\\)+?</nowiki>\\)")
;;         (code "\\(<code.*?>\\(\n\\|.\\)+?</code>\\)")
;;         (file "\\(<file.*?>\\(\n\\|.\\)+?</file>\\)")
;;         (php "\\(<php>\\(\n\\|.\\)+?</php>\\)")
;;         (html "\\(<html>\\(\n\\|.\\)+?</html>\\)")
;;         (cphp "\\(<PHP>\\(\n\\|.\\)+?</PHP>\\)")
;;         (chtml "\\(<HTML>\\(\n\\|.\\)+?</HTML>\\)")
;;         )
;;     (list (cons (mapconcat 'identity (list ident percent nowiki code file
;;                                            php html cphp chtml) "\\|")
;;                 font-lock-preprocessor-face))))
;;
;; (defface dokuwiki-face-heading1 '((default (:height 200))) "Level 1 Heading")
;; (defconst dokuwiki-mode-font-lock-keywords-heading1 '(("^[[:space:]]?\\(=\\{6\\}\\)\\(.*?\\)\\(=\\{6\\}\\)"
;;                                                        (1 'font-lock-keyword-face)
;;                                                        (2 'dokuwiki-face-heading1)
;;                                                        (3 'font-lock-keyword-face))))
;;
;; (defface dokuwiki-face-heading2 '((default (:height 180))) "Level 2 Heading")
;; (defconst dokuwiki-mode-font-lock-keywords-heading2 '(("^[[:space:]]?\\(=\\{5\\}\\)\\(.*?\\)\\(=\\{5\\}\\)"
;;                                                        (1 'font-lock-keyword-face)
;;                                                        (2 'dokuwiki-face-heading2)
;;                                                        (3 'font-lock-keyword-face))))
;;
;; (defface dokuwiki-face-heading3 '((default (:height 150))) "Level 3 Heading")
;; (defconst dokuwiki-mode-font-lock-keywords-heading3 '(("^[[:space:]]?\\(=\\{4\\}\\)\\(.*?\\)\\(=\\{4\\}\\)"
;;                                                        (1 'font-lock-keyword-face)
;;                                                        (2 'dokuwiki-face-heading3)
;;                                                        (3 'font-lock-keyword-face))))
;;
;; (defface dokuwiki-face-heading4 '((default (:weight ultra-bold :height 130))) "Level 4 Heading")
;; (defconst dokuwiki-mode-font-lock-keywords-heading4 '(("^[[:space:]]?\\(=\\{3\\}\\)\\(.*?\\)\\(=\\{3\\}\\)"
;;                                                        (1 'font-lock-keyword-face)
;;                                                        (2 'dokuwiki-face-heading4)
;;                                                        (3 'font-lock-keyword-face))))
;;
;; (defface dokuwiki-face-heading5 '((default (:weight extra-bold))) "Level 5 Heading")
;; (defconst dokuwiki-mode-font-lock-keywords-heading5 '(("^[[:space:]]?\\(=\\{2\\}\\)\\(.*?\\)\\(=\\{2\\}\\)"
;;                                                        (1 'font-lock-keyword-face)
;;                                                        (2 'dokuwiki-face-heading5)
;;                                                        (3 'font-lock-keyword-face))))
;;
;;
;; (defconst dokuwiki-mode-font-lock-keywords-named-link '(("\\(\\[\\[\\)\\([^]]+\\)\\(|{?{?\\)\\(.*?\\)\\(}?}?\\]\\]\\)"
;;                                                    (1 'font-lock-keyword-face)
;;                                                    (2 'font-lock-string-face)
;;                                                    (3 'font-lock-keyword-face)
;;                                                    (4 'font-lock-string-face)
;;                                                    (5 'font-lock-keyword-face))))
;;
;; (defconst dokuwiki-mode-font-lock-keywords-image-link '(("\\({{\\)\\(.*?\\)\\(}}\\)"
;;                                                          (1 'font-lock-keyword-face)
;;                                                          (2 'font-lock-string-face)
;;                                                          (3 'font-lock-keyword-face))))
;;
;;
;; (defconst dokuwiki-mode-font-lock-keywords-link '(("\\(\\[\\[\\)\\(.*?\\)\\(\\]\\]\\)"
;;                                                    (1 'font-lock-keyword-face)
;;                                                    (2 'font-lock-string-face)
;;                                                    (3 'font-lock-keyword-face))))
;;
;;
;; (defconst dokuwiki-mode-font-lock-keywords-bare-link (list (cons "\\(http://\\)?\w+\\.[[:alpha:]]+\\.[^[:space:]]+"
;;                                                              font-lock-string-face)))
;;
;; (defconst dokuwiki-mode-font-lock-keywords-email-link '(("\\(<\\)\\(.+@.+\\)\\(>\\)"
;;                                                    (1 'font-lock-keyword-face)
;;                                                    (2 'font-lock-string-face)
;;                                                    (3 'font-lock-keyword-face))))
;;
;;
;; (defface dokuwiki-face-bold '((default (:bold t))) "Bold")
;; (defconst dokuwiki-mode-font-lock-keywords-bold '(("\\([*][*]\\)\\(.*?\\)\\([*][*]\\)"
;;                                                    (1 'font-lock-keyword-face)
;;                                                    (2 'dokuwiki-face-bold)
;;                                                    (3 'font-lock-keyword-face))))
;;
;; (defface dokuwiki-face-italic '((default (:slant italic))) "Italic")
;; (defconst dokuwiki-mode-font-lock-keywords-italic '(("\\(//\\)\\(.*?\\)\\(//\\)"
;;                                                    (1 'font-lock-keyword-face)
;;                                                    (2 'dokuwiki-face-italic)
;;                                                    (3 'font-lock-keyword-face))))
;;
;; (defface dokuwiki-face-underlined '((default (:underline t))) "Underlined")
;; (defconst dokuwiki-mode-font-lock-keywords-underlined '(("\\(__\\)\\(.*?\\)\\(__\\)"
;;                                                    (1 'font-lock-keyword-face)
;;                                                    (2 'dokuwiki-face-underlined)
;;                                                    (3 'font-lock-keyword-face))))
;;
;; (defface dokuwiki-face-strike-through '((default (:strike-through t))) "Strike-through")
;; (defconst dokuwiki-mode-font-lock-keywords-strike-through '(("\\(<del>\\)\\(.*?\\)\\(</del>\\)"
;;                                                    (1 'font-lock-keyword-face)
;;                                                    (2 'dokuwiki-face-strike-through)
;;                                                    (3 'font-lock-keyword-face))))
;;
;; (defconst dokuwiki-mode-font-lock-horizontal-line (list (cons "----+" font-lock-keyword-face)))
;;
;; (defconst dokuwiki-mode-font-lock-constants
;;   (let ((smilies '("8-)" "8-O" ":-(" ":-)" "=)" ":-/" ":-\\" ":-?" ":-D" ":-P" ":-O"
;;                    ":-X" ":-|" ";-)" "^_^" ":?:" ":!:" "LOL" "FIXME" "DELETEME"))
;;         (entities '("<->" "->" "<-" "<=>" "=>" "<=" ">>" "<<"
;;                     "---" "--" "(c)" "(tm)" "(r)" "..."))
;;         (abbrs '("ACL" "AFAICS" "AFAIK" "AFAIR" "API" "ASAP" "ASCII" "BTW" "CMS"
;;                  "CSS" "DNS" "EOF" "EOL" "EOM" "EOT" "FAQ" "FTP" "FOSS" "FLOSS" "FUD"
;;                  "GB" "GHz" "GPL" "GUI" "HTML" "IANAL" "IE" "IIRC" "IMHO" "IMO" "IOW"
;;                  "IRC" "IRL" "KISS" "LAN" "LGPL" "LOL" "MathML" "MB" "MHz" "MSIE"
;;                  "OMG" "OS" "OSS" "OTOH" "PITA" "RFC" "ROTFL" "RTFM" "spec " "TIA"
;;                  "TL;DR" "TOC" "URI" "URL" "W3C" "WTF?" "WYSIWYG" "YMMV"))
;;         (macros '("~~NOTOC~~" "~~NOCACHE~~")))
;;     (list (cons (regexp-opt (append smilies entities abbrs macros) nil) font-lock-constant-face)))
;;   "DokuWiki 'constants'")
;;
;;
;;
;; (defconst dokuwiki-mode-font-lock-quote (list (cons "^>+" font-lock-keyword-face)))
;;
;; (defconst dokuwiki-mode-font-lock-keywords-other (list (cons (regexp-opt '(
;;                                                                            "<sub>" "</sub>"
;;                                                                            "<sup>" "</sup>"
;;                                                                            "((" "))"
;;                                                                            "''"
;;                                                                            " * "
;;                                                                            " - "
;;                                                                            "|" "^"
;;                                                                            ) nil)
;;                                                              font-lock-keyword-face)))
;;
;; (defface dokuwiki-face-linebreak '((default (:foreground "red1" :weight bold))) "Forced line break")
;; (defconst dokuwiki-font-lock-linebreak '(("[\\]\\{2\\}$"
;;                                           (0 'dokuwiki-face-linebreak))))
;;
;; (defvar dokuwiki-mode-font-lock (append
;;                                  dokuwiki-mode-font-lock-verbatim
;;                                  dokuwiki-mode-font-lock-keywords-heading1
;;                                  dokuwiki-mode-font-lock-keywords-heading2
;;                                  dokuwiki-mode-font-lock-keywords-heading3
;;                                  dokuwiki-mode-font-lock-keywords-heading4
;;                                  dokuwiki-mode-font-lock-keywords-heading5
;;                                  dokuwiki-mode-font-lock-horizontal-line
;;                                  dokuwiki-mode-font-lock-quote
;;                                  dokuwiki-mode-font-lock-constants
;;                                  dokuwiki-mode-font-lock-keywords-named-link
;;                                  dokuwiki-mode-font-lock-keywords-image-link
;;                                  dokuwiki-mode-font-lock-keywords-link
;;                                  dokuwiki-mode-font-lock-keywords-bare-link
;;                                  dokuwiki-mode-font-lock-keywords-email-link
;;                                  dokuwiki-mode-font-lock-keywords-bold
;;                                  dokuwiki-mode-font-lock-keywords-italic
;;                                  dokuwiki-mode-font-lock-keywords-underlined
;;                                  dokuwiki-mode-font-lock-keywords-strike-through
;;                                  dokuwiki-mode-font-lock-keywords-other
;;                                  dokuwiki-font-lock-linebreak
;;                                  )
;;   "Syntax highlighting expressions for dokuwiki-mode")
;;
;; (defun dokuwiki-ident-line ()
;;   "Ident current line as DokuWiki markup"
;;   (interactive)
;;   (beginning-of-line))
;;
;;
;; ;; taken from http://stackoverflow.com/a/15239704
;; (defun test-font-lock-extend-region ()
;;   "Extend the search region to include an entire block of text."
;;   ;; Avoid compiler warnings about these global variables from font-lock.el.
;;   ;; See the documentation for variable `font-lock-extend-region-functions'.
;;   (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
;;   (save-excursion
;;     (goto-char font-lock-beg)
;;     (let ((found (or (re-search-backward "\n\n" nil t) (point-min))))
;;       (goto-char font-lock-end)
;;       (when (re-search-forward "\n\n" nil t)
;;         (beginning-of-line)
;;         (setq font-lock-end (point)))
;;       (setq font-lock-beg found))))
;;
;;
;; (define-derived-mode dokuwiki-mode fundamental-mode "DokuWiki"
;;   (interactive)
;;   (kill-all-local-variables)
;;   (use-local-map dokuwiki-mode-map)
;;   (set-syntax-table dokuwiki-mode-syntax-table)
;;   (set (make-local-variable 'font-lock-multiline) t)
;;   (set (make-local-variable 'font-lock-defaults) '(dokuwiki-mode-font-lock t))
;;   (set (make-local-variable 'ident-line-function) 'dokuwiki-ident-line)
;;   (setq major-mode 'dokuwiki-mode)
;;   (run-hooks 'dokuwiki-mode-hook)
;;   (add-hook 'font-lock-extend-region-functions
;;             'test-font-lock-extend-region)
;;   )
;; ;;___________end of dokuwiki-mode______________
;;

(add-to-list 'auto-mode-alist '("\\.dokuwiki\\'" . dokuwiki-mode))
(add-to-list 'auto-mode-alist '("\\.dwiki\\'" . dokuwiki-mode))

(provide 'dokuwiki)
;;; dokuwiki.el ends here
