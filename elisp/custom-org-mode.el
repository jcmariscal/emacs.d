;;;; ORG-MODE
;; disable company-mode in org
(setq company-global-modes '(not org-mode))

;;;; ORG PUBLISHING
;; depends on nip.el.gpg
(load-library "./nip.el.gpg")
(require 'ox-publish)
(setq org-publish-project-alist
      (list
	`("project-notes-helper-org-files"
	 ;:base-directory "~/foo/notes/"
	 :base-directory ,my-project-notes-base-directory
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
