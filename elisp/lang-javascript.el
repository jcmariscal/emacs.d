;; js2-mode
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :bind (:map js2-mode-map
              (("C-x C-e" . js-send-last-sexp)
               ("C-M-x" . js-send-last-sexp-and-go)
               ("C-c C-b" . js-send-buffer-and-go)
               ("C-c C-l" . js-load-file-and-go)))
  :mode
  ("\\.js$" . js2-mode)
  ("\\.json$" . js2-jsx-mode)
  :config
  (custom-set-variables '(js2-strict-inconsistent-return-warning nil))
  (custom-set-variables '(js2-strict-missing-semi-warning nil))
  (setq js-indent-level 2)
  (setq js2-indent-level 2)
  (setq js2-basic-offset 2))

;; Run a JavaScript interpreter in an inferior process window
;; https://github.com/redguardtoo/js-comint
(use-package js-comint
  :config
  (setq inferior-js-program-command "node"))

;; js2-refactor :- refactoring options for emacs
;; https://github.com/magnars/js2-refactor.el
(use-package js2-refactor :defer t
  :diminish js2-refactor-mode
  :config
  (js2r-add-keybindings-with-prefix "C-c j r")
  (add-hook 'js2-mode-hook 'js2-refactor-mode))

(provide 'lang-javascript)