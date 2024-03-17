;;; backup.el --- script backups for init.el. ---
;;; Commentary:
;; Some backup codes for lsp-mode and consult-lsp.
;; They are overly laggy.

;;; Code:
(require 'consult-lsp)
(add-hook 'lsp-mode-hook
          (lambda () (local-set-key (kbd "C-M-.") #'consult-lsp-symbols)
            (local-set-key (kbd "C-M-;") #'consult-lsp-file-symbols)))

;; LSP-mode
;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          (c++-mode . lsp)
;;          (c-mode . lsp)
;;          ;; if you want which-key integration
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)
;; (use-package lsp-ui :commands lsp-ui-mode)
;; (require 'lsp-ui)
;; ;; (global-set-key (kbd "C-.") lsp-find-definition)
;; (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;; (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
;; (setq lsp-headerline-arrow ">")
(menu-bar-mode 0)

(provide 'backup)
;;; backup.el ends here
