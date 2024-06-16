;; Stolen from purcell's .emacs.d. -*- lexical-binding: t -*-
;; 

(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

;; Markdown-mode
(use-package markdown-mode
  :ensure t
  :defer t)

(use-package cmake-mode
  :ensure t
  :defer t)

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package smart-mode-line
  :ensure t
  :config (smart-mode-line-enable))

(use-package indent-guide
  :ensure t
  :config (indent-guide-global-mode))

(use-package treemacs
  :ensure t
  :bind (("C-x C-m" . treemacs)))

;; Multi-cursor
;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :ensure t
  :bind (("C-+" . 'mc/mark-next-like-this)))
