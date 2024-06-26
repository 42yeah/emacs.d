;;; Emacs Bedrock
;;;
;;; Extra config: Base enhancements

;;; Usage: Append or require this file from init.el to enable various UI/UX
;;; enhancements.
;;;
;;; The consult package in particular has a vast number of functions that you
;;; can use as replacements to what Emacs provides by default. Please see the
;;; consult documentation for more information and help:
;;;
;;;     https://github.com/minad/consult
;;;
;;; In particular, many users may find `consult-line' to be more useful to them
;;; than isearch, so binding this to `C-s' might make sense. This is left to the
;;; user to configure, however, as isearch and consult-line are not equivalent.

;;; Contents:
;;;
;;;  - Motion aids
;;;  - Power-ups: Embark and Consult
;;;  - Minibuffer and completion
;;;  - Misc. editing enhancements

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Motion aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package latex
  :ensure auctex
  :mode (("\\.tex\\'" . LaTeX-mode))
  :config (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq TeX-auto-save t) (setq TeX-parse-self t) (setq-default TeX-master nil))

(use-package avy
  :ensure t
  :demand t
  :bind (("C-c j"   . avy-goto-line)
         ("M-g M-g" . avy-goto-line)
         ("M-j"     . avy-goto-char-timer))
  :bind* (("C-j" . avy-goto-char)
          ("C-S-j" . avy-goto-char-2)))

;; Navigational aids

(use-package beacon
  :ensure t
  :config (beacon-mode))

;; Smooth, pixel scroll (hopefully)
(defun pixel-scroll-kbd-up ()
  (interactive)
  (let ((half-height (/ (window-pixel-height) 2)))
    (pixel-scroll-precision-interpolate (* 0.5 half-height))))

(defun pixel-scroll-kbd-down ()
  (interactive)
  (let ((half-height (/ (window-pixel-height) 2)))
    (pixel-scroll-precision-interpolate (* 0.5 (- half-height)))))

(defun pixel-scroll-kbd-up-precise ()
  (interactive)
  (let ((half-height (/ (window-pixel-height) 2)))
    (pixel-scroll-precision-interpolate (* 0.25 half-height))))

(defun pixel-scroll-kbd-down-precise ()
  (interactive)
  (let ((half-height (/ (window-pixel-height) 2)))
    (pixel-scroll-precision-interpolate (* 0.25 (- half-height)))))

(global-set-key (kbd "C-v") 'pixel-scroll-kbd-down)
(global-set-key (kbd "M-v") 'pixel-scroll-kbd-up)
(global-set-key (kbd "C-S-v") 'pixel-scroll-kbd-down-precise)
(global-set-key (kbd "C-M-S-v") 'pixel-scroll-kbd-up-precise)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Power-ups: Embark and Consult
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Consult: Misc. enhanced commands
(use-package consult
  :ensure t
  :bind (;; Drop-in replacements
         ("C-x b" . consult-buffer)     ; orig. switch-to-buffer
         ("M-y"   . consult-yank-pop)   ; orig. yank-pop
         ;; Searching
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)       ; Alternative: rebind C-s to use
         ("M-s s" . consult-line)       ; consult-line instead of isearch, bind
         ("M-s L" . consult-line-multi) ; isearch to M-s s
         ("M-s o" . consult-outline)
         ("M-s g" . consult-grep)
         ("M-s l" . consult-line)            ; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)      ; needed by consult-line to detect isearch
         ("C-x r b" . consult-bookmark)      ; orig. bookmark-jump
         ("M-g m" . consult-mark)
         ;; ("C-u C-SPC" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("C-c !" . consult-flymake)
         ;; Isearch integration
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ; orig. isearch-edit-string
         :map Info-mode-map
         ("s" . consult-info)
         )
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<"))

(use-package embark
  :ensure t
  :demand t
  :after avy
  :bind (("C-c a" . embark-act))        ; bind this to an easy key to hit
  :init
  ;; Add the option to run embark when using avy
  (defun bedrock/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select
  (setf (alist-get ?. avy-dispatch-alist) 'bedrock/avy-action-embark))

(use-package embark-consult
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer and completion
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vertico: better vertical completion for minibuffer commands
(use-package vertico
  :ensure t
  :init
  ;; You'll want to make sure that e.g. fido-mode isn't enabled
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; Popup completion-at-point
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous))
  :config
  (setq global-corfu-modes '((not debugger-mode) t)))

;; Part of corfu
(use-package corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

;; Fancy completion-at-point functions; there's too much in the cape package to
;; configure here; dive in when you're comfortable!
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package eshell
  :init
  (defun bedrock/setup-eshell ()
    ;; Something funny is going on with how Eshell sets up its keymaps; this is
    ;; a work-around to make C-r bound in the keymap
    (keymap-set eshell-mode-map "C-r" 'consult-history))
  :hook ((eshell-mode . bedrock/setup-eshell)))

;; Orderless: powerful completion style
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Misc. editing enhancements
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Modify search results en masse
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

;; Cut/copy whole line or region. SO useful.
(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-global-mode))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))

;; Windmove state swap left/right
(use-package windmove
  :bind (("C-S-<left>" . windmove-swap-states-left)
         ("C-S-<right>" . windmove-swap-states-right)
         ("C-S-<up>" . windmove-swap-states-up)
         ("C-S-<down>" . windmove-swap-states-down)))
