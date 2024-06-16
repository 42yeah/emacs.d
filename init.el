;;;  ________                                                _______                 __                            __
;;; /        |                                              /       \               /  |                          /  |
;;; $$$$$$$$/ _____  ____   ______   _______  _______       $$$$$$$  | ______   ____$$ | ______   ______   _______$$ |   __
;;; $$ |__   /     \/    \ /      \ /       |/       |      $$ |__$$ |/      \ /    $$ |/      \ /      \ /       $$ |  /  |
;;; $$    |  $$$$$$ $$$$  |$$$$$$  /$$$$$$$//$$$$$$$/       $$    $$</$$$$$$  /$$$$$$$ /$$$$$$  /$$$$$$  /$$$$$$$/$$ |_/$$/
;;; $$$$$/   $$ | $$ | $$ |/    $$ $$ |     $$      \       $$$$$$$  $$    $$ $$ |  $$ $$ |  $$/$$ |  $$ $$ |     $$   $$<
;;; $$ |_____$$ | $$ | $$ /$$$$$$$ $$ \_____ $$$$$$  |      $$ |__$$ $$$$$$$$/$$ \__$$ $$ |     $$ \__$$ $$ \_____$$$$$$  \
;;; $$       $$ | $$ | $$ $$    $$ $$       /     $$/       $$    $$/$$       $$    $$ $$ |     $$    $$/$$       $$ | $$  |
;;; $$$$$$$$/$$/  $$/  $$/ $$$$$$$/ $$$$$$$/$$$$$$$/        $$$$$$$/  $$$$$$$/ $$$$$$$/$$/       $$$$$$/  $$$$$$$/$$/   $$/

;;; Minimal init.el

;;; Contents:
;;;
;;;  - Basic settings
;;;  - Discovery aids
;;;  - Minibuffer/completion settings
;;;  - Interface enhancements/defaults
;;;  - Tab-bar configuration
;;;  - Theme
;;;  - Optional extras
;;;  - Built-in customization framework

;;; Guardrail

(when (< emacs-major-version 29)
  (error (format "Emacs Bedrock only works with Emacs 29 and newer; you have version ~a" emacs-major-version)))

(if (eq system-type 'windows-nt)
    (setq url-proxy-services '(("http" . "127.0.0.1:10809")
                               ("https" . "127.0.0.1:10809")))
  (setq url-proxy-services '(("http" . "127.0.0.1:7890")
                             ("https" . "127.0.0.1:7890"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Package initialization
;;
;; We'll stick to the built-in GNU and non-GNU ELPAs (Emacs Lisp Package
;; Archive) for the base install, but there are some other ELPAs you could look
;; at if you want more packages. MELPA in particular is very popular. See
;; instructions at:
;;
;;    https://melpa.org/#/getting-started
;;
;; You can simply uncomment the following if you'd like to get started with
;; MELPA packages quickly:
;;
;; (with-eval-after-load 'package
;;   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; If you want to turn off the welcome screen, uncomment this
;(setopt inhibit-splash-screen t)

(setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
(setopt display-time-default-load-average nil) ; this information is useless for most

;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
;; Some systems don't do file notifications well; see
;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Save history of minibuffer
(savehist-mode)

;; Move through windows with Ctrl-<arrow keys>
(windmove-default-keybindings 'control) ; You can use other modifiers here

;; Fix archaic defaults
(setopt sentence-end-double-space nil)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'bedrock--backup-file-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Discovery aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the help buffer after startup
;; (add-hook 'after-init-hook 'help-quick)
(add-hook 'after-init-hook 'recentf-mode)
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude `("/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'")))

;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion

(setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
(setopt completions-detailed t)                        ; Show annotations
(setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                     ; This is arbitrary
(setopt completions-detailed t)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)            ; Much more eager
;(setopt completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;; For a fancier built-in completion option, try ido-mode,
;; icomplete-vertical, or fido-mode. See also the file extras/base.el

;(icomplete-vertical-mode)
;(fido-vertical-mode)
;(setopt icomplete-delay-completions-threshold 4000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mode line information
(setopt line-number-mode t)                        ; Show current line in modeline
(setopt column-number-mode t)                      ; Show column as well

(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

;; We won't set these, but they're good to know about
;;
(setopt indent-tabs-mode nil)
(setopt tab-width 4)

;; Misc. UI tweaks
(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling

;; Use common keystrokes by default
;; (cua-mode)

;; Display line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 3)           ; Set a minimum width

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)
(defun do-truncate-lines ()
  (setq truncate-lines t))
(add-hook 'prog-mode-hook 'do-truncate-lines)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tab-bar configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the tab-bar as soon as tab-bar functions are invoked
(setopt tab-bar-show 1)

;; Add the time to the tab-bar, if visible
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setopt display-time-format "%a %F %T")
(setopt display-time-interval 1)
(display-time-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config
  (load-theme 'modus-vivendi))          ; for light theme, use modus-operandi

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Optional extras
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Uncomment the (load-file …) lines or copy code from the extras/ elisp files
;; as desired

;; UI/UX enhancements mostly focused on minibuffer and autocompletion interfaces
;; These ones are *strongly* recommended!
(load-file (expand-file-name "extras/base.el" user-emacs-directory))

;; Packages for software development
(load-file (expand-file-name "extras/dev.el" user-emacs-directory))

;; My local, custom scripts
(if (file-exists-p "custom.el")
    (load-file (expand-file-name "custom.el" user-emacs-directory)))

;; My global, custom scripts
(load-file (expand-file-name "custom-global.el" user-emacs-directory))

;; PragmataPro Ligatures
(load-file (expand-file-name "pragmatapro-lig.el" user-emacs-directory))
(pragmatapro-lig-global-mode)

;; Vim-bindings in Emacs (evil-mode configuration)
;(load-file (expand-file-name "extras/vim-like.el" user-emacs-directory))

;; Org-mode configuration
;; WARNING: need to customize things inside the elisp file before use! See
;; the file extras/org-intro.txt for help.
;(load-file (expand-file-name "extras/org.el" user-emacs-directory))

;; Email configuration in Emacs
;; WARNING: needs the `mu' program installed; see the elisp file for more
;; details.
;(load-file (expand-file-name "extras/email.el" user-emacs-directory))

;; Tools for academic researchers
;(load-file (expand-file-name "extras/researcher.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in customization framework
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager) "dvi2tty")
     ((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi")
     (output-pdf "Okular") (output-html "xdg-open")))
 '(beacon-blink-when-point-moves-vertically 5)
 '(beacon-color "#ffaa11")
 '(beacon-size 40)
 '(blink-cursor-mode nil)
 '(c-basic-offset 4)
 '(c-default-style
   '((java-mode . "java") (awk-mode . "awk") (csharp-mode . "csharp")
     (other . "linux")))
 '(c-offsets-alist '((inline-open . +) (innamespace . +)))
 '(c-ts-mode-indent-offset 4)
 '(c-ts-mode-indent-style 'linux)
 '(column-number-mode t)
 '(corfu-auto t)
 '(corfu-auto-delay 0.1)
 '(corfu-auto-prefix 1)
 '(corfu-quit-no-match t)
 '(css-indent-offset 4)
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
     "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223"
     "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e"
     "efcecf09905ff85a7c80025551c657299a4d18c5fcfedd3b2f2b6287e4edd659"
     "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e"
     "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3"
     "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e"
     "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2"
     "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8"
     "6f96a9ece5fdd0d3e04daea6aa63e13be26b48717820aa7b5889c602764cf23a"
     "d6b934330450d9de1112cbb7617eaf929244d192c4ffb1b9e6b63ad574784aad"
     "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882"
     "4e2e42e9306813763e2e62f115da71b485458a36e8b4c24e17a2168c45c9cf9d"
     "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9"
     "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19"
     "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
     default))
 '(delete-selection-mode t)
 '(display-time-mode t)
 '(dogears-functions
   '(avy-goto-char avy-goto-char-2 avy-goto-line avy-goto-char-timer))
 '(electric-pair-mode t)
 '(global-visible-mark-mode t)
 '(hl-todo-color-background nil)
 '(kind-icon-default-style
   '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.7 :scale 1.0
              :background nil))
 '(lambda-line-vc-symbol "")
 '(mc/always-run-for-all t)
 '(modus-themes-vivendi-color-overrides nil)
 '(package-selected-packages
   '(annotate auctex avy beacon cape cmake-mode consult-eglot
              corfu-terminal dogears embark-consult hl-todo
              indent-guide json-mode kind-icon magit marginalia
              markdown-mode multiple-cursors orderless
              rainbow-delimiters smart-mode-line standard-themes
              svelte-mode symbol-overlay treemacs vertico visible-mark
              vterm wgrep which-key whole-line-or-region yaml-mode))
 '(pixel-scroll-mode t)
 '(pixel-scroll-precision-interpolation-between-scroll 0.001)
 '(pixel-scroll-precision-interpolation-total-time 0.1)
 '(pixel-scroll-precision-mode t)
 '(pixel-scroll-precision-use-momentum nil)
 '(recenter-positions '(middle bottom top))
 '(safe-local-variable-values
   '((eval when (fboundp 'rainbow-mode) (rainbow-mode 1))
     (vc-default-patch-addressee . "bug-gnu-emacs@gnu.org")
     (etags-regen-ignores "test/manual/etags/")
     (etags-regen-regexp-alist
      (("c" "objc") "/[ \11]*DEFVAR_[A-Z_ \11(]+\"\\([^\"]+\\)\"/\\1/"
       "/[ \11]*DEFVAR_[A-Z_ \11(]+\"[^\"]+\",[ \11]\\([A-Za-z0-9_]+\\)/\\1/"))
     (TeX-master . t) (reftex-default-bibliography "./adaptive.bib")))
 '(sml/theme 'automatic)
 '(tab-bar-close-button-show nil)
 '(tab-bar-mode t)
 '(tool-bar-mode nil)
 '(visible-mark-max 20))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "PragmataPro Liga" :foundry "FSD " :slant normal :weight regular :height 120 :width normal)))))
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
