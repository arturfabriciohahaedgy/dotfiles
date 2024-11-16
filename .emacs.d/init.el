(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update=prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "10:00"))

(setq user-emacs-directory "~/.cache/emacs")
(use-package no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(pixel-scroll-precision-mode)
(setq pixel-scroll-precision-large-scroll-height 35.0)
(setq scroll-conservatively 2)

(setq inhibit-startup-message t)
(setq native-comp-async-report-warnings-errors nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
		treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(recentf-mode 1)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; which-key
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; general
(use-package general)

;; Appearence

;; modus-themes
(use-package modus-themes
  :init
  (setq modus-themes-bold-constructs t
	modus-themes-italic-constructs t))

;; all-the-icons
(use-package all-the-icons
  :if (display-graphic-p))

;; nerd-ocons
(use-package nerd-icons
  :if (display-graphic-p))

;; doom-themes
(use-package doom-themes)

;; doom-modeline
(use-package doom-modeline
  :config
  (setq doom-modeline-icon t)
  :init
  (doom-modeline-mode t))

;; dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 5)
			  (bookmarks . 5)
			  (agenda . 5)
			  (projects . 5))
	dashboard-display-icons-p t
	dashboard-set-file-icons t
	dashboard-icon-type 'nerd-icons))
  
;;;; Load theme
(load-theme 'modus-operandi t)

;;;; Set fonts
(set-face-attribute 'default nil :font "Iosevka Nerd Font Mono-14")
(set-face-attribute 'fixed-pitch nil :font "Iosevka Nerd Font Mono-14")
(set-face-attribute 'variable-pitch nil :font "Cantarell-17")

;; dired
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(setf dired-kill-when-opening-new-dired-buffer t)

;; COMPLETIONS!!

(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package embark
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-narrow-key "<"))

;; Dev

;;;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;; yasnippet
(use-package yasnippet
  :init
  (yas-global-mode t))

(use-package yasnippet-snippets)

;;;; markdown

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;;;; treesit-auto
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;;; eat (terminal)
(use-package eat)

;;;; magit
(use-package magit
  :commands magit-status)

;;;; treemacs
(use-package treemacs
  :config
  (treemacs-filewatch-mode t))

;;;; autocompletions

;; (use-package company
;;   :bind (:map company-active-map
;; 	      ("<tab>" . company-complete-selection))
;;   :custom
;;   (company-minimum-prefix-length 3)
;;   (company-idle-delay 0.5))
;; (add-hook 'after-init-hook 'global-company-mode)

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

(use-package corfu
  :custom
  (corfu-preview-current nil)
  :init
  (global-corfu-mode))

;;;; gopls
;; (use-package go
;;   :hook (before-save-hook . 'gofmt-before-save))

;;;; haskell
(use-package haskell-mode
  :custom
  (haskell-stylish-on-save t))
(eval-after-load 'haskell-mode (load-library "haskell-mode"))

;;;; typescript
;;(use-package tide
;;  :after (company)
;;  :hook ((typescript-ts-mode . tide-setup)
;;         (tsx-ts-mode . tide-setup)
;;         (typescript-ts-mode . tide-hl-identifier-mode)
;;         (before-save . tide-format-before-save)))

;; (add-hook 'before-save-hook 'tide-format-before-save)

;; (use-package prettier
;;   :hook
;;   ((typescript-ts-mode . prettier-mode)))

;;;; C and C++
;; (setq-default c-basic-offset 4)
;; (setq-default indent-tabs-mode nil)

;;;; eglot
(use-package eglot
  :config
  (electric-pair-mode)
  :hook
  ;(go-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure))


;; (add-hook 'eglot-managed-mode 'company-mode)
;; (add-hook 'go-mode-hook 'eglot-ensure) 


;; Bindings
;;;; General bindings

;;;; emabark
(general-define-key
 "C-." 'embark-act
 "C-;" 'embark-dwim
 "C-h B" 'embark-bindings)

;;;; consult
(general-define-key
 :prefix "C-c"
 "M-x" 'consult-mode-command
 "h" 'consult-history
 "k" 'consult-kmacro
 "m" 'consult-man
 "i" 'consult-info)

(general-define-key
 :prefix "C-x"
 "M-:" 'consult-complex-command
 "b" 'consult-buffer
 "4 b" 'consult-buffer-other-window
 "5 b" 'consult-buffer-other-tab
 "r f" 'consult-recent-file
 "r b" 'consult-bookmark
 "p b" 'consult-project-buffer)

(general-define-key
 "M-#" 'consult-register-load
 "M-'" 'consult-register-store
 "C-M-#" 'consult-register
 "M-y" 'consult-yank-pop)

(general-define-key
 :prefix "M-g"
 "e" 'consult-compile-error
 "f" 'consult-flymake
 "g" 'consult-goto-line
 "M-g" 'consult-goto-line
 "o" 'consult-outline
 "m" 'consult-mark
 "k" 'consult-global-mark
 "i" 'consult-imenu
 "I" 'consult-imenu-multi)

(general-define-key
 :prefix "M-s"
 "d" 'consult-find
 "c" 'consult-locate
 "g" 'consult-grep
 "G" 'consult-git-grep
 "r" 'consult-ripgrep
 "l" 'consult-line
 "L" 'consult-line-multi
 "k" 'consult-keep-lines
 "u" 'consult-focus-lines
 "e" 'consult-isearch-history)

(general-define-key
 :keymaps 'isearch-mode-map
 "M-e" 'consult-isearch-history
 :prefix "M-s"
 "e" 'consult-isearch-history
 "l" 'consult-line
 "L" 'consult-line-multi)

(general-define-key
 :keymaps 'minibuffer-local-map
 "M-s" 'consult-history
 "M-r" 'consult-history)

;;;; Dired
(general-define-key
 :keymaps 'dired-mode-map
 "b" 'dired-single-up-directory
 "f" 'dired-single-buffer)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9a977ddae55e0e91c09952e96d614ae0be69727ea78ca145beea1aae01ac78d2" default))
 '(package-selected-packages
   '(corfu eat treesit-auto yasnippet embark nerd-icons all-the-icons treemacs markdown-mode dired-single magit yasnippet-snippets which-key vertico rainbow-delimiters orderless no-littering modus-themes marginalia haskell-mode go general embark-consult doom-themes doom-modeline dashboard company-box auto-package-update all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
