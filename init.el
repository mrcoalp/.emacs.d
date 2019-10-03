(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;hide/show things
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(show-paren-mode t)
(global-hl-line-mode t)
(global-visual-line-mode 1)
(setq ring-bell-function 'ignore)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq mode-require-final-newline t)
(fringe-mode 16)
(winner-mode t)


;;save state
(desktop-save-mode t)
(save-place-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (nimbus)))
 '(custom-safe-themes
   (quote
    ("85286ccba3ccaa775f99890fe93b0dc4963a42c41f25ef409edecd6a8f8652b3" "75cce15f30f64af33ba3f3f987861b26eb78f9d264f51d69aa0578d5bf618c9d" default)))
 '(package-selected-packages
   (quote
    (flycheck yasnippet-snippets ggtags yasnippet flx ivy ivy-mode editorconfig company-quickhelp smartparens web-mode tide js2-mode company company-mode diff-hl magit mode-icons which-key use-package all-the-icons all-the-icons-dired eshell-git-prompt nimbus-theme typescript-mode)))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2d3743" :foreground "#e1e1e0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "outline" :family "FuraCode NF")))))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;powerline
(use-package powerline
  :ensure t
  :config(powerline-default-theme))

;;show line numbers in left side
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

;;load nimbus theme
(use-package nimbus-theme
  :ensure t
  :config(load-theme 'nimbus t))

;;set eshell theme
(use-package eshell-git-prompt
  :ensure t
  :config(eshell-git-prompt-use-theme 'git-radar))

;;dired icons
(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons
  :init(add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
(setq inhibit-compacting-font-caches t)

;;which key helps listing key combs
(use-package which-key
  :ensure t
  :config(which-key-mode))

;;mode icons
(use-package mode-icons
  :ensure t
  :config(mode-icons-mode))

;;magit
(use-package magit
  :ensure t
  :commands (magit-status)
  :bind(("C-x g" . magit-status)))

;;diff-hl
(use-package diff-hl
  :ensure t
  :hook((dired-mode . diff-hl-dired-mode)
	(magit-post-refresh . diff-hl-magit-post-refresh))
  :config (global-diff-hl-mode t))

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package ivy
  :ensure t
  :init (setq ivy-initial-inputs-alist nil)
  :custom (ivy-use-virtual-buffers t)
  :config (ivy-mode 1)
  (use-package flx
    :ensure t))

;;==========LANGUAGES==========;;
;;utils
(use-package company
  :ensure t
  :custom
  (company-require-match nil)
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.2)
  (company-tooltip-align-annotation t)
  (company-frontends '(company-pseudo-tooltip-frontend
		               company-echo-metadata-frontend))
  :commands (company-mode global-company-mode company-complete
                          company-complete-common company-manual-begin
                          company-grab-line)
  :bind (
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :hook ((prog-mode . company-mode)
         (comint-mode . company-mode)))

(use-package company-quickhelp
  :ensure t
  :after company
  :commands (company-quickhelp-mode)
  :init
  (company-quickhelp-mode 1)
  (use-package pos-tip
    :commands (pos-tip-show)))

;;checks for errors
(use-package flycheck
  :ensure t
  :init(global-flycheck-mode))

;;snippets
(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;;global needs to be installed in system
(use-package ggtags
  :ensure t
  :config (add-hook 'c-mode-common-hook
		    (lambda ()
		      (when(derived-mode-p 'c-mode 'c++-mode 'java-mode)
			(ggtags-mode 1)))))

;; JavaScript and TypeScript
(use-package js2-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-formater-before-save)))

(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))
  (setq web-mode-enable-current-element-highlight t))
