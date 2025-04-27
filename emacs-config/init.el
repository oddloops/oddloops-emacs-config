(setq inhibit-startup-message t)     ; Do not show splash screen
(setq visible-bell nil)              ; Flash when the bell rings

;; Set emacs variables
(defvar default-font-size 125)

;; -------------------------------------------------------------------
;; Font Configuration ------------------------------------------------
;; -------------------------------------------------------------------
(set-face-attribute 'default nil :height default-font-size)

;; -------------------------------------------------------------------
;; Global Configuration ----------------------------------------------
;; -------------------------------------------------------------------
(global-display-line-numbers-mode 1) ; Displays line numbers

; Disable line numbers for modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-auto-revert-mode 1)          ; Revert buffers when file has changed
(setq gloabl-auto-revert-non-file-buffers t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; ESC quits prompt

(recentf-mode 1)                     ; Enable recent file mode

(setq history-length 25)             ; Set history length for mini buffer
(savehist-mode 1)                    ; Enable save history mode for mini-buffer inputs

(save-place-mode 1)                  ; Mode for saving last cursor location in file

(setq make-backup-files nil)         ; Disable Emacs backups
(setq create-lockfiles nil)          ; Disable lock files

;; -------------------------------------------------------------------
;; Package Configuration ---------------------------------------------
;; -------------------------------------------------------------------
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; -------------------------------------------------------------------
;; Projectile Configuration ------------------------------------------
;; -------------------------------------------------------------------
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; -------------------------------------------------------------------
;; MaGit Configuration -----------------------------------------------
;; -------------------------------------------------------------------
(use-package magit)

;; -------------------------------------------------------------------
;; Ivy Configuration -------------------------------------------------
;; -------------------------------------------------------------------
(use-package ivy
  :diminish
  :bind
  (("C-s" . swiper)
  :map ivy-minibuffer-map
   ("TAB" . ivy-alt-done)
   ("C-l" . ivy-alt-done)
   ("C-j" . ivy-next-line)
   ("C-k" . ivy-previous-line)
  :map ivy-switch-buffer-map
   ("C-k" . ivy-previous-line)
   ("C-l" . ivy-done)
   ("C-d" . ivy-switch-buffer-kill)
  :map ivy-reverse-i-search-map
   ("C-k" . ivy-previous-line)
   ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :init (ivy-rich-mode 1))
 
;; -------------------------------------------------------------------
;; Counsel Configuration ---------------------------------------------
;; -------------------------------------------------------------------
(use-package counsel
  :bind (("M-X" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode  
  :config
  (setq which-key-idle-delay 0.2))

;; -------------------------------------------------------------------
;; Org Mode Configuration --------------------------------------------
;; -------------------------------------------------------------------
(defun org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)

  ;; org-mode exclusive key-mappings
  (define-key org-mode-map (kbd "C-c C-g C-r") 'org-shiftmetaright) ; change the level of an org item, use SMR
  (define-key org-mode-map (kbd "C-c <up>") 'org-priority-up)
  (define-key org-mode-map (kbd "C-c <down>") 'org-priority-down)
)

(use-package org
  :hook (org-mode . org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t
	org-agenda-files '("~/org")
	org-log-done 'time
	rg-return-follows-link  t
	)
  (define-key global-map "\C-cl" 'org-store-link) ; storing links shortcut
  (define-key global-map "\C-ca" 'org-agenda)     ; viewing agenda shortcut
  (define-key global-map "\C-cc" 'org-capture)    ; starting capture shortcut

 )

;; -------------------------------------------------------------------
;; Make/Send to directories Configuration ----------------------------
;; -------------------------------------------------------------------
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(setq projectile-known-projects-file (expand-file-name "tmp/projectile-bookmarks.eld" user-emacs-directory)
      lsp-session-file (expand-file-name "tmp/.lsp-session-v1" user-emacs-directory))

(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))

;; Move customization variables to a separate file and then load them
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; -------------------------------------------------------------------
;; Modus Theme Configuration -----------------------------------------
;; -------------------------------------------------------------------
(load-theme 'modus-vivendi-deuteranopia t)        ; load modus-vivendi t theme
