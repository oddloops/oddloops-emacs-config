(push '(fullscreen . maximized) default-frame-alist)
(setq inhibit-startup-message t)     ; Do not show splash screen
(setq visible-bell nil)              ; Flash when the bell rings
(tool-bar-mode -1)                   ; Disable toolbar
(scroll-bar-mode -1)                 ; Disable scroll bar
(setq package-enable-at-startup nil)
(setq default-font-size 125)
(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))

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
(setq global-auto-revert-non-file-buffers t)

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
;; Jinx Spell Chcker Configuration -----------------------------------
;; -------------------------------------------------------------------
(use-package jinx
  :defer t
  :hook (
	 (text-mode . jinx-mode)
	 (latex-mode . jinx-mode)
	 (markdown-mode . jinx-mode)
	 (org-mode . jinx-mode))
  :bind (("C-M-$" . jinx-correct))
  :init (setq jinx-languages "en_US"))

;; -------------------------------------------------------------------
;; Projectile Configuration ------------------------------------------
;; -------------------------------------------------------------------
(use-package projectile
  :defer t
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Github/projects")
    (setq projectile-project-search-path '("~/Github/projects")))
  (setq projectile-switch-project-action #'projectile-dired))
;; -------------------------------------------------------------------
;; MaGit Configuration -----------------------------------------------
;; -------------------------------------------------------------------
(use-package magit
  :defer t)

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
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

;; -------------------------------------------------------------------
;; which-key Configuration--------------------------------------------
;; -------------------------------------------------------------------
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode  
  :config
  (setq which-key-idle-delay 0.2))

;; -------------------------------------------------------------------
;; Org Mode Configuration --------------------------------------------
;; ------------------------------------------------------------------- 
(defun org-mode-custom-setup ()
  ;; Org-specific layout
  (org-indent-mode)
  (visual-line-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)

  (setq visual-fill-column-width 110
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package org
  :hook (org-mode . org-mode-custom-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers nil
        org-fontify-emphasize-text t
        org-agenda-files '("~/org")
        org-log-done 'time
        org-return-follows-link t)
  (define-key org-mode-map (kbd "C-c C-g C-r") 'org-shiftmetaright)
  (define-key org-mode-map (kbd "C-c <up>") 'org-priority-up)
  (define-key org-mode-map (kbd "C-c <down>") 'org-priority-down)
  :bind
  (("\C-cl" . org-store-link)
   ("\C-ca" . org-agenda)
   ("\C-cc" . org-capture)))

(use-package visual-fill-column)

;; -------------------------------------------------------------------
;; Org Babel----------------------------------------------------------
;; -------------------------------------------------------------------
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (C . t)
     (python . t)
     (latex . t))))
(setq org-startup-with-latex-preview t)
(setq org-confirm-babel-evaluate t)
(setq org-babel-python-command "python3")

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("clang" . "src C"))
(add-to-list 'org-structure-template-alist '("latex" . "src latex"))

;; -------------------------------------------------------------------
;; Make/Send to directories Configuration ----------------------------
;; -------------------------------------------------------------------
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(setq projectile-known-projects-file (expand-file-name "tmp/projectile-bookmarks.eld" user-emacs-directory)
      lsp-session-file (expand-file-name "tmp/.lsp-session-v1" user-emacs-directory))

;; Move customization variables to a separate file and then load them
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; -------------------------------------------------------------------
;; Modus Theme Configuration -----------------------------------------
;; -------------------------------------------------------------------
(load-theme 'modus-vivendi-deuteranopia t)
