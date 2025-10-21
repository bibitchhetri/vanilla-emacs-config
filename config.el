(defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          (or (bound-and-true-p straight-base-dir)
              user-emacs-directory)))
        (bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
;; Ensure use-package is installed via straight.el
(straight-use-package 'use-package)

;; Make use-package use straight.el by default
(setq straight-use-package-by-default t)

(setq evil-want-keybinding nil
      evil-want-integration t
      evil-split-window-right t
      evil-split-window-below t)

(straight-use-package 'evil)
(require 'evil)
(evil-mode 1)

(straight-use-package 'evil-collection)
(with-eval-after-load 'evil
  (require 'evil-collection)
  (evil-collection-init))

(straight-use-package 'evil-tutor)

(straight-use-package 'general)
(require 'general)
(general-evil-setup)

(general-create-definer my/leader-keys
  :states '(normal insert visual emacs)
  :keymaps 'override
  :prefix "SPC"
  :global-prefix "M-SPC")

(my/leader-keys
 ;; Buffer
 "b"  '(:ignore t :which-key "buffer")
 "b b" '(ivy-switch-buffer :which-key "Switch buffer")
 "b k" '(kill-this-buffer :which-key "Kill buffer")
 "b n" '(next-buffer :which-key "Next buffer")
 "b p" '(previous-buffer :which-key "Previous buffer")
 "b r" '(revert-buffer :which-key "Reload buffer")
 "b B" '(ivy-switch-buffer-other-window :which-key "Switch buffer other window")

 ;; File
 "f" '(:ignore t :which-key "file")
 "f f" '(counsel-find-file :which-key "Find file")
 "f r" '(counsel-recentf :which-key "Recent files")
 "f d" '(counsel-dired :which-key "Open dired")
 "f p" '(sudo-edit-find-file :which-key "Sudo find file")
 "f P" '(sudo-edit :which-key "Sudo edit file")

 ;; Window
 "w"  '(:ignore t :which-key "window")
 "w v" '(split-window-right :which-key "Split vertical")
 "w s" '(split-window-below :which-key "Split horizontal")
 "w c" '(delete-window :which-key "Delete window")
 "w o" '(delete-other-windows :which-key "Maximize window")
 ;; Window navigation
 "w j" '(evil-window-down :which-key "Move to window below")
 "w k" '(evil-window-up :which-key "Move to window above")
 "w h" '(evil-window-left :which-key "Move to window left")
 "w l" '(evil-window-right :which-key "Move to window right")
 "w w" '(other-window :which-key "Switch to other window")

 ;; Search
 "s" '(:ignore t :which-key "search")
 "s f" '(counsel-rg :which-key "Search in files")
 "s b" '(counsel-switch-buffer :which-key "Search buffers")
 "s m" '(counsel-imenu :which-key "Search in buffer")

 ;; Evaluate
 "e" '(:ignore t :which-key "Evaluate")    
 "e b" '(eval-buffer :which-key "Evaluate elisp in buffer")
 "e d" '(eval-defun :which-key "Evaluate defun containing or after point")
 "e e" '(eval-expression :which-key "Evaluate an elisp expression")
 "e l" '(eval-last-sexp :which-key "Evaluate elisp expression before point")
 "e r" '(eval-region :which-key "Evaluate elisp in region")

 ;; Help
 "h" '(:ignore t :which-key "Help")
 "h f" '(counsel-describe-function :which-key "Describe function")
 "h v" '(counsel-describe-variable :which-key "Describe variable")
 "h r r" (lambda () (interactive) (load-file "~/.emacs.d/init.el")) :which-key "Reload emacs config"

 ;; Toggle
 "t" '(:ignore t :which-key "Toggle")
 "t l" '(display-line-numbers-mode :which-key "Toggle line numbers")
 "t t" '(visual-line-mode :which-key "Toggle truncated lines")

 ;; Applications
 "a" '(:ignore t :which-key "applications")
 "a a" '(counsel-M-x :which-key "M-x")
 "a r" '(ivy-resume :which-key "Resume last search"))

(add-hook 'emacs-startup-hook
          (lambda ()
            (evil-mode 1)
            (evil-collection-init)
            (which-key-mode 1)
            (global-visual-line-mode 1)
            (global-display-line-numbers-mode 1)
            (setq display-line-numbers-type 'relative)
            (recentf-mode 1)
            (column-number-mode 1)
            (display-time-mode 1)))

(when (member "JetBrains Mono" (font-family-list))
  (set-face-attribute 'default nil :font "JetBrains Mono" :height 110 :weight 'medium)
  (add-to-list 'default-frame-alist '(font . "JetBrains Mono-11")))

(when (member "Ubuntu" (font-family-list))
  (set-face-attribute 'variable-pitch nil :font "Ubuntu" :height 120 :weight 'medium)
  (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height 110 :weight 'medium))

(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)
(setq-default line-spacing 0.12)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; macOS window styling
(when (eq system-type 'darwin)
  ;; Remove window decorations and add rounded corners
  (add-to-list 'default-frame-alist '(undecorated-round . t))
  ;; Apply rounded corners to existing frame
  (set-frame-parameter (selected-frame) 'undecorated-round t))

(global-display-line-numbers-mode 1)
(global-visual-line-mode 1)
(setq display-line-numbers-type 'relative)

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq version-control t
      kept-new-versions 5
      kept-old-versions 2
      delete-old-versions t)
(unless (file-exists-p "~/.emacs.d/backups")
  (make-directory "~/.emacs.d/backups" t))

(straight-use-package 'toc-org)
(straight-use-package 'org-bullets)

(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode 1)
            (org-bullets-mode 1)
            (toc-org-enable)))

(setq org-bullets-bullet-list '("◉" "○" "◈" "◇" "▪" "▫"))

(require 'org-tempo)

(use-package sudo-edit
:config
  (my/leader-keys
    ;; file with privilege i.e f p
    "f p" '(sudo-edit-find-file :wk "Sudo find file")
    "f P" '(sudo-edit :wk "Sudo edit file")))

(straight-use-package 'which-key)
(require 'which-key)
(which-key-mode 1)

(setq which-key-side-window-location 'bottom
      which-key-sort-order #'which-key-key-order-alpha
      which-key-sort-uppercase-first nil
      which-key-add-column-padding 1
      which-key-max-display-columns nil
      which-key-min-display-lines 6
      which-key-side-window-slot 0
      which-key-side-window-max-height 0.25
      which-key-idle-delay 0.8
      which-key-max-description-length 25
      which-key-allow-imprecise-window-fit nil
      which-key-separator "   ")

(straight-use-package 'ivy)
(straight-use-package 'counsel)
(straight-use-package 'ivy-rich)

;; Ivy configuration
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq enable-recursive-minibuffers t)

;; Enable Ivy mode
(ivy-mode 1)

;; Counsel configuration
(counsel-mode 1)

;; Ivy-rich configuration
(ivy-rich-mode 1)

;; Optional: all-the-icons-ivy-rich (if available)
(when (require 'all-the-icons-ivy-rich nil t)
  (all-the-icons-ivy-rich-mode 1))

;; Ivy-rich customizations
(setq ivy-virtual-abbreviate 'full
      ivy-rich-switch-buffer-align-virtual-buffer t
      ivy-rich-path-style 'abbrev)

(ivy-set-display-transformer 'ivy-switch-buffer
                             'ivy-rich-switch-buffer-transformer)

;; Additional Ivy keybindings
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-x B") 'ivy-switch-buffer-other-window)
(global-set-key (kbd "C-s") 'counsel-grep-or-swiper)
(global-set-key (kbd "C-r") 'counsel-grep-or-swiper-backward)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)

(straight-use-package 'all-the-icons)

(straight-use-package 'all-the-icons-dired)

;; Enable all-the-icons-dired in dired mode
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; Set transparency and blur for new frames (GUI mode only)
(when (display-graphic-p)
  ;; Transparency settings
  (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
  ;; Apply transparency to existing frame
  (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
  
  ;; Blur effect for macOS
  (when (eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    ;; Enable blur effect
    (add-to-list 'default-frame-alist '(ns-background-blur . t))
    ;; Apply blur to existing frame
    (set-frame-parameter (selected-frame) 'ns-background-blur t)))

(load-theme 'tango-dark t)
