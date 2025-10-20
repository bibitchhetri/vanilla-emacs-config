;; Straight.el bootstrapper
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

;; Set default fonts safely
(when (member "JetBrains Mono" (font-family-list))
  (set-face-attribute 'default nil :font "JetBrains Mono" :height 110 :weight 'medium)
  (add-to-list 'default-frame-alist '(font . "JetBrains Mono-11")))

(when (member "Ubuntu" (font-family-list))
  (set-face-attribute 'variable-pitch nil :font "Ubuntu" :height 120 :weight 'medium)
  (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height 110 :weight 'medium))

;; Italic comments and keywords
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

(setq-default line-spacing 0.12)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

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
  "b"  '(:ignore t :which-key "buffer")
  "b b" '(switch-to-buffer :which-key "Switch buffer")
  "b k" '(kill-this-buffer :which-key "Kill buffer")
  "b n" '(next-buffer :which-key "Next buffer")
  "b p" '(previous-buffer :which-key "Previous buffer")
  "b r" '(revert-buffer :which-key "Reload buffer"))

(my/leader-keys
  "w"  '(:ignore t :which-key "window")
  "w v" '(split-window-right :which-key "Split vertical")
  "w s" '(split-window-below :which-key "Split horizontal")
  "w d" '(delete-window :which-key "Delete window")
  "w o" '(delete-other-windows :which-key "Maximize window"))

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq version-control t
      kept-new-versions 5
      kept-old-versions 2
      delete-old-versions t)
(unless (file-exists-p "~/.emacs.d/backups")
  (make-directory "~/.emacs.d/backups" t))

(straight-use-package 'toc-org)
(straight-use-package 'org-bullets)

;; Set up bullets and org enhancements in a single hook
(add-hook 'org-mode-hook
          (lambda ()
            ;; Org indent and bullets
            (org-indent-mode 1)
            (org-bullets-mode 1)
            ;; Enable table of contents
            (toc-org-enable))))

;; Optional: customize bullet characters
(setq org-bullets-bullet-list '("◉" "○" "●" "◆" "▷"))

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
      which-key-allow-imprecise-window-fit t
      which-key-separator "   ")
