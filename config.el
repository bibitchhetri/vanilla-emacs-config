;; Increase garbage collection threshold during startup for better performance
(setq gc-cons-threshold (* 50 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)))) ; 16MB

(setq enable-local-variables :safe
      enable-local-eval nil
      read-process-output-max (* 1024 1024)) ; 1MB

(setq comp-async-report-warnings-errors 'error-only
      comp-deferred-compilation t
      comp-ctxt-optimization-level 0)

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

(straight-use-package 'use-package)
(setq straight-use-package-by-default t
      straight-check-for-modifications '(find-when-checking))

(setq evil-want-keybinding nil
      evil-want-integration t
      evil-split-window-right t
      evil-split-window-below t
      evil-undo-system 'undo-redo)

(straight-use-package 'evil)
(require 'evil)
(evil-mode 1)

;; Scrolling & redo keybindings for all evil states
(with-eval-after-load 'evil
  (dolist (map '(evil-normal-state-map evil-visual-state-map evil-insert-state-map))
    (define-key (symbol-value map) (kbd "C-d") 'evil-scroll-down)
    (define-key (symbol-value map) (kbd "C-u") 'evil-scroll-up)
    (define-key (symbol-value map) (kbd "C-r") 'undo-redo)))

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
 ;; Buffer operations
 "b" '(:ignore t :which-key "buffer commands")
 "b b" '(ivy-switch-buffer :which-key "Switch buffer")
 "b k" '(kill-current-buffer :which-key "Kill buffer")
 "b n" '(next-buffer :which-key "Next buffer")
 "b p" '(previous-buffer :which-key "Previous buffer")
 "b r" '(revert-buffer :which-key "Reload buffer")
 "b B" '(ivy-switch-buffer-other-window :which-key "Switch buffer other window")
 "b d" '(kill-current-buffer :which-key "Kill buffer")
 "b o" '(other-window :which-key "Switch to other window")
 "b s" '(save-buffer :which-key "Save buffer")

 ;; File operations
 "f" '(:ignore t :which-key "file commands")
 "f f" '(counsel-find-file :which-key "Find file")
 "f r" '(counsel-recentf :which-key "Recent files")
 "f d" '(counsel-dired :which-key "Open dired")
 "f D" '(peep-dired :which-key "Peep dired preview")
 "f s" '(save-buffer :which-key "Save file")
 "f S" '(write-file :which-key "Save as")
 "f p" '(sudo-edit-find-file :which-key "Sudo find file")
 "f P" '(sudo-edit :which-key "Sudo edit file")

 ;; Project operations  
 "p" '(:ignore t :which-key "project commands")
 "p p" '(projectile-switch-project :which-key "Switch project")
 "p f" '(projectile-find-file :which-key "Find file in project")
 "p s" '(projectile-switch-project :which-key "Switch project")
 "p t" '(projectile-toggle-between-implementation-and-test :which-key "Toggle impl/test")

 ;; Search
 "s" '(:ignore t :which-key "search commands")
 "s f" '(counsel-rg :which-key "Search in files")
 "s b" '(counsel-switch-buffer :which-key "Search buffers")
 "s m" '(counsel-imenu :which-key "Search in buffer")
 "s w" '(swiper :which-key "Search in buffer (swiper)")

 ;; Help
 "h" '(:ignore t :which-key "help commands")
 "h f" '(counsel-describe-function :which-key "Describe function")
 "h v" '(counsel-describe-variable :which-key "Describe variable")
 "h r" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :which-key "Reload config")

 ;; Evaluate/Elisp
 "e" '(:ignore t :which-key "eval/elisp commands")
 "e b" '(eval-buffer :which-key "Evaluate buffer")
 "e d" '(eval-defun :which-key "Evaluate defun")
 "e e" '(eval-expression :which-key "Evaluate expression")
 "e r" '(eval-region :which-key "Evaluate region")
 "e l" '(eval-last-sexp :which-key "Evaluate last sexp")

 ;; Toggle
 "t" '(:ignore t :which-key "toggle commands")
 "t l" '(display-line-numbers-mode :which-key "Toggle line numbers")
 "t t" '(visual-line-mode :which-key "Toggle visual line")
 "t e" '(eshell :which-key "Open eshell")
 "t v" '(my/vterm-toggle :which-key "Toggle vterm")
 "t V" '(my/vterm :which-key "Open vterm")
 "t T" '(toggle-transparency :which-key "Toggle transparency")

 ;; Window
 "w" '(:ignore t :which-key "window commands")
 "w v" '(split-window-right :which-key "Split vertical")
 "w s" '(split-window-below :which-key "Split horizontal")
 "w c" '(delete-window :which-key "Close window")
 "w o" '(delete-other-windows :which-key "Maximize window")
 "w k" '(evil-window-up :which-key "Move up")
 "w j" '(evil-window-down :which-key "Move down")
 "w h" '(evil-window-left :which-key "Move left")
 "w l" '(evil-window-right :which-key "Move right")
 "w w" '(other-window :which-key "Switch window")
 "w H" '(buf-move-left :which-key "Buffer move left")
 "w J" '(buf-move-down :which-key "Buffer move down")
 "w K" '(buf-move-up :which-key "Buffer move up")
 "w L" '(buf-move-right :which-key "Buffer move right")

 ;; Applications/Tools
 "a" '(:ignore t :which-key "applications/tools")
 "a a" '(counsel-M-x :which-key "M-x")
 "a r" '(ivy-resume :which-key "Resume last search")

 ;; Quit
 "q" '(:ignore t :which-key "quit commands")
 "q q" '(save-buffers-kill-terminal :which-key "Quit Emacs")
 "q Q" '(kill-emacs :which-key "Kill Emacs")

 ;; Quick access
 "SPC" '(counsel-M-x :which-key "M-x"))

(straight-use-package 'all-the-icons)
(straight-use-package 'all-the-icons-dired)
(straight-use-package 'nerd-icons)

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(straight-use-package 'peep-dired)
(straight-use-package 'dired-hacks)
(straight-use-package 'dired-open)

(setq dired-dwim-target t
      dired-hide-details-hide-symlink-targets nil
      dired-listing-switches "-alh"
      dired-create-destination-dirs 'ask
      dired-vc-rename-file t
      dired-make-directory-clickable t
      dired-auto-revert-buffer t
      dired-vc-enable t)

(require 'dired-open)
(setq dired-open-extensions
      '(("gif" . "open")
        ("jpg" . "open")
        ("png" . "open")
        ("pdf" . "open")
        ("zip" . "unzip")
        ("gz" . "gunzip"))
      dired-open-use-nohup t)

(with-eval-after-load 'dired
  (require 'peep-dired)
  (setq dired-omit-extensions (delete "DS_Store" dired-omit-extensions))
  
  (defun my/dired-next-line-or-peep ()
    (interactive)
    (if peep-dired
        (peep-dired-next-file)
      (dired-next-line 1)))
  
  (defun my/dired-prev-line-or-peep ()
    (interactive)
    (if peep-dired
        (peep-dired-prev-file)
      (dired-previous-line 1)))
  
  (evil-define-key 'normal dired-mode-map
    (kbd "h") 'dired-up-directory
    (kbd "l") 'dired-find-file
    (kbd "j") 'my/dired-next-line-or-peep
    (kbd "k") 'my/dired-prev-line-or-peep
    (kbd "C-d") 'dired-hide-details-toggle
    (kbd "q") 'peep-dired))

(straight-use-package 'neotree)

;; Neotree configuration
(setq neo-theme 'nerd
      neo-window-width 35
      neo-smart-open t
      neo-show-hidden-files t
      neo-mode-line-type 'none
      neo-auto-indent-point t
      neo-show-updir-line t
      neo-hidden-regexp-list '("^\\." "\\.pyc$" "~$" "\\.elc$" "\\.class$" "\\.jar$")
      neo-create-file-auto-open t
      neo-banner-message nil
      neo-confirm-create-file 'off-p
      neo-confirm-create-directory 'off-p
      neo-window-fixed-size nil)

(defun my/neotree-toggle ()
  (interactive)
  (neotree-toggle)
  (when neo-global--window
    (select-window neo-global--window)
    (neotree-refresh)))

(defun my/neotree-find-file ()
  (interactive)
  (neotree-dir default-directory)
  (when neo-global--window
    (select-window neo-global--window)))

(defun my/neotree-select-window ()
  (interactive)
  (when neo-global--window
    (select-window neo-global--window)))

;; Disable line numbers in neotree
(add-hook 'neotree-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)))

;; Neotree keybindings
(with-eval-after-load 'neotree
  (evil-define-key 'normal neotree-mode-map
    (kbd "RET") 'neotree-enter
    (kbd "TAB") 'neotree-stretch-toggle
    (kbd "SPC") 'neotree-quick-look
    (kbd "q") 'neotree-hide
    (kbd "c") 'neotree-create-node
    (kbd "d") 'neotree-delete-node
    (kbd "r") 'neotree-rename-node
    (kbd "R") 'neotree-refresh
    (kbd "h") 'neo-buffer--hide-dotfiles-toggle
    (kbd "H") 'neo-buffer--hide-gitignored-files-toggle
    (kbd "g") 'neotree-refresh
    (kbd "s") 'neotree-hidden-file-toggle
    (kbd "U") 'neotree-select-up-node))

;; Add neotree to leader keybindings
(my/leader-keys
  "n" '(:ignore t :which-key "neotree commands")
  "n n" '(my/neotree-toggle :which-key "Toggle neotree")
  "n f" '(my/neotree-find-file :which-key "Find file in neotree")
  "n r" '(neotree-refresh :which-key "Refresh neotree")
  "n w" '(my/neotree-select-window :which-key "Select neotree window"))

(setq backup-directory-alist `(("." . "~/.emacs.d/backups"))
      version-control t
      kept-new-versions 5
      kept-old-versions 2
      delete-old-versions t)
(unless (file-exists-p "~/.emacs.d/backups")
  (make-directory "~/.emacs.d/backups" t))

;; Auto-save files
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-save-list/" t)))
(unless (file-exists-p "~/.emacs.d/auto-save-list")
  (make-directory "~/.emacs.d/auto-save-list" t))

(straight-use-package 'buffer-move)
(require 'buffer-move)

(straight-use-package 'origami)

(defface origami-fold-header-face
  '((t (:background unspecified :box nil)))
  "Face for origami fold headers."
  :group 'origami)

(require 'origami)
(add-hook 'prog-mode-hook 'origami-mode)

(defun my/origami-toggle-all ()
  (interactive)
  (if (not (eq last-command 'my/origami-toggle-all))
      (progn
        (origami-close-all-nodes (current-buffer))
        (setq this-command 'my/origami-toggle-all))
    (origami-open-all-nodes (current-buffer))))

(defun my/origami-recursively-toggle-node ()
  (interactive)
  (save-excursion
    (origami-toggle-node (current-buffer) (point) t)))

(my/leader-keys
  "z" '(:ignore t :which-key "folding commands")
  "z a" '(origami-toggle-node :which-key "Toggle fold")
  "z R" '(origami-open-all-nodes :which-key "Open all folds")
  "z M" '(origami-close-all-nodes :which-key "Close all folds")
  "z r" '(origami-open-node-recursively :which-key "Open fold recursively")
  "z m" '(origami-close-node-recursively :which-key "Close fold recursively")
  "z o" '(origami-show-only-node :which-key "Show only this fold")
  "z z" '(my/origami-toggle-all :which-key "Toggle all folds")
  "z n" '(origami-next-fold :which-key "Next fold")
  "z p" '(origami-previous-fold :which-key "Previous fold")
  "z t" '(my/origami-recursively-toggle-node :which-key "Recursively toggle fold"))

(with-eval-after-load 'diminish
  (diminish 'origami-mode))

(when (display-graphic-p)
  (let ((default-font (or (car (member "JetBrains Mono" (font-family-list)))
                          (car (member "SF Mono" (font-family-list))))))
    (when default-font
      (set-face-attribute 'default nil :font default-font :height 110 :weight 'medium)
      (add-to-list 'default-frame-alist `(font . ,(concat default-font "-11"))))))

(when (and (display-graphic-p) (member "Ubuntu" (font-family-list)))
  (set-face-attribute 'variable-pitch nil :font "Ubuntu" :height 120 :weight 'medium))

(when (display-graphic-p)
  (let ((terminal-font (or (car (member "JetBrains Mono" (font-family-list)))
                          (car (member "SF Mono" (font-family-list)))
                          (car (member "Monaco" (font-family-list))))))
    (when terminal-font
      (with-eval-after-load 'vterm
        (when (facep 'vterm)
          (set-face-attribute 'vterm nil :font terminal-font :height 110))))))

(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)
(setq-default line-spacing 0.12)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(undecorated-round . t))
  (set-frame-parameter (selected-frame) 'undecorated-round t))

(straight-use-package 'ivy)
(straight-use-package 'counsel)
(straight-use-package 'ivy-rich)
(straight-use-package 'swiper)
(straight-use-package 'all-the-icons-ivy-rich)
(straight-use-package 'flx)
(straight-use-package 'prescient)

(setq ivy-use-virtual-buffers t
      ivy-count-format "(%d/%d) "
      enable-recursive-minibuffers t
      ivy-re-builders-alist '((t . ivy--regex-fuzzy)
                              (counsel-rg . ivy--regex-plus)
                              (counsel-ag . ivy--regex-plus)
                              (counsel-pt . ivy--regex-plus)
                              (counsel-grep . ivy--regex-plus)
                              (swiper . ivy--regex-plus)
                              (swiper-isearch . ivy--regex-plus))
      ivy-sort-functions-alist '((counsel-find-file . ivy-sort-files-alphabetically)
                                 (counsel-recentf . ivy-sort-files-by-mtime)
                                 (counsel-buffer . ivy-sort-buffer-by-mtime))
      ivy-case-fold-search-default t
      ivy-initial-inputs-alist nil)

(ivy-mode 1)
(counsel-mode 1)
(ivy-rich-mode 1)

(when (require 'flx nil t)
  (setq ivy-flx-limit 10000))

(when (require 'prescient nil t)
  (prescient-persist-mode 1)
  (setq prescient-save-file (expand-file-name "prescient-save.el" user-emacs-directory))
  (setq ivy-prescient-retain-classic-highlighting t)
  (when (fboundp 'ivy-prescient-mode)
    (ivy-prescient-mode 1)))

(add-hook 'after-init-hook
          (lambda ()
            (when (require 'all-the-icons-ivy-rich nil t)
              (all-the-icons-ivy-rich-mode 1))))

(setq ivy-virtual-abbreviate 'full
      ivy-rich-switch-buffer-align-virtual-buffer t
      ivy-rich-path-style 'abbrev)

(ivy-set-display-transformer 'ivy-switch-buffer
                             'ivy-rich-switch-buffer-transformer)

(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-x B") 'ivy-switch-buffer-other-window)
(global-set-key (kbd "C-S-s") 'swiper)
(global-set-key (kbd "C-S-r") 'swiper-isearch-backward)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

(global-set-key (kbd "C-c t") 'my/vterm-toggle)
(global-set-key (kbd "C-c T") 'my/vterm-toggle)

(when (fboundp 'vterm-mode)
  (with-eval-after-load 'vterm
    (when (boundp 'vterm-mode-map)
      (define-key vterm-mode-map (kbd "C-c C-j") 'vterm-send-down)
      (define-key vterm-mode-map (kbd "C-c C-k") 'vterm-send-up)
      (define-key vterm-mode-map (kbd "C-c C-l") 'vterm-send-right)
      (define-key vterm-mode-map (kbd "C-c C-h") 'vterm-send-left)
      (define-key vterm-mode-map (kbd "C-c C-c") 'vterm-send-C-c)
      (define-key vterm-mode-map (kbd "C-c C-d") 'vterm-send-C-d))))

(straight-use-package 'toc-org)
(straight-use-package 'org-bullets)

;; Enable proper link following in org-mode
(setq org-link-search-must-match-exact-headline nil
      org-link-search-headline-must-match-exact nil)

(with-eval-after-load 'org
  (setq org-return-follows-link t)
  (setq org-tab-follows-link t)
  (setq org-link-search-must-match-exact-headline nil)
  (define-key org-mode-map (kbd "RET") 'org-open-at-point)
  (define-key org-mode-map (kbd "TAB") 'org-next-link)
  (define-key org-mode-map (kbd "S-TAB") 'org-previous-link)
  (define-key org-mode-map (kbd "C-c C-o") 'org-open-at-point))

(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode 1)
            (org-bullets-mode 1)
            (toc-org-enable)
            ;; Ensure keyboard navigation works
            (local-set-key (kbd "RET") 'org-open-at-point)
            (local-set-key (kbd "TAB") 'org-next-link)
            (local-set-key (kbd "S-TAB") 'org-previous-link)))

(setq org-bullets-bullet-list '("◉" "○" "◈" "◇" "▪" "▫"))

(require 'org-tempo)

(straight-use-package 'eglot)

;; Eglot configuration for efficient LSP support
(require 'eglot)

;; Connect to LSP servers for supported languages
(add-hook 'prog-mode-hook
          (lambda ()
            (when (or (memq major-mode '(python-mode python-ts-mode))
                      (memq major-mode '(rust-mode rust-ts-mode))
                      (memq major-mode '(go-mode go-ts-mode))
                      (memq major-mode '(c-mode c++-mode c++-ts-mode))
                      (memq major-mode '(javascript-mode js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode))
                      (memq major-mode '(java-mode java-ts-mode))
                      (memq major-mode '(bash-mode sh-mode))
                      (memq major-mode '(ruby-mode ruby-ts-mode))
                      (memq major-mode '(php-mode)))
              (eglot-ensure))))

;; Better LSP settings
(setq eglot-autoshutdown t
      eglot-confirm-server-initiated-edits nil
      eglot-extend-to-xref t
      eglot-connect-timeout 60
      eglot-ignored-server-capabilities '(:documentLinkProvider :documentFormattingProvider)
      eglot-sync-connect 1)

;; Performance optimization
(setq eglot-events-buffer-size 0)

;; Flymake configuration for better diagnostics
(setq flymake-error-bitmap nil
      flymake-note-bitmap nil
      flymake-warning-bitmap nil
      flymake-suppress-zero-counters nil)

;; Keybindings for eglot/LSP
(my/leader-keys
  "l" '(:ignore t :which-key "lsp commands")
  "l c" '(eglot-reconnect :which-key "Reconnect")
  "l d" '(eglot-shutdown :which-key "Shutdown")
  "l r" '(eglot-rename :which-key "Rename symbol")
  "l f" '(eglot-format :which-key "Format")
  "l a" '(eglot-code-action :which-key "Code action")
  "l q" '(eglot-code-action-quickfix :which-key "Quick fix")
  "l s" '(eglot-signature-help :which-key "Signature")
  "l e" '(flymake-show-diagnostics :which-key "Diagnostics")
  "l n" '(flymake-goto-next-error :which-key "Next error")
  "l p" '(flymake-goto-prev-error :which-key "Previous error")
  "l g" '(xref-find-definitions :which-key "Go to definition")
  "l R" '(xref-find-references :which-key "Find references"))

;; Better diagnostics display
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (set-face-attribute 'eglot-highlight-symbol-face nil
                                :background (face-attribute 'highlight :background)
                                :foreground (face-attribute 'highlight :foreground))))

(define-key eglot-mode-map [remap xref-find-definitions] 'eglot-find-declaration)
(define-key eglot-mode-map [remap xref-find-references] 'eglot-find-references)

(setq eglot-autoshutdown t)

(straight-use-package 'flycheck)
(straight-use-package 'diminish)

(require 'diminish)
(diminish 'flycheck-mode)
(diminish 'eldoc-mode)
(diminish 'abbrev-mode)
(diminish 'auto-revert-mode)
(diminish 'ivy-mode "Ivy")
(diminish 'ivy-rich-mode)
(diminish 'counsel-mode)
(diminish 'which-key-mode)
(diminish 'org-indent-mode)
(diminish 'company-mode "Cmp")
(diminish 'company-box-mode)

(global-flycheck-mode)

(setq flycheck-check-syntax-automatically '(save mode-enabled)
      flycheck-checker 'python-pylint
      flycheck-command-wrapper-function
      (lambda (command) (append '("nice" "-n5") command)))

(my/leader-keys
  "c" '(:ignore t :which-key "check commands")
  "c c" '(flycheck-clear :which-key "Clear errors")
  "c n" '(flycheck-next-error :which-key "Next error")
  "c p" '(flycheck-previous-error :which-key "Previous error")
  "c l" '(flycheck-list-errors :which-key "List errors")
  "c v" '(flycheck-verify-setup :which-key "Verify setup")
  "c d" '(flycheck-disable-checker :which-key "Disable checker"))

(add-hook 'prog-mode-hook 'flycheck-mode)

(setq flycheck-indication-mode 'left-fringe
      flycheck-display-errors-function
      (lambda (errors)
        (let ((messages (mapcar #'flycheck-error-message errors)))
          (message "%s" (string-join messages "\n")))))

(with-eval-after-load 'flycheck
  (flycheck-add-next-checker 'python-flake8 'python-mypy)
  (setq flycheck-python-flake8-executable "flake8"
        flycheck-python-mypy-executable "mypy"
        flycheck-javascript-eslint-executable "eslint"
        flycheck-javascript-jshint-executable "jshint"
        flycheck-typescript-tsc-executable "tsc"
        flycheck-rust-cargo-executable "cargo"
        flycheck-rust-clippy-executable "clippy-driver"
        flycheck-c++-gcc-executable "g++"
        flycheck-c++-clang-executable "clang++"
        flycheck-gcc-include-path nil)
  (add-to-list 'flycheck-checkers 'javascript-eslint)
  (add-to-list 'flycheck-checkers 'javascript-jshint)
  (add-to-list 'flycheck-checkers 'typescript-tsc)
  (add-to-list 'flycheck-checkers 'rust-clippy)
  (add-to-list 'flycheck-checkers 'rust-cargo)
  (add-to-list 'flycheck-checkers 'c-gcc)
  (add-to-list 'flycheck-checkers 'c++-gcc)
  (add-to-list 'flycheck-checkers 'c++-clang))

(straight-use-package 'company)
(straight-use-package 'company-box)

(add-hook 'after-init-hook 'global-company-mode)

(setq company-minimum-prefix-length 2
      company-idle-delay 0.5
      company-selection-wrap-around t
      company-show-numbers t
      company-tooltip-minimum-width 80
      company-tooltip-limit 20
      company-tooltip-align-annotations t
      company-require-match nil
      company-global-modes '(not eshell-mode shell-mode vterm-mode)
      company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
      company-dabbrev-code-everywhere t
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case t
      company-dabbrev-other-buffers t)

(when (require 'company-box nil t)
  (company-box-mode)
  (setq company-box-show-single-candidate t
        company-box-doc-enable t
        company-box-icons-unknown 'fa-question-circle))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<shift-tab>") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous))

(my/leader-keys
  "o" '(:ignore t :which-key "completion commands")
  "o c" '(company-complete :which-key "Complete")
  "o h" '(company-show-doc-buffer :which-key "Show docs")
  "o i" '(company-yasnippet :which-key "Snippet")
  "o m" '(company-manual-begin :which-key "Manual")
  "o r" '(company-abort :which-key "Abort"))

(defun my/install-tree-sitter-python ()
  "Install Python tree-sitter grammar."
  (interactive)
  (when (treesit-available-p)
    (message "Installing Python tree-sitter grammar...")
    (call-interactively 'treesit-install-language-grammar)
    (message "Python grammar installation complete!")))

;; To enable Python syntax highlighting, run:
;; M-x my/install-tree-sitter-python RET python RET

;; Helper to check if a grammar is available
(defun my/treesit-grammar-available-p (lang)
  "Check if tree-sitter grammar for LANG is available."
  (when (and (fboundp 'treesit-available-p) (treesit-available-p))
    (let ((mode-name (intern (format "%s-ts-mode" lang))))
      (require mode-name nil t))))

;; NOTE: Tree-sitter mode remapping is disabled until grammars are installed
;; To enable tree-sitter syntax highlighting:
;; 1. Run: M-x treesit-install-language-grammar RET python RET
;; 2. Wait for installation to complete
;; 3. Restart Emacs
;; 4. Uncomment the remapping below when ready

;; Prefer tree-sitter modes when grammars are installed (Emacs 29+)
;; (when (fboundp 'treesit-available-p)
;;   (setq major-mode-remap-alist
;;         '((bash-mode       . bash-ts-mode)
;;           (c-mode          . c-ts-mode)
;;           (c++-mode        . c++-ts-mode)
;;           (css-mode        . css-ts-mode)
;;           (dockerfile-mode . dockerfile-ts-mode)
;;           (go-mode         . go-ts-mode)
;;           (html-mode       . html-ts-mode)
;;           (java-mode       . java-ts-mode)
;;           (javascript-mode . js-ts-mode)
;;           (json-mode       . json-ts-mode)
;;           (python-mode     . python-ts-mode)
;;           (rust-mode       . rust-ts-mode)
;;           (sh-mode         . bash-ts-mode)
;;           (typescript-mode . typescript-ts-mode)
;;           (yaml-mode       . yaml-ts-mode))))

;; Tree-sitter specific settings for optimal performance
(when (fboundp 'treesit-font-lock-recompute-features)
  ;; Enable query-based syntax highlighting for better performance
  (setq treesit-font-lock-level 4)
  (setq treesit-font-lock-feature-list
        '((comment definition)
          (keyword string)
          (function type constant)
          (assignment builtin operator property))))

;; Automatically use tree-sitter where available
(defun my/prefer-tree-sitter-modes ()
  "Automatically use tree-sitter based modes when available."
  (when (and (fboundp 'treesit-available-p) (treesit-available-p))
    (when (eq major-mode 'python-mode)
      (condition-case nil
          (when (treesit-language-available-p "python")
            (python-ts-mode))
        (error nil)))
    (when (eq major-mode 'javascript-mode)
      (condition-case nil
          (when (treesit-language-available-p "javascript")
            (js-ts-mode))
        (error nil)))
    (when (eq major-mode 'typescript-mode)
      (condition-case nil
          (when (treesit-language-available-p "typescript")
            (typescript-ts-mode))
        (error nil)))
    (when (eq major-mode 'go-mode)
      (condition-case nil
          (when (treesit-language-available-p "go")
            (go-ts-mode))
        (error nil)))
    (when (eq major-mode 'rust-mode)
      (condition-case nil
          (when (treesit-language-available-p "rust")
            (rust-ts-mode))
        (error nil)))
    (when (eq major-mode 'json-mode)
      (condition-case nil
          (when (treesit-language-available-p "json")
            (json-ts-mode))
        (error nil)))
    (when (eq major-mode 'yaml-mode)
      (condition-case nil
          (when (treesit-language-available-p "yaml")
            (yaml-ts-mode))
        (error nil)))))

;; Enable tree-sitter for supported languages
(add-hook 'prog-mode-hook 'my/prefer-tree-sitter-modes)

(straight-use-package 'magit)

;; Magit keybindings (using 'G' prefix to avoid conflict with window bindings)
(my/leader-keys
  "G" '(:ignore t :which-key "git commands")
  "G s" '(magit-status :which-key "Status")
  "G d" '(magit-diff :which-key "Diff")
  "G l" '(magit-log :which-key "Log")
  "G c" '(magit-commit :which-key "Commit")
  "G p" '(magit-push :which-key "Push")
  "G P" '(magit-pull :which-key "Pull")
  "G b" '(magit-branch :which-key "Branch")
  "G m" '(magit-merge :which-key "Merge")
  "G r" '(magit-revert :which-key "Revert")
  "G h" '(magit-checkout :which-key "Checkout"))

;; Basic magit configuration
(setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
      magit-diff-refine-hunk t
      magit-save-repository-buffers 'dontask)

;; Auto-refresh magit buffers
(add-hook 'after-save-hook 'magit-after-save-refresh-status)

(straight-use-package 'sudo-edit)
(require 'sudo-edit)

;; Keybindings for sudo-edit are defined in the main leader-keys section above

(straight-use-package 'eshell-syntax-highlighting)

(setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
      eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
      eshell-history-size 5000
      eshell-buffer-maximum-lines 10000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input 'all
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands '("bash" "fish" "htop" "ssh" "top" "zsh")
      eshell-highlight-prompt t
      eshell-prompt-regexp "^[^#$%>\n]*[#$%>] *"
      eshell-prompt-function
      (lambda nil
        (concat
         (propertize (abbreviate-file-name (eshell/pwd)) 'face 'eshell-prompt)
         (propertize " $ " 'face 'eshell-prompt))))

(let ((eshell-dir (concat user-emacs-directory "eshell")))
  (unless (file-exists-p eshell-dir) (make-directory eshell-dir t))
  (unless (file-exists-p eshell-rc-script)
    (write-region "# Eshell profile\n\necho \"Welcome to Eshell!\"\n" nil eshell-rc-script))
  (unless (file-exists-p eshell-aliases-file)
    (write-region "# Eshell aliases\nalias ll 'ls -la'\nalias .. 'cd ..'\nalias c 'clear'\n" nil eshell-aliases-file)))

(with-eval-after-load 'esh-mode
  (when (require 'eshell-syntax-highlighting nil t)
    (eshell-syntax-highlighting-global-mode +1)))

(defun eshell/clear () (interactive) (let ((inhibit-read-only t)) (erase-buffer)))
(defun eshell-previous-prompt () (interactive) (eshell-bol) (re-search-backward eshell-prompt-regexp nil t))
(defun eshell-next-prompt () (interactive) (re-search-forward eshell-prompt-regexp nil t))

(with-eval-after-load 'eshell
  (define-key eshell-mode-map (kbd "C-c C-p") 'eshell-previous-prompt)
  (define-key eshell-mode-map (kbd "C-c C-n") 'eshell-next-prompt))

(straight-use-package 'vterm)

;; Optimized vterm configuration for normal terminal behavior
(setq vterm-max-scrollback 10000
      vterm-buffer-name-string "vterm %s"
      vterm-kill-buffer-on-exit t
      vterm-timer-delay 0.01
      vterm-use-vterm-prompt-detection-method t
      vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=off"
      ;; Prevent unwanted scrolling
      vterm-scroll-to-bottom-on-input nil
      vterm-scroll-to-bottom-on-output nil
      vterm-clear-scrollback t)

(add-hook 'vterm-mode-hook
          (lambda ()
            (setq-local scroll-margin 0)
            (setq-local scroll-conservatively 0)
            (setq-local scroll-preserve-screen-position nil)
            (setq-local scroll-step 1)))

;; Optimized vterm function
(defun my/vterm ()
  "Open vterm in a new buffer with normal terminal behavior."
  (interactive)
  (condition-case err
      (progn
        (require 'vterm)
        (let ((buf (vterm (generate-new-buffer-name "*vterm*"))))
          (with-current-buffer buf
            (setq-local scroll-margin 0)
            (setq-local scroll-conservatively 0))
          buf))
    (error (message "vterm failed: %s" (error-message-string err))
           (eshell))))

(straight-use-package 'vterm-toggle)

(defun my/vterm-toggle-safe-init ()
  (condition-case err
      (progn
        (require 'vterm-toggle)
        (setq vterm-toggle-fullscreen-p nil
              vterm-toggle-scope 'project)
        (message "vterm-toggle loaded successfully")
        t)
    (error (message "Failed to load vterm-toggle: %s" (error-message-string err))
           nil)))

(when (my/vterm-toggle-safe-init)
  (defun my/vterm-toggle ()
    (interactive)
    (condition-case err
        (vterm-toggle)
      (error (message "Failed to toggle vterm: %s" (error-message-string err))
             (eshell)))))

(straight-use-package 'doom-themes)
(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)

;; Load the theme (doom-one, doom-vibrant, doom-city-lights, doom-tokyo-night, etc.)
;; Ensure theme loads after doom-themes is fully initialized
(with-eval-after-load 'doom-themes
  (condition-case nil
      (load-theme 'doom-tokyo-night t)
    (error (message "Failed to load doom-tokyo-night theme, falling back to doom-one")
           (load-theme 'doom-one t))))

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)
;; or for treemacs users
(doom-themes-treemacs-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

(defun toggle-transparency ()
  "Toggle transparency between 100% and 85%."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter nil 'alpha
                         (if (equal alpha '(85 . 85))
                             '(100 . 100)
                           '(85 . 85)))))

;; Enable transparency by default
(set-frame-parameter (selected-frame) 'alpha '(85 . 85))

(windmove-default-keybindings)

;; Auto-focus new windows
(advice-add 'split-window-below :after #'other-window)
(advice-add 'split-window-right :after #'other-window)

(straight-use-package 'which-key)
(require 'which-key)

;; Configure which-key
(setq which-key-idle-delay 0.3
      which-key-show-transient-maps t
      which-key-sort-order 'which-key-key-order-alpha
      ;; Use minibuffer when posframe is not available, otherwise posframe will handle it
      which-key-popup-type 'minibuffer
      which-key-side-window-location 'bottom
      which-key-side-window-max-width 0.33
      which-key-side-window-max-height 0.25
      ;; Show longer descriptions for better understanding
      which-key-max-description-length 80
      which-key-separator " → "
      which-key-prefix-prefix "+"
      ;; Enable extended meanings to show more keybinding info
      which-key-enable-extended-meanings t
      ;; Show major mode for better context
      which-key-show-major-mode t
      ;; Show more context about keybindings
      which-key-description-column 35
      which-key-side-window-max-height 0.4
      which-key-use-key-binding-name t)

;; Hide unnecessary items from which-key
(setq which-key-allow-multiple-replacements t
      which-key-replacement-alist
      '((("which-key-command" . "\\` +") . nil)
        (("which-key-show-next-page-cycle" . "\\` +") . nil)
        (("which-key-show-previous-page-cycle" . "\\` +") . nil)))

(which-key-mode)

(straight-use-package 'dashboard)
(straight-use-package 'page-break-lines)
(straight-use-package 'projectile)

(require 'all-the-icons)
(require 'projectile)

(projectile-mode +1)
(setq projectile-project-search-path '("~/workspaces/" "~/projects/" "~/code/" "~/")
      dashboard-projects-backend 'projectile)

(setq dashboard-banner-logo-title "BKC's Emacs Dashboard"
      dashboard-startup-banner (expand-file-name "img/logo.png" user-emacs-directory)
      dashboard-center-content t
      dashboard-vertically-center-content t
      dashboard-navigation-cycle t
      dashboard-show-shortcuts t
      dashboard-use-navigator t
      dashboard-icon-type 'all-the-icons
      dashboard-set-heading-icons t
      dashboard-set-file-icons t
      dashboard-items '((recents . 5) (bookmarks . 5) (projects . 5))
      dashboard-set-navigator t
      dashboard-set-footer t
      dashboard-footer-messages '("\"Powered by Bibit Kunwar Chhetri\"")
      dashboard-footer-icon (all-the-icons-octicon "heart" :height 1.1 :v-adjust 0.0)
      dashboard-footer-message-fn (lambda () (car dashboard-footer-messages)))

(dashboard-setup-startup-hook)
(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))

(defun my/dashboard-first-load-refresh ()
  (when (string= (buffer-name) "*dashboard*")
    (run-with-timer 0.5 nil 'dashboard-refresh-buffer)))

(defun my/dashboard-refresh-with-new-message ()
  "Refresh dashboard with a new random footer message."
  (dashboard-refresh-buffer))

;; Remove automatic refresh on first load to prevent cursor jumping
;; (add-hook 'dashboard-mode-hook 'my/dashboard-first-load-refresh)

;; Dashboard keybindings (Evil mode handles navigation automatically)
(with-eval-after-load 'dashboard
  (define-key dashboard-mode-map (kbd "RET") 'dashboard-enter)
  (define-key dashboard-mode-map (kbd "r") 'my/dashboard-refresh-with-new-message)
  (define-key dashboard-mode-map (kbd "g") 'my/dashboard-refresh-with-new-message))

(defun dashboard-insert-items-with-separators ()
  (let ((width (frame-width)))
    (insert (propertize (make-string width ?─) 'face 'shadow))
    (dashboard-insert-newline))
  (dashboard-insert-items)
  (dashboard-insert-newline)
  (let ((width (frame-width)))
    (insert (propertize (make-string width ?─) 'face 'shadow))))

(setq dashboard-startupify-list '(dashboard-insert-banner
                                   dashboard-insert-newline
                                   dashboard-insert-banner-title
                                   dashboard-insert-newline
                                   dashboard-insert-navigator
                                   dashboard-insert-newline
                                   dashboard-insert-init-info
                                   dashboard-insert-newline
                                   dashboard-insert-newline
                                   dashboard-insert-items-with-separators
                                   dashboard-insert-newline
                                   dashboard-insert-newline
                                   dashboard-insert-footer))

(setq dashboard-navigator-buttons
      `(((,(when (fboundp 'all-the-icons-octicon)
             (all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0))
          "Homepage" "Browse homepage"
          (lambda (&rest _) (browse-url "https://github.com/bibitchhetri")))
         ("★" "Blog" "Show stars" (lambda (&rest _) (browse-url "https://bibitkunwar.com.np")) warning)
         ("?" "" "?/h" #'show-help nil "<" ">")
         (,(when (fboundp 'all-the-icons-faicon)
             (all-the-icons-faicon "refresh" :height 1.1 :v-adjust 0.0))
          "Refresh" "Refresh dashboard"
          (lambda (&rest _) (dashboard-refresh-buffer)))
         ("🔄" "Restart" "Restart Emacs" (lambda (&rest _) (restart-emacs)) error))))

(with-eval-after-load 'dashboard
  (when (file-exists-p (expand-file-name "img/logo.png" user-emacs-directory))
    (setq dashboard-startup-banner (expand-file-name "img/logo.png" user-emacs-directory))))

(global-page-break-lines-mode)

(my/leader-keys
  "d" '(:ignore t :which-key "dashboard commands")
  "d d" '(dashboard-open :which-key "Open dashboard")
  "d r" '(my/dashboard-refresh-with-new-message :which-key "Refresh dashboard")
  "d a" '(projectile-add-known-project :which-key "Add project"))

(defun my/add-common-projects ()
  (interactive)
  (let ((common-projects '("~/.emacs.d" "~/.dotfiles" "~/workspaces" "~/projects" "~/code")))
    (dolist (project common-projects)
      (when (file-exists-p (expand-file-name project))
        (projectile-add-known-project project)))))

;; Configure mini-buffer to appear as a centered child frame with border
;; Using ivy-posframe for centered Ivy prompts

;; Install ivy-posframe if needed
(straight-use-package 'ivy-posframe)

;; Configure centered mini-buffer with borders
(when (require 'ivy-posframe nil t)
  ;; Enable ivy-posframe by default
  (setq ivy-posframe-mode t)
  
  ;; Configure the appearance of the centered frame
  ;; Get the current frame's alpha value to sync transparency
  (let ((current-alpha (frame-parameter nil 'alpha)))
    (setq ivy-posframe-parameters
          (append
           '((left-fringe . 8)
             (right-fringe . 8)
             (child-frame-border-width . 4))
           (if current-alpha
               `((alpha . ,(max (car current-alpha) 90)))
             '((alpha . 90)))
           (when (eq system-type 'darwin)
             '((undecorated-round . t)
               (child-frame-border-width . 4)))))
  
  ;; Add distinct visible borders with padding
  (setq ivy-posframe-border-width 4
        ivy-posframe-internal-border-width 3)
  
  ;; Position at center of screen
  (setq ivy-posframe-display-at-frame-center t)
  
  ;; Show numbers in candidates
  (setq ivy-posframe-show-numbers t)
  
  ;; Customize the frame appearance - make it appropriately sized and centered
  (setq ivy-posframe-width 90
        ivy-posframe-min-width 70
        ivy-posframe-height 20
        ivy-posframe-min-height 10)
  
  ;; Configure faces after theme loads
  (defun my/setup-ivy-posframe-faces ()
    "Setup ivy-posframe faces safely after theme loads with distinct borders."
    (when (require 'ivy-posframe nil t)
      (condition-case nil
          (progn
            ;; Ensure ivy-posframe mode is enabled
            (ivy-posframe-mode 1)
            ;; Just inherit from default - theme will handle colors
            (set-face-attribute 'ivy-posframe nil :inherit 'default)
            ;; Make border stand out with a distinct blue color
            (condition-case nil
                (let ((border-color (if (eq (frame-parameter nil 'background-mode) 'dark)
                                         "#74b9ff"  ; bright blue for dark theme
                                       "#0984e3"))) ; darker blue for light theme
                  (set-face-attribute 'ivy-posframe-border nil
                                      :background border-color
                                      :foreground border-color
                                      :box t
                                      :box-color border-color))
              (error nil)))
        (error nil))))
  
  ;; Set up faces after UI is ready
  (run-with-idle-timer 0.5 nil #'my/setup-ivy-posframe-faces)
  (add-hook 'window-setup-hook #'my/setup-ivy-posframe-faces)
  
  ;; Custom function to toggle
  (defun my/toggle-mini-frame ()
    "Toggle centered mini-buffer frame."
    (interactive)
    (if ivy-posframe-mode
        (progn
          (ivy-posframe-mode -1)
          (message "Centered mini-frame disabled"))
      (progn
        (ivy-posframe-mode 1)
        (message "Centered mini-frame enabled"))))
  
  ;; Add to leader keys
  (my/leader-keys
    "m" '(:ignore t :which-key "mini-frame commands")
    "m m" '(my/toggle-mini-frame :which-key "Toggle centered mini-frame")))

;; Configure which-key to also use centered posframe
(straight-use-package 'which-key-posframe)

(when (require 'which-key-posframe nil t)
  ;; Enable which-key-posframe by default
  (which-key-posframe-mode t)
  
  ;; Configure the posframe appearance
  ;; Get the current frame's alpha value to sync transparency
  (let ((current-alpha (frame-parameter nil 'alpha)))
    (setq which-key-posframe-parameters
          (append
           '((left-fringe . 8)
             (right-fringe . 8)
             (child-frame-border-width . 4))
           (if current-alpha
               `((alpha . ,(max (car current-alpha) 90)))
             '((alpha . 90)))
           (when (eq system-type 'darwin)
             '((undecorated-round . t)
               (child-frame-border-width . 4)))))
  
  ;; Position at center of screen
  (setq which-key-posframe-poshandler #'posframe-poshandler-frame-center)
  
  ;; Set size - make it larger for better visibility
  (setq which-key-posframe-width 0.40
        which-key-posframe-height 0.35)
  
  ;; Set distinct visible border with enhanced styling
  (setq which-key-posframe-border-width 4)
  
  ;; Show major mode in header
  (setq which-key-show-major-mode t
        ;; Better display of keybindings
        which-key-max-description-length 80
        which-key-description-column 40
        ;; Use the same settings from main which-key
        which-key-sort-order 'which-key-key-order-alpha
        which-key-separator " → "
        which-key-prefix-prefix "+")
  
  ;; Auto-hide after selection
  (setq which-key-posframe-hide-dead-keys t)
  
  ;; Configure faces after theme loads
  (defun my/setup-which-key-posframe-faces ()
    "Setup which-key-posframe faces safely after theme loads with distinct borders."
    (when (require 'which-key-posframe nil t)
      (condition-case nil
          (progn
            ;; Just inherit from default - theme will handle colors
            (set-face-attribute 'which-key-posframe nil :inherit 'default)
            ;; Make border stand out with a distinct color (different from ivy)
            (condition-case nil
                (let ((border-color (if (eq (frame-parameter nil 'background-mode) 'dark)
                                         "#74b9ff"  ; bright blue for dark theme
                                       "#0984e3"))) ; darker blue for light theme
                  (set-face-attribute 'which-key-posframe-border nil
                                      :background border-color
                                      :foreground border-color
                                      :box t
                                      :box-color border-color))
              (error nil)))
        (error nil))))
  
  ;; Set up faces after UI is ready
  (run-with-idle-timer 0.5 nil #'my/setup-which-key-posframe-faces)
  (add-hook 'window-setup-hook #'my/setup-which-key-posframe-faces)
  
  ;; Function to toggle which-key posframe
  (defun my/toggle-which-key-frame ()
    "Toggle centered which-key frame."
    (interactive)
    (if which-key-posframe-mode
        (progn
          (which-key-posframe-mode -1)
          (message "Centered which-key disabled"))
      (progn
        (which-key-posframe-mode 1)
        (message "Centered which-key enabled"))))
  
  ;; Add to leader keys
  (my/leader-keys
    "m k" '(my/toggle-which-key-frame :which-key "Toggle centered which-key")))))

;; Ensure ivy-posframe is enabled at startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (fboundp 'ivy-posframe-mode)
              (ivy-posframe-mode 1))
            (when (fboundp 'which-key-posframe-mode)
              (which-key-posframe-mode 1))
            (message "Emacs ready in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))
