
(require 'package)
(setq pacakge-enable-at-startup nil)
(add-to-list 'package-archives
                '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))

(use-package try
:ensure t)

(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(use-package doom-themes
:ensure t)

(load-theme 'doom-vibrant t) ;load theme

;;Set fonts
(set-default-font "Space Mono 11")

(setq make-backup-files nil); stop creating backup~ files
(setq auto-save-default nil); stop creating #autosave# files
(set-default 'truncate-lines t); stop wraping lines
(menu-bar-mode -1); disable menubar
(toggle-scroll-bar -1); disable scrollbar
(tool-bar-mode -1); disable toolbar 
(setq inhibit-startup-message t); start with scratch buffer
(setq-default tab-width 4); tab width
(global-linum-mode t); show line numbers
(show-paren-mode 1); show matching parenth
(setq show-paren-delay 0); turn of matching delay
;; use relative line numbers
(use-package linum-relative
        :ensure t
        :config
                (linum-relative-global-mode t)
                (setq linum-relative-current-symbol "")
)
;; dired use only one buffer
(put 'dired-find-alternate-file 'disabled nil)

(use-package neotree
:ensure t
:init
(add-hook 'neotree-mode-hook
                        (lambda ()
                        (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                        (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
                        (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
                        (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;;Custom funciton to open netoreee in project root folder
(defun neotree-project-dir-toggle ()
"Open NeoTree using the project root, using find-file-in-project, or the current buffer directory."
(interactive)
(let ((project-dir
        (ignore-errors
        ;;; Pick one: projectile or find-file-in-project
        (projectile-project-root)
        ;(ffip-project-root)
        ))
        ;(file-name (buffer-file-name))
        )
(if (and (fboundp 'neo-global--window-exists-p)
                (neo-global--window-exists-p))
        (neotree-hide)
(progn
        (neotree-show)
        (if project-dir
                (neotree-dir project-dir))
        ))))

:config
 (setq
  neo-autorefresh t
  neo-theme 'ascii
  neo-vc-integration '(face char))
        (setq neo-force-change-root t)
)
(global-set-key (kbd "C-\\") #'neotree-project-dir-toggle)
(custom-set-faces
 '(neo-vc-added-face ((t (:foreground "lime green"))))
 '(neo-vc-edited-face ((t (:foreground "gold"))))
)

 ;(neo-theme (quote ascii))

(use-package autopair
        :ensure t
        :init
        (autopair-global-mode t)
)

(use-package projectile
        :ensure t
        :init
        (projectile-global-mode)
)

(use-package helm
:ensure t)

(use-package helm-ag
:ensure t)

(use-package general :ensure t
        :config
        (general-evil-setup t)

        (general-define-key
        :states '(normal emacs)
        :prefix ","

                "f" '(find-file :which-key "find file")
                "w" '(save-buffer)
                "p" '(projectile-find-file)

                ;;Window navigation
                "xl" '(evil-window-right)
                "xj" '(evil-window-down)
                "xk" '(evil-window-up)
                "xh" '(evil-window-left)

                ;;Buffer management
                "l" '(switch-to-buffer)
                "k" '(kill-buffer)

                ;;Searching
                "ss" '(helm-do-ag)
                "sh" '(helm-ag-project-root)

                ;;Go to definition
                "dg" '(dumb-jump-go)
                "db" '(dumb-jump-back)
                "ds" '(dumb-jump-quick-look)

                ;;Go mode
                "gd" '(godef-jump)

                ;;JS import
                "ii" '(js-import)
        )
)

(use-package dumb-jump
  :config (setq dumb-jump-selector 'helm)
  :ensure)

(use-package flycheck
        :ensure t
        :init
        (global-flycheck-mode)
        (setq flycheck-check-syntax-automatically '(mode-enabled save))
        (setq-default flycheck-temp-prefix "~/.eslintrc")
        :config
        (setq
        flycheck-disabled-checkers
        (append flycheck-disabled-checkers
                '(javascript-jshint))
        )
)
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'vue-mode)

;(use-package company
;:ensure t
;:init
;(add-hook 'after-init-hook 'global-company-mode)
;:config
;(setq company-dabbrev-downcase 0)
;)

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    ))

(use-package imenu-list
:ensure t
:config
(setq imenu-list-auto-resize t)
(setq imenu-list-focus-after-activation t)
)
(global-set-key (kbd "C-'") #'imenu-list-smart-toggle)



(use-package yasnippet
:ensure t
:init
(yas-global-mode 1)
)

(use-package yasnippet-snippets
:ensure t)

(use-package php-auto-yasnippets
:ensure t
:config
(payas/ac-setup)
)

(use-package hl-todo
:ensure t
:init
(global-hl-todo-mode t)
)

(use-package highlight-numbers
:ensure t
:config
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
)

(use-package js-import
:ensure t)

(use-package js2-mode
:ensure t
:init
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'load-path "/home/shmiga/github.com/tern/emacs/")
)
(autoload 'tern-mode "tern.el" nil t)

(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

(eval-after-load 'tern
'(progn
        (require 'tern-auto-complete)
        (tern-ac-setup)))

;(use-package vue-mode
;       :ensure t
;       :config
;       ;; 0, 1, or 2, representing (respectively) none, low, and high coloring
;       (setq mmm-submode-decoration-level 0))

(use-package web-mode
:ensure t
:init
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade.php\\'" . web-mode))
)

(use-package go-mode
        :ensure t
        :init
        (defun my-go-mode-hook ()
        (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
        ; Godef jump key binding                                                      
        (local-set-key (kbd "M-.") 'godef-jump)
        (local-set-key (kbd "M-*") 'pop-tag-mark)
        )
        (add-hook 'go-mode-hook 'my-go-mode-hook)
)

(use-package go-autocomplete
:ensure t)

(use-package auto-complete-config
:ensure t)

(ac-config-default)

(use-package exec-path-from-shell
:ensure t)

(defun my-go-mode-hook ()
        ; Call Gofmt before saving
        (add-hook 'before-save-hook 'gofmt-before-save)
        ; Customize compile command to run go build
        (if (not (string-match "go" compile-command))
                (set (make-local-variable 'compile-command)
                        "go build -v && go test -v && go vet"))
        ; Godef jump key binding
        (local-set-key (kbd "M-.") 'godef-jump)
        (local-set-key (kbd "M-*") 'pop-tag-mark)
        )

(use-package php-mode
:ensure t)

(use-package evil
  :ensure t
  :init
  (evil-mode 1)
)

(use-package emmet-mode
        :ensure t
        :init
        (add-hook 'vue-mode-hook 'emmet-mode)
        (add-hook 'html-mode-hook 'emmet-mode)
        (add-hook 'web-mode-hook 'emmet-mode)
)

(use-package git-gutter
:ensure t
:init
        (global-git-gutter-mode)
        ;(custom-set-variables
        ;'(git-gutter:window-width 2)
        ;'(git-gutter:modified-sign "~")
        ;'(git-gutter:added-sign "+")
        ;'(git-gutter:deleted-sign "-"))

        ;(set-face-background 'git-gutter:modified "none") ;; background color
        (set-face-foreground 'git-gutter:added "green")
        (set-face-foreground 'git-gutter:deleted "red")
        (set-face-foreground 'git-gutter:modified "yellow")
)

(use-package multiple-cursors
:ensure t)
(global-set-key (kbd "C-l") 'mc/mark-next-like-this)

;(use-package rainbow-delimiters
;:ensure t
;:init
;(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;)

(use-package json-mode
:ensure t)

(use-package evil-commentary
:ensure t
:init
(evil-commentary-mode t)
)

(use-package auto-highlight-symbol
:ensure t
:init
(auto-highlight-symbol-mode 1)
)



;;Package that shows shows shortkeys after <C-x> is pressed
(use-package which-key
  :ensure t
  :config (which-key-mode))


;;Enables mode that shows buffers
;(setq indo-enable-flex-matching t)
;(setq ido-everywhere t)
;(ido-mode 1)

;;Opens buffer list
(defalias 'list-buffers 'ibuffer)

;;For swiper to use <C-x><C-f>

(use-package counsel
  :ensure t
)

;;Better searching in file with <C-s>
(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))



;;Themes
(use-package color-theme
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-safe-themes
   (quote
    ("1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "d5b121d69e48e0f2a84c8e4580f0ba230423391a78fcb4001ccb35d02494d79e" "946e871c780b159c4bb9f580537e5d2f7dba1411143194447604ecbaf01bd90c" "6f11ad991da959fa8de046f7f8271b22d3a97ee7b6eca62c81d5a917790a45d9" "b81bfd85aed18e4341dbf4d461ed42d75ec78820a60ce86730fc17fc949389b2" default)))
 '(package-selected-packages
   (quote
    (evil evil-mode color-theme color-themes auto-complete counsel swiper ace-window which-key try use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
