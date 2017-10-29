
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-safe-themes
   (quote
	("5900bec889f57284356b8216a68580bfa6ece73a6767dfd60196e56d050619bc" "365d9553de0e0d658af60cff7b8f891ca185a2d7ba3fc6d29aadba69f5194c7f" "611e38c2deae6dcda8c5ac9dd903a356c5de5b62477469133c89b2785eb7a14d" "4182c491b5cc235ba5f27d3c1804fc9f11f51bf56fb6d961f94788be034179ad" "c768203377670d37ee31151e3ebc82e4886951ff1e1a58fc68d57bc77e1b586e" "237f00ee07bd00b8e2406fa5e439ece62b8f7a396b02fca6e2dcf2190888be70" "c3e93048268d6489af814c3da8f5d4f31febef7a3c4392051d3c12d0c301cefd" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "53a9ec5700cf2bb2f7059a584c12a5fdc89f7811530294f9eaf92db526a9fb5f" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "a4c9e536d86666d4494ef7f43c84807162d9bd29b0dfd39bdf2c3d845dcc7b2e" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "d5b121d69e48e0f2a84c8e4580f0ba230423391a78fcb4001ccb35d02494d79e" "946e871c780b159c4bb9f580537e5d2f7dba1411143194447604ecbaf01bd90c" "6f11ad991da959fa8de046f7f8271b22d3a97ee7b6eca62c81d5a917790a45d9" "b81bfd85aed18e4341dbf4d461ed42d75ec78820a60ce86730fc17fc949389b2" default)))
 '(git-gutter:modified-sign "~")
 '(hl-todo-keyword-faces
   (quote
	(("HOLD" . "#d0bf8f")
	 ("TODO" . "#c3be1ff")
	 ("NEXT" . "#dca3a3")
	 ("THEM" . "#dc8cc3")
	 ("PROG" . "#7cb8bb")
	 ("OKAY" . "#7cb8bb")
	 ("DONT" . "#5f7f5f")
	 ("FAIL" . "#8c5353")
	 ("DONE" . "#afd8af")
	 ("NOTE" . "#d0bf8f")
	 ("KLUDGE" . "#d0bf8f")
	 ("HACK" . "#d0bf8f")
	 ("FIXME" . "#cc9393")
	 ("XXX" . "#cc9393")
	 ("XXXX" . "#cc9393")
	 ("???" . "#cc9393"))))
 '(package-selected-packages
   (quote
	(json-mode evil-commentary auto-highlight-symbol dumb-jump evil-mc evil-multiedit multiedit evil evil-mode color-theme color-themes auto-complete counsel swiper ace-window which-key try use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground "#5f5f87" :slant italic))))
 '(font-lock-comment-face ((t (:foreground "#5f5f87"))))
 '(font-lock-constant-face ((t (:foreground "#56B6C2"))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "#5F5F87"))))
 '(font-lock-variable-name-face ((t (:foreground "#66d9ef"))))
 '(git-gutter:modified ((t (:foreground "goldenrod"))))
 '(highlight-numbers-number ((t (:inherit nil :foreground "#9c91e4"))))
 '(hl-todo ((t (:foreground "magenta" :weight bold))))
 '(js2-object-property ((t (:foreground "gainsboro"))))
 '(neo-vc-added-face ((t (:foreground "lime green"))))
 '(neo-vc-edited-face ((t (:foreground "gold"))))
 '(org-level-1 ((t (:background "#1d1f20" :foreground "#fb2874" :weight bold :height 1.0))))
 '(php-object-op ((t (:inherit default :foreground "#fb2874"))))
 '(php-variable-name ((t (:inherit font-lock-variable-name-face :foreground "white smoke"))))
 '(todo-mark ((t (:foreground "magenta" :weight bold))))
 '(web-mode-error-face ((t (:underline (:color "yellow" :style wave)))))
 '(web-mode-variable-name-face ((t (:inherit font-lock-variable-name-face :foreground "white smoke")))))
