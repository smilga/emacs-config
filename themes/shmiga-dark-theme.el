(deftheme shmiga-dark
  "Atom One Dark - An Emacs port of the Atom One Dark theme from Atom.io.")

(defvar shmiga-dark-colors-alist
  '(("shmiga-dark-accent"   . "#528BFF")
    ("shmiga-dark-fg"       . "#ABB2BF")
    ("shmiga-dark-bg"       . "#1C1C1C")
    ("shmiga-dark-bg-1"     . "#121417")
    ("shmiga-dark-bg-hl"    . "#2F343D")
    ("shmiga-dark-gutter"   . "#666D7A")
    ("shmiga-dark-accent"   . "#AEB9F5")
    ("shmiga-dark-mono-1"   . "#ABB2BF")
    ("shmiga-dark-mono-2"   . "#828997")
    ("shmiga-dark-mono-3"   . "#8A8A8A")
    ("shmiga-dark-cyan"     . "#56B6C2")
    ("shmiga-dark-blue"     . "#61AFEF")
    ("shmiga-dark-gold"     . "#FFFC5E")
    ("shmiga-dark-green"    . "#98C379")
    ("shmiga-dark-red-1"    . "#E06C75")
    ("shmiga-dark-red-2"    . "#BE5046")
    ("shmiga-dark-orange-1" . "#D19A66")
    ("shmiga-dark-cyan-2"   . "#E5C07B")
    ("shmiga-dark-gray"     . "#3E4451")
    ("shmiga-dark-silver"   . "#F2F2F2")
    ("shmiga-dark-salad"    . "#5FFF5F")
    ("shmiga-dark-cyan-2"   . "#FF7B5E")
    ("shmiga-dark-light"    . "#5f5f87")
    ("shmiga-dark-black"    . "#0F1011"))
  "List of Shmiga Dark colors.")

(defmacro shmiga-dark-with-color-variables (&rest body)
  "Bind the colors list around BODY."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@ (mapcar (lambda (cons)
                      (list (intern (car cons)) (cdr cons)))
                    shmiga-dark-colors-alist))
     ,@body))

(shmiga-dark-with-color-variables
  (custom-theme-set-faces
   'shmiga-dark

   `(default ((t (:foreground ,shmiga-dark-fg :background ,shmiga-dark-bg))))
   `(success ((t (:foreground ,shmiga-dark-light))))
   `(warning ((t (:foreground ,shmiga-dark-cyan-2))))
   `(error ((t (:foreground ,shmiga-dark-red-1 :weight bold))))
   `(link ((t (:foreground ,shmiga-dark-salad :underline t :weight bold))))
   `(link-visited ((t (:foreground ,shmiga-dark-salad :underline t :weight normal))))
   `(cursor ((t (:background ,shmiga-dark-accent))))
   `(fringe ((t (:background ,shmiga-dark-bg))))
   `(region ((t (:background ,shmiga-dark-gray))))
   `(highlight ((t (:background ,shmiga-dark-gray))))
   `(hl-line ((t (:background ,shmiga-dark-bg-hl))))
   `(vertical-border ((t (:foreground ,shmiga-dark-bg))))
   `(secondary-selection ((t (:background ,shmiga-dark-bg-1))))
   `(query-replace ((t (:inherit (isearch)))))
   `(minibuffer-prompt ((t (:foreground ,shmiga-dark-silver))))

   `(font-lock-builtin-face ((t (:foreground ,shmiga-dark-cyan))))
   `(font-lock-comment-face ((t (:foreground ,shmiga-dark-light))))
   `(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
   `(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
   `(font-lock-function-name-face ((t (:foreground ,shmiga-dark-salad))))
   `(font-lock-keyword-face ((t (:foreground ,shmiga-dark-gold))))
   `(font-lock-preprocessor-face ((t (:foreground ,shmiga-dark-mono-2))))
   `(font-lock-string-face ((t (:foreground ,shmiga-dark-mono-3))))
   `(font-lock-type-face ((t (:foreground ,shmiga-dark-cyan-2))))
   `(font-lock-constant-face ((t (:foreground ,shmiga-dark-cyan))))
   `(font-lock-variable-name-face ((t (:foreground ,shmiga-dark-red-1))))
   `(font-lock-warning-face ((t (:foreground ,shmiga-dark-mono-3 :bold t))))

   ;; mode-line
   `(mode-line ((t (:background ,shmiga-dark-black :foreground ,shmiga-dark-silver))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-emphasis ((t (:weight bold))))
   `(mode-line-inactive ((t (:background ,shmiga-dark-gray))))

   ;; ido
   `(ido-first-match ((t (:foreground ,shmiga-dark-gold :weight bold))))
   `(ido-only-match ((t (:foreground ,shmiga-dark-red-1 :weight bold))))
   `(ido-subdir ((t (:foreground ,shmiga-dark-salad))))
   `(ido-virtual ((t (:foreground ,shmiga-dark-mono-3))))

   ;; ace-jump
   `(ace-jump-face-background ((t (:foreground ,shmiga-dark-mono-3 :background ,shmiga-dark-bg-1 :inverse-video nil))))
   `(ace-jump-face-foreground ((t (:foreground ,shmiga-dark-red-1 :background ,shmiga-dark-bg-1 :inverse-video nil))))

   ;; company-mode
   `(company-tooltip ((t (:foreground ,shmiga-dark-fg :background ,shmiga-dark-bg-1))))
   `(company-tooltip-annotation ((t (:foreground ,shmiga-dark-mono-2 :background ,shmiga-dark-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,shmiga-dark-fg :background ,shmiga-dark-gray))))
   `(company-tooltip-mouse ((t (:background ,shmiga-dark-gray))))
   `(company-tooltip-common ((t (:foreground ,shmiga-dark-cyan-2 :background ,shmiga-dark-bg-1))))
   `(company-tooltip-common-selection ((t (:foreground ,shmiga-dark-cyan-2 :background ,shmiga-dark-gray))))
   `(company-preview ((t (:background ,shmiga-dark-bg))))
   `(company-preview-common ((t (:foreground ,shmiga-dark-cyan-2 :background ,shmiga-dark-bg))))
   `(company-scrollbar-fg ((t (:background ,shmiga-dark-mono-1))))
   `(company-scrollbar-bg ((t (:background ,shmiga-dark-bg-1))))

   ;; compilation
   `(compilation-face ((t (:foreground ,shmiga-dark-fg))))
   `(compilation-line-number ((t (:foreground ,shmiga-dark-mono-2))))
   `(compilation-column-number ((t (:foreground ,shmiga-dark-mono-2))))

   ;; isearch
   `(isearch ((t (:foreground ,shmiga-dark-bg :background ,shmiga-dark-gold))))
   `(isearch-fail ((t (:foreground ,shmiga-dark-red-2 :background nil))))
   `(lazy-highlight ((t (:foreground ,shmiga-dark-gold :background ,shmiga-dark-bg-1 :underline ,shmiga-dark-gold))))

   ;; diff-hl (https://github.com/dgutov/diff-hl)
   '(diff-hl-change ((t (:foreground "#E9C062" :background "#8b733a"))))
   '(diff-hl-delete ((t (:foreground "#CC6666" :background "#7a3d3d"))))
   '(diff-hl-insert ((t (:foreground "#A8FF60" :background "#547f30"))))

   ;; dired-mode
   '(dired-directory ((t (:inherit (font-lock-keyword-face)))))
   '(dired-flagged ((t (:inherit (diff-hl-delete)))))
   '(dired-symlink ((t (:foreground "#FD5FF1"))))

   ;; helm
   `(helm-header ((t (:foreground ,shmiga-dark-mono-2
                      :background ,shmiga-dark-bg
                      :underline nil
                      :box (:line-width 6 :color ,shmiga-dark-bg)))))
   `(helm-source-header ((t (:foreground ,shmiga-dark-cyan-2
                             :background ,shmiga-dark-bg
                             :underline nil
                             :weight bold
                             :box (:line-width 6 :color ,shmiga-dark-bg)))))
   `(helm-selection ((t (:background ,shmiga-dark-gray))))
   `(helm-selection-line ((t (:background ,shmiga-dark-gray))))
   `(helm-visible-mark ((t (:foreground ,shmiga-dark-bg :foreground ,shmiga-dark-cyan-2))))
   `(helm-candidate-number ((t (:foreground ,shmiga-dark-light :background ,shmiga-dark-bg-1))))
   `(helm-separator ((t (:background ,shmiga-dark-bg :foreground ,shmiga-dark-red-1))))
   `(helm-M-x-key ((t (:foreground ,shmiga-dark-orange-1))))
   `(helm-bookmark-addressbook ((t (:foreground ,shmiga-dark-orange-1))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,shmiga-dark-gold))))
   `(helm-bookmark-info ((t (:foreground ,shmiga-dark-light))))
   `(helm-bookmark-man ((t (:foreground ,shmiga-dark-cyan-2))))
   `(helm-bookmark-w3m ((t (:foreground ,shmiga-dark-gold))))
   `(helm-match ((t (:foreground ,shmiga-dark-cyan-2))))
   `(helm-ff-directory ((t (:foreground ,shmiga-dark-cyan :background ,shmiga-dark-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,shmiga-dark-fg :background ,shmiga-dark-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,shmiga-dark-light :background ,shmiga-dark-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,shmiga-dark-red-1 :background ,shmiga-dark-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,shmiga-dark-cyan-2 :background ,shmiga-dark-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,shmiga-dark-bg :background ,shmiga-dark-cyan-2 :weight normal))))
   `(helm-buffer-not-saved ((t (:foreground ,shmiga-dark-red-1))))
   `(helm-buffer-process ((t (:foreground ,shmiga-dark-mono-2))))
   `(helm-buffer-saved-out ((t (:foreground ,shmiga-dark-fg))))
   `(helm-buffer-size ((t (:foreground ,shmiga-dark-mono-2))))
   `(helm-buffer-directory ((t (:foreground ,shmiga-dark-gold))))
   `(helm-grep-cmd-line ((t (:foreground ,shmiga-dark-cyan))))
   `(helm-grep-file ((t (:foreground ,shmiga-dark-fg))))
   `(helm-grep-finish ((t (:foreground ,shmiga-dark-light))))
   `(helm-grep-lineno ((t (:foreground ,shmiga-dark-mono-2))))
   `(helm-grep-finish ((t (:foreground ,shmiga-dark-red-1))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-swoop-target-line-block-face ((t (:background ,shmiga-dark-mono-3 :foreground "#222222"))))
   `(helm-swoop-target-line-face ((t (:background ,shmiga-dark-mono-3 :foreground "#222222"))))
   `(helm-swoop-target-word-face ((t (:background ,shmiga-dark-gold :foreground "#ffffff"))))
   `(helm-locate-finish ((t (:foreground ,shmiga-dark-light))))
   `(info-menu-star ((t (:foreground ,shmiga-dark-red-1))))

   ;; git-commit
   `(git-commit-comment-action  ((t (:foreground ,shmiga-dark-light :weight bold))))
   `(git-commit-comment-branch  ((t (:foreground ,shmiga-dark-salad :weight bold))))
   `(git-commit-comment-heading ((t (:foreground ,shmiga-dark-cyan-2 :weight bold))))

   ;; js2-mode
   `(js2-function-call ((t (:inherit (font-lock-function-name-face)))))
   `(js2-function-param ((t (:foreground ,shmiga-dark-mono-1))))
   `(js2-jsdoc-tag ((t (:foreground ,shmiga-dark-gold))))
   `(js2-jsdoc-type ((t (:foreground ,shmiga-dark-cyan-2))))
   `(js2-jsdoc-value((t (:foreground ,shmiga-dark-red-1))))
   `(js2-object-property ((t (:foreground ,shmiga-dark-red-1))))

   ;; magit
   `(magit-section-highlight ((t (:background ,shmiga-dark-bg-hl))))
   `(magit-section-heading ((t (:foreground ,shmiga-dark-cyan-2 :weight bold))))
   `(magit-section-heading-selection ((t (:foreground ,shmiga-dark-fg :weight bold))))
   `(magit-diff-file-heading ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,shmiga-dark-gray :weight bold))))
   `(magit-diff-file-heading-selection ((t (:foreground ,shmiga-dark-cyan-2 :background ,shmiga-dark-bg-hl :weight bold))))
   `(magit-diff-hunk-heading ((t (:foreground ,shmiga-dark-mono-2 :background ,shmiga-dark-gray))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,shmiga-dark-mono-1 :background ,shmiga-dark-mono-3))))
   `(magit-diff-hunk-heading-selection ((t (:foreground ,shmiga-dark-gold :background ,shmiga-dark-mono-3))))
   `(magit-diff-context ((t (:foreground ,shmiga-dark-fg))))
   `(magit-diff-context-highlight ((t (:background ,shmiga-dark-bg-1 :foreground ,shmiga-dark-fg))))
   `(magit-diffstat-added ((t (:foreground ,shmiga-dark-light))))
   `(magit-diffstat-removed ((t (:foreground ,shmiga-dark-red-1))))
   `(magit-process-ok ((t (:foreground ,shmiga-dark-light))))
   `(magit-process-ng ((t (:foreground ,shmiga-dark-red-1))))
   `(magit-log-author ((t (:foreground ,shmiga-dark-cyan-2))))
   `(magit-log-date ((t (:foreground ,shmiga-dark-mono-2))))
   `(magit-log-graph ((t (:foreground ,shmiga-dark-silver))))
   `(magit-sequence-pick ((t (:foreground ,shmiga-dark-cyan-2))))
   `(magit-sequence-stop ((t (:foreground ,shmiga-dark-light))))
   `(magit-sequence-part ((t (:foreground ,shmiga-dark-orange-1))))
   `(magit-sequence-head ((t (:foreground ,shmiga-dark-salad))))
   `(magit-sequence-drop ((t (:foreground ,shmiga-dark-red-1))))
   `(magit-sequence-done ((t (:foreground ,shmiga-dark-mono-2))))
   `(magit-sequence-onto ((t (:foreground ,shmiga-dark-mono-2))))
   `(magit-bisect-good ((t (:foreground ,shmiga-dark-light))))
   `(magit-bisect-skip ((t (:foreground ,shmiga-dark-orange-1))))
   `(magit-bisect-bad ((t (:foreground ,shmiga-dark-red-1))))
   `(magit-blame-heading ((t (:background ,shmiga-dark-bg-1 :foreground ,shmiga-dark-mono-2))))
   `(magit-blame-hash ((t (:background ,shmiga-dark-bg-1 :foreground ,shmiga-dark-gold))))
   `(magit-blame-name ((t (:background ,shmiga-dark-bg-1 :foreground ,shmiga-dark-cyan-2))))
   `(magit-blame-date ((t (:background ,shmiga-dark-bg-1 :foreground ,shmiga-dark-mono-3))))
   `(magit-blame-summary ((t (:background ,shmiga-dark-bg-1 :foreground ,shmiga-dark-mono-2))))
   `(magit-dimmed ((t (:foreground ,shmiga-dark-mono-2))))
   `(magit-hash ((t (:foreground ,shmiga-dark-gold))))
   `(magit-tag  ((t (:foreground ,shmiga-dark-orange-1 :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,shmiga-dark-light :weight bold))))
   `(magit-branch-local   ((t (:foreground ,shmiga-dark-salad :weight bold))))
   `(magit-branch-current ((t (:foreground ,shmiga-dark-salad :weight bold :box t))))
   `(magit-head           ((t (:foreground ,shmiga-dark-salad :weight bold))))
   `(magit-refname        ((t (:background ,shmiga-dark-bg :foreground ,shmiga-dark-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,shmiga-dark-bg :foreground ,shmiga-dark-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,shmiga-dark-bg :foreground ,shmiga-dark-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,shmiga-dark-light))))
   `(magit-signature-bad       ((t (:foreground ,shmiga-dark-red-1))))
   `(magit-signature-untrusted ((t (:foreground ,shmiga-dark-orange-1))))
   `(magit-cherry-unmatched    ((t (:foreground ,shmiga-dark-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,shmiga-dark-gold))))
   `(magit-reflog-commit       ((t (:foreground ,shmiga-dark-light))))
   `(magit-reflog-amend        ((t (:foreground ,shmiga-dark-gold))))
   `(magit-reflog-merge        ((t (:foreground ,shmiga-dark-light))))
   `(magit-reflog-checkout     ((t (:foreground ,shmiga-dark-salad))))
   `(magit-reflog-reset        ((t (:foreground ,shmiga-dark-red-1))))
   `(magit-reflog-rebase       ((t (:foreground ,shmiga-dark-gold))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,shmiga-dark-light))))
   `(magit-reflog-remote       ((t (:foreground ,shmiga-dark-cyan))))
   `(magit-reflog-other        ((t (:foreground ,shmiga-dark-cyan))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,shmiga-dark-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,shmiga-dark-gold))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,shmiga-dark-salad))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,shmiga-dark-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,shmiga-dark-light))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,shmiga-dark-orange-1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,shmiga-dark-cyan-2))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,shmiga-dark-red-1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,shmiga-dark-red-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,shmiga-dark-mono-1))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,shmiga-dark-mono-2))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,shmiga-dark-mono-3))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,shmiga-dark-black))))

   ;; rbenv
   `(rbenv-active-ruby-face ((t (:foreground ,shmiga-dark-light))))

   ;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,shmiga-dark-red-1 :background ,shmiga-dark-gray :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,shmiga-dark-gray :weight bold))))

   ;; web-mode
   `(web-mode-symbol-face ((t (:foreground ,shmiga-dark-orange-1))))

   ;; flx-ido
   `(flx-highlight-face ((t (:inherit (link) :weight bold))))

   ;; rpm-spec-mode
   `(rpm-spec-tag-face ((t (:foreground ,shmiga-dark-salad))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground "#FFFFFF" :background ,shmiga-dark-red-2))))
   `(rpm-spec-macro-face ((t (:foreground ,shmiga-dark-cyan-2))))
   `(rpm-spec-var-face ((t (:foreground ,shmiga-dark-red-1))))
   `(rpm-spec-doc-face ((t (:foreground ,shmiga-dark-gold))))
   `(rpm-spec-dir-face ((t (:foreground ,shmiga-dark-cyan))))
   `(rpm-spec-package-face ((t (:foreground ,shmiga-dark-red-2))))
   `(rpm-spec-ghost-face ((t (:foreground ,shmiga-dark-red-2))))
   `(rpm-spec-section-face ((t (:foreground ,shmiga-dark-cyan-2))))

   ;; term
   `(term-color-black ((t :foreground ,shmiga-dark-mono-1)))
   `(term-color-blue ((t (:foreground ,shmiga-dark-salad))))
   `(term-color-cyan ((t :foreground ,shmiga-dark-cyan)))
   `(term-color-green ((t (:foreground ,shmiga-dark-light))))
   `(term-color-magenta ((t :foreground ,shmiga-dark-gold)))
   `(term-color-red ((t :foreground ,shmiga-dark-red-1)))
   `(term-color-white ((t :foreground ,shmiga-dark-fg)))
   `(term-color-yellow ((t (:foreground ,shmiga-dark-orange-1))))

   ;; linum
   `(linum ((t (:foreground ,shmiga-dark-gutter :background ,shmiga-dark-bg))))
   ;; hlinum
   `(linum-highlight-face ((t (:foreground ,shmiga-dark-accent :background ,shmiga-dark-bg))))

   ;; latex-mode
   `(font-latex-sectioning-0-face ((t (:foreground ,shmiga-dark-salad :height 1.0))))
   `(font-latex-sectioning-1-face ((t (:foreground ,shmiga-dark-salad :height 1.0))))
   `(font-latex-sectioning-2-face ((t (:foreground ,shmiga-dark-salad :height 1.0))))
   `(font-latex-sectioning-3-face ((t (:foreground ,shmiga-dark-salad :height 1.0))))
   `(font-latex-sectioning-4-face ((t (:foreground ,shmiga-dark-salad :height 1.0))))
   `(font-latex-sectioning-5-face ((t (:foreground ,shmiga-dark-salad :height 1.0))))
   `(font-latex-bold-face ((t (:foreground ,shmiga-dark-light :weight bold))))
   `(font-latex-italic-face ((t (:foreground ,shmiga-dark-light))))
   `(font-latex-warning-face ((t (:foreground ,shmiga-dark-red-1))))
   `(font-latex-doctex-preprocessor-face ((t (:foreground ,shmiga-dark-cyan))))

   ;; org-mode
   `(org-date ((t (:foreground ,shmiga-dark-cyan))))
   `(org-footnote ((t (:foreground ,shmiga-dark-cyan))))
   `(org-sexp-date ((t (:foreground ,shmiga-dark-cyan))))

   ;; undo-tree
   `(undo-tree-visualizer-current-face ((t (:foreground ,shmiga-dark-red-1))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,shmiga-dark-orange-1))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,shmiga-dark-cyan))))

   ;; php-mode
   `(php-variable-name ((t (:foreground ,shmiga-dark-silver))))
   `(php-property-name ((t (:foreground ,shmiga-dark-silver))))
   `(php-variable-sigil ((t (:foreground ,shmiga-dark-silver))))
   `(php-builtin ((t (:foreground ,shmiga-dark-salad))))
   `(php-method-call ((t (:foreground ,shmiga-dark-salad))))
   ))

(shmiga-dark-with-color-variables
  (custom-theme-set-variables
   'shmiga-dark
;;;;; fill-column-indicator
   `(fci-rule-color ,shmiga-dark-gray)
   ))

(defvar shmiga-dark-theme-force-faces-for-mode t
  "If t, shmiga-dark-theme will use Face Remapping to alter the theme faces for
the current buffer based on its mode in an attempt to mimick the Atom One Dark
Theme from Atom.io as best as possible.
The reason this is required is because some modes (html-mode, jyaml-mode, ...)
do not provide the necessary faces to do theming without conflicting with other
modes.
Current modes, and their faces, impacted by this variable:
* js2-mode: font-lock-constant-face, font-lock-doc-face, font-lock-variable-name-face
")

;; Many modes in Emacs do not define their own faces and instead use standard Emacs faces when it comes to theming.
;; That being said, to have a real "Atom One Dark Theme" for Emacs, we need to work around this so that these themes look
;; as much like "Atom One Dark Theme" as possible.  This means using per-buffer faces via "Face Remapping":
;;
;;   http://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Remapping.html
;;
;; Of course, this might be confusing to some when in one mode they see keywords highlighted in one face and in another
;; mode they see a different face.  That being said, you can set the `shmiga-dark-theme-force-faces-for-mode` variable to
;; `nil` to disable this feature.
(defun shmiga-dark-theme-change-faces-for-mode ()
  (interactive)
  (and (eq shmiga-dark-theme-force-faces-for-mode t)
       (cond
        ((member major-mode '(js2-mode))
         ;; shmiga-dark-orange-1
         (face-remap-add-relative 'font-lock-constant-face :foreground "#D19A66")
         (face-remap-add-relative 'font-lock-doc-face '(:inherit (font-lock-comment-face)))
         ;; shmiga-dark-mono-1
         (face-remap-add-relative 'font-lock-variable-name-face :foreground "#ABB2BF"))
        )))

(add-hook 'after-change-major-mode-hook 'shmiga-dark-theme-change-faces-for-mode)

;;;###autoload
(and load-file-name
    (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory
                  (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'shmiga-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; shmiga-dark-theme.el ends here
