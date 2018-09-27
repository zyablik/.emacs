(package-initialize)

; enable melpa
(add-to-list 'package-archives
    '("melpa" . "http://melpa.milkbox.net/packages/") t)

; enable marmalade
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t

;(add-to-list 'load-path "~/.emacs.d")

; --------------------------------- autogen ---------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-use-notify t)
 '(ccls-sem-function-colors
 (quote
  ("#b58000" "#624724" "#bb6800" "#b28f5f" "#a64be7" "#5834ee" "#b48923" "#733400" "#825800" "#a66825")))
 '(ccls-sem-macro-colors
 (quote
  ("#b76400" "#95070d" "#b87242" "#a81e00" "#764215" "#b24a03" "#6b1a01" "#8639ee" "#b24a41" "#9f3d19")))
 '(ccls-sem-namespace-colors
 (quote
  ("#126800" "#289174" "#2e9618" "#06512b" "#43861d" "#114a00" "#139c41" "#4e8739" "#288f59" "#0e6f1a")))
 '(ccls-sem-parameter-colors
 (quote
  ("##126800" "#289174" "#1e8608" "#06512b" "#43861d" "#114a00" "#139c41" "#4e8739" "#288f59" "#0e6f1a")))
 '(ccls-sem-type-colors
 (quote
  ("#b17f93" "#a5038b" "#6b374f" "#b32086" "#701330" "#ad528c" "#ae0834" "#7d0f57" "#ad4a60" "#b0135a")))
 '(custom-enabled-themes (quote (tsdh-light)))
 '(imenu-auto-rescan t)
 '(lsp-imenu-show-container-name nil)
 '(package-selected-packages
 (quote
  (undo-tree evil-nerd-commenter back-button buffer-move ido-vertical-mode imenu-list ggtags yasnippet-classic-snippets yasnippet company-lsp company rainbow-mode avy ccls lsp-ui ivy-xref visual-regexp-steroids visual-regexp function-args ivy-hydra counsel bury-successful-compilation multiple-cursors cmake-font-lock popup-kill-ring hl-anything hl-todo clean-aindent-mode bm flx-ido hlinum ibuffer-projectile iedit smex projectile projectile-speedbar sr-speedbar))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 97 :width normal :foundry "PfEd" :family "Hack"))))
 '(diff-added ((t (:background "white" :foreground "forest green"))))
 '(diff-removed ((t (:background "white" :foreground "orange red"))))
 '(diredp-compressed-file-suffix ((t (:foreground "#7b68ee"))))
 '(diredp-ignored-file-name ((t (:foreground "#aaaaaa"))))
 '(lsp-ui-sideline-code-action ((t (:foreground "orange")))))

; smex for lru in counsel-M-x

; --------------------------------- hooks ---------------------------------------

(defun all-modes-hook()
    ; consider _ and - as part of the word (while selection)
    (modify-syntax-entry ?_ "w")
    (modify-syntax-entry ?- "w")
    (modify-syntax-entry ?+ "w")
    (hl-highlight-mode) ; enable S-<f8>
)

(add-hook 'prog-mode-hook
    (lambda()
        (all-modes-hook)
        (linum-mode)
        (hlinum-activate)
;        (dtrt-indent-mode 1)
        (setq show-trailing-whitespace t)))

(add-hook 'special-mode-hook 'hl-line-mode)

(add-hook 'text-mode-hook
    (lambda()
        (hl-line-mode)
        (all-modes-hook)
 ))

(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)

(add-hook 'c-mode-common-hook (lambda ()
        (when (derived-mode-p 'c-mode 'c++-mode)
              (ggtags-mode 1))))

;----------------------------------don't bother me ---------------------------------

; do not display the splash screen
(setq inhibit-startup-screen t)

; y instead of yes
(fset 'yes-or-no-p 'y-or-n-p)

; dont ask to kill a buffer with a live process attached to it
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

(setq confirm-nonexistent-file-or-buffer nil)

; disable backup files
(setq make-backup-files        nil)

; disable .saves files
(setq auto-save-list-file-name nil)

; disable auto saving
(setq auto-save-default        nil)

; hide toolbar
(setq tool-bar-mode nil)

; just load .dir-locals.el
(setq enable-local-variables :all)

; --------------------------------- ccls ---------------------------------------

(setq ccls-executable "~/ccls/Release/ccls")
;(setq ccls-extra-args '("-log-file=../../ccls.log"))
(setq ccls-cache-dir ".ccls-cache")

(add-hook 'after-init-hook 'global-company-mode)
(require 'company-lsp)

;(setq lsp-ui-doc-include-signature nil)  ; don't include type signature in the child frame
(setq lsp-ui-sideline-show-symbol nil)  ; don't show symbol on the right of info

; https://github.com/MaskRay/ccls/blob/master/src/config.h

(setq ccls-extra-init-params '(
                               :index (:comments 2)
                               :completion (:detailedLabel t :caseSensitivity 0)))

(setq company-show-numbers t)

; required for company-lsp func arguments snippets
(yas-global-mode 1)

; No delay in showing suggestions.
(setq company-idle-delay 0.1)

; Show suggestions after entering one character.
(setq company-minimum-prefix-length 1)

(setq company-selection-wrap-around t)
(setq company-dabbrev- nil)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case nil)
(setq company-dabbrev-minimum-length 1)
(setq company-dabbrev-other-buffers nil)

(defun enbale-ccls-and-stuff ()
    (interactive)
    (message "enbale-ccls-and-stuff")
    (semantic-mode -1)
    (ggtags-mode -1)
    (lsp-ccls-enable)
    (flycheck-mode)
    (ccls-use-default-rainbow-sem-highlight)
    (set (make-local-variable 'company-backends) '((company-dabbrev company-lsp)))
    (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)) ; disabling client-side cache and sorting because the ccls server does a better job.

(add-hook 'lsp-mode-hook 'lsp-ui-mode)

;(add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

; do not show extra prompt when looking for references (M-.)
(setq xref-prompt-for-identifier (append xref-prompt-for-identifier (list 'xref-find-references)))

(setq ccls-sem-highlight-method 'font-lock)

; https://github.com/MaskRay/Config/blob/master/home/.config/doom/modules/private/my-cc/autoload.el

(defun ccls/base () (interactive) (lsp-ui-peek-find-custom 'base "$ccls/base"))
(defun ccls/callers () (interactive) (lsp-ui-peek-find-custom 'callers "$ccls/callers"))
;(defun ccls/callers () (interactive) (lsp-find-custom "$ccls/callers"))

(defun ccls/vars (kind) (lsp-find-custom "$ccls/vars" (plist-put (lsp--text-document-position-params) :kind kind)))
(defun ccls/vars_variable()  (interactive) (ccls/vars 3)) ;; field or local variable
(defun ccls/vars_field()     (interactive) (ccls/vars 1)) ;; field
(defun ccls/vars_parameter() (interactive) (ccls/vars 4)) ;; parameter

(defun ccls/bases ()
  (interactive)
  (lsp-ui-peek-find-custom 'base "$ccls/inheritanceHierarchy"
                           (append (lsp--text-document-position-params) '(:flat t :level 3))))
(defun ccls/derived ()
  (interactive)
  (lsp-ui-peek-find-custom 'derived "$ccls/inheritanceHierarchy"
                           (append (lsp--text-document-position-params) '(:flat t :level 3 :derived t))))
(defun ccls/members ()
  (interactive)
  (lsp-ui-peek-find-custom 'base "$ccls/memberHierarchy"
(append (lsp--text-document-position-params) '(:flat t))))

;; The meaning of :role corresponds to https://github.com/maskray/ccls/blob/master/src/symbol.h

;; References w/ Role::Address bit (e.g. variables explicitly being taken addresses)
(defun ccls/references-address ()
  (interactive)
  (lsp-ui-peek-find-custom
   'address "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context
              '(:role 128))))

;; References w/ Role::Dynamic bit (macro expansions)
(defun ccls/references-macro ()
  (interactive)
  (lsp-ui-peek-find-custom
   'address "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context '(:role 64))))

;; References w/o Role::Call bit (e.g. where functions are taken addresses)
(defun ccls/references-not-call ()
  (interactive)
  (lsp-ui-peek-find-custom
   'address "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context '(:excludeRole 32))))

;; References w/ Role::Read
(defun ccls/references-read ()
  (interactive)
  (lsp-ui-peek-find-custom
   'read "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context '(:role 8))))

;; References w/ Role::Write
(defun ccls/references-write ()
  (interactive)
  (lsp-ui-peek-find-custom
   'write "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context '(:role 16))))

;; xref-find-apropos (workspace/symbol), match highlighted

(defun my/highlight-pattern-in-text (pattern line)
  (when (> (length pattern) 0)
    (let ((i 0))
     (while (string-match pattern line i)
       (setq i (match-end 0))
       (add-face-text-property (match-beginning 0) (match-end 0) 'isearch t line)
       )
     line)))

(with-eval-after-load 'lsp-methods
  ;;; Override
  ;; This deviated from the original in that it highlights pattern appeared in symbol
  (defun lsp--symbol-information-to-xref (pattern symbol)
   "Return a `xref-item' from SYMBOL information."
   (let* ((location (gethash "location" symbol))
          (uri (gethash "uri" location))
          (range (gethash "range" location))
          (start (gethash "start" range))
          (name (gethash "name" symbol)))
     (xref-make (format "[%s] %s"
                        (alist-get (gethash "kind" symbol) lsp--symbol-kind)
                        (my/highlight-pattern-in-text (regexp-quote pattern) name))
                (xref-make-file-location (string-remove-prefix "file://" uri)
                                         (1+ (gethash "line" start))
                                         (gethash "character" start)))))

  (cl-defmethod xref-backend-apropos ((_backend (eql xref-lsp)) pattern)
    (let ((symbols (lsp--send-request (lsp--make-request
                                       "workspace/symbol"
                                       `(:query ,pattern)))))
      (mapcar (lambda (x) (lsp--symbol-information-to-xref pattern x)) symbols)))
)

; /usr/share/emacs/25.2/etc/images/
; ccls-member-hierarchy times out (
(tool-bar-add-item "mail/outbox" (lambda() (interactive) (ccls-call-hierarchy nil)) 'callers-hier)
(tool-bar-add-item "mail/inbox"  (lambda() (interactive) (ccls-call-hierarchy t))   'callees-hier)

(tool-bar-add-item "ezimage/box-minus"  (lambda() (interactive) (ccls-inheritance-hierarchy nil)) 'inh-base-hier)
(tool-bar-add-item "ezimage/box-minus"  (lambda() (interactive) (ccls-inheritance-hierarchy t))   'inh-derv-hier)

(tool-bar-add-item "gud/down" 'lsp-ui-peek-find-implementation 'find-impl)
; (tool-bar-add-item "gud/down" 'lsp-goto-implementation 'find-impl) ; same via ivy-xref

(tool-bar-add-item "gud/down" 'ccls-member-hierarchy 'member-hier)
(tool-bar-add-item "copy" 'lsp-rename 'rename)
(tool-bar-add-item "newsticker/narrow" 'ccls-preprocess-file 'preprocess)

(global-set-key (kbd "M-<kp-8>") (lambda() (interactive) (ccls-navigate "U")))
(global-set-key (kbd "M-<kp-5>") (lambda() (interactive) (ccls-navigate "D")))
(global-set-key (kbd "M-<kp-4>") (lambda() (interactive) (ccls-navigate "L")))
(global-set-key (kbd "M-<kp-6>") (lambda() (interactive) (ccls-navigate "R")))

; --------------------------------- ibuffer ---------------------------------------

;Unless you turn this variable on you will be prompted every time you want to delete a buffer, even unmodified ones
(setq ibuffer-expert t)

(setq ibuffer-show-empty-filter-groups nil)

; --------------------------------- projectile ---------------------------------------

(projectile-mode +1)
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)
(setq projectile-track-known-projects-automatically nil)
(setq projectile-verbose t)
(setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
(setq projectile-git-command "find . -type f -not -path '*/.git/*' -not -path '*/.hg/*' -not -path '*.pyc' -not -path '*/.ccls-cache/*' -print0")

(setq grep-find-template "find <D> <X> -type f <F> -exec grep <C> -I -C1  -n -e <R> /dev/null {} +")

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


(add-hook 'ibuffer-mode-hook
    (lambda()
        (ibuffer-auto-mode 1); ibuffer-auto-mode is a minor mode that automatically keeps the buffer list up to date
        (setq ibuffer-filter-groups ; add some default groups and projectile
            (append
                    (quote (
                        ("dired" (mode . dired-mode))
                        ("emacs" (or
                            (name . "^\\*scratch\\*$")
                            (name . "^\\*Messages\\*$")
                            (name . "^.emacs$")))))
                    (ibuffer-projectile-generate-filter-groups))) ; add projectile based filters to ibuffer
))

(setq ibuffer-projectile-prefix "")

; --------------------------------- highlights ---------------------------------------

; highlight matching parenthesis
(setq show-paren-style 'parenthesis)
(show-paren-mode 1)

; highlight current line number
(require 'hlinum)

; highlight current line
;(global-hl-line-mode 1)

; --------------------------------- speedbar ---------------------------------------

(setq sr-speedbar-auto-refresh t)
(setq sr-speedbar-delete-windows t)
(setq speedbar-show-unknown-files t)
(setq sr-speedbar-right-side nil)

; --------------------------------- ido ---------------------------------------

(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)

(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

; better heuristics and highlights for flex matching for ido
(flx-ido-mode 1)

; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;(setq flx-ido-threshold 10000)

; --------------------------------- recentf ---------------------------------------

(recentf-mode t)
(setq recentf-max-saved-items 50)

; --------------------------------- bookmarks ---------------------------------------

(setq bm-restore-repository-on-load t)
(setq bm-repository-file "~/.emacs.d/bm-repository")

(require 'bm)

(setq bm-highlight-style 'bm-highlight-only-fringe)

;; make bookmarks persistent as default
(setq-default bm-buffer-persistence t)

;; Loading the repository from file when on start up.
(add-hook' after-init-hook 'bm-repository-load)

;; Restoring bookmarks when on file find.
(add-hook 'find-file-hooks 'bm-buffer-restore)

;; Saving bookmark data on killing a buffer
(add-hook 'kill-buffer-hook 'bm-buffer-save)

;; Saving the repository to file when on exit.
;; kill-buffer-hook is not called when emacs is killed, so we
;; must save all bookmarks first.
(add-hook 'kill-emacs-hook '(lambda nil
                              (bm-buffer-save-all)
                              (bm-repository-save)))

; --------------------------------- my code ---------------------------------------

(defun duplicate-line()
    (interactive)
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (next-line 1)
    (yank))

(defun toggle-camelcase-underscores ()
  "Toggle between camelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (replace-string "_" " " nil start end)
            (upcase-initials-region start end)
            (replace-string " " "" nil start end)
            (downcase-region start (1+ start))
          )
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))

(defun my-kill-emacs ()
  "save some buffers, then exit unconditionally"
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))

(defun yank-current-word ()
  "replace current word with last entry from kill-ring"
  (interactive)
  (left-word)
  (mark-word)
  (delete-active-region)
  (yank)
)

; skip Ibuffer while switching buffers
(defun my-next-buffer ()
  (interactive)
  (next-buffer)
  (when (string= "*Ibuffer*" (buffer-name))
      (next-buffer)))

(defun my-previous-buffer ()
  (interactive)
  (previous-buffer)
  (when (string= "*Ibuffer*" (buffer-name))
      (previous-buffer)))

; --------------------------------- bindings ---------------------------------------

; replace BufferMenu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x 4 b") 'ibuffer-other-window)

; switch window on M-arrow
(windmove-default-keybindings 'meta)

(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)

(global-set-key (kbd "C-x C-r") 'counsel-recentf)

(global-set-key (kbd "C-z") 'undo)

; global bookmarks
(global-set-key (kbd "C-<f6>") 'bookmark-set)
(global-set-key (kbd "<f6>") 'bookmark-bmenu-list)

; local bookmarks
(global-set-key (kbd "C-<f5>") 'bm-toggle)
(global-set-key (kbd "<f5>")   'bm-next)
(global-set-key (kbd "<S-f5>") 'bm-previous)
(global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)

(global-set-key (kbd "<f9>") 'toggle-camelcase-underscores)

(global-set-key (kbd "S-<f8>") 'hl-highlight-thingatpt-local)

(global-set-key (kbd "<f11>") #'imenu-list-smart-toggle)

(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)
(setq imenu-list-auto-resize t)
(setq imenu-list-focus-after-activation t)

(add-hook 'speedbar-reconfigure-keymaps-hook
    (lambda ()
        (define-key speedbar-mode-map [backspace] 'speedbar-up-directory)
        (define-key speedbar-mode-map [M-up] 'speedbar-restricted-prev)
        (define-key speedbar-mode-map [M-down] 'speedbar-restricted-next)))

(global-set-key (kbd "C-d") 'duplicate-line)

(add-hook 'c-mode-common-hook
    (lambda ()
        (local-set-key (kbd "C-d") #'duplicate-line)))

(global-set-key (kbd "C-<tab>") 'previous-buffer)
(global-set-key (kbd "C-S-<iso-lefttab>") 'next-buffer)

(global-set-key (kbd "C-v") 'yank)

(global-set-key (kbd "M-y") 'popup-kill-ring)

; multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

(global-unset-key (kbd "<insert>"))

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-f") 'swiper)
(global-set-key (kbd "C-S-s") (lambda() (interactive) (swiper (thing-at-point 'symbol))))
(global-set-key (kbd "C-M-s") 'swiper-multi)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

(add-hook 'ivy-mode-hook
    (lambda ()
      (define-key ivy-minibuffer-map (kbd "S-<down>") (lambda() (interactive) (ivy-call) (ivy-next-line)))
      (define-key ivy-minibuffer-map (kbd "S-<up>") (lambda() (interactive) (ivy-call) (ivy-previous-line)))
      (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-alt-done)))

(global-set-key (kbd "M-%") 'vr/query-replace)

(global-set-key (kbd "C-:") 'avy-goto-char-2)

(global-set-key (kbd "C-x C-c") 'my-kill-emacs)

(global-set-key (kbd "C-M-<insert>") (lambda() (interactive) (kill-new (thing-at-point 'symbol)))) ; copy current word
(global-set-key (kbd "S-M-<insert>") 'yank-current-word)

(global-set-key (kbd "C-c C-<up>")     'buf-move-up)
(global-set-key (kbd "C-c C-<down>")   'buf-move-down)
(global-set-key (kbd "C-c C-<left>")   'buf-move-left)
(global-set-key (kbd "C-c C-<right>")  'buf-move-right)

(global-set-key (kbd "C-S-x C-<left>") 'my-next-buffer)
(global-set-key (kbd "C-S-x C-<right>") 'my-previous-buffer)

(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)

(global-set-key (kbd "<escape> <escape>") 'keyboard-escape-quit)

; --------------------------------- ivy ---------------------------------------

(ivy-mode 1)

(setq ivy-height 20)

; enable fuzzy matching for all except search
(setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t      . ivy--regex-fuzzy)))

(setq ivy-wrap t)

(setq ivy-count-format "(%d/%d) ")

; auto set calling to before ivy-xref-show-xrefs and off after. catch C-g to ensure off
(defun my-ivy-xref-show-xrefs (XREFS ALIST)
  (interactive)
  (let ((inhibit-quit t))
      (setq-default ivy-calling t)
      (with-local-quit
          (ivy-xref-show-xrefs XREFS ALIST))
      (setq-default ivy-calling nil)
      (setq quit-flag nil)))

(setq xref-show-xrefs-function #'my-ivy-xref-show-xrefs)

; --------------------------------- misc ---------------------------------------

(delete-selection-mode)

(setq linum-format "%d ")

; enable multicursors
(require 'iedit)

; auto-reload changed outside files
(global-auto-revert-mode t)

; scroll up/down to bounds
(setq scroll-error-top-bottom t)

(setq-default cursor-type 'bar)

; hide details in dired by defalt
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

; save session on quit
(desktop-save-mode t)

(setq mouse-wheel-progressive-speed nil)

(bury-successful-compilation 1)

; function-arguments for moo-jump-local
(fa-config-default)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

; enable only for ccls
(global-flycheck-mode -1)

(back-button-mode 1)

; force vr-steroids to override orig vr
(require 'visual-regexp-steroids)

(which-function-mode 1)
(setq which-func-unknown "∅")

(setq mode-line-format (delete (assoc 'which-func-mode
                                      mode-line-format) mode-line-format)
      which-func-header-line-format '(which-func-mode ("" which-func-format)))

 (defadvice which-func-ff-hook (after header-line activate)
   (when which-func-mode
     (setq mode-line-format (delete (assoc 'which-func-mode
                                           mode-line-format) mode-line-format)
           header-line-format which-func-header-line-format)))

(global-undo-tree-mode)
(setq undo-tree-visualizer-diff t)

; --------------------------------- indentation ---------------------------------------

; disable auto indentation
(electric-indent-mode -1)

(setq default-tab-width 4)
(setq c-basic-offset 4)

; do not use tabs for indentation
(setq-default indent-tabs-mode nil)

(clean-aindent-mode t)
(setq clean-aindent-is-simple-indent t)

(global-set-key (kbd "RET") 'newline-and-indent)

