(package-initialize)

; enable melpa
(add-to-list 'package-archives
    '("melpa" . "http://melpa.milkbox.net/packages/") t)

; enable marmalade
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'load-path (format "%s/dotemacs" (getenv "HOME")))

; --------------------------------- autogen ---------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-use-notify t)
 '(company-frontends '(company-preview-common-frontend company-pseudo-tooltip-unless-just-one-frontend-with-delay))
 '(anzu-mode-lighter "")
 '(anzu-replace-threshold 100)
 '(anzu-replace-to-string-separator " => ")
 '(company-backends '((company-dabbrev company-yasnippet)))
 '(custom-enabled-themes '(tsdh-light))
 '(lsp-imenu-show-container-name nil)
 '(package-selected-packages
 ; smex for lru in counsel-M-x
 '(symbol-overlay anzu ivy-posframe cmake-font-lock flycheck smartrep visible-mark qt-pro-mode expand-region json-mode qml-mode dockerfile-mode yaml-mode easy-kill buffer-flip git-gutter-fringe git-gutter clang-format undo-tree evil-nerd-commenter back-button buffer-move ido-vertical-mode imenu-list ggtags company-lsp company rainbow-mode avy ccls lsp-ui ivy-xref function-args ivy-hydra counsel bury-successful-compilation multiple-cursors popup-kill-ring hl-todo clean-aindent-mode bm flx-ido ibuffer-projectile iedit smex projectile projectile-speedbar sr-speedbar))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "SRC" :slant normal :weight normal :height 99 :width normal))))
 '(diff-added ((t (:background "white" :foreground "forest green"))))
 '(diff-removed ((t (:background "white" :foreground "orange red"))))
 '(diredp-compressed-file-suffix ((t (:foreground "#7b68ee"))))
 '(diredp-ignored-file-name ((t (:foreground "#aaaaaa"))))
 '(line-number-current-line ((t (:background "dark gray" :foreground "gainsboro"))))
 '(lsp-ui-sideline-code-action ((t (:foreground "orange")))))

; --------------------------------- hooks ---------------------------------------

;; (setq debug-on-error t)

(global-superword-mode t)

(defun all-modes-hook()
    (make-local-variable 'company-backends)
    (make-local-variable 'company-frontends)
)

(add-hook 'prog-mode-hook
    (lambda()
        (all-modes-hook)
        (hs-minor-mode)
        (auto-mark-mode t) ; enable it only for prog modes for performance reasons
;        (dtrt-indent-mode 1)
        (setq show-trailing-whitespace t)))

(add-hook 'text-mode-hook
    (lambda()
        (all-modes-hook)
 ))

(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)

(add-hook 'c-mode-common-hook (lambda ()
        (when (derived-mode-p 'c-mode 'c++-mode)
              (setq ggtags-highlight-tag nil) ; workaround for "iedit fails when both gtags and git-gutter active"
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
;; (setq ccls-extra-args (list (format "--log-file=%s/ccls.log" (getenv "HOME"))))
(setq ccls-cache-dir (format "%s/.ccls-cache" (getenv "HOME")))

(add-hook 'after-init-hook 'global-company-mode)

(require 'company-lsp)

(setq ccls-sem-function-colors  '("#b58000" "#624724" "#bb6800" "#b28f5f" "#a64be7" "#5834ee" "#b48923" "#733400" "#825800" "#a66825"))
(setq ccls-sem-macro-colors     '("#b76400" "#95070d" "#b87242" "#a81e00" "#764215" "#b24a03" "#6b1a01" "#8639ee" "#b24a41" "#9f3d19"))
(setq ccls-sem-namespace-colors '("#126800" "#289174" "#2e9618" "#06512b" "#43861d" "#114a00" "#139c41" "#4e8739" "#288f59" "#0e6f1a"))
(setq ccls-sem-parameter-colors '("#126800" "#289174" "#1e8608" "#06512b" "#43861d" "#114a00" "#139c41" "#4e8739" "#288f59" "#0e6f1a"))
(setq ccls-sem-type-colors      '("#b17f93" "#a5038b" "#6b374f" "#b32086" "#701330" "#ad528c" "#ae0834" "#7d0f57" "#ad4a60" "#b0135a"))

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
(setq company-idle-delay 0.0)

; show suggestions after entering two characters
(setq company-minimum-prefix-length 2)

(setq company-selection-wrap-around t)

(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case nil)
(setq company-dabbrev-minimum-length 1)
(setq company-dabbrev-other-buffers nil) ; show only abbrevs from current buffer

(setq company-require-match 'never) ; non-completion character closes tooltip
(setq company-begin-commands '(self-insert-command yank)) ; begin completion after yank
(setq company-tooltip-idle-delay 1.0) ; company-pseudo-tooltip-unless-just-one-frontend-with-delay
(setq company-tooltip-limit 15)

(defun company-complete-with-backends (backends)
       (interactive)
       (set (make-local-variable 'old-frontends) company-frontends)
       (set (make-local-variable 'old-backends) company-backends)

       (company-cancel)

       (defun restore-company-ends(arg)
         (setq company-frontends old-frontends)
         (setq company-backends old-backends)
         (setq company-completion-cancelled-hook nil)
         (setq company-completion-finished-hook nil)
         (define-key company-active-map (kbd "<up>") nil)
         (define-key company-active-map (kbd "<down>") nil))

       (setq company-completion-cancelled-hook 'restore-company-ends)
       (setq company-completion-finished-hook 'restore-company-ends)

       (setq company-frontends '(company-pseudo-tooltip-frontend))
       (setq company-backends backends)

       (define-key company-active-map (kbd "<up>") 'company-select-previous-or-abort)
       (define-key company-active-map (kbd "<down>") 'company-select-next-or-abort)

       (company-manual-begin))

(advice-add 'select-window :after
    (lambda (_ &optional _)
        (if (bound-and-true-p lsp-ui-mode)
            (if (< (window-pixel-width) 1200)
                (progn
                  ;; (message "disable sideline")
                  (lsp-ui-sideline-enable nil))
              (progn
                ;; (message "enable sideline")
                (lsp-ui-sideline-enable t))))))

(defun enable-ccls-and-stuff ()
    (interactive)
    (message "enable-ccls-and-stuff")
    (semantic-mode -1)
    (ggtags-mode -1)
    (lsp-ccls-enable)
    (flycheck-mode)
    (ccls-use-default-rainbow-sem-highlight)
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

(global-set-key (kbd "C-SPC") (lambda () (interactive) (company-complete-with-backends '((company-gtags company-elisp company-lsp)))))

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

(define-key projectile-mode-map (kbd "C-p") 'projectile-command-map)


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

(defun my-forward-symbol ()
    (interactive "^") ; ^ is for handle-shift-selection()
    (if (eq (char-after) ?\C-j)
        (forward-char 1)
        (re-search-forward "\\(\\sw\\|\\s_\\)+\\|.$" nil 'move 1)))

(defun my-backward-symbol ()
    (interactive "^") ; ^ is for handle-shift-selection()
    (if (eq (char-before) ?\C-j)
        (forward-char -1)
        (if (re-search-backward "\\(\\sw\\|\\s_\\)+\\|^." nil 'move)
	    (skip-syntax-backward "w_"))))

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
(global-set-key (kbd "S-<f5>") 'bm-previous)
(global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)

(global-set-key (kbd "<f9>") 'toggle-camelcase-underscores)

(global-set-key (kbd "<f8>") 'symbol-overlay-put)
(global-set-key (kbd "s-<f8>") 'symbol-overlay-remove-all)

(global-set-key (kbd "<f11>") #'imenu-list-smart-toggle)

(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)
(setq imenu-list-auto-resize t)
(setq imenu-list-focus-after-activation t)

(add-hook 'speedbar-reconfigure-keymaps-hook
    (lambda ()
        (define-key speedbar-mode-map [backspace] 'speedbar-up-directory)
        (define-key speedbar-mode-map [M-up] 'speedbar-restricted-prev)
        (define-key speedbar-mode-map [M-down] 'speedbar-restricted-next)))

(global-set-key (kbd "s-d") 'duplicate-line)

(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "M-y") 'counsel-yank-pop)

; multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

(global-unset-key (kbd "<insert>"))

(global-set-key (kbd "C-s") 'save-buffer)

(global-set-key (kbd "C-f") 'swiper)
(global-set-key (kbd "C-S-f") (lambda() (interactive) (swiper (thing-at-point 'symbol))))
(global-set-key (kbd "s-f") 'swiper-all)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") (lambda() (interactive) (let ((ivy-extra-directories '()))(counsel-find-file)))) ; hide . and ..
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-c C-r") 'ivy-resume) ; Recalls the state of the completion session just before its last exit. Useful after an accidental C-m (ivy-done). 
(global-set-key (kbd "C-c g") 'counsel-rg)

(add-hook 'ivy-mode-hook
    (lambda ()
      (define-key ivy-minibuffer-map (kbd "S-<down>") (lambda() (interactive) (ivy-call) (ivy-next-line)))
      (define-key ivy-minibuffer-map (kbd "S-<up>") (lambda() (interactive) (ivy-call) (ivy-previous-line)))
      (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)
      (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-alt-done)))

(global-set-key (kbd "M-%") 'anzu-query-replace-regexp)

(global-set-key (kbd "C-:") 'avy-goto-char-2)

(global-set-key (kbd "C-x C-c") 'my-kill-emacs)

(global-set-key (kbd "C-M-<insert>") (lambda() (interactive) (kill-new (thing-at-point 'symbol)))) ; copy current word
(global-set-key (kbd "S-M-<insert>") 'yank-current-word)

(global-set-key (kbd "C-c C-<up>")     'buf-move-up)
(global-set-key (kbd "C-c C-<down>")   'buf-move-down)
(global-set-key (kbd "C-c C-<left>")   'buf-move-left)
(global-set-key (kbd "C-c C-<right>")  'buf-move-right)

(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)

(global-set-key (kbd "<escape> <escape>") 'keyboard-escape-quit)

(global-set-key [C-M-tab] 'clang-format-region)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

(global-set-key (kbd "C-<right>") 'my-forward-symbol)
(global-set-key (kbd "C-<left>")  'my-backward-symbol)

(require 'profiler)
(global-set-key (kbd "<f7>") (lambda ()
    (interactive)
    (if (profiler-running-p)
         (progn
            (profiler-report)
            (profiler-stop))
         (profiler-start 'cpu))))

(require 'smartrep)
(smartrep-define-key global-map "C-x"
  '(("n" . git-gutter:next-hunk)
    ("p" . git-gutter:previous-hunk)))


(global-set-key (kbd "s-SPC") 'just-one-space) ; M-SPC occupied by krunner
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

(require 'ivy-posframe)
(setq ivy-posframe-height-alist '((counsel-yank-pop . 20)
                                  (counsel-find-file . 30)
                                  (t      . 20)))

(setq ivy-posframe-display-functions-alist
      '((counsel-find-file . ivy-posframe-display-at-window-center)
        (counsel-yank-pop  . ivy-posframe-display-at-point)
        (t                 . nil)))
(ivy-posframe-mode 1)

; --------------------------------- git-gutter ---------------------------------

(global-git-gutter-mode t)

(set-face-foreground 'git-gutter:modified "purple")
(set-face-foreground 'git-gutter:added "dark green")
(set-face-foreground 'git-gutter:deleted "dark red")

(add-to-list 'git-gutter:update-hooks 'focus-in-hook)

(setq git-gutter:lighter "")

; --------------------------------- which-func ---------------------------------------
(which-function-mode 1)
(setq which-func-unknown "∅")

(setq mode-line-format (delete (assoc 'which-func-mode
                                      mode-line-format) mode-line-format)
      which-func-header-line-format '(which-func-mode ("" which-func-format)))

(defadvice which-func-ff-hook (after header-line activate)
    (when which-func-mode
        (setq mode-line-format
            (delete (assoc 'which-func-mode mode-line-format) mode-line-format)
            header-line-format which-func-header-line-format)))

(global-set-key [remap kill-ring-save] 'easy-kill)

; --------------------------------- easy-kill ---------------------------------------
(require 'easy-kill) ; to define easy-kill-base-map

; override some default prefixes
(setq easy-kill-alist
   '( (?w word " ")
      (?s sexp "")
      (?d defun "")
      (?l line "")
      (?b buffer-file-name nil)))

; add M-w f for current function name
(define-key easy-kill-base-map (kbd "f") (lambda()
    (interactive)
    (easy-kill-adjust-candidate 'which-function (which-function))))

; --------------------------------- back-button --------------------------------
(setq mark-ring-max 32)
(setq global-mark-ring-max 64)

(require 'auto-mark) ; https://www.emacswiki.org/emacs/AutoMark

(setq auto-mark-only-last-mark-on-sameline t)

(setq auto-mark-push-mark 'back-button-push-mark-local-and-global)

(setq auto-mark-command-class-alist
        '((goto-line . jump)
          (avy-goto-char-2 . jump)
          (xref-find-definitions . jump)
          (indent-for-tab-command . ignore)
          (undo-tree-undo . ignore)))

;; ignore whitespace via auto-mark-command-classifiers:
;; (setq auto-mark-command-classifiers
;;       (list (lambda (command)
;;               (if (and (eq command 'self-insert-command)
;;                        (eq last-command-event ? )
;;                        )
;;                   'ignore))))

(global-auto-mark-mode 1)

(setq back-button-index-timeout 5)

(back-button-mode 1)

; https://github.com/kleiba/visual-mark-ring-mode/blob/master/visual-mark-ring-mode.el
; show marks on F8, for debug auto-mark
(autoload 'visual-mark-ring-mode "visual-mark-ring-mode" "" t)
(autoload 'visual-mark-ring-activate "visual-mark-ring-mode" "" t)
(setq visual-mark-ring-overlays nil)
(global-set-key (kbd "S-<f8>") (lambda()
   (interactive)
   (visual-mark-ring-activate)))


; --------------------------------- misc ---------------------------------------

(global-display-line-numbers-mode t)

(delete-selection-mode)

; Put column number into modeline
(column-number-mode 1)

(setq imenu-auto-rescan t)

; default value is 1 sec which is very annoying, cause it makes typing scratchy and jerky
(setq semantic-idle-scheduler-idle-time 10)

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

(global-anzu-mode +1)

(global-undo-tree-mode)
(setq undo-tree-visualizer-diff t)
(setq undo-tree-enable-undo-in-region t)

(global-set-key (kbd "C-<tab>") 'buffer-flip)
(setq buffer-flip-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-<tab>")   'buffer-flip-forward)
        (define-key map (kbd "C-S-<iso-lefttab>") 'buffer-flip-backward)
        (define-key map (kbd "M-ESC")     'buffer-flip-abort)
        map))

(setq buffer-flip-skip-patterns '("^\\*Ibuffer\\*"))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(add-to-list 'auto-mode-alist '("\\.pr[io]$" . qt-pro-mode)) ;; open .pro and .pri files with qt-pro mode

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

