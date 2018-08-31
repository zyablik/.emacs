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
 '(comment-style (quote extra-line))
 '(custom-enabled-themes (quote (tsdh-light)))
 '(delete-selection-mode t)
 '(package-selected-packages
   (quote
    (auto-complete bm flx-ido hlinum ibuffer-projectile ido-vertical-mode iedit smex projectile projectile-speedbar sr-speedbar))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 97 :width normal :foundry "PfEd" :family "Hack")))))

; --------------------------------- hooks ---------------------------------------

(defun all-modes-hook()
    (auto-complete-mode)
    ; consider _ and - as part of the word (while selection)
    (modify-syntax-entry ?_ "w")
    (modify-syntax-entry ?- "w")
)

(add-hook 'prog-mode-hook
    (lambda()
        (all-modes-hook)
        (linum-mode)
        (hlinum-activate)
        (setq show-trailing-whitespace t)))

(add-hook 'special-mode-hook 'hl-line-mode)

(add-hook 'text-mode-hook
    (lambda()
        (all-modes-hook)
        (hl-line-mode)))

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
(setq projectile-git-command "find . -type f -not -path '*/.git/*' -not -path '*/.hg/*' -not -path '*.pyc' -print0")

(setq grep-find-template "find <D> <X> -type f <F> -exec grep <C> -I -C1  -n -e <R> /dev/null {} +")

(define-key projectile-mode-map (kbd "C-c") 'projectile-command-map)

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

(defun toggle-highlight-at-point()
    (interactive)
    (let*(
;              (word-at-point (thing-at-point 'word 'no-propertie))
;              (hi-lockified-word-at-point (concat "\\_<" word-at-point "\\_>"))
          (hi-lockified-word-at-point (find-tag-default-as-symbol-regexp))
          (hi-lock-flattened-patterns (mapcar 'car hi-lock-interactive-patterns))
        )
        (if (member hi-lockified-word-at-point hi-lock-flattened-patterns)
          (unhighlight-regexp hi-lockified-word-at-point)
          (highlight-symbol-at-point))
    )
)

; do not bother me at start
(setq hi-lock-file-patterns-policy (lambda (pattern) t))

; highlight current line number
(require 'hlinum)

; --------------------------------- speedbar ---------------------------------------

(setq sr-speedbar-auto-refresh t)
(setq sr-speedbar-delete-windows t)
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

(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

; --------------------------------- autocomplete ---------------------------------------

; auto-complete
(ac-config-default) ; enable auto-complete
(setq ac-disable-faces nil) ; enable completion inside strings
(setq ac-auto-show-menu nil)
;(defun ac-common-setup ()
;  (setq ac-sources (append ac-sources '(ac-source-filename))))


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

; --------------------------------- bindings ---------------------------------------

; replace BufferMenu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x 4 b") 'ibuffer-other-window)

; smex - ido for M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

; switch window on M-arrow
(windmove-default-keybindings 'meta)

(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

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

(global-set-key (kbd "S-<f8>") 'toggle-highlight-at-point)

(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)

(global-set-key (kbd "C-d") 'duplicate-line)

(add-hook 'c++-mode-hook
    (lambda ()
        (local-set-key (kbd "C-d") #'duplicate-line)))

(global-set-key (kbd "C-<tab>") 'previous-buffer)
(global-set-key (kbd "C-S-<iso-lefttab>") 'next-buffer)

(global-set-key (kbd "M-y") 'popup-kill-ring)
; --------------------------------- misc ---------------------------------------

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

; --------------------------------- indentation ---------------------------------------

; disable auto indentation
(electric-indent-mode -1)

(setq default-tab-width 4)
(setq c-basic-offset 4)

; do not use tabs for indentation
(setq-default indent-tabs-mode nil)
