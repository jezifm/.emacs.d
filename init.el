;;; Initialize Variable

(setq save-abbrevs 'silently)                                         ; abbrev warning
(setq inhibit-startup-message t)                                      ; disable splash
(setq ns-function-modifier 'control)                                  ; map modifier (fn) to control (ctrl)
(setq make-backup-files nil)                                          ; remove backup
(setq auto-save-default nil)                                          ; remove backup
(setq mouse-wheel-progressive-speed nil)                              ; smooth scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))) ; smooth scroll
(setq split-width-threshold 160)                                      ; default split to vertical

(add-to-list 'default-frame-alist '(fullscreen . maximized))          ; maximize window

(defalias 'yes-or-no-p 'y-or-n-p)

;; key bindings unset
(global-unset-key (kbd "C-x C-c"))  ; disable quit
(global-unset-key (kbd "C-x c"))    ; disable quit
(global-unset-key (kbd "C-z"))      ; disable minimize
(global-unset-key (kbd "s-t"))      ; disable font-panel

;; disable prompt
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)

;; core key bindings
(global-set-key (kbd "<f5>") 'sort-lines)
(global-set-key (kbd "C-c C-<return>") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)
(global-set-key (kbd "C-x C-j") (lambda () (interactive) (dired default-directory)))
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal) ; remap quit-key
(global-set-key (kbd "M-i") 'back-to-indentation)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs Version

(when (version< emacs-version "26")
  (error (format "Emacs version %s not supported. Please update to 26 or higher."
                 emacs-version)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Manager - el-get

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Manager - melpa

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Local Dependencies
;;
;; dependencies included in `.emacs.d' but is not within package
;; managers above

(setq settings-dir (expand-file-name "settings" user-emacs-directory))
(add-to-list 'load-path settings-dir)
(autoload 'auto-capitalize-mode "auto-capitalize")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Install missing dependencies

;; package to install
(autoload 'packages-install "setup-package")
(autoload 'package-initialize "package")
(defun init--install-packages ()
  (packages-install
   '(ace-jump-mode
     ace-window
     auctex
     avy
     change-inner
     dockerfile-mode
     ein
     emmet-mode
     exec-path-from-shell
     expand-region
     go-mode
     hackernews
     helm
     helm-ag
     helm-projectile
     hydra
     impatient-mode
     iy-go-to-char
     key-chord
     latex-preview-pane
     lorem-ipsum
     magit
     markdown-mode
     multiple-cursors
     nyan-mode
     ob-go
     ob-http
     ob-ipython
     org-plus-contrib
     paredit
     plantuml-mode
     prodigy
     quelpa
     redis
     restclient
     swift-mode
     swiper
     undo-tree
     use-package
     visual-regexp
     web-beautify
     web-mode
     window-numbering
     yaml-mode
     yasnippet
     yasnippet-snippets
     )))

;; prepare installer
(package-initialize)
(when (not (package-installed-p 'dash))
  (package-refresh-contents)
  (package-install 'dash))

;; install missing packages
(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; load packages
(autoload 'use-package "use-package-core")
(use-package gitignore-mode :ensure t :defer t)
(use-package help-fns+      :defer t  :commands describe-keymap)
(use-package hydra          :ensure t :defer t :commands defhydra)
(use-package treemacs       :ensure t :defer t)
(use-package which-key      :ensure t :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Host Specific Customization
;;
;; persist bookmarks and other customization when switching `.emacs.d'

;; ensure customization directory
(setq custom-host-dir "~/.emacs.d.local")
(unless (file-directory-p custom-host-dir)
  (make-directory custom-host-dir))

;; update variables to use host
(add-to-list 'load-path (expand-file-name "init.el" custom-host-dir))
(setq custom-file (expand-file-name "custom.el" custom-host-dir))
(setq bookmark-default-file (expand-file-name "bookmarks" custom-host-dir))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs core

;; key bindings custom
(global-set-key (kbd "C-z") 'jez/shell-shortcut)
(global-set-key (kbd "M-%") 'vr/query-replace)
(global-set-key (kbd "M-J") 'jez/simplify)
(global-set-key (kbd "M-j") 'jez/join-line)
(global-set-key (kbd "C-c C-d") 'insert-date)
(global-set-key (kbd "C-x |") 'toggle-window-split)

;; Define local functions
(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y-%m-%d")
                 ((equal prefix '(4)) "%Y-%m-%d %H:%M:%S")
                 ((equal prefix '(16)) "%A, %d. %B %Y")))
        (system-time-locale "en_US"))
    (insert (format-time-string format))))

(defun jez/annotate ()
  "Insert annotation for commenting"
  (interactive)
  (insert "--jez ")
  (insert-date nil))

(defun jez/clear-font-properties ()
  "Clear font properties"
  (interactive)
  (let ((inhibit-read-only t))
    (set-text-properties (point-min) (point-max) nil)))

(defun jez/shell-shortcut ()
  "Create shell buffer based on current buffer name"
  (interactive)
  (let* ((buffer-name-current (buffer-name (current-buffer)))
         (buffer-name-shell (format "sh-%s" buffer-name-current)))
    (save-current-buffer
      (shell buffer-name-shell))
    (when (equal (count-windows) 1)
      (split-window-right)
      (other-window 1))
    (switch-to-buffer buffer-name-shell)))

(defun jez/guess-shell-buffer-name ()
  "Return name of buffer with 'sh-' as prefix"
  (interactive)
  (let* ((shell-pattern "^sh-.*")
         (buffers (--map (buffer-name it) (buffer-list)))
         (shell-buffers (--filter (string-match shell-pattern it)
                                  buffers)))
    (car shell-buffers)))

(defun jez/join-line ()
  "Custom join line"
  (interactive)
  (join-line -1))

(defun jez/simplify ()
  "Combine multiline"
  (interactive)
  (back-to-indentation)
  (let ((column (current-column)))
    (while (progn
             (forward-line 1)
             (goto-char (line-beginning-position))
             (skip-chars-forward "[:space:]")
             (if (and (/= (current-column) column)
                      (/= (point) (point-max)))
                 (progn
                   (delete-indentation)
                   t)
               nil)))))

(defun align-repeat (start end regexp)
  "Repeat alignment with respect to
     the given regular expression.
Note: just like `align-regexp' but better"
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun jez/camelize (s)
  "Convert under_score string S to CamelCase string."
  (mapconcat 'identity (mapcar
                        #'(lambda (word) (capitalize (downcase word)))
                        (split-string s "_")) ""))

(defun jez/snake-case ()
  "Convert region to snake case"
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (downcase-region (point-min) (point-max))
      (goto-char (point-min))
      (replace-string " " "_"))))

(defun jez/abbreviate (string &optional separator)
  "Return first letter on each word of STRING using SEPERATOR"
  (interactive "s")
  (let* ((separator (or separator "_")))
    (s-join "" (--map (s-left 1 it) (s-split separator string)))))

(defun jez/clear-buffers ()
  "Clears all buffer except from some default"
  (interactive)
  (let* ((ignore-list '("*scratch*" "*Messages*" "*Pymacs*" "init.el"))
         (buffer-list
          (seq-filter
           '(lambda (buffer) (not (member (buffer-name buffer) ignore-list)))
           (buffer-list))))
    (mapc 'kill-buffer buffer-list)))

(defun jez/insert-current-buffer-name ()
  "Insert name of current buffer"
  (interactive)
  (insert (buffer-name (current-buffer))))

(defun jez/occur (regexp)
  "List lines containing REGEXP on current buffer"
  (interactive "sView lines matching: ")
  (let ((current-buffer (buffer-name))
        (buffer-name (format "*occur*-%s" (buffer-name)))
        (current-window (selected-window)))
    (switch-to-buffer-other-window buffer-name)
    (erase-buffer)
    (insert-buffer current-buffer)
    (keep-lines regexp)
    ;;    (select-window current-window)
    ))

(defun jez/change-theme ()
  "Change current emacs theme without confirmation"
  (interactive)
  (let ((theme (intern (completing-read "Load custom theme: "
                                        (mapcar 'symbol-name
                                                (custom-available-themes))))))
    (load-theme theme t)
    (enable-theme theme)))

(defun xah-copy-file-path (&optional @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not dired, copy value of
`default-directory' (which is usually the “current” dir when that
buffer was created)

URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2017-09-01"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if @dir-path-only-p
         (progn
           (message "Directory path copied: 「%s」" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: 「%s」" $fpath)
         $fpath )))))

(defun jez/outline-mode-adhoc (regex)
  "Enable outline-mode using REGEX as pattern"
  (interactive "sRegex: ")
  (outline-minor-mode t)
  (defun jez/outline-mode-adhoc-level ()
    (skip-chars-forward (rx (not alnum)))
    (current-column))
  (setq-local outline-regexp regex)
  (setq-local outline-level 'jez/outline-mode-adhoc-level))

(defun toggle-window-split ()
  "Change split orientation"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs Built-in Mode

(use-package menu-bar   :defer 2 :config (menu-bar-mode -1))
(use-package scroll-bar :defer 2 :config (scroll-bar-mode -1))
(use-package tool-bar   :defer 2 :config (tool-bar-mode -1))
(use-package elec-pair  :defer 2 :config (electric-pair-mode t))
(use-package paren      :defer 2 :config (show-paren-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs GUI

(use-package zenburn-theme :ensure t :defer t)
(use-package blackboard-theme :ensure t :defer t)
(use-package smart-mode-line :ensure t :defer t)
(use-package smart-mode-line-powerline-theme :ensure t :defer 2)
(add-to-list 'custom-theme-load-path (expand-file-name "custom-themes/emacs-darkane-theme/" user-emacs-directory))
(use-package custom
  :defer 2
  :config
  ;; main-theme
  (load-theme 'blackboard t t)
  (enable-theme 'blackboard)
  ;; mode-line
  (require 'smart-mode-line)
  (require 'smart-mode-line-powerline-theme)
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'powerline)
  (sml/setup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window Numbering

(use-package window-numbering
  :ensure t
  :defer t
  :bind (("M-1" . select-window-1)
         ("M-2" . select-window-2)
         ("M-0" . hydra-window-stuff/body))
  :commands window-numbering-remove-keymap
  :config
  (window-numbering-mode 1)
  (define-key window-numbering-keymap (kbd "M-0") nil)

  (defun window-numbering-remove-keymap ()
    "Fix keymap conflict with `magit'"
    (interactive)
    (mapc
     (lambda (num) (define-key magit-mode-map (kbd (format "M-%s" num)) nil))
     (number-sequence 1 5)))

  (defhydra hydra-window-stuff (:hint nil)
    "
          Split: _v_ert  _s_:horz
         Delete: _c_lose  _o_nly
  Switch Window: _h_:left  _j_:down  _k_:up  _l_:right
        Buffers: _p_revious  _n_ext  _b_:select  _f_ind-file  _F_projectile
         Winner: _u_ndo  _r_edo
         Resize: _H_:splitter left  _J_:splitter down  _K_:splitter up  _L_:splitter right
           Move: _a_:up  _z_:down  _i_menu"


    ("z" scroll-up-line)
    ("a" scroll-down-line)
    ("i" idomenu)

    ("u" winner-undo)
    ("r" winner-redo)

    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)

    ("p" previous-buffer)
    ("n" next-buffer)
    ("b" ido-switch-buffer)
    ("f" ido-find-file)
    ("F" projectile-find-file)

    ("s" split-window-below)
    ("v" split-window-right)

    ("c" delete-window)
    ("o" delete-other-windows)

    ("H" hydra-move-splitter-left)
    ("J" hydra-move-splitter-down)
    ("K" hydra-move-splitter-up)
    ("L" hydra-move-splitter-right)

    ("q" nil))

  :hook (magit-mode . window-numbering-remove-keymap))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Mode

(use-package org
  :ensure t
  :defer t
  :bind ("C-c l" . org-store-link)
  :config
  (setq user-full-name "Jezrael Arciaga")
  (setq user-mail-address "jezarciaga@gmail.com")
  (setq org-use-speed-commands t)
  (setq org-export-coding-system 'utf-8)
  (setq org-log-done 'time)
  (setq org-src-fontify-natively t)
  (setq org-todo-keywords '((sequence "TODO" "STARTED" "WAITING" "|" "DONE")))
  (setq org-todo-keyword-faces
        '(("TODO" . "red")
          ("STARTED" . "orange")
          ("WAITING" . "yellow")
          ("DONE" . "green")))
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-languages
        '((ditaa . t)
          (dot . t)
          (emacs-lisp . t)
          (gnuplot . t)
          (go . t)
          (http . t)
          (ipython . t)
          (js . t)
          (plantuml . t)
          (python . t)
          (shell . t)
          (sql . t)
          ))
  (when (string-equal system-type "windows-nt")
    (assq-delete-all 'sh org-babel-languages))
  (when (file-exists-p "~/.emacs.d/publish-settings.el")
    (load-file "~/.emacs.d/publish-settings.el"))

  (defun org-babel-temp-file (prefix &optional suffix)
    "Create a temporary file in the `org-babel-temporary-directory'.
Passes PREFIX and SUFFIX directly to `make-temp-file' with the
value of `temporary-file-directory' temporarily set to the value
of `org-babel-temporary-directory'."
    (if (file-remote-p default-directory)
        (let ((prefix
               ;; We cannot use `temporary-file-directory' as local part
               ;; on the remote host, because it might be another OS
               ;; there.  So we assume "/tmp", which ought to exist on
               ;; relevant architectures.
               (concat (file-remote-p default-directory)
                       ;; REPLACE temporary-file-directory with /tmp:
                       (expand-file-name prefix "/tmp/"))))
          (make-temp-file prefix nil suffix))
      (let ((temporary-file-directory
             (or (and (boundp 'org-babel-temporary-directory)
                      (file-exists-p org-babel-temporary-directory)
                      org-babel-temporary-directory)
                 temporary-file-directory)))
        (make-temp-file prefix nil suffix))))

  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (defun shk-fix-inline-images ()
    "Render images after executing org code"
    (interactive)
    (when org-inline-image-overlays
      (org-redisplay-inline-images)))

  (defun org-babel-execute:yaml (body params) body)

  (defun jez/org-disable-font-theme ()
    "Remove font changes from current theme"
    (interactive)
    (dolist (face '(org-level-1
		    org-level-2
		    org-level-3
		    org-level-4
		    org-level-5))
      (set-face-attribute face nil :weight 'normal :height 1.0)))

  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  (advice-add 'org-latex--inline-image :around
              (lambda (orig link info)
                (concat
                 "\\begin{center}"
                 (funcall orig link info)
                 "\\end{center}")))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-languages)
  :hook ((org-after-todo-statistics . org-summary-todo)
         (org-babel-after-execute . shk-fix-inline-images)
         (org-mode . auto-fill-mode)
         (org-mode . jez/org-disable-font-theme)))

(use-package org-capture
  :bind (("C-c c" . org-capture)
         ("C-c o" . (lambda () (interactive) (find-file "~/organizer.org"))))
  :config
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  (setq org-default-notes-file "~/organizer.org")
  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-execute-hook 'org-display-inline-images 'append)
  (set-register ?o (cons 'file "~/organizer.org")))

(use-package ox-md
  :after org)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Outshine Mode

(use-package imenu :commands imenu-choose-buffer-index)

(use-package outshine
  :ensure t
  :defer t
  :bind (
         :map outline-minor-mode-map
         ("C-c n" . outline-next-visible-heading)
         ("C-c p" . outline-previous-visible-heading)
         ("C-<tab>" . outline-cycle)
         ("C-#" . hydra-outshine/body)
         ("M-<up>" . jez/outline-move-subtree-up)
         ("M-<down>" . jez/outline-move-subtree-down))
  :init (require 'helm)
  :config
  (defun jez/outline-move-subtree-up (args)
    "Same `outline-move-subtree-up' but will not expand"
    (interactive "p")
    (outline-move-subtree-up args)
    (outline-hide-subtree))

  (defun jez/outline-move-subtree-down (args)
    "Same `outline-move-subtree-down' but will not expand"
    (interactive "p")
    (outline-move-subtree-down args)
    (outline-hide-subtree))

  (defhydra hydra-outshine (:hint nil :columns 3)
    "Outshine Commands"
    ;; navigation
    ("n" outline-next-visible-heading "outline-next-visible-heading")
    ("p" outline-previous-visible-heading "outline-previous-visible-heading")
    ("f" outline-forward-same-level "outline-forward-same-level")
    ("u" outline-up-heading "outline-up-heading")
    ("b" outline-backward-same-level "outline-backward-same-level")
    ("F" outshine-next-block "outshine-next-block")
    ("B" outshine-previous-block "outshine-previous-block")
    ("j" outshine-navi "outshine-navi")
    ("J" outshine-imenu "outshine-imenu")
    ("g" outshine-imenu "outshine-imenu")
    ;; visibility
    ("c" outline-cycle "outline-cycle")
    ("a" outline-show-all "outline-show-all")
    ("l" outline-hide-sublevels "outline-hide-sublevels")
    ("C" (outshine-cycle-buffer) "outshine-cycle-buffer")
    ("r" outshine-narrow-to-subtree "outshine-narrow-to-subtree")
    ("w" widen "widen")
    ;; editing
    ("U" jez/outline-move-subtree-up "outline-move-subtree-up")
    ("D" jez/outline-move-subtree-down "outline-move-subtree-down")
    ("+" outline-demote "outline-demote")
    ("-" outline-promote "outline-promote")
    ("i" outshine-insert-heading "outshine-insert-heading")
    ("^" outshine-sort-entries "outshine-sort-entries")
    ("m" outline-mark-subtree "outline-mark-subtree")
    ("q" nil "leave"))
  :hook ((outline-minor-mode . outshine-hook-function)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Boookmark

(use-package bookmark
  :ensure t
  :defer t
  :init (require 'projectile)
  :config
  (unless (assoc "init.el" bookmark-alist)
    (find-file "~/.emacs.d/init.el")
    (bookmark-set "init.el")
    (kill-buffer))
  ;; add bookmarks to projectile
  (setq projectile-known-projects
	(-filter
         'file-directory-p
         (-distinct
          (sort
           (union (projectile-relevant-known-projects)
	          (mapcar (lambda (e) (cdr (assq 'filename (cdr e)))) bookmark-alist))
           'string<)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired Mode

(use-package dired
  :defer t
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-dwim-target t)            ; default dest to other window
  (setq truncate-lines t)
  (setq dired-listing-switches "-lah"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Expand Region

(use-package expand-region
  :ensure t
  :defer t
  :bind (("C-=" . er/expand-region)
         ("M-=" . er/contract-region)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Multiple Cursor

(use-package multiple-cursors
  :ensure t
  :defer t
  :config
  (defun jez/mark-word ()
    "Use to highlight a word"
    (interactive)
    (let ((non-word "[^[:alnum:]-_]"))
      (search-backward-regexp non-word)
      (forward-char)
      (set-mark (point))
      (search-forward-regexp non-word)
      (backward-char)))

  (defun jez/mark-multiple (arg)
    "Simulate sublime function on multiple cursor"
    (interactive "p")
    (if (not (region-active-p))
        (jez/mark-word)
      (mc/mark-next-like-this arg)))

  (defun jez/mark-multiple (arg)
    "Simulate sublime function on multiple cursor"
    (interactive "p")
    (if (not (region-active-p))
        (jez/mark-word)
      (mc/mark-next-like-this arg)))

  (defun jez/mc-add-cmds-once (func)
    "Add FUNC to `mc--default-cmds-to-run-once'. Prevent duplicate entry"
    (push func mc--default-cmds-to-run-once)
    (remove-duplicates mc--default-cmds-to-run-once))

  (defun jez/mc-add-cmds-all (func)
    "Add FUNC to `mc--default-cmds-to-run-for-all'. Prevent duplicate entry"
    (push func mc--default-cmds-to-run-for-all)
    (remove-duplicates mc--default-cmds-to-run-once))

  ;; run once
  (jez/mc-add-cmds-once 'jez/mark-multiple)
  (jez/mc-add-cmds-once 'vr/mc-mark)
  ;; run to all cursor
  (jez/mc-add-cmds-all 'org-self-insert-command)
  (jez/mc-add-cmds-all 'paredit-backward-kill-word)
  (jez/mc-add-cmds-all 'paredit-open-parenthesis)
  (jez/mc-add-cmds-all 'paredit-open-round)

  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-S-c C-S-a" . mc/edit-beginnings-of-lines)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-S-e" . mc/edit-ends-of-lines)
         ("C-c C-<" . mc/mark-all-like-this)
         ("s-d" . jez/mark-multiple)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helm Mode

(use-package helm
  :ensure t
  :defer t
  :bind (("C-c h" . helm-command-prefix)
         ("C-h d" . helm-dash)
         ("C-h m" . helm-describe-modes)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-x b" . helm-buffers-list)
         ("C-x r b" . helm-bookmarks)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)
         :map helm-projectile-projects-map
         ("C-j" . helm-maybe-exit-minibuffer)
         ("C-l" . jez/erase-minibuffer))
  :config
  (helm-mode 1)
  (projectile-mode)
  (helm-projectile-on)
  (setq projectile-completion-system 'helm)
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t)
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (defun jez/erase-minibuffer (arg)
    (interactive "p")
    (move-beginning-of-line arg)
    (delete-region (point) (point-max))))

(use-package helm-bookmark
  :defer t
  :bind (:map helm-bookmark-map ("C-j" . helm-maybe-exit-minibuffer)))
(use-package helm-config :after helm)
(use-package helm-dash :ensure t :after helm)
(use-package helm-describe-modes :ensure t :after helm)
(use-package helm-descbinds :ensure t :after helm)
(use-package helm-tramp
  :ensure t
  :defer t
  :bind ("C-c s" . helm-tramp)
  :config (setq tramp-default-method "ssh"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Swiper

(use-package swiper
  :ensure t
  :defer t
  :bind ("C-s" . jez/swiper)
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
  (defun jez/swiper (arg)
    "Custom `swiper' that default to symbol on point if prefix was provided"
    (interactive "p")
    (let* ((prefix (/= arg 1))
           (symbol (symbol-at-point))
           (symbol-name (symbol-name symbol)))
      (if (and prefix symbol)
          (swiper symbol-name)
        (swiper)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Magit Mode

(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status)
  :config
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ace Jump Mode

(use-package ace-jump-mode
  :ensure t
  :defer t
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-c C-SPC" . ace-jump-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keychord

(use-package iy-go-to-char
  :ensure t
  :defer t
  :commands (iy-go-to-char iy-go-to-char-backward))

(use-package key-chord
  :ensure t
  :defer 2
  :config
  (key-chord-mode 1)
  (key-chord-define-global "fj" 'iy-go-to-char))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tramp Mode

(use-package tramp
  :ensure t
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '((regexp-quote (system-name)) nil nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Yasnippet

(use-package yasnippet
  :diminish yas-minor-mode
  :defer 2
  :bind (("C-c y d" . yas-load-directory)
         ("C-c y i" . yas-insert-snippet)
         ("C-c y f" . yas-visit-snippet-file)
         ("C-c y n" . yas-new-snippet)
         ("C-c y t" . yas-tryout-snippet)
         ("C-c y l" . yas-describe-tables)
         ("C-c y g" . yas/global-mode)
         ("C-c y m" . yas/minor-mode)
         ("C-c y a" . yas-reload-all)
         ("C-c y x" . yas-expand))
  :bind (:map yas-keymap
              ("C-i" . yas-next-field-or-maybe-expand))
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  (setq yas-indent-line 'fixed)
  (setq yas-buffer-local-condition `always)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/yasnippet-snippets")
  (yas-global-mode 1))

(use-package auto-yasnippet
  :ensure t
  :defer t
  :bind (("C-c y a" . aya-create)
         ("C-c y e" . aya-expand)
         ("C-c y o" . aya-open-line))
  :after yasnippet)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto Complete

(use-package setup-hippie
  :bind (("C-." . hippie-expand-no-case-fold)
         ("C-:" . hippie-expand-lines)
         ("C-," . completion-at-point)))

(use-package auto-complete
  :ensure t
  :defer t
  :config (global-auto-complete-mode -1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python Mode

(use-package python
  :ensure t
  :defer t
  :mode (("\\.py\\'" . python-mode)
         ("\\.tac\\'" . python-mode))
  :bind (
         :map python-mode-map
         ("<tab>" . indent-for-tab-command))

  :config
  (setq truncate-lines t)
  (defun py-outline-level ()
    "Return header level on current point"
    (let (buffer-invisibility-spec)
      (save-excursion
        (skip-chars-forward "    ")
        (current-column))))

  (defun outshine-python-mode-hook ()
    (outline-minor-mode t)
    (setq-local outline-regexp "[ \t]*### \\|[ \t]*\\b\\(class\\|def\\|if\\|elif\\|else\\|while\\|for\\|try\\|except\\|with\\) ")
    (setq-local outline-level 'py-outline-level)
    (setq-local outshine-use-speed-commands nil))

  (define-key python-mode-map (kbd "C-c C-p") nil)
  :hook ((python-mode . outshine-python-mode-hook)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elpy

(use-package elpy
  :ensure t
  :defer t
  :config
  (define-key elpy-mode-map (kbd "C-c C-p") nil)
  (defun jez/python-disable-company-mode () (company-mode -1))
  (defun jez/disable-elpy () (ignore-errors (elpy-mode -1)))
  :hook ((org-mode . jez/disable-elpy)
         (shell-mode . jez/disable-elpy)
         (python-mode . elpy-mode)
         (elpy-mode . jez/python-disable-company-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Email - GNUS

;; always on topic mode
(use-package gnus
  :ensure t
  :defer t
  :config
  (setq gnus-select-method
        '(nnimap "gmail"
	         (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
	         (nnimap-server-port "imaps")
	         (nnimap-stream ssl)))

  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQL Mode

(use-package sql
  :ensure t
  :defer t
  :bind (
         :map sql-interactive-mode-map
         ("C-c u" . sqlup-capitalize-keywords-in-region)
         ("C-h s" . sqlformat)
         ("M-<return>" . comint-send-input))
  :hook ((sql-mode . sqlup-mode)
         (sql-interactive-mode  . sqlup-mode))
  :config
  (defun jez/sql-connect (connection &optional new-name)
    "Modify sql-connect to use CONNECTION name as buffer name"
    (interactive
     (if sql-connection-alist
         (list (sql-read-connection "Connection: " nil '(nil))
               current-prefix-arg)
       (user-error "No SQL Connections defined")))
    (let ((buffer-name (format "*sql-%s*" connection)))
      (when (get-buffer buffer-name)
        (switch-to-buffer buffer-name))
      (sql-connect connection new-name)
      (rename-buffer buffer-name)))
  (define-key sql-interactive-mode-map (kbd "RET") nil)
  (toggle-truncate-lines t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Undo Tree Mode

(use-package undo-tree
  :commands undo-tree-mode
  :ensure t
  :defer t
  :bind ("C-x u" . undo-tree-visualize)
  :config (undo-tree-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Plantuml - For diagram and UML

(use-package plantuml-mode
  :ensure t
  :defer t
  :commands plantuml-mode
  :config
  (setq org-plantuml-jar-path "~/.emacs.d/elpa/contrib/scripts/plantuml.jar")
  (setq plantuml-jar-path "~/.emacs.d/elpa/contrib/scripts/plantuml.jar"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Nyan Mode

(use-package nyan-mode
  :ensure t
  :defer 2
  :config
  (nyan-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prodigy

(use-package prodigy
  :commands prodigy
  :ensure t
  :defer t
  :config
  (when (file-exists-p "~/.emacs.d/prodigy-settings.el")
    (load-file "~/.emacs.d/prodigy-settings.el")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HackerNews

(use-package hackernews
  :commands hackernews
  :ensure t
  :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paredit Mode

(use-package paredit
  :ensure t
  :defer t
  :commands paredit-mode enable-paredit-mode
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (eval-expression-minibuffer-setup . enable-paredit-mode)
         (ielm-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode))
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VIM's - Change Inner

(use-package change-inner
  :ensure t
  :defer t
  :bind (("M-I" . change-inner)
         ("M-O" . change-inner)
         ("s-i" . jez/copy-inner)
         ("s-o" . jez/copy-outer))
  :config
  (load-file "~/.emacs.d/custom-ci.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Avy Zap

(use-package avy-zap
  :ensure t
  :defer t
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bash - run bash init script

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emmet Mode

(use-package emmet-mode
  :ensure t
  :defer t
  :hook ((sgml-mode . emmet-mode)
         (css-mode . emmet-mode)
         (web-mode . emmet-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs - Nifty Tricks

(defun line-copy-char (&optional b)
  "Copy a character exactly below/above the point
to the current point of the cursor (default is above)."
  (interactive "p")
  (let (p col s)
    (setq p (point))
    (setq col (current-column))
    (forward-line (if b -1 1))
    (move-to-column col)
    (setq s (buffer-substring (point) (+ (point) 1)))
    (goto-char p)
    (insert s)))
(define-key global-map [f12] 'line-copy-char)
(define-key global-map [(shift f12)] '(lambda ()(interactive)(line-copy-char nil)))
(global-set-key (kbd "M-SPC") 'cycle-spacing)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shell Mode

(use-package shell
  :defer t
  :config
  (defun sh-outline-level ()
    (save-excursion
      (skip-chars-forward "#")
      (current-column)))
  (defun outshine-sh-mode-hook ()
    (setq-local outshine-use-speed-commands t)
    (outline-minor-mode t)
    (setq-local outline-regexp "#")
    (setq-local outline-level 'sh-outline-level)
    (setq-local outshine-use-speed-commands nil))
  :bind ("s-k" . comint-clear-buffer)
  :hook (sh-mode . outshine-sh-mode-hook))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Web Mode

(use-package pug-mode :defer t :ensure t :mode ("\\.pug\\'"))
(use-package web-mode
  :ensure t
  :defer t
  :bind ("C-<tab>" . web-mode-fold-or-unfold)
  :mode (("\\.phtml\\'"      . web-mode)
         ("\\.tpl\\.php\\'"  . web-mode)
         ("\\.[agj]sp\\'"    . web-mode)
         ("\\.as[cp]x\\'"    . web-mode)
         ("\\.erb\\'"        . web-mode)
         ("\\.mustache\\'"   . web-mode)
         ("\\.djhtml\\'"     . web-mode)
         ("\\.html?\\'"      . web-mode))
  :config
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight nil)
  (defun jez/web-mode-hook ()
    (electric-pair-local-mode -1)
    (toggle-truncate-lines t)
    (web-mode-set-engine "django"))
  :hook ((web-mode . jez/web-mode-hook)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; JS Mode

(use-package js2-mode
  :ensure t
  :defer t
  :mode (("\\.js\\'" . js2-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CSS Mode

(use-package css-mode
  :ensure t
  :defer t
  :commands jez/css-minify
  :bind (:map css-mode-map ("C-c m" . jez/css-minify))
  :config
  (defun jez/css-minify-uglify ()
    "CSS Minify current buffer using `uglify'"
    (let ((minified (shell-command-to-string (format "uglifycss %s" buffer-file-name))))
      (erase-buffer)
      (insert minified)))
  (defun jez/css-minify-requests ()
    "CSS Minify current buffer via `cssminifier.com'"
    (let ((url-request-method "POST")
          (url-request-extra-headers
           '(("Content-Type" . "application/x-www-form-urlencoded")))
          (url-request-data (format "input=%s" (buffer-substring (point-min) (point-max)))))
      (url-retrieve "https://cssminifier.com/raw"
                    (lambda (status current-buffer)
                      (let ((body (buffer-substring (1+ url-http-end-of-headers) (point-max))))
                        (with-current-buffer current-buffer
                          (erase-buffer)
                          (insert body))))
                    (list (buffer-name (current-buffer))))))
  (defun jez/css-minify ()
    "CSS Minify current buffer"
    (interactive)
    (if (executable-find "uglifycss")
        (jez/css-minify-uglify)
      (jez/css-minify-requests))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Occur mode

(use-package replace
  :defer t
  :bind (:map occur-mode-map
         ("p" . jez/occur-prev)
         ("n" . jez/occur-next))
  :config
  (defun jez/occur-prev ()
    "Same with `occur-prev' but also move to occurence"
    (interactive)
    (occur-prev)
    (occur-mode-display-occurrence))

  (defun jez/occur-next ()
    "Same with `occur-next' but also move to occurence"
    (interactive)
    (occur-next)
    (occur-mode-display-occurrence)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elisp Mode

(use-package elisp-mode
  :defer t
  :bind (:map emacs-lisp-mode-map
         ("C-c d" . jez/describe-symbol-at-point)
	 ("C-c C-d" . jez/describe-symbol-at-point))
  :init
  (defun jez/describe-symbol-at-point (arg)
    "Describe current symbol on point on other window"
    (interactive "p")
    (describe-symbol (symbol-at-point))
    (when (< arg 4)
      (other-window 1)))

  (defun jez/emacs-lisp-mode-hook ()
    (interactive)
    (setq indent-tabs-mode nil)
    (toggle-truncate-lines t))

  (defun outshine-emacs-lisp-mode-hook ()
    (setq-local outshine-use-speed-commands t)
    (outline-minor-mode t))

  :hook ((emacs-lisp-mode . jez/emacs-lisp-mode-hook)
         (emacs-lisp-mode . outshine-emacs-lisp-mode-hook)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Docker

(use-package docker :ensure t :defer t)
(use-package docker-tramp :ensure t :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; YAML Mode

(use-package yaml-mode
  :ensure t
  :defer t
  :mode ("\\.yml\\'" . yaml-mode)
  :config
  (defun yaml-outline-level ()
    (let (buffer-invisibility-spec)
      (save-excursion
        (skip-chars-forward (rx (repeat 2 space)))
        (current-column))))
  (defun jez/yaml-mode-hook ()
    (outline-minor-mode t)
    (setq outline-regexp (rx (* (group (repeat 2 space))) alnum))
    (setq outline-level 'yaml-outline-level))
  :hook (yaml-mode . jez/yaml-mode-hook))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Latex Mode

(use-package tex-mode
 :defer t
 :config
 (defun jez/latex-mode-hook ()
   (setq-local TeX-save-query nil)
   (setq-local TeX-command-force "Latex"))
 :hook ((latex-mode . jez/latex-mode-hook)
        (LaTeX-mode . jez/latex-mode-hook)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Startup

(defun jez/show-bookmarks ()
  (call-interactively 'list-bookmarks))

(defun jez/display-init-time ()
  (defun jez/show-init-load-time ()
    (message "It took %s to load emacs" (emacs-init-time)))
  (advice-add 'display-startup-echo-area-message :after #'jez/show-init-load-time))

(defun jez/startup ()
  "Run after emacs init"
  (jez/show-bookmarks)
  (jez/display-init-time))

(add-hook 'after-init-hook 'jez/startup)
