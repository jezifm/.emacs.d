;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialize Variable

(setq emacs-starttime (current-time))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq ns-function-modifier 'control)
(setq dired-dwim-target t)              ; default dest to other window

(setq save-abbrevs 'silently)

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
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
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

(package-initialize)            ; fix issue package not yet initialize
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Local Dependencies
;;
;; dependencies included in `.emacs.d' but is not within package
;; managers above

(setq settings-dir (expand-file-name "settings" user-emacs-directory))
(add-to-list 'load-path settings-dir)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Install missing dependencies

;; package to install
(defun init--install-packages ()
  (packages-install
   '(ace-jump-mode
     ace-window
     auctex
     avy
     change-inner
     color-theme-modern
     cyberpunk-theme
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
     sublime-themes
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
(when (not (package-installed-p 'dash))
  (package-refresh-contents)
  (package-install 'dash))
(require 'setup-package)

;; install missing packages
(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; use package
(require 'use-package)


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

;; mode defaults
(electric-pair-mode t)
(recentf-mode t)
(show-paren-mode t)

;; mode properties
(setq recentf-max-menu-items 25)

;; key bindings custom
(global-set-key (kbd "C-z") 'jez/shell-shortcut)
(global-set-key (kbd "M-%") 'vr/query-replace)
(global-set-key (kbd "M-J") 'jez/simplify)
(global-set-key (kbd "M-j") 'jez/join-line)

;; Define local functions
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Boookmark

(require 'bookmark)
(unless (assoc "init.el" bookmark-alist)
  (find-file "~/.emacs.d/init.el")
  (bookmark-set "init.el")
  (kill-buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired Mode

(setq dired-dwim-target t)              ; default dest to other window
(add-hook 'dired-mode-hook (lambda () (setq truncate-lines t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CSS Mode

(require 'css-mode)
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
    (jez/css-minify-requests)))

(define-key css-mode-map (kbd "C-c m") 'jez/css-minify)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs GUI

;; ensure we have the theme
(load-file (concat user-emacs-directory "custom-themes/emacs-darkane-theme/darkane-theme.el"))
(load-theme 'darkane t t)
(enable-theme 'darkane)

;; remove backup files
(setq auto-save-default nil)
(setq make-backup-files nil)

;; remove scrollbar, menu bars, and toolbars
;; when is a special form of "if", with no else clause, it reads:
;; (when <condition> <code-to-execute1> <code-to-execute2> ...)
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; maximize window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; no splash screen
(setq inhibit-startup-message t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Expand Region

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("M-=" . er/contract-region)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Multiple Cursor

(use-package multiple-cursors
  :ensure t
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

  (defun jez/add-mc-once (func)
    "Add FUNC to `mc--default-cmds-to-run-once'. Prevent duplicate entry"
    (push func mc--default-cmds-to-run-once)
    (remove-duplicates mc--default-cmds-to-run-once))

  (defun jez/add-mc-all (func)
    "Add FUNC to `mc--default-cmds-to-run-for-all'. Prevent duplicate entry"
    (push func mc--default-cmds-to-run-for-all)
    (remove-duplicates mc--default-cmds-to-run-once))

  (jez/add-mc-once 'jez/mark-multiple)
  (jez/add-mc-all 'paredit-backward-kill-word)

  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-S-c C-S-a" . mc/edit-beginnings-of-lines)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-S-e" . mc/edit-ends-of-lines)
         ("C-c C-<" . mc/mark-all-like-this)
         ("s-d" . jez/mark-multiple))
  :after expand-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helm Mode

(use-package helm
  :ensure t
  :bind (("C-c h" . helm-command-prefix)
         ("C-h d" . helm-dash)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-x p p" . helm-do-ag-project-root)
         ("C-x r b" . helm-filtered-bookmarks)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action))
  :config
  (helm-mode 1)
  (projectile-mode t)
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t))

(use-package helm-projectile
  :ensure t
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  :bind ("C-x p f" . helm-projectile-find-file)
  :after helm)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Magit Mode

(global-set-key (kbd "C-x g") 'magit-status)

;; combine diff
(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Mode

;; disable backup file
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

;; enable task capture
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-export-coding-system 'utf-8)
(setq org-log-done 'time)

;; enable languages
(require 'org)
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
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
;; fix - remove sh when running on windows
(when (string-equal system-type "windows-nt")
  (assq-delete-all 'sh org-babel-languages))
(org-babel-do-load-languages
 'org-babel-load-languages
 org-babel-languages)

;; home directory
(global-set-key (kbd "C-c o") (lambda () (interactive) (find-file "~/organizer.org")))

;; set register
(set-register ?o (cons 'file "~/organizer.org"))

;; use org-refile to file or jump to headings
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

;; save all capture to single file
(setq org-default-notes-file "~/organizer.org")

;; disable prompt on source block eval
(setq org-confirm-babel-evaluate nil)

;; display images in buffer after eval
(add-hook 'org-babel-execute-hook 'org-display-inline-images 'append)

;; center all images
(advice-add 'org-latex--inline-image :around
            (lambda (orig link info)
              (concat
               "\\begin{center}"
               (funcall orig link info)
               "\\end{center}")))

;; set author
(setq user-full-name "Jezrael Arciaga")
(setq user-mail-address "jezarciaga@gmail.com")

;; enable markdown export
(eval-after-load "org" '(require 'ox-md nil t))

;; ;; send html email using org mode
;; (require 'org-mime)
;; (setq org-mime-library 'mml)
;; ;; set code blocks background to dark
;; (add-hook 'org-mime-html-hook
;;    (lambda ()
;;      (org-mime-change-element-style
;;       "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
;;       "#E6E1DC" "#232323"))))
;; ;; set block quotes offset
;; (add-hook 'org-mime-html-hook
;;    (lambda ()
;;      (org-mime-change-element-style
;;       "blockquote" "border-left: 2px solid gray; padding-left: 4px;")))

;; org todo sequence
(setq org-todo-keywords
      '((sequence "TODO" "SCHEDULED" "|" "DONE")))

;; update tasks state base on subtask
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; syntax highlighting
(setq org-src-fontify-natively t)

;; inline image auto refresh
(defun shk-fix-inline-images ()
  (interactive)
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))
(add-hook 'org-babel-after-execute-hook 'shk-fix-inline-images)

;; auto fill mode on org mode
(add-hook 'org-mode-hook 'auto-fill-mode)

;; store link
(define-key org-mode-map (kbd "C-c l") 'org-store-link)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ace Jump Mode

(require 'ace-jump-mode)

(defun jez/ace-enable-key-bind ()
  "Allow C-c C-SPC to trigger ace jump mode"
  (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
  (define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode))
(jez/ace-enable-key-bind)

;; issue 1 - `ace-jump-mode' not enabled in `org-mode'
;; issue 2 - `org-mode' not working properly when `ace-jump-mode' was
;;           enabled. specially when exporting to html
;; when org-mode starts it (org-mode-map) overrides the ace-jump-mode.
;; (add-hook 'org-mode-hook (jez/ace-enable-key-bind))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Trump Mode

(require 'tramp)
(setq tramp-default-method "ssh")
;; fix issue - sudo can only use the local host
(add-to-list 'tramp-default-proxies-alist
             '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '((regexp-quote (system-name)) nil nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Yasnippet

(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/yasnippet-snippets")
;; (add-hook 'prog-mode-hook #'yas-minor-mode)
(yas-global-mode 1)
(setq yas-buffer-local-condition `always)
(yas-reload-all)

;; fix indent line in python
(setq yas-indent-line 'fixed)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Annotation

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
(global-set-key (kbd "C-c C-d") 'insert-date)

(defun jez/annotate ()
  "Insert annotation for commenting"
  (interactive)
  (insert "--jez ")
  (insert-date nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto Complete

(require 'setup-hippie)
(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-:") 'hippie-expand-lines)
(global-set-key (kbd "C-,") 'completion-at-point)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python Mode

(defun jez/python-mode-hook ()
  (setq truncate-lines t)
  (hs-minor-mode t))
(add-hook 'python-mode-hook 'jez/python-mode-hook)
(add-to-list 'auto-mode-alist '("\\.tac\\'" . python-mode))  ; twisted


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elpy

(defun jez/elpy-mode-hook ()
  "Disable company mode"
  (company-mode -1))
(add-hook 'elpy-mode-hook 'jez/elpy-mode-hook)

;; fix conflict
(defun jez/disable-elpy ()
  "Ensure `elpy-mode' is disabled"
  (ignore-errors (elpy-mode -1)))

(ignore-errors
  (require 'elpy)
  (elpy-enable)
  (add-hook 'org-mode-hook 'jez/disable-elpy)
  (add-hook 'shell-mode-hook 'jez/disable-elpy))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Email - GNUS

;; always on topic mode
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQL Mode

(require 'sql)
(add-hook 'sql-mode-hook 'sqlup-mode)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)
(define-key sql-mode-map (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)
(define-key sql-mode-map (kbd "C-h s") 'sqlformat)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Publish settings
(when (file-exists-p "~/.emacs.d/publish-settings.el")
  (load-file "~/.emacs.d/publish-settings.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keychord
;; move to char similar to "f" in vim, f+g forward, d+f backward

(require 'key-chord)
(require 'iy-go-to-char)

(key-chord-mode 1)
(key-chord-define-global "fj" 'iy-go-to-char)
;; (key-chord-define-global "df" 'iy-go-to-char-backward)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Undo Tree Mode

(undo-tree-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window Numbering

(require 'window-numbering)
(window-numbering-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Plantuml - For diagram and UML

(setq org-plantuml-jar-path "~/.emacs.d/elpa/contrib/scripts/plantuml.jar")
(setq plantuml-jar-path "~/.emacs.d/elpa/contrib/scripts/plantuml.jar")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Nyan Mode

(require 'nyan-mode)
(nyan-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prodigy

(require 'prodigy)

(when (file-exists-p "~/.emacs.d/prodigy-settings.el")
  (load-file "~/.emacs.d/prodigy-settings.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HackerNews

(require 'hackernews)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; YAML mode

;; (require 'org)
(defun org-babel-execute:yaml (body params) body)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Swiper

(require 'swiper)
(setq ivy-re-builders-alist '((t . ivy--regex-plus)))

(defun jez/swiper (args)
  "Custom `swiper' that default to symbol on point if prefix was provided"
  (interactive "p")
  (let* ((prefix (>= args 4))
         (symbol (symbol-at-point))
         (symbol-name (symbol-name symbol)))
    (if (and prefix symbol)
        (swiper symbol-name)
      (swiper))))

(global-set-key (kbd "C-s") 'jez/swiper)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paredit Mode

(ignore-errors
  (require 'paredit)
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VIM's - Change Inner

(require 'change-inner)
(load-file "~/.emacs.d/custom-ci.el")
(global-set-key (kbd "M-I") 'change-inner)
(global-set-key (kbd "M-O") 'change-outer)

(global-set-key (kbd "s-i") 'jez/copy-inner)
(global-set-key (kbd "s-o") 'jez/copy-outer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hydra

;; ein is not working properly uncomment when fixed --jez 2017-03-27
(defun jez/ein-hydra ()
  "Add custom hydra to ein"
  (interactive "P")
  (defhydra hydra-ein (ein:notebook-multilang-mode-map "C-c")
    "ein"
    ("p" ein:worksheet-goto-prev-input "prev")
    ("n" ein:worksheet-goto-next-input "next")))
(add-hook 'ein:connect-mode-hook 'jez/ein-hydra)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bash - run bash init script

(ignore-errors
  (exec-path-from-shell-initialize))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emmet Mode

(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . emmet-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . emmet-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . emmet-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . emmet-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . emmet-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . emmet-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . emmet-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . emmet-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-Complete

(global-auto-complete-mode -1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQL default

(require 'sql)
(define-key sql-interactive-mode-map (kbd "M-<return>") 'comint-send-input)
(define-key sql-interactive-mode-map (kbd "RET") nil)

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

(add-hook 'sql-interactive-mode-hook (lambda () (toggle-truncate-lines t)))


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

(define-key shell-mode-map (kbd "s-k") '(lambda () (interactive)
                                          (erase-buffer)
                                          (comint-send-input)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Web Mode

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight nil)

(defun jez/web-mode-hook ()
  (electric-pair-local-mode -1)
  (emmet-mode t)
  (toggle-truncate-lines t))
(add-hook 'web-mode-hook 'jez/web-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Occur mode

(defun jez/occur-prev ()
  (interactive)
  (occur-prev)
  (occur-mode-display-occurrence))

(defun jez/occur-next ()
  (interactive)
  (occur-next)
  (occur-mode-display-occurrence))

(defun jez/occur-mode-hook ()
  (define-key occur-mode-map (kbd "p") 'jez/occur-prev)
  (define-key occur-mode-map (kbd "n") 'jez/occur-next))

(add-hook 'occur-mode-hook 'jez/occur-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs lisp mode

(defun jez/describe-symbol-at-point ()
  "Describe current symbol on point on other window"
  (interactive)
  (describe-symbol (symbol-at-point)))

(defun jez/emacs-lisp-mode-hook ()
  (interactive)
  (setq indent-tabs-mode nil)
  (define-key emacs-lisp-mode-map (kbd "C-c d") 'jez/describe-symbol-qa-point)
  (define-key emacs-lisp-mode-map (kbd "C-c C-d") 'jez/describe-symbol-at-point)
  (toggle-truncate-lines t))
(add-hook 'emacs-lisp-mode-hook 'jez/emacs-lisp-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Startup

(defun jez/display-time-elapsed ()
  (interactive)
  ;; patch display message method
  (defun display-startup-echo-area-message ()
    (message "It took %.2f seconds to load emacs"
             (float-time
              (time-subtract (current-time) emacs-starttime)))))

(defun jez/display-bookmarks ()
  (interactive)
  (list-bookmarks)
  (switch-to-buffer "*Bookmark List*"))

(defun startup ()
  (interactive)
  (jez/display-bookmarks)
  (jez/display-time-elapsed))

(startup)
