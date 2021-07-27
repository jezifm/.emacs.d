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
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)
(setq-default column-number-mode t)
(setq-default delete-selection-mode t)
(setq-default save-interprogram-paste-before-kill t)
(setq-default set-mark-command-repeat-pop t)
(setq create-lockfiles nil)

(defalias 'yes-or-no-p 'y-or-n-p)

;; disable prompt
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)

;; registers
(set-register ?i '(file . "~/.emacs.d/init.el"))
(set-register ?b '(file . "~/.bashrc"))

;; environment
(setenv "TERM" "xterm")

;;; Emacs Version

(when (version< emacs-version "26")
  (error (format "Emacs version %s not supported. Please update to 26 or higher."
                 emacs-version)))


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


;;; Package Manager - melpa

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)


;;; Local Dependencies
;;
;; dependencies included in `.emacs.d' but is not within package
;; managers above

(setq settings-dir (expand-file-name "settings" user-emacs-directory))
(add-to-list 'load-path settings-dir)
(autoload 'auto-capitalize-mode "auto-capitalize")


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
(use-package which-key      :ensure t :defer t)
(use-package sqlup-mode     :ensure t :defer t)
(use-package unfill :ensure t :defer t)


;;; Host Specific Customization
;;
;; persist bookmarks and other customization when switching `.emacs.d'

;; ensure customization directory
(setq custom-host-dir "~/.emacs.d.local")
(unless (file-directory-p custom-host-dir)
  (make-directory custom-host-dir))

;; update variables to use host
(defun jez-load-custom-initel ()
  (interactive)
  (let* ((file-directory (expand-file-name "init.el" custom-host-dir)))
    (if (file-exists-p file-directory)
        (progn
         (load-file file-directory)
         (message "%s loaded" file-directory))
      (message "no custom init file found"))))

(add-hook 'after-init-hook 'jez-load-custom-initel)
(setq custom-file (expand-file-name "custom.el" custom-host-dir))
(setq bookmark-default-file (expand-file-name "bookmarks" custom-host-dir))


;;; Emacs core

;; local hydra
(defhydra jez-hydra-window (:hint nil :columns 3)
  ("<right>" next-buffer "next-buffer")
  ("<left>" previous-buffer "previous-buffer"))

;; Define local functions
(defun jez-insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y-%m-%d")
                 ((equal prefix '(4)) "%Y-%m-%d %H:%M:%S")
                 ((equal prefix '(16)) "%A, %d. %B %Y")))
        (system-time-locale "en_US"))
    (insert (format-time-string format))))

(defun jez-insert-time ()
  "Inserts time"
  (interactive)
  (jez-insert-date '(4)))

(defun jez-annotate ()
  "Insert annotation for commenting"
  (interactive)
  (insert "--jez ")
  (jez-insert-date nil))

(defun jez-clear-font-properties ()
  "Clear font properties"
  (interactive)
  (let ((inhibit-read-only t))
    (set-text-properties (point-min) (point-max) nil)))

(defun jez-pad-region (arg)
  "Add `=` to highlighted region''"
  (interactive "P")
  (let* ((beg (region-beginning))
         (end (region-end))
         (length (- end beg))
         (pad (- (/ (- 72 length) 2) 1))
         (char (if arg (read-string "Pad character: ") comment-start)))
    (save-excursion
      (goto-char end)
      (insert " ")
      (insert-char (string-to-char char) pad)
      (goto-char beg)
      (insert-char (string-to-char char) pad)
      (insert " "))))

(defun jez-shell-clean-buffer-name (buffer-name)
  "Return base name from BUFFER-NAME.
eg. *shell foo* -> foo"
  (save-match-data
    (if (string-match "^\\*shell \\(.*\\)\\*$" buffer-name)
        (match-string 1 buffer-name)
      buffer-name)))

(defun jez-shell-build-name (name)
  "Return unique buffer-name based on NAME"
  (interactive "MName: ")
  (save-match-data
    (cond ((string-match "\\*shell \\(.*\\) <\\(.*\\)>\\*" name)
           (format "*shell %s <%s>*" (match-string 1 name) (1+ (string-to-number (match-string 2 name)))))
          ((string-match "\\*shell \\(.*\\)\\*" name)
           (format "*shell %s <1>*" (match-string 1 name)))
          (t (format "*shell %s*" name)))))

(defun jez-shell-get-count (buffer)
  "Check how many shells have we created for BUFFER-NAME"
  (save-match-data
    (if (string-match "\\*shell.*<\\(.*\\)>\\*" buffer)
        (string-to-number (match-string 1 buffer))
      1)))

(defun jez-shell-shortcut ()
  "Create shell buffer based on current buffer name"
  (interactive)
  (let* ((buffer-name-current (buffer-name (current-buffer)))
         (counter (jez-shell-get-count buffer-name-current))
         (buffer-name-shell (jez-shell-build-name buffer-name-current)))
    (save-current-buffer
      (shell buffer-name-shell))
    (when (equal (count-windows) 1)
      (split-window-right)
      (other-window 1))
    (switch-to-buffer buffer-name-shell)))

(defun jez-guess-shell-buffer-name ()
  "Return name of buffer with 'sh-' as prefix"
  (interactive)
  (let* ((shell-pattern "^\*shell .*\*")
         (buffers (--map (buffer-name it) (buffer-list)))
         (shell-buffers (--filter (string-match shell-pattern it)
                                  buffers)))
    (car shell-buffers)))

(defun jez-shell-run-project ()
  "Run shell on chosen directory"
  (interactive)
  (let* ((workspace "~/workspace/")
         (directories (helm-list-directory  workspace))
         (directories-relative (--map (s-replace-regexp ".*/\\(.*\\)$" "\\1" it) directories))
         (directory-chosen (helm-comp-read "Choose directory:" directories-relative))
         (default-directory (format "%s%s" workspace directory-chosen)))
    (shell (format "*shell %s*" directory-chosen))))

(defun jez-create-shell-buffer (&optional name)
  "Create shell buffer using NAME"
  (interactive)
  (unless name
    (setq name (helm-read-string "buffer name: ")))
  (shell (format "*shell %s*" name)))

(defun jez-join-line (count)
  "Custom join line"
  (interactive "p")
  (dotimes (_ count nil)
    (join-line -1)))

(defun jez-simplify ()
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

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun jez-camelize (s)
  "Convert under_score string S to CamelCase string."
  (mapconcat 'identity (mapcar
                        #'(lambda (word) (capitalize (downcase word)))
                        (split-string s "_")) ""))

(defun jez-titleize-from-snake (s)
  "Convert under_score string S to CamelCase string."
  (s-titleize (s-replace-regexp "_" " " s)))

(defun jez-slugify (text)
  "Slufigy TEXT"
  (let ((case-fold-search nil)
        (regex-configs '(("\\([a-z]\\)\\([A-Z]\\)" . "\\1-\\2")
                         ("[^a-zA-Z0-9_.-]+" . "-")
                         ("[._-]+\\([._-]\\)" . "\\1"))))
    (downcase
     (loop for i in regex-configs
           do (setq text (s-replace-regexp (car i) (cdr i) text))
           finally return text))))

(defun jez-slugify-selection (arg)
  "Slugify text in region"
  (interactive "P")
  (let* ((start (region-beginning))
         (end (region-end))
         (text-old (buffer-substring start end))
         (text-new (jez-slugify text-old)))
    (delete-region start end)
    (insert text-new)))

(defun jez-snake-case ()
  "Convert region to snake case"
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (downcase-region (point-min) (point-max))
      (goto-char (point-min))
      (replace-string " " "_"))))

(defun jez-abbreviate (string &optional separator)
  "Return first letter on each word of STRING using SEPERATOR"
  (interactive "s")
  (let* ((separator (or separator "_")))
    (s-join "" (--map (s-left 1 it) (s-split separator string)))))

(defun jez-clear-buffers ()
  "Clears all buffer except from some default"
  (interactive)
  (let* ((ignore-list '("*scratch*" "*Messages*" "*Pymacs*" "init.el"))
         (buffer-list
          (seq-filter
           '(lambda (buffer) (not (member (buffer-name buffer) ignore-list)))
           (buffer-list))))
    (mapc 'kill-buffer buffer-list)))

(defun jez-insert-buffer-name-current ()
  "Insert name of current buffer"
  (interactive)
  (insert (buffer-name (current-buffer))))

(defun jez-insert-buffer-name ()
  "Insert name of buffer"
  (interactive)
  (insert (helm-comp-read "Buffer: " (mapcar 'buffer-name (buffer-list)))))

(defun jez-occur (regexp)
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

(defun jez-black-theme ()
  "Simple black theme"
  (interactive)
  (set-background-color "black")
  (set-foreground-color "white")
  (set-face-background 'mode-line "black")
  (set-face-foreground 'mode-line "white")
  (set-face-foreground 'region "black")
  (set-face-background 'region "white")
  ;; (set-face-foreground 'text-cursor "black")
  ;; Set mouse color
  (set-mouse-color "navy")
  ;; Set highlighting colors for isearch and drag
  (set-face-foreground 'highlight "white")
  (set-face-background 'highlight "blue")
  (set-face-foreground 'region "cyan")
  (set-face-background 'region "blue")
  (set-face-foreground 'secondary-selection "skyblue")
  (set-face-background 'secondary-selection "darkblue"))

(defun jez-reset-themes ()
  "Remove all enabled themes"
  (interactive)
  (let* ((always-on-themes '(smart-mode-line-powerline))
         (predicate (lambda (theme) (not (member theme always-on-themes))))
         (themes-to-disable (seq-filter predicate custom-enabled-themes)))
    (mapcar 'disable-theme themes-to-disable)))

(defun jez-change-theme (&optional theme)
  "Load and enable THEME without confirmation"
  (interactive)
  (let* ((themes-available (mapcar 'symbol-name (custom-available-themes)))
         (theme (or theme (intern (completing-read "Load custom theme: " themes-available)))))
    (jez-reset-themes)
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

(defun jez-replace-regexp (from-string to-string)
    "Replace all occurence of FROM-STRING in current buffer with TO-STRING"
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward from-string nil t)
        (replace-match to-string nil nil))))

(defun jez-replace-regexp-multiple (&rest body)
    "Replace multiple regex"
    (let* ((length (length body))
           (indexes (number-sequence 0 (1- length) 2)))
      (dolist (index indexes nil)
        (let ((from (nth index body))
              (to (nth (1+ index) body)))
          (jez-replace-regexp from to)))))

(defun jez-copy-lines-matching-re (re)
  "find all lines matching the regexp RE in the current buffer
putting the matching lines in a buffer named *matching*"
  (interactive "sRegexp to match: ")
  (let ((result-buffer (get-buffer-create "*matching*")))
    (with-current-buffer result-buffer
      (erase-buffer))
    (save-match-data
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (princ (buffer-substring-no-properties (line-beginning-position)
                                                 (line-beginning-position 2))
                 result-buffer))))
    pop-to-buffer result-buffer))

(defun jez-outline-mode-adhoc (regex)
  "Enable outline-mode using REGEX as pattern"
  (interactive "sRegex: ")
  (outline-minor-mode t)
  (defun jez-outline-mode-adhoc-level ()
    (skip-chars-forward (rx (not alnum)))
    (current-column))
  (setq-local outline-regexp regex)
  (setq-local outline-level 'jez-outline-mode-adhoc-level))

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

(defun insert-random-text (&optional prefix)
  "Insert random string with PREFIX"
  (interactive)
  (let ((prefix (or prefix "")))
    (insert (make-temp-name prefix))))

(defvar jez-shell-command-command nil "string command to run on `jez-shell-command-buffer'")
(defvar jez-shell-command-buffer nil "buffer name to run `jez-shell-command-command'")

(defun jez-shell-command (arg)
  "Run `jez-shell-command-command' on `jez-shell-command-buffer' buffer"
  (interactive "P")
  (when arg
    (jez-shell-command-bind-noargs))
  (if (buffer-file-name)
      (save-buffer))
  (let* ((window (nth 0 (seq-filter (lambda (window)
                                      (s-equals-p (buffer-name (window-buffer window))
                                                  jez-shell-command-buffer))
                                    (window-list))))
         (window (or window (nth 1 (window-list)))))
    (when (not (equal jez-shell-command-buffer (buffer-name (current-buffer))))
      (set-window-buffer window jez-shell-command-buffer)))
  (with-current-buffer jez-shell-command-buffer
    (comint-clear-buffer)
    (goto-char (point-max))
    (insert jez-shell-command-command)
    (comint-send-input)
    (comint-send-input)))

(defun jez-shell-command-bind (&optional command buffer)
  "Bind wrapper for `jez-shell-command' to `C-r''"
  (interactive)
  (let* ((default-buffer-name (jez-guess-shell-buffer-name))
         (buffer (or buffer (helm-comp-read (format "shell buffer (%s): " default-buffer-name) (mapcar 'buffer-name (buffer-list)) :default default-buffer-name)))
         (command (or command (with-current-buffer buffer
                                (let* ((last-shell-command (ring-ref comint-input-ring 0))
                                       (prompt-message (format "shell command (%s): " last-shell-command)))
                                  (read-string prompt-message nil nil last-shell-command))))))
    (setq jez-shell-command-buffer buffer)
    (setq jez-shell-command-command command)
    (global-set-key (kbd "C-r") 'jez-shell-command)
    (condition-case nil
        (message (format "C-r binded to %s: %s" buffer command))
      (error
       (message "C-r binded to %s" buffer)))
    ))

(defun jez-shell-command-bind-noargs ()
  (interactive)
  (let* ((buffer (jez-guess-shell-buffer-name))
         (command (with-current-buffer buffer (ring-ref comint-input-ring 0))))
    (setq jez-shell-command-buffer buffer)
    (set-text-properties 0 (length command) nil command)
    (setq jez-shell-command-command command)
    (global-set-key (kbd "C-r") 'jez-shell-command)
    (condition-case nil
        (message (format "C-r binded to %s: %s" buffer command))
      (error
       (message "C-r binded to %s" buffer)))
    ))

(defun jez-fix-shell-buffer-name ()
  "Rename shell buffer based on its current directory"
  (interactive)
  (when (string-equal major-mode "shell-mode")
    (let* ((current-directory (s-replace-regexp ".*/\\(.*\\)/?" "\\1" default-directory))
           (new-name (format "*shell %s*" current-directory)))
      (rename-buffer new-name))))

(defun jez-package-install (pkg &optional dont-select)
  "Modified version of package-install. When failed, run refresh content and try again"
  (interactive
   (progn
     ;; Initialize the package system to get the list of package
     ;; symbols for completion.
     (unless package--initialized
       (package-initialize t))
     (unless package-archive-contents
       (package-refresh-contents))
     (list (intern (completing-read
                    "Install package: "
                    (delq nil
                          (mapcar (lambda (elt)
                                    (unless (package-installed-p (car elt))
                                      (symbol-name (car elt))))
                                  package-archive-contents))
                    nil t))
           nil)))
  (condition-case nil
      (package-install pkg dont-select)
    (error
     (progn
       (package-refresh-contents)
       (package-install pkg dont-select)))))

(defun jez-kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

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

;;; Emacs Global Keys
;; makes it easier to see conflicting keys

(use-package bind-key
  :ensure t
  :bind ((:map isearch-mode-map
               ("s-s" . isearch-repeat-forward)
               ("s-r" . isearch-repeat-backward)))
  :config
  ;; key bindings unset
  (global-unset-key (kbd "C-x C-c"))  ; disable quit
  (global-unset-key (kbd "C-x c"))    ; disable quit
  (global-unset-key (kbd "C-z"))      ; disable minimize
  (global-unset-key (kbd "s-t"))      ; disable font-panel
  (global-unset-key (kbd "s-p"))      ; disable ns-print-buffer

  (bind-keys
   ("<f12>" . line-copy-char)
   ("<f5>" . sort-lines)
   ("C-c C-<return>" . delete-trailing-whitespace)
   ("C-c j d" . jez-insert-date)
   ("C-c j t" . jez-insert-time)
   ("C-c j z" . jez-create-shell-buffer)
   ("C-c t" . toggle-truncate-lines)
   ("C-x <left>" . jez-hydra-window/previous-buffer)
   ("C-x <right>" . jez-hydra-window/next-buffer)
   ("C-x C-<left>" . jez-hydra-window/previous-buffer)
   ("C-x C-<right>" . jez-hydra-window/next-buffer)
   ("C-x C-j" . (lambda () (interactive) (dired default-directory)))
   ("C-x r q" . save-buffers-kill-terminal) ; remap quit-key
   ("C-x |" . toggle-window-split)
   ("C-z" . jez-shell-shortcut)
   ("M-J" . jez-simplify)
   ("M-SPC" . cycle-spacing)
   ("M-i" . back-to-indentation)
   ("M-j" . jez-join-line)
   ("M-n" . (lambda (arg) (interactive "p") (next-line (* arg 5))))
   ("M-p" . (lambda (arg) (interactive "p") (previous-line (* arg 5))))
   ("s-r" . isearch-backward-regexp)
   ("s-s" . isearch-forward-regexp)
   ("C-M-<backspace>" . jez-kill-back-to-indentation)
   ("S-<return>" . sanityinc/newline-at-end-of-line)
   ))


;;; Emacs Built-in Mode

(use-package menu-bar   :defer 2 :config (menu-bar-mode -1))
(use-package scroll-bar :defer 2 :config (scroll-bar-mode -1))
(use-package tool-bar   :defer 2 :config (tool-bar-mode -1))
(use-package elec-pair  :defer 2 :config (electric-pair-mode t))
(use-package paren      :defer 2 :config (show-paren-mode t))
(use-package recentf    :defer 2 :config (recentf-mode t))


;;; Emacs GUI

(use-package zenburn-theme :ensure t :defer t)
(use-package blackboard-theme :ensure t :defer t)
(use-package color-theme-sanityinc-tomorrow :ensure t :defer t)
(use-package smart-mode-line :ensure t :defer t)
(use-package smart-mode-line-powerline-theme :ensure t :defer 2)
(add-to-list 'custom-theme-load-path (expand-file-name "custom-themes/emacs-darkane-theme/" user-emacs-directory))
(use-package custom
  :defer 2
  :config
  ;; main-theme
  (jez-change-theme 'sanityinc-tomorrow-bright)
  ;; mode-line
  (require 'smart-mode-line)
  (require 'smart-mode-line-powerline-theme)
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'powerline)
  (sml/setup)
  (when (eq window-system 'w32)
    (set-face-attribute 'default nil :family "Consolas" :height 110)))


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


;;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :commands treemacs-add-project-to-workspace
  )


;;; Ediff

(use-package ediff-util
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  (winner-mode)
  :hook (ediff-after-quit-hook-internal . winner-undo))

;;; Org Mode

(use-package org
  :ensure t
  :defer t
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-<right>" . org-todo)
         ("C-<left>" . jez-org-todo-previous))
  :config
  (setq user-full-name "Jezrael Arciaga")
  (setq user-mail-address "jezarciaga@gmail.com")
  (setq org-use-speed-commands t)
  (setq org-export-coding-system 'utf-8)
  (setq org-log-done 'time)
  (setq org-src-fontify-natively t)
  (setq org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE")))
  (setq org-todo-keyword-faces
        '(("TODO" . "red")
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
          (plantuml . t)
          ))
  (setq org-src-preserve-indentation t)
  (setq org-export-with-sub-superscripts '{})
  (setq org-use-sub-superscripts '{})
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

  (defun jez-org-disable-font-theme ()
    "Remove font changes from current theme"
    (interactive)
    (dolist (face '(org-level-1
		    org-level-2
		    org-level-3
		    org-level-4
		    org-level-5))
      (set-face-attribute face nil :weight 'normal :height 1.0)))

  (defun jez-org-delete-trailing-whitespace-before-save ()
    "Add hook delete trailing whitespace when saving in org mode"
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))

  (defun jez-org-todo-previous (&optional arg)
    (interactive)
    "Same with `org-todo' but switch to previous set of keywords"
    (org-todo 'previousset))

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
         ;; (org-mode . auto-fill-mode)
         (org-mode . jez-org-disable-font-theme)))

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


;;; Outshine Mode

(use-package imenu :commands imenu-choose-buffer-index)

(use-package outshine
  :ensure t
  :defer t
  :bind (
         :map outline-minor-mode-map
         ("C-c n" . outline-next-visible-heading)
         ("C-c p" . outline-previous-visible-heading)
         ("C-<tab>" . outshine-cycle)
         ("C-#" . hydra-outshine/body)
         ("M-<up>" . jez-outline-move-subtree-up)
         ("M-<down>" . jez-outline-move-subtree-down))
  :init (require 'helm)
  :config
  (defun jez-outline-move-subtree-up (args)
    "Same `outline-move-subtree-up' but will not expand"
    (interactive "p")
    (outline-move-subtree-up args)
    (outline-hide-subtree))

  (defun jez-outline-move-subtree-down (args)
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
    ("<tab>" outline-cycle "outline-cycle")
    ("a" outline-show-all "outline-show-all")
    ("l" outline-hide-sublevels "outline-hide-sublevels")
    ("C" (outshine-cycle-buffer) "outshine-cycle-buffer")
    ("r" outshine-narrow-to-subtree "outshine-narrow-to-subtree")
    ("w" widen "widen")
    ;; editing
    ("U" jez-outline-move-subtree-up "outline-move-subtree-up")
    ("D" jez-outline-move-subtree-down "outline-move-subtree-down")
    ("+" outline-demote "outline-demote")
    ("-" outline-promote "outline-promote")
    ("i" outshine-insert-heading "outshine-insert-heading")
    ("^" outshine-sort-entries "outshine-sort-entries")
    ("m" outline-mark-subtree "outline-mark-subtree")
    ("q" nil "leave"))
  :hook ((outline-minor-mode . outshine-mode)))


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


;;; Dired Mode

(use-package dired
  :defer t
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-dwim-target t)            ; default dest to other window
  (setq truncate-lines t)
  (setq dired-listing-switches "-lah"))


;;; Expand Region

(use-package expand-region
  :ensure t
  :defer t
  :bind (("C-=" . er/expand-region)
         ("M-=" . er/contract-region)))


;;; Multiple Cursor

(use-package multiple-cursors
  :ensure t
  :defer t
  :config
  (defun jez-mark-word ()
    "Use to highlight a word"
    (interactive)
    (let ((non-word "[^[:alnum:]-_]"))
      (search-backward-regexp non-word)
      (forward-char)
      (set-mark (point))
      (search-forward-regexp non-word)
      (backward-char)))

  (defun jez-mark-multiple (arg)
    "Simulate sublime function on multiple cursor"
    (interactive "p")
    (if (not (region-active-p))
        (jez-mark-word)
      (mc/mark-next-like-this arg)))

  (defun jez-mark-multiple (arg)
    "Simulate sublime function on multiple cursor"
    (interactive "p")
    (if (not (region-active-p))
        (jez-mark-word)
      (mc/mark-next-like-this arg)))

  (defun jez-mc-add-cmds-once (func)
    "Add FUNC to `mc--default-cmds-to-run-once'. Prevent duplicate entry"
    (push func mc--default-cmds-to-run-once)
    (remove-duplicates mc--default-cmds-to-run-once))

  (defun jez-mc-add-cmds-all (func)
    "Add FUNC to `mc--default-cmds-to-run-for-all'. Prevent duplicate entry"
    (push func mc--default-cmds-to-run-for-all)
    (remove-duplicates mc--default-cmds-to-run-for-all))
  ;; run once
  (jez-mc-add-cmds-once 'jez-mark-multiple)
  (jez-mc-add-cmds-once 'vr/mc-mark)
  (jez-mc-add-cmds-once 'mc/insert-numbers)
  ;; run to all cursor
  (jez-mc-add-cmds-all 'org-self-insert-command)
  (jez-mc-add-cmds-all 'paredit-backward-kill-word)
  (jez-mc-add-cmds-all 'paredit-open-parenthesis)
  (jez-mc-add-cmds-all 'paredit-open-round)

  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-S-c C-S-a" . mc/edit-beginnings-of-lines)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-S-e" . mc/edit-ends-of-lines)
         ("C-c C-<" . mc/mark-all-like-this)
         ("s-d" . jez-mark-multiple)))


;;; Helm Mode

(use-package projectile
 :defer t
 :config
 (defadvice projectile-on (around exlude-tramp activate)
   "This should disable projectile when visiting a remote file"
   (unless  (--any? (and it (file-remote-p it))
                    (list
                     (buffer-file-name)
                     list-buffers-directory
                     default-directory
                     dired-directory))
     ad-do-it))
 (setq projectile-mode-line "Projectile")
 (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
 (projectile-mode))

(use-package helm
  :ensure t
  :defer t
  :bind (
         ("C-c h"   . helm-command-prefix)
         ("C-h d"   . helm-dash)
         ("C-h m"   . helm-describe-modes)
         ("C-x C-b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-x b"   . helm-mini)
         ("C-x r b" . helm-bookmarks)
         ("M-x"     . helm-M-x)
         ("M-y"     . helm-show-kill-ring)
         :map helm-map
         ("<tab>"   . helm-execute-persistent-action)
         ("C-i"     . helm-execute-persistent-action)
         ("M-x"     . helm-select-action)
         :map helm-projectile-projects-map
         ("C-j"     . helm-maybe-exit-minibuffer)
         ("C-l"     . jez-erase-minibuffer)
         :map shell-mode-map
         ("C-c C-l" . 'helm-comint-input-ring)
         :map minibuffer-local-map
         ("C-c C-l" . 'helm-minibuffer-history)
         )
  :config
  (require 'shell)
  (require 'projectile)
  (helm-mode 1)
  (helm-projectile-on)
  (setq projectile-completion-system 'helm)
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t)
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t)
  (setq projectile-git-submodule-command nil)


  (defun jez-erase-minibuffer (arg)
    (interactive "p")
    (move-beginning-of-line arg)
    (delete-region (point) (point-max))))

(use-package helm-bookmark
  :defer t
  :bind (:map helm-bookmark-map ("C-j" . helm-maybe-exit-minibuffer))
  :after helm)
(use-package helm-config :after helm)
(use-package helm-dash :ensure t :after helm)
(use-package helm-describe-modes :ensure t :after helm)
(use-package helm-descbinds :ensure t :after helm)
(use-package helm-tramp
  :ensure t
  :defer t
  :bind ("C-c s" . helm-tramp)
  :config (setq tramp-default-method "ssh")
  :after helm)


;;; Swiper

(use-package swiper
  :ensure t
  :defer t
  :bind ("C-s" . jez-swiper)
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
  (defun jez-swiper (arg)
    "Custom `swiper' that default to symbol on point if prefix was provided"
    (interactive "p")
    (let* ((prefix (/= arg 1))
           (symbol (symbol-at-point))
           (symbol-name (symbol-name symbol)))
      (if (and prefix symbol)
          (swiper symbol-name)
        (swiper)))))


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
  (defun jez-magit-visit-branch-url ()
  "Build the URL or the pull requestion on GitHub corresponding
to the current branch. Uses Magit."
  (interactive)
  (browse-url
   (format "%s/branch/%s"
           (replace-regexp-in-string ".*:\\(.*\\)\\.git$"
                                     "https://bitbucket.org/\\1"
                                     (magit-get "remote" (magit-get-current-remote) "url"))
           (magit-get-current-branch))))
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map))


;;; Git Timemachine
(use-package git-timemachine
  :ensure t
  :defer t)

;;; Ace Jump Mode

(use-package ace-jump-mode
  :ensure t
  :defer t
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-c C-SPC" . ace-jump-mode)))


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


;;; Tramp Mode

(use-package tramp
  :ensure t
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '((regexp-quote (system-name)) nil nil)))


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
  (defun jez-buffer-abbreviate ()
    "docstring"
    (interactive)
    (let ((directory-name (file-name-nondirectory (directory-file-name (file-name-directory buffer-file-name))))
          (buffer-abbrev (jez-abbreviate (buffer-name (current-buffer)))))
      (if (string-match "[[:alpha:]]" buffer-abbrev)
          buffer-abbrev
        directory-name)))

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
  :config
  (defun python-args-to-docstring-numpy ()
    "return docstring format for the python arguments in yas-text"
    (let* ((indent (concat "\n" (make-string (current-column) 32)))
           (args (python-split-args yas-text))
           (format-arg (lambda(arg)
                         (concat (nth 0 arg) " : " (if (nth 1 arg) ", optional") "\n")))
           (formatted-params (mapconcat format-arg args indent))
           (formatted-ret (mapconcat format-arg (list (list "out")) indent)))
      (unless (string= formatted-params "")
        (mapconcat 'identity
                   (list (format "%sParameters%s---------" indent indent) formatted-params
                         (format "%sReturns%s------" indent indent) formatted-ret)
                   indent))))
  :after yasnippet)


;;; Auto Complete

(use-package setup-hippie
  :bind (("C-." . hippie-expand-no-case-fold)
         ("C-:" . hippie-expand-lines)
         ("C-," . jez-helm-hippie-expand))

  :config
  ;; https://stackoverflow.com/questions/6515009/how-to-configure-emacs-to-have-it-complete-the-path-automatically-like-vim/6556788#6556788

  (defun jez-hippie-expand-completions (&optional hippie-expand-function)
    "Return the full list of possible completions generated by `hippie-expand'.
The optional argument can be generated with `make-hippie-expand-function'."
    (require 'cl)
    (let ((this-command 'jez-hippie-expand-completions)
          (last-command last-command)
          (buffer-modified (buffer-modified-p))
          (hippie-expand-function (or hippie-expand-function 'hippie-expand)))
      (cl-flet ((ding)) ; avoid the (ding) when hippie-expand exhausts its options.
        (while (progn
                 (funcall hippie-expand-function nil)
                 (setq last-command 'jez-hippie-expand-completions)
                 (not (equal he-num -1)))))
      ;; Evaluating the completions modifies the buffer, however we will finish
      ;; up in the same state that we began.
      (set-buffer-modified-p buffer-modified)
      ;; Provide the options in the order in which they are normally generated.
      (delete he-search-string (reverse he-tried-table))))

  (defmacro jez-helm-hippie-expand-with (hippie-expand-function)
    "Generate an interactively-callable function that offers helm-based completion
using the specified hippie-expand function."
    `(call-interactively
      (lambda (&optional selection)
        (interactive
         (let ((options (jez-hippie-expand-completions ,hippie-expand-function)))
           (if options
               (list (helm-comp-read "Completions: " options)))))
        (if selection
            (he-substitute-string selection t)
          (message "No expansion found")))))

  (defun jez-helm-hippie-expand ()
    "Offer helm-based completion for the word at point."
    (interactive)
    (jez-helm-hippie-expand-with 'hippie-expand)))


(use-package auto-complete
  :ensure t
  :defer t
  :config (global-auto-complete-mode -1))


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
  (defun jez-python-mode-hook ()
    (setq truncate-lines t))

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

  (setq python-shell-interpreter "python3")
  (setq elpy-rpc-python-command "python3")
  (define-key python-mode-map (kbd "C-c C-p") nil)

  ;; django factory boy
  (defun jez-django-model-to-factory ()
    "Convert Django Data Model to factory"
    (interactive)
    (let ((regex-configs
           '(
             ("= models.*bool[^)]*)" . "= factory.fuzzy.FuzzyChoice([True, False])")
             ("= models.*integer[^)]*)" . "= factory.Sequence(lambda n: n)")
             ("= models.*text[^)]*)" . "= factory.fuzzy.FuzzyText(length=10)")
             ("= models.*char[^)]*)" . "= factory.fuzzy.FuzzyText(length=10)")
             ("= models.*decimal[^)]*)" . "= factory.fuzzy.FuzzyDecimal(low=0.01, high=100000.00, precision=4)")
             ("= models.*float[^)]*)" . "= factory.fuzzy.FuzzyDecimal(low=0.01, high=100000.00, precision=4)")
             ("= models.*json[^)]*)" . "= None")
             ("= models..*datetime[^)]*)" . "= factory.Sequence(lambda n: timezone.now() + datetime.timedelta(days=n))")
             ("= models.TimeField[^)]*)" . "= factory.LazyFunction(timezone.now().time)")
             ("= models.UUID[^)]*)" . "= factory.LazyFunction(uuid.uuid4)")
             ("= models.Email[^)]*)" . "= factory.LazyFunction(lambda: '{faker.word()}@{faker.word()}.com')")
             ("= models.ForeignKey(\\([^,]*\\),[^)]*)" . "= factory.SubFactory(\\1Factory)")
             ("= models.OneToOneField(\\([^,]*\\),[^)]*)" . "= factory.SubFactory(\\1Factory)")
             ("^class \\(.*\\)(.*):" . "class \\1Factory(factory.django.DjangoModelFactory):")
             ("FactoryFactory" . "Factory")
             )))
      (save-excursion
        (loop for i in regex-configs
              do (jez-replace-regexp (car i) (cdr i)))))
    (flush-lines "AutoSlugField")
    (flush-lines "#")
    (flush-lines "^ +id = "))
  :hook ((python-mode . outshine-python-mode-hook)
         (python-mode . jez-python-mode-hook)))


;;; Elpy

(use-package elpy
  :ensure t
  :defer t
  :bind (
         :map elpy-mode-map
              ("C-c C-l f" . elpy-autopep8-fix-code)
              ;; ("C-c C-l f" . elpy-yapf-fix-code)
              )
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-rpc-timeout 10)
  (define-key elpy-mode-map (kbd "C-c C-p") nil)
  (defun jez-python-disable-company-mode () (company-mode -1))
  (defun jez-disable-elpy () (ignore-errors (elpy-mode -1)))
  (setq elpy-shell-echo-output nil)
  (defun jez-toggle-aggressive-pep8 (args)
    "Toggle auto-pep8 on save"
    (interactive "P")
    (if (eq 'elpy-autopep8-fix-code (car before-save-hook))
        (progn
          (remove-hook 'before-save-hook 'elpy-autopep8-fix-code t)
          (message "aggressive pep8 disabled"))
      (add-hook 'before-save-hook 'elpy-autopep8-fix-code nil t)
      (message "aggressive pep8 enabled")))
  :hook ((org-mode . jez-disable-elpy)
         (shell-mode . jez-disable-elpy)
         ;; (python-mode . flymake-mode)
         (python-mode . jez-python-disable-company-mode)))


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


;;; SQL Mode

(use-package sql
  :ensure t
  :defer t
  :mode ("\\.sqltemplate\\'")
  :bind (
         :map sql-interactive-mode-map
         ("C-c u" . sqlup-capitalize-keywords-in-region)
         ("M-<return>" . comint-send-input)
         :map sql-mode-map
         ("C-c C-c" . jez-sql-send-paragraph)
         ("C-c C-n" . jez-sql-send-paragraph-move-forward)
         ("C-<return>" . jez-sql-send-paragraph-move-forward)
         ("C-c C-l c" . sql-connect)
         ("C-c C-l d" . jez-sql-view-columns)
         ("C-c C-l e" . jez-sql-explain-region)
         ("C-c C-l a" . jez-sql-explain-analyze-region)
         ("C-c C-l x" . jez-sql-expand)
         ("C-c C-l 1" . jez-sql-view-single-record)
         ("C-c C-l l" . jez-sql-count-table)
         ("C-c C-l z" . jez-sql-view-table-size)
         ("C-c t" . jinja2-insert-tag)
         ("C-c v" . jez-jinja-insert-var)
         )
  ;; :hook (
  ;;        (sql-mode . sqlup-mode)
  ;;        (sql-interactive-mode  . sqlup-mode)
  ;;        )
  :config
  (defun jez-jinja-insert-var ()
    "Insert an empty tag"
    (interactive)
    (insert "{{ ")
    (save-excursion
      (insert " }}")))

  (defun jez-sql-table-at-line ()
    "Extract table name in current line"
    (interactive)
    (let* ((table (thing-at-point 'line))
           (table (s-trim table))
           (table (s-replace-regexp " .*" "" table)))
      table))

  (defun jez-sql-paragraph-at-point ()
    (interactive)
    (let ((start (save-excursion
                   (condition-case nil
                       (progn
                         (search-backward ";")
                         (forward-char 1)
                         (point))
                     (error
                      (point-min)))))
          (end (save-excursion
                 (condition-case nil
                     (progn
                       (search-forward ";")
                       (point))
                   (error
                    (point-max))))))
      (buffer-substring-no-properties start end)))

  (defun jez-sql-table-at-point ()
    (interactive)
    (let ((statement (jez-sql-paragraph-at-point)))
      (s-replace-regexp "[[:ascii:]]*from[[:space:]\n]*\\([^[:space:]\n]*\\)[[:ascii:]]*"
                        "\\1"
                        statement)))

  (defun jez-sql-send-string (sql)
    "Send string to sql-buffer and ensures semicolon"
    (interactive)
    (set-window-buffer (nth 1 (window-list)) (sql-find-sqli-buffer))
    (let ((sql-trimmed (s-replace-regexp ";+$" "" sql)))
      (sql-send-string (format "%s;" sql-trimmed))))

  (defun jez-sql-view-table-size ()
    (interactive)
    (let* ((table (jez-sql-table-at-line)))
      (sql-send-string (format "select '%s' tablename, pg_size_pretty(pg_total_relation_size('%s')) size;" table table))))

  (defun jez-sql-view-columns ()
    "view column of table under cursor"
    (interactive)
    (let ((table (jez-sql-table-at-line)))
      (sql-send-string (format "\\d %s" table))))

  (defun jez-sql-view-single-record ()
    "view a single record of table under cursor"
    (interactive)
    (let ((table (jez-sql-table-at-line)))
      (sql-send-string (format "select * from  %s  limit 1;" table))))

  (defun jez-sql-count-table ()
    "view a single record of table under cursor"
    (interactive)
    (let ((table (jez-sql-table-at-line)))
      (sql-send-string (format "select count(*) from  %s ;" table))))

  (defun jez-sql-explain-region (arg)
    "run explain on region"
    (interactive "P")
    (let ((paragraph (if mark-active
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (jez-sql-paragraph-at-point))))
      (when arg
        (with-current-buffer sql-buffer
          (goto-char (point-max))
          (comint-clear-buffer)))
      (jez-sql-send-string (format "explain %s" paragraph))))

  (defun jez-sql-explain-analyze-region (arg)
    "run explain on region"
    (interactive "P")
    (let ((paragraph (if mark-active
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (jez-sql-paragraph-at-point))))
      (when arg
        (with-current-buffer sql-buffer
          (goto-char (point-max))
          (comint-clear-buffer)))
      (jez-sql-send-string (format "explain analyze %s" paragraph))))

  (defun jez-sql-expand ()
    "toggle expand on sql buffer"
    (interactive)
    (sql-send-string "\\x"))

  (defun jez-sql-format-multiline-comma (arg)
    "Converts values between parenthesis to multiline"
    (interactive "P")
    (save-excursion
      (goto-char (point-min))
      (ignore-errors
        (while t
          (search-forward "(")
          (when (looking-at "[^)]*,.*)")
            (save-excursion
              (call-interactively 'newline)
              (insert "    ")
              (while (looking-at "[^)]*,.*)")
                (search-forward ",")
                (call-interactively 'newline))
              (search-forward ")")
              (backward-char)
              (call-interactively 'newline)
              (delete-backward-char tab-width)))))))

  (defun jez-sqlformat-buffer (&optional DISPLAY-ERRORS)
    (interactive)
    (sqlformat-buffer 'DISPLAY-ERRORS)
    (let ((regex-configs
           '(
             ("drop table if exists \\(.*\\);" . "drop table if exists\n    \\1\n;")
             ("drop table \\(.*\\);" . "drop table\n    \\1\n;")
             ("create temp table \\(.*\\) as (" . "create temp table\n    \\1\nas (")
             ("create temp table \\(.*\\) as" . "create temp table\n    \\1\nas")
             ("create table if not exists \\(.*\\) (" . "create table if not exists\n    \\1\n (")
             ("create table \\(.*\\) (" . "create table \n    \\1\n (")
             ("create table \\(.*\\) as" . "create table \n    \\1\nas")
             ("\\( *\\)    join \\(.*\\) on \\(.*\\)" . "\\1join\n\\1    \\2\n\\1on\n\\1    \\3")
             ("\\(.*\\)    left join \\(.*\\) on \\(.*\\)" . "\\1left join\n\\1    \\2\n\\1on\n\\1    \\3")
             ("\\(.*\\)on\n\\(.*\\)\n +and \\(.*\\)" . "\\1on\n\\2\n\\1    and \\3")
             ("create index on \\(.*\\) (\\(.*\\));" . "create index on\n    \\1 (\n        \\2\n    )\n;")
             ("create index \\(.*\\) on \\(.*\\) (\\(.*\\));" . "create index\n    \\1\non\n    \\2\n(\n    \\3\n);")
             ("create index \\(.*\\) on \\(.*\\) using \\(.*\\)" . "create index\n    \\1\non\n    \\2\nusing\n    \\3")
             ("alter table \\(.*\\) rename to \\(.*\\);" . "alter table\n    \\1\nrename to\n    \\2\n;")
             ("alter index \\(.*\\) rename to \\(.*\\);" . "alter index\n    \\1\nrename to\n    \\2\n;")
             ("    add constraint \\(.*\\) foreign key \\(.*\\) references \\(.*\\) \\(deferrable.*\\)" . "add constraint\n    \\1\nforeign key\n    \\2\nreferences\n    \\3\n\\4")
             ("\\(.*\\)limit \\(.*\\)" . "\\1limit\n\\1    \\2")
             ;; ("begin\n;" . "begin;")
             (" -> " . "->")
             (" ->> " . "->>")
             ("\\(.*\\)alter table \\(.*\\) set schema \\(.*\\)" . "\\1alter table\n\\1    \\2\nset schema\n    \\3")
             ("\\(.*\\)alter table \\(.*\\)" . "\\1alter table\n\\1    \\2")
             ("\\(.*\\)    add column \\(.*\\)" . "\\1add column\n\\1    \\2")
             ("\\(.*\\)    add primary key \\(.*\\)" . "\\1add primary key\n\\1    \\2")
             (" \\{1\\}::" "::")
             ("generate_series\\(.*\\)\n)" "generate_series\\1)")
             ("row_number() over\\(.*\\)\n)" "row_number() over\\1)")
             ("create schema \\(.*\\);" "create schema\n    \\1\n;")
             ("\\()\\|'\\);" . "\\1\n;")
             ("^)\n;" . ");")
             ("\\([[:alnum:]]+\\);" . "\\1\n;")
             ("\\(commit\\|begin\\|end\\)\n;" . "\\1;")
             ("delete from \\(.*\\) using \\(.*\\)" . "delete from\n    \\1\nusing\n    \\2")
             ("delete from \\(.*\\)" . "delete from\n    \\1")
             ("insert into \\(.*\\)" . "insert into\n    \\1")
             ("where \\(.*\\)" . "where\n    \\1")
             ("\\(.*\\)    alter column \\(.*\\)" . "\\1alter column\n\\1    \\2")
             ("\\(.*\\)    using \\(.*\\)" . "\\1using\n\\1    \\2")
             ("    \\(using.*\\\)" . "\\1")
             ("    drop constraint \\(.*\\);" . "drop constraint\n    \\1\n;")
             )))
      (save-excursion
        (loop for i in regex-configs
              do (replace-regexp (car i) (cdr i) nil (point-min) (point-max)))))
    )

  (defun print-to-file (filename data)
    (with-temp-file filename
      (prin1 data (current-buffer))))

  (defun read-from-file (filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (cl-assert (eq (point) (point-min)))
      (read (current-buffer))))

  (defun jez-sql-send-paragraph ()
    (interactive)
    (let ((paragraph (jez-sql-paragraph-at-point)))
      (jez-sql-send-string paragraph)))

  (defun jez-sql-send-paragraph-move-forward ()
    (interactive)
    (jez-sql-send-paragraph)
    (condition-case nil
        (search-forward ";")
      (error
       (save-excursion
         (search-backward ";" nil nil 2)
         (jez-sql-send-paragraph))))
    (ignore-errors
        (progn
          (next-line)
          (back-to-indentation))))

  (defun jez-sql-list-tables (&optional arg)
    "List tables available in current SQL process"
    (interactive "P")
    (let ((sql-buffer-process (sql-find-sqli-buffer))
          (sql-command "\\dt *.*")
          (buffer-out "*sql-result*"))
      (get-buffer-create buffer-out)
      (sql-redirect  sql-buffer-process "\\x off" buffer-out)
      (sql-redirect  sql-buffer-process sql-command buffer-out)
      (with-current-buffer buffer-out
        (buffer-disable-undo)
        (keep-lines ".*|.*|.*|.*")
        (save-excursion
          (goto-char (point-min))
          (kill-line 1))
        (jez-replace-regexp " " "")
        (jez-replace-regexp "^\\(.*\\)|\\(.*\\)|.*|.*" "\\1.\\2")
        (s-split "[[:space:]]" (buffer-substring-no-properties 1 (point-max))))))

  (defun jez-sql-list-tables-cached (&optional arg)
    "Cached version of `jez-sql-list-tables`''"
    (interactive "P")
    (let* ((file-name (format "jez-sql-list-tables-cached-%s-%s-%s.el" sql-server sql-database sql-port))
           (file-path (expand-file-name file-name temporary-file-directory))
           (time-now (string-to-number (format-time-string "%s" (current-time))))
           tables)
      (when (and (file-exists-p file-path) (not arg))
        (let* ((data (read-from-file file-path))
               (time-cached (cdr (assoc 'time data)))
               (secs-passed (-  time-now time-cached)))
          (when (> 3600 secs-passed)
            (setq tables (cdr (assoc 'tables data))))))
      (unless tables
        (setq tables (jez-sql-list-tables arg))
        (print-to-file file-path
                       `((tables . ,tables)
                         (time . ,time-now))))
      tables))

  (defun jez-sql-connect (connection &optional new-name)
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

  (defun jez-sql-mode-hook ()
    (setq-default tab-width 4))

  (defun jez-sql-fk-to-cascade ()
    "Convert foreign key description from \d+ `table' to cascade"
    (interactive)
    (jez-replace-regexp " *\"\\(.*\\)\" FOREIGN KEY (\\(.*\\)) REFERENCES \\(.*\\)(\\(.*\\)) DEFERRABLE INITIALLY DEFERRED"
                        "alter table {{table}}
drop constraint \\1,
add constraint \\3
foreign key (\\2)
references \\3(\\4)
on delete cascade;"
))


  (define-key sql-interactive-mode-map (kbd "RET") nil)
  (toggle-truncate-lines t)
  :hook ((sql-mode . jez-sql-mode-hook)))

(use-package sqlformat
  :ensure t
  :config
  (setq sqlformat-command 'pgformatter) ; brew install pgformatter
  (setq sqlformat-args '("-g"
                         "--keyword-case" "1"))
  :after sql
  :bind (:map sql-mode-map
              ("C-c C-l f" . jez-sqlformat-buffer)
              ("C-c C-l r" . sqlformat-region)))

;;; Undo Tree Mode

(use-package undo-tree
  :commands undo-tree-mode
  :ensure t
  :defer t
  :bind ("C-x u" . undo-tree-visualize)
  :config (undo-tree-mode t))


;;; Plantuml - For diagram and UML

(use-package plantuml-mode
  :ensure t
  :defer t
  :commands plantuml-mode
  :config
  (setq org-plantuml-jar-path "~/.emacs.d/elpa/contrib/scripts/plantuml.jar")
  (setq plantuml-jar-path "~/.emacs.d/elpa/contrib/scripts/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-indent-regexp-start (list plantuml-indent-regexp-block-start
                                           plantuml-indent-regexp-group-start
                                           plantuml-indent-regexp-activate-start
                                           plantuml-indent-regexp-box-start
                                           plantuml-indent-regexp-ref-start
                                           plantuml-indent-regexp-legend-start
                                           plantuml-indent-regexp-note-start
                                           plantuml-indent-regexp-newif-start
                                           plantuml-indent-regexp-loop-start
                                           plantuml-indent-regexp-fork-start
                                           plantuml-indent-regexp-title-start
                                           plantuml-indent-regexp-header-start
                                           plantuml-indent-regexp-footer-start
                                           plantuml-indent-regexp-macro-start
                                           plantuml-indent-regexp-oldif-start
                                           plantuml-indent-regexp-user-control-start
                                           ".*!foreach.*$"
                                           ".*!procedure.*$"
                                           "^ *repeat *$"
                                           ))
  (setq plantuml-indent-regexp-end (list plantuml-indent-regexp-block-end
                                         plantuml-indent-regexp-group-end
                                         plantuml-indent-regexp-activate-end
                                         plantuml-indent-regexp-box-end
                                         plantuml-indent-regexp-ref-end
                                         plantuml-indent-regexp-legend-end
                                         plantuml-indent-regexp-note-end
                                         plantuml-indent-regexp-newif-end
                                         plantuml-indent-regexp-loop-end
                                         plantuml-indent-regexp-fork-end
                                         plantuml-indent-regexp-title-end
                                         plantuml-indent-regexp-header-end
                                         plantuml-indent-regexp-footer-end
                                         plantuml-indent-regexp-macro-end
                                         plantuml-indent-regexp-oldif-end
                                         plantuml-indent-regexp-user-control-end
                                         ".*!endfor.*$"
                                         ".*!endprocedure.*$"
                                         ".*repeat while.*"
                                         ))
  (setq plantuml-indent-level 4)
  )


;;; Nyan Mode

(use-package nyan-mode
  :ensure t
  :defer 2
  :config
  (nyan-mode 1))


;;; Prodigy

(use-package prodigy
  :commands prodigy
  :ensure t
  :defer t
  :config
  ;; we are now moving to .emacs.d.local
  (when (file-exists-p "~/.emacs.d/prodigy-settings.el")
    (ignore-errors
     (copy-file "~/.emacs.d/prodigy-settings.el" "~/.emacs.d.local/")))
  (when (file-exists-p "~/.emacs.d.local/prodigy-settings.el")
    (load-file "~/.emacs.d.local/prodigy-settings.el"))
  (setq prodigy-view-buffer-maximum-size t))


;;; HackerNews

(use-package hackernews
  :commands hackernews
  :ensure t
  :defer t)


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


;;; VIM's - Change Inner

(use-package change-inner
  :ensure t
  :defer t
  :bind (("M-I" . change-inner)
         ("M-O" . change-outer)
         ("s-i" . jez-copy-inner)
         ("s-o" . jez-copy-outer))
  :config
  (load-file "~/.emacs.d/custom-ci.el"))


;; Avy Zap

(use-package avy-zap
  :ensure t
  :defer t
  :bind (
         ("M-z" . zap-to-char)
         ("M-Z" . avy-zap-up-to-char-dwim)))


;;; Bash - run bash init script

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))


;;; Emmet Mode

(use-package emmet-mode
  :ensure t
  :defer t
  :hook ((sgml-mode . emmet-mode)
         (css-mode . emmet-mode)
         (web-mode . emmet-mode)))


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
    (setq-local outline-level 'sh-outline-level))
  (defun jez-shell-mode-hook ()
    (comint-send-string (get-buffer-process (current-buffer)) "export TERM=xterm\n"))
  :bind (("s-k" . comint-clear-buffer)
         ("M-k" . comint-clear-buffer))
  :hook ((sh-mode . outshine-sh-mode-hook)
         (shell-mode . jez-shell-mode-hook))
  )


;;; Web Mode

(use-package pug-mode
  :defer t
  :ensure t
  :mode ("\\.pug\\'")
  :config
  (setq pug-tab-width 2)
  (defun jez-pug-mode-hook ()
    (setq-local indent-tabs-mode nil))
  :hook (pug-mode . jez-pug-mode-hook))

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
  (defun jez-reindent-html-region ()
    "Reindent currently selected region using web-mode"
    (interactive)
    (let ((start (region-beginning))
          (end (region-end))
          (current-buffer (current-buffer)))
      (with-temp-buffer
        (web-mode)
        (insert-buffer-substring current-buffer start end)
        (mark-whole-buffer)
        (indent-for-tab-command)
        ;; (web-mode-dom-normalize)
        (let ((temp-buffer (current-buffer)))
          (with-current-buffer current-buffer
            (delete-region start end)
            (insert-buffer temp-buffer))))))
  (defun jez-web-mode-hook ()
    (electric-pair-local-mode -1)
    (toggle-truncate-lines t)
    (web-mode-set-engine "django"))
  :hook ((web-mode . jez-web-mode-hook)))


;;; JS Mode

(use-package js2-mode
  :ensure t
  :defer t
  :bind (:map js2-mode-map
              ("C-c n" . js2-next-error))
  :config
  (setq js-indent-level 4)
  :mode (("\\.js\\'" . js2-mode)))

;;; Typescript Mode
(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode))
  )


;;; CSS Mode

(use-package css-mode
  :ensure t
  :defer t
  :commands jez-css-minify
  :bind (:map css-mode-map ("C-c m" . jez-css-minify))
  :config
  (defun jez-css-minify-uglify ()
    "CSS Minify current buffer using `uglify'"
    (let ((minified (shell-command-to-string (format "uglifycss %s" buffer-file-name))))
      (erase-buffer)
      (insert minified)))
  (defun jez-css-minify-requests ()
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
  (defun jez-css-minify ()
    "CSS Minify current buffer"
    (interactive)
    (if (executable-find "uglifycss")
        (jez-css-minify-uglify)
      (jez-css-minify-requests))))


;;; Occur mode

(use-package replace
  :defer t
  :bind (:map occur-mode-map
         ("p" . jez-occur-prev)
         ("n" . jez-occur-next))
  :config
  (defun jez-occur-prev ()
    "Same with `occur-prev' but also move to occurence"
    (interactive)
    (occur-prev)
    (occur-mode-display-occurrence))

  (defun jez-occur-next ()
    "Same with `occur-next' but also move to occurence"
    (interactive)
    (occur-next)
    (occur-mode-display-occurrence))

  (defun jez-occur-mode-hook ()
    (toggle-truncate-lines t))
  :hook ((occur-mode . jez-occur-mode-hook)))



;;; Elisp Mode

(use-package elisp-mode
  :defer t
  :bind (:map emacs-lisp-mode-map
         ("C-x C-a d" . jez-describe-symbol-at-point))
  :init
  (defun jez-describe-symbol-at-point (arg)
    "Describe current symbol on point on other window"
    (interactive "p")
    (describe-symbol (symbol-at-point))
    (when (< arg 4)
      (other-window 1)))

  (defun jez-emacs-lisp-mode-hook ()
    (interactive)
    (setq indent-tabs-mode nil)
    (toggle-truncate-lines t))

  (defun outshine-emacs-lisp-mode-hook ()
    (setq-local outshine-use-speed-commands t)
    (outline-minor-mode t))

  :hook ((emacs-lisp-mode . jez-emacs-lisp-mode-hook)
         (emacs-lisp-mode . outshine-emacs-lisp-mode-hook)))


;;; Docker

(use-package docker :ensure t :defer t)
(use-package docker-tramp :ensure t :defer t)


;;; YAML Mode

(use-package yaml-mode
  :ensure t
  :defer t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.template\\'" . yaml-mode))
  :config
  (defun yaml-outline-level ()
    (let (buffer-invisibility-spec)
      (save-excursion
        (skip-chars-forward (rx (repeat 2 space)))
        (current-column))))
  (defun jez-yaml-mode-hook ()
    (outline-minor-mode t)
    (setq outline-regexp (rx (* (group (repeat 2 space))) alnum))
    (setq outline-level 'yaml-outline-level))
  :hook (yaml-mode . jez-yaml-mode-hook))


;;; Markdown Mode

(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (defun jez-markdown-mode-hook ()
    (toggle-truncate-lines t)
    (jez-outline-mode-adhoc "#")
    (setq-local outshine-use-speed-commands t))
  :hook ((markdown-mode . jez-markdown-mode-hook)
         (markdown-mode . auto-fill-mode)))


;;; Latex Mode

(use-package tex-mode
 :defer t
 :config
 (defun jez-latex-mode-hook ()
   (setq-local TeX-save-query nil)
   (setq-local TeX-command-force "Latex"))
 :hook ((latex-mode . jez-latex-mode-hook)
        (LaTeX-mode . jez-latex-mode-hook)))


;;; Nginx Mode

(use-package nginx-mode
  :ensure t
  :defer t
  :mode ("Caddyfile\\'" . nginx-mode))


;;; Visual Regexp

(use-package visual-regexp
 :ensure t
 :defer t
 :bind (("C-c r" . replace-regexp)
        ("C-c %" . vr/query-replace)
        ("M-%" . vr/query-replace)
        ("C-M-/" . vr/mc-mark)))


;;; Avy

(use-package avy
  :ensure t
  :defer t
  :bind* (("M-/" . avy-goto-char-timer)
          ("C-;" . avy-goto-char-timer)
          ("M-g f" . avy-goto-line)
          ("M-g y" . avy-copy-line))
  :config
  (avy-setup-default))


;;; Transpose Mark

(use-package transpose-mark
  :ensure t
  :defer t
  :commands (transpose-mark
             transpose-mark-line
             transpose-mark-region))


;;; Dumb-Jump Mode

(use-package dumb-jump
 :ensure t
 :defer t
 :bind* ("C-M-g" . dumb-jump-go)
 :config (setq dumb-jump-selector 'helm)
 :hook (python-mode . dumb-jump-mode))


;;; Terraform Mode
(use-package terraform-mode
 :ensure t
 :defer t
 :mode (("\\.tf\\'" . terraform-mode)))


;;; Synosaurus Mode
(use-package synosaurus
  :ensure t
  :defer t
  :hook (org-mode . synosaurus-mode))


;;; PHP Mode
(use-package php-mode
  :defer
  :config
  (define-key php-mode-map (kbd "C-.") nil)
  (define-key php-mode-map (kbd "C-:") nil)
  (define-key php-mode-map (kbd "C-,") nil)
  (defun jez-php-mode-hook ()
    (setq-local c-basic-offset 4))
  :hook (php-hook . jez-php-mode-hook))


;;; Jenkins
(use-package jenkins
  :ensure t
  :defer t
  :commands butler-server-list
  :bind (:map
         jenkins-mode-map
         ("t" . jez-jenkins--visit-trend-from-main-screen)
         ("c" . jez-jenkins--visit-config-from-main-screen))
  :config
  (defun jez-jenkins--visit-trend-from-main-screen ()
    "Open browser trend for current job"
    (interactive)
    (let ((jobname (tabulated-list-get-id)))
      (browse-url (format "%sjob/%s/buildTimeTrend" (get-jenkins-url) jobname))))
  (defun jez-jenkins--visit-config-from-main-screen ()
    "Open browser trend for current job"
    (interactive)
    (let ((jobname (tabulated-list-get-id)))
      (browse-url (format "%sjob/%s/configure" (get-jenkins-url) jobname)))))



;;; Ein
(use-package ein
  :ensure t
  :commands (ein:notebooklist-open))


;;; Dimmer
(use-package dimmer
  :ensure t
  :defer t
  :init
  ;TODO: performance issue. lag when switching app
  (dimmer-mode -1)
  :config
  ;TODO: exclude dimmer on swiper
  (setq dimmer-prevent-dimming-predicates '(helm--alive-p window-minibuffer-p))
  (setq dimmer-exclusion-regexp-list
        '(
          " \\*\\(LV\\|transient\\)\\*"
          "*LV*"
          "^*Messages*"
          "^.\\*Echo.*\\*"
          "^.\\*which-key\\*$"
          "^\\*Minibuf-[0-9]+\\*"
          "^\\*[h|H]elm.*\\*"
          ".*magit-diff.*"
          "\\*Help\\*"
          ))
  (setq dimmer-fraction 0.50))


;;; Uniquify
(use-package emacs
  :ensure t
  :defer t
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))


;;; Beacon
(use-package beacon
  :ensure t
  :defer t
  :init
  (beacon-mode 1)
  :config
  (setq-default beacon-lighter "")
  (setq-default beacon-size 20))


;;; Keyfreq
(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (make-directory "~/.emacs.d/data" :parent)
  (setq keyfreq-file "~/.emacs.d/data/keyfreq")
  (keyfreq-autosave-mode 1))


;;; Jinja
(use-package jinja2-mode
  :ensure t
  :defer t
  :mode (("\\.j2\\'" . jinja2-mode)
         ("\\.jinja2\\'"  . jinja2-mode)))


;;; Move Dup
(use-package move-dup
  :ensure t
  :defer t
  :bind (("M-<up>" . md-move-lines-up)
         ("M-<up>" . md-move-lines-up)
         ("M-<down>" . md-move-lines-down)
         ("M-S-<up>" . md-move-lines-up)
         ("M-S-<down>" . md-move-lines-down)
         ("C-c d" . md-duplicate-down)
         ("C-c u" . md-duplicate-up)))


;;; Rainbow Delimiters
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (python-mode . rainbow-delimiters-mode)))

;TODO: add sessions

(use-package desktop
  :ensure t
  :config
  (setq desktop-path '("~/.emacs.d/"))
  (setq desktop-dirname "~/.emacs.d/")
  (setq desktop-base-file-name "emacs-desktop")
  ;; disable unable to recover from crash
  (desktop-save-mode -1))

(use-package symbol-overlay
  :ensure t
  :config
  :bind (
         ("s-m" . symbol-overlay-put)
         ("s-M" . symbol-overlay-remove-all)
         ("s-n" . symbol-overlay-jump-next)
         ("s-p" . symbol-overlay-jump-prev)
         ))

(use-package copy-as-format
  :ensure t)

;;; Scratch
(use-package scratch
  :ensure t)

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode -1))

;;; Startup

(defun jez-show-bookmarks ()
  (call-interactively 'list-bookmarks))

(defun jez-display-init-time ()
  (defun jez-show-init-load-time ()
    (message "It took %s to load emacs" (emacs-init-time)))
  (advice-add 'display-startup-echo-area-message :after #'jez-show-init-load-time))

(defun jez-startup ()
  "Run after emacs init"
  (jez-display-init-time)
  (jez-show-bookmarks))

(add-hook 'after-init-hook 'jez-startup)
(put 'scroll-left 'disabled nil)
