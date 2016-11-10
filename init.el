;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs core

;; disable minimize
(global-unset-key (kbd "C-z"))

;; disable full yes no - "Resist the temptation to guess"
(defalias 'yes-or-no-p 'y-or-n-p)

;; sort lines
(global-set-key (kbd "<f5>") 'sort-lines)
;; set function as ctrl
(setq ns-function-modifier 'control)

;; change quit key
(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)

;; delete blank lines
(global-set-key (kbd "C-c C-<return>") 'delete-blank-lines)

;; regex replace
(global-set-key (kbd "C-c r") 'query-replace-regexp)

;; back to indentation
(global-set-key (kbd "M-i") 'back-to-indentation)

;; set abbrev silently
(setq save-abbrevs 'silently)

;; electric mode
(electric-pair-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Manager - el-get
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
;; Package Manager - melpa

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; for important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line


;; org emacs lisp package archive
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Dependencies

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
(add-to-list 'load-path settings-dir)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install missing dependencies
(when (not (package-installed-p 'dash))
  (package-install 'dash))

(require 'setup-package)

(defun init--install-packages ()
  (packages-install
   '(expand-region
     helm
     ob-http
     ob-redis
     ob-ipython
     org-plus-contrib
     yasnippet
     key-chord
     iy-go-to-char
     magit
     window-numbering
     multiple-cursors
     nyan-mode
     go-mode
     ob-go
     prodigy
     ace-jump-mode
     hackernews
     redis
     sublime-themes
     cyberpunk-theme
     dockerfile-mode
     yaml-mode
     lorem-ipsum)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs GUI
(load-theme 'cyberpunk t)
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Remove scrollbar, menu bars, and toolbars
; when is a special form of "if", with no else clause, it reads:
; (when <condition> <code-to-execute1> <code-to-execute2> ...)
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; maximize window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; no splash screen please ... jeez
(setq inhibit-startup-message t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expand Region

(require 'expand-region)
(global-set-key (kbd "C-=")  'er/expand-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple Cursor

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-c C-S-a") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-S-c C-S-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm

(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

;; set helm hotkeys
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; override buffer list
(global-set-key (kbd "C-x b") 'helm-buffers-list)

(helm-mode 1)
(setq projectile-global-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit

(global-set-key (kbd "C-x g") 'magit-status)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("38e64ea9b3a5e512ae9547063ee491c20bd717fe59d9c12219a0b1050b439cdd" "4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" "a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" default)))
 '(helm-mode t)
 '(linum-format " %7i ")
 '(markdown-command "/usr/local/bin/pandoc")
 '(org-agenda-files (quote ("~/organizer.org" "~/workspace/tasks/tasks.org")))
 '(package-selected-packages
   (quote
    (material-theme lorem-ipsum sublime-themes nyan-mode visual-regexp multiple-cursors yasnippet window-numbering org-plus-contrib ob-ipython ob-http magit key-chord iy-go-to-char helm expand-region)))
 '(projectile-global-mode t)
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
;; Org mode

;; Disable backup file
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

;; Enable task capture
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-export-coding-system 'utf-8)
(setq org-log-done 'time)

;; Enable languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (http . t)
   (python . t)
   (ipython . t)
   (sh . t)
   (js . t)
   (http . t)
   (dot . t)
   (sql . t)
   (ditaa . t)
   (plantuml . t)
   (go . t)
   ))

;; Home Directory
(global-set-key (kbd "C-c o")
		(lambda () (interactive) (find-file "~/organizer.org")))

;; set register
(set-register ?o (cons 'file "~/organizer.org"))

;; Use org-refile to file or jump to headings
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

;; Save all capture to single file
(setq org-default-notes-file "~/organizer.org")

;; Disable prompt on source block eval
(setq org-confirm-babel-evaluate nil)

;; Display images in buffer after eval
(add-hook 'org-babel-execute-hook 'org-display-inline-images 'append)

;; center all images
(advice-add 'org-latex--inline-image :around
            (lambda (orig link info)
              (concat
               "\\begin{center}"
               (funcall orig link info)
               "\\end{center}")))

;; Set author
(setq user-full-name "Jezrael Arciaga")
(setq user-mail-address "jezarciaga@gmail.com")

;; enable markdown export
(eval-after-load "org"
  '(require 'ox-md nil t))

;; Send HTML email using org mode
(require 'org-mime)
(setq org-mime-library 'mml)

;; set code blocks background to dark
(add-hook 'org-mime-html-hook
          (lambda ()
            (org-mime-change-element-style
             "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                           "#E6E1DC" "#232323"))))

;; set block quotes offset
(add-hook 'org-mime-html-hook
          (lambda ()
            (org-mime-change-element-style
             "blockquote" "border-left: 2px solid gray; padding-left: 4px;")))

;; Update tasks state base on subtask
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; Syntax highlighting
(setq org-src-fontify-natively t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datetime

(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
		 ((not prefix) "%d.%m.%Y")
		 ((equal prefix '(4)) "%Y-%m-%d")
		 ((equal prefix '(16)) "%A, %d. %B %Y")))
	(system-time-locale "de_DE"))
    (insert (format-time-string format))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ace Jump mode

(add-to-list 'load-path "~/.emacs.d/ace-jump-mode")
(require 'ace-jump-mode)

(defun jez/ace-enable-key-bind ()
  "Allow C-c C-SPC to trigger ace jump mode"
  (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
  (define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode))
(jez/ace-enable-key-bind)

;; fix issue - not working in org mode
;; When org-mode starts it (org-mode-map) overrides the ace-jump-mode.
;; (add-hook 'org-mode-hook (jez/ace-enable-key-bind))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wind-move

(global-set-key (kbd "C-c C-j") 'windmove-left)
(global-set-key (kbd "C-c C-k") 'windmove-down)
(global-set-key (kbd "C-c C-l") 'windmove-up)
(global-set-key (kbd "C-c C-;") 'windmove-right)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trump mode

(require 'tramp)
(setq tramp-default-method "ssh")
;; Fix issue - sudo can only use the local host
(add-to-list 'tramp-default-proxies-alist
             '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '((regexp-quote (system-name)) nil nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippet

(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/yasnippet-snippets")
;; (add-hook 'prog-mode-hook #'yas-minor-mode)
(yas-global-mode 1)
(yas-reload-all)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippet custom

;; python google docstring. through =defg=
(defun python-args-to-google-docstring (text &optional make-fields)
  "Return a reST docstring format for the python arguments in yas-text."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args text))
     (nr 0)
         (formatted-args
      (mapconcat
       (lambda (x)
         (concat "   " (nth 0 x)
             (if make-fields (format " ${%d:arg%d}" (incf nr) nr))
             (if (nth 1 x) (concat " \(default " (nth 1 x) "\)"))))
       args
       indent)))
    (unless (string= formatted-args "")
      (concat
       (mapconcat 'identity
          (list "" "Args:" formatted-args)
          indent)
       "\n"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert pair

(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-{") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timestamp

;; insert todays date
(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
		 ((not prefix) "%d.%m.%Y")
		 ((equal prefix '(4)) "%Y-%m-%d")
		 ((equal prefix '(16)) "%A, %d. %B %Y")))
	(system-time-locale "en_US"))
    (insert (format-time-string format))))

(global-set-key (kbd "C-c d") 'insert-date)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto complete
(require 'setup-hippie)
(global-set-key "\M- " 'hippie-expand)
(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-:") 'hippie-expand-lines)
(global-set-key (kbd "C-,") 'completion-at-point)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jedi - temporary disable. feature is slow

;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Email - GNUS

;; Always on topic mode
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SQL Mode

(add-hook 'sql-mode-hook 'sqlup-mode)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)
(global-set-key (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Publish settings
(when (file-exists-p "~/.emacs.d/publish-settings.el")
  (load-file "~/.emacs.d/publish-settings.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ropemacs

;; make sure packages are installed
(when (not (el-get-package-exists-p 'ropemacs))
  (el-get-install 'ropemacs))
(when (not (el-get-package-exists-p 'pymacs))
  (el-get-install 'pymacs))

;; load pymacs
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-shortcuts t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keychord - temporary disabled. Adding latency on each keypress

(require 'key-chord)
(require 'iy-go-to-char)

(key-chord-mode 1)

;; move to char similar to "f" in vim, f+g forward, d+f backward
(key-chord-define-global "fj" 'iy-go-to-char)
(key-chord-define-global "df" 'iy-go-to-char-backward)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Numbering
(require 'window-numbering)
(window-numbering-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plantuml - For diagram and UML

(setq org-plantuml-jar-path "~/.emacs.d/elpa/contrib/scripts/plantuml.jar")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nyan Mode

(require 'nyan-mode)
(nyan-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prodigy

(require 'prodigy)

(when (file-exists-p "~/.emacs.d/prodigy-settings.el")
  (load-file "~/.emacs.d/prodigy-settings.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HackerNews

(require 'hackernews)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YAML mode

(require 'org)
(defun org-babel-execute:yaml (body params) body)
