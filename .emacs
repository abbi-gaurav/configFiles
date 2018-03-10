;;; package -- Summary
;;; Commentary:
;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(setq select-enable-clipboard t)
(delete-selection-mode 1)

(require 'ido)
(ido-mode t)

(setenv "PAGER" "/bin/cat")

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

(require 'calendar)

; org mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-ct" 'org-time-stamp-inactive)
(setq org-startup-indented t)
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "BLOCKED" "DONE")))

; agenda files dierctory
(load-library "find-lisp")
(setq-default org-agenda-files
      (find-lisp-find-files "~/OneDrive - SAP SE/po/wip" "\.org$"))

;org-mode lines wrap
(define-key org-mode-map "\M-q" 'toggle-truncate-lines)

; enable language execution in org mode
(require 'ob-sh)
(require 'ob-haskell)
(require 'ob-scala)
(require 'ob-java)

(eval-after-load "org"
  '(require 'ox-md nil t))

;; fontify code in code blocks
(setq org-src-fontify-natively t)

;; set src code indent in org src block
(setq org-src-tab-acts-natively t)

; org-bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(org-babel-do-load-languages
     'org-babel-load-languages
     '((ditaa . t)))

;; This happened to be the directory it gets installed with Fedora
(setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.11.0/libexec/ditaa-0.11.0-standalone.jar")

; temp fix for cycling through agenda files
(defun org-cycle-agenda-files2 ()
  "Cycle through the files in `org-agenda-files'.
If the current buffer visits an agenda file, find the next one in the list.
If the current buffer does not, find the first agenda file."
  (interactive)
  (let* ((fs (org-agenda-files t))
   (files (append fs (list (car fs))))
   (tcf (if buffer-file-name (file-truename buffer-file-name)))
   file)
    (unless files (user-error "No agenda files"))
    (catch 'exit
      (while (setq file (pop files))
  (if (equal (file-truename file) tcf)
      (when (car files)
        (find-file (car files))
        (throw 'exit t))))
      (find-file (car fs)))
    (if (buffer-base-buffer) (org-pop-to-buffer-same-window (buffer-base-buffer)))))

(global-set-key (kbd "<f9>") 'org-cycle-agenda-files2)

;set quick note
(setq org-default-notes-file (concat org-directory "~/notes/QuickNotes.org"))
(define-key global-map "\C-cc" 'org-capture)

(setq initial-major-mode 'org-mode)

; An agenda file for life
(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "")
          (alltodo ""
                   ((org-agenda-skip-function
                     '(or (air-org-skip-subtree-if-priority ?A)
                          (org-agenda-skip-if nil '(scheduled deadline))))))))))

; org-mode ends here

; command execution
(setq default-directory (concat (getenv "HOME") "/"))
(setq command-line-default-directory "~/")
(setenv "ESHELL" (expand-file-name "~/localBin/eshell"))
(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "/bin/bash")
(setenv "SHELL" shell-file-name)
; add custom paths
(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))

; command execution ends

; keep a list of recently opened files
(recentf-mode 1) 
;; set F7 to list recently opened file
(global-set-key (kbd "<f7>") 'recentf-open-files)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-completion-addsuffix t)
 '(comint-completion-autolist t)
 '(comint-input-ignoredups t)
 '(comint-move-point-for-output t)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(exec-path
   (quote
    ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/Cellar/emacs/25.3/libexec/emacs/25.3/x86_64-apple-darwin16.7.0")))
 '(fci-rule-color "#073642")
 '(haskell-stylish-on-save t)
 '(hindent-reformat-buffer-on-save t)
 '(hindent-style nil)
 '(org-agenda-skip-deadline-prewarning-if-scheduled t)
 '(org-agenda-skip-scheduled-if-deadline-is-shown t)
 '(org-babel-load-languages (quote ((emacs-lisp . t) (sh . t))))
 '(org-enforce-todo-dependencies t)
 '(package-selected-packages
   (quote
    (spacemacs-theme hindent color-theme-sanityinc-solarized scala-mode groovy-mode org-bullets intero clojure-mode xterm-color flycheck yaml-mode persistent-scratch markdown-mode logview log4j-mode)))
 '(persistent-scratch-autosave-mode t)
 '(spacemacs-theme-custom-colors (quote ((highlight . "yellow")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; flycheck
(package-install 'flycheck)
(global-flycheck-mode)

;; haskell start
;; Install Intero
(package-install 'intero)
(package-install 'hindent)
(require 'hindent)
(add-hook 'haskell-mode-hook (lambda() (intero-mode)))
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

;;  haskell end

;; markdown
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.html.md\\'" . markdown-mode))

(setq backup-directory-alist `(("." . "~/.emacsBkup")))

; color settings
; interpret and use ansi color codes in shell output windows
;(ansi-color-for-comint-mode-on)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

; parenthesis
; highlight matching parenthesis
(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

;add js2-ide mode
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

(global-linum-mode t)
(setq column-number-mode t)

; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.raml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\Jenkinsfile\\'" . groovy-mode))
(add-hook 'yaml-mode-hook
    '(lambda ()
       (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))

; color
(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))
(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))

; to undo C-/
; to redo C-g C-/

; enable windows navigatin with shift + arrow
; (windmove-default-keybindings)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)


; enable highlight line mode
; (global-hl-line-mode 1)

; artist-mode
; to fix right click issue
(eval-after-load "artist"
   '(define-key artist-mode-map [(down-mouse-3)] 'artist-mouse-choose-operation)
   )

; make ssh as default tramp mode
(setq tramp-default-method "ssh")

; auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)

; switch to previous buffer
(global-set-key (kbd "C-c b") 'switch-to-prev-buffer)

; enable autofill
; enable 120 characters long fill column
; (setq-default fill-column 120)
; (setq-default auto-fill-function 'do-auto-fill)

; apply theme
(add-hook 'after-init-hook (lambda () (load-theme 'spacemacs-dark)))

; insert special chars
(global-set-key (kbd "C-x 8 l") "λ")

;;; .emacs ends here
