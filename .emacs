(setq x-select-enable-clipboard t)
(delete-selection-mode 1)
(require 'ido)
(ido-mode t)
(setenv "PAGER" "/bin/cat")
(put 'downcase-region 'disabled nil)

(put 'upcase-region 'disabled nil)

(require 'calendar)

(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Done!"))

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
;; (setq org-log-done t)
;; (setq org-log-done 'time)
(setq org-startup-indented t)
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)
(setq default-directory (concat (getenv "HOME") "/"))
(setq command-line-default-directory "~/")

(recentf-mode 1) ; keep a list of recently opened files
;; set F7 to list recently opened file
(global-set-key (kbd "<f7>") 'recentf-open-files)

;;(require 'org-latex)
(setenv "PATH" (concat "/usr/local/texlive/2014basic/bin/universal-darwin:" (getenv "PATH")))


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
 '(exec-path
   (quote
    ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin" "/usr/local/texlive/2014basic/bin/universal-darwin")))
 '(org-agenda-files
   (quote
    ("~/Dropbox/personal/taskMgmt/JobLearning.org" "~/Dropbox/personal/taskMgmt/notes.org" "~/learning/objective-c/docs/iOSdev.org" "~/Dropbox/personal/taskMgmt/Learning.org" "~/Dropbox/personal/JobSearch/gaurav/InterviewQuestions.org" "~/Dropbox/personal/taskMgmt/Todo.org" "~/Dropbox/personal/taskMgmt/BlogsToWrite.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
)

(autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'". markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'". markdown-mode))

(setq ispell-program-name "/usr/local/bin/aspell")

(setq backup-directory-alist `(("." . "~/.emacsBkup")))

; shell customizations


; interpret and use ansi color codes in shell output windows
;(ansi-color-for-comint-mode-on)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
; make completion buffers disappear after 3 seconds.
;(add-hook 'completion-setup-hook
;  (lambda () (run-at-time 3 nil
;    (lambda () (delete-windows-on "*Completions*")))))

; highlight matching parenthesis
(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)

; note taking
(setq org-default-notes-file (concat org-directory "~/Dropbox/personal/taskMgmt/notes.org"))
(define-key global-map "\C-cr" 'org-capture)

;org mode lines wrap
(define-key org-mode-map "\M-q" 'toggle-truncate-lines)

;add js2-ide mode
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

(global-linum-mode t)
