
(add-to-list 'load-path "/Users/mb/lisp/slime/")
(require 'slime)

(slime-setup :autodoc t :record-changed-definitions t)

(setq inferior-lisp-program "/Users/mb/bin/openmcl"
      lisp-indent-function 'common-lisp-indent-function
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      common-lisp-hyperspec-root "file:///Users/mb/lisp/HyperSpec/"
      slime-startup-animation nil
      slime-enable-evaluate-in-emacs t
      slime-log-events t
      slime-outline-mode-in-events-buffer nil
      slime-repl-return-behaviour :send-only-if-after-complete
      slime-autodoc-use-multiline-p t
      slime-highlight-compiler-notes t
      slime-fuzzy-completion-in-place nil)

(require 'mic-paren)

(paren-activate)

(setf paren-priority 'close)

(defmacro defslime-start (name lisp)
  `(defun ,name ()
     (interactive)
     (slime-start :program ,lisp)))

(defslime-start openmcl "/Users/mb/bin/openmcl")
(defslime-start clisp "/Users/mb/bin/clisp")
(defslime-start cmucl "/Users/mb/lisp/cmucl/bin/lisp")
(defslime-start sbcl "/usr/local/bin/sbcl")
(defslime-start abcl "/Users/mb/bin/abcl")

(define-key global-map (kbd "<f9>") 'slime-selector)

;;;; http://www.bloodandcoffee.net/campbell/paredit.el
(add-to-list 'load-path "~/.emacs.d/paredit/")
(require 'paredit)

(define-key slime-mode-map (kbd "(") 'paredit-open-parenthesis)
(define-key slime-mode-map (kbd ")") 'paredit-close-parenthesis)

(define-key slime-mode-map (kbd "\"") 'paredit-doublequote)
(define-key slime-mode-map (kbd "\\") 'paredit-backslash)

(define-key slime-mode-map (kbd "RET") 'paredit-newline)
(define-key slime-mode-map (kbd "<return>") 'paredit-newline)
(define-key slime-mode-map (kbd "C-j") 'newline)

;;;; nb: this assumes dvorak key layout
(define-key slime-mode-map (kbd "C-h") 'backward-sexp)
(define-key slime-mode-map (kbd "C-t") 'transpose-sexps)
(define-key slime-mode-map (kbd "C-M-t") 'transpose-chars)
(define-key slime-mode-map (kbd "C-n") 'forward-sexp)
(define-key slime-mode-map (kbd "C-k") 'kill-sexp)
(define-key slime-mode-map (kbd "C-M-k") 'paredit-kill)
(define-key slime-mode-map (kbd "C-'") 'paredit-splice-sexp)
(define-key slime-mode-map (kbd "C-M-l") 'paredit-recentre-on-sexp)
(define-key slime-mode-map (kbd "C-,") 'paredit-backward-slurp-sexp)
(define-key slime-mode-map (kbd "C-.") 'paredit-forward-slurp-sexp)
(define-key slime-mode-map (kbd "C-<") 'paredit-backward-barf-sexp)
(define-key slime-mode-map (kbd "C->") 'paredit-forward-barf-sexp)
(define-key slime-mode-map (kbd "C-/") 'backward-up-list)
(define-key slime-mode-map (kbd "C-=") 'down-list)
(define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
(define-key slime-mode-map (kbd "C-c TAB") 'slime-complete-form)
;;;; this may seem strange, but i often use the C-<whatever> motion
;;;; commands in sequence to reformat code and having to take a finger off of control
;;;; to add a return is a pain
(define-key slime-mode-map (kbd "C-<return>") 'paredit-newline)
;;;; i hate having to take my key off of ctrl for this and i don't use complete-form anyway...
(define-key slime-mode-map (kbd "C-c C-i") 'slime-inspect)
(define-key global-map (kbd "<f12>") 'slime-selector)

(setf slime-save-buffers nil)

(require 'parenface)

(require 'bridge)

(autoload 'install-bridge "bridge" "Install a process bridge." t)

(setq bridge-hook 
      '(lambda ()
        ;; Example options
        (setq bridge-source-insert nil) ;Don't insert in source buffer
        (setq bridge-destination-insert nil) ;Don't insert in dest buffer
        ;; Handle copy-it messages yourself
        ))

(defvar slime-auto-compile-timer nil)

(defun slime-enable-auto-compile ()
  (setf slime-auto-compile-timer
        (run-with-idle-timer 3 t `(lambda ()
                                    (when (and slime-mode
                                               (slime-sexp-at-point)
                                               (slime-connected-p))
                                      (slime-compile-defun))))))

(defun slime-disable-auto-compile ()
  (cancel-timer slime-auto-compile-timer))

(setf slime-display-edit-hilights nil)

(require 'tramp)

(setf slime-filename-translations
      (list
       (slime-create-filename-translator :machine-instance "soren"
                                         :remote-host "80.68.86.18"
                                         :username "animaliter")
       (slime-create-filename-translator :machine-instance "mail"
                                         :remote-host "85.88.193.69"
                                         :username "marvin")
       (slime-create-filename-translator :machine-instance "deng-mbari"
                                         :remote-host "deng-mbari"
                                         :username "mbaringer")
       (slime-create-filename-translator :machine-instance "debian3164lamp"
                                         :remote-host "talkisaction.com"
                                         :username "talk")
       (list ".*" 'identity 'identity)))

;; setup {} and [] to be treated like ()

(modify-syntax-entry ?{ "(}" lisp-mode-syntax-table)
(modify-syntax-entry ?} "){" lisp-mode-syntax-table)
(modify-syntax-entry ?[ "(]" lisp-mode-syntax-table)
(modify-syntax-entry ?] ")[" lisp-mode-syntax-table)

;; would love to have < and > match as well, but this totally breaks
;; yaclml :(
(defun deng-mbari ()
  (interactive)
  (slime-connect "127.0.0.1" 5005))

(defun mb:disable-font-lock-after-big-change (max-chars)
  (when (< max-chars (- (point-max) (point-min)))
    (font-lock-mode -1)))

(defun mb:slime-narrow-buffer (num-lines)
  "Narrow the repl leaving NUM-LINES of output"
  (set-marker slime-repl-last-input-start-mark nil)
  (let ((inhibit-read-only t))
    (narrow-to-region (save-excursion
                        (goto-char (slime-repl-input-line-beginning-position))
                        (forward-line (- num-lines))
                        (point))
                      (point-max))))

(defun slime-repl-clear-buffer ()
  "Delete the entire output generated by the Lisp process."
  (interactive)
  ;; need to add this since we narrow the region...
  (widen)
  (slime-eval-async `(swank:clear-repl-results))
  (set-marker slime-repl-last-input-start-mark nil)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (slime-repl-input-line-beginning-position))
    (goto-char slime-repl-input-start-mark)))

;; (add-hook 'slime-repl-mode-hook
;;           (lambda ()
;; ;            (push (lambda (start end changed)
;; ;                    (mb:disable-font-lock-after-big-change 100))
;; ;                  after-change-functions)
;;             (push (lambda () (mb:slime-narrow-buffer 1000))
;;                   post-command-hook)))
