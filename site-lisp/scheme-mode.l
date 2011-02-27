;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;;
;
; scheme-mode
; MATSUOKA Hiroshi
; project page: http://fun.sci.fukuoka-u.ac.jp/wiki/index.php?xyzzy
;
(provide "scheme-mode")

(in-package "editor")
(export '(*scheme-mode-hook*
          *scheme-keyword-file*
          *scheme-mode-map*
          *scheme-indent-map*
          scheme-mode
          *scheme-run-command*
          *scheme-run-pop-to-args*
          make-scheme-scratch
          *scheme-mode-scratch-buffer*
          *scheme-process*
          *scheme-process-open-hook*
          *scheme-process-close-hook*
          *scheme-process-send-filter*
          *scheme-process-read-filter*
          *scheme-view-mode-map*
          *scheme-mode-version*
          scheme-view-mode))

(defvar *scheme-mode-version* "20090118")
;;; scheme-mode
(defvar *scheme-mode-map* nil)
(unless *scheme-mode-map*
  (setq *scheme-mode-map* (make-sparse-keymap))
  (define-key *scheme-mode-map* #\RET 'scheme-newline-and-indent)
  (define-key *scheme-mode-map* #\LFD #'(lambda () (interactive) (scheme-eval-last-sexp) (newline)))
  (define-key *scheme-mode-map* '(#\C-c #\C-e) 'scheme-eval-current-buffer)
  (define-key *scheme-mode-map* #\TAB 'scheme-indent-line)
  (define-key *scheme-mode-map* #\M-TAB 'scheme-completion)
  )

(defvar *scheme-process* nil)
(defvar *scheme-process-open-hook* 'scheme-default-open-hook)
(defvar *scheme-process-close-hook* nil)
(defvar *scheme-process-send-filter* 'scheme-default-send-filter)
(defvar *scheme-process-read-filter* 'scheme-default-read-filter)

(defvar *scheme-run-command* nil)
(defvar *scheme-run-pop-to-args* nil)
(defvar *scheme-keyword-hash-table* nil)
(defvar *scheme-keyword-file* "Scheme")
(defvar *scheme-mode-scratch-buffer* "*scheme scratch*")

(setf *scheme-indent-map*
  '(("define" 2)
    ("begin" 2)))
    
(defun scheme-indent-line ()
  (interactive)
  (goto-bol)
  (smart-indentation
   (or (calc-scheme-mode-indent)
       (save-excursion
         (max 0 (calc-lisp-indent (point)))))))

(defun calc-scheme-mode-indent ()
  (save-excursion
    (protect-match-data
      (cond
       ((not (backward-up-list 1 t)) nil)
       ((not (looking-at "(\\([^ \n]+\\)")) nil)
       ((not (find
              (match-string 1) *scheme-indent-map*
              :key #'car :test #'equal)) nil)
       (t
        (+ (current-column)
           (cadr
            (find
             (match-string 1) *scheme-indent-map*
             :key #'car :test #'equal))))))))

(defun scheme-electric-close ()
  (interactive))

(defun scheme-newline-and-indent ()
  (interactive)
  (newline)
  (scheme-indent-line))

(defun scheme-default-send-filter (proc sexp)
  (format nil "~A\r\n" sexp))

(defun scheme-default-read-filter (proc text)
  (when *scheme-run-pop-to-args*
    (unless (find-buffer (car *scheme-run-pop-to-args*))
      (save-excursion
        (switch-to-buffer (car *scheme-run-pop-to-args*))
        (setup-temp-buffer (selected-buffer))))
    (apply 'pop-to-buffer *scheme-run-pop-to-args*))
  (insert text)
  (set-window (get-buffer-window (process-buffer proc)))
  (refresh-screen))

(defun scheme-default-open-hook (buffer)
  (set-buffer buffer)
  (make-process *scheme-run-command*))

(defun scheme-open-process ()
  (interactive)
  (when (null *scheme-process*)
    (let* ((process (funcall *scheme-process-open-hook* (selected-buffer))))
      (setf *scheme-process* process)
      (when *scheme-process*
        (set-process-sentinel
         *scheme-process*
         #'(lambda (proc)
             (when *scheme-process-close-hook*
               (funcall *scheme-process-close-hook* proc))
             (setf *scheme-process* nil)))
        (set-process-filter *scheme-process* *scheme-process-read-filter*))))
  *scheme-process*)

(defun scheme-eval-string (sexp)
  (when sexp
    (let ((process (scheme-open-process)))
      (when process
        (process-send-string process (funcall *scheme-process-send-filter* process sexp))))))

(defun scheme-eval-current-buffer ()
  (interactive)
  (let* ((text (buffer-substring (point-min) (point-max))))
    (when (and text (> (length text) 0))
      (scheme-eval-string text))))

(defun scheme-eval-last-sexp()
  (interactive)
  (save-excursion
    (let* ((p (point))
           (s (progn (backward-sexp) (point)))
           (e (progn (forward-sexp) (point)))
           (text (buffer-substring s e)))
      (goto-char p)
      (when (and text (> (length text) 0))
        (scheme-eval-string text)))))

(defvar *scheme-mode-syntax-table* nil)
(unless *scheme-mode-syntax-table*
  (setq *scheme-mode-syntax-table* (make-syntax-table))
  (dotimes (x 127)
    (let ((c (code-char x)))
      (unless (alphanumericp c)
        (set-syntax-symbol *scheme-mode-syntax-table* c))))
  (set-syntax-whitespace *scheme-mode-syntax-table* #\SPC)
  (set-syntax-whitespace *scheme-mode-syntax-table* #\TAB)
  (set-syntax-whitespace *scheme-mode-syntax-table* #\C-l)
  (set-syntax-whitespace *scheme-mode-syntax-table* #\RET)
  (set-syntax-start-comment *scheme-mode-syntax-table* #\; nil)
  (set-syntax-end-comment *scheme-mode-syntax-table* #\LFD nil t)
  (set-syntax-string *scheme-mode-syntax-table* #\")
  (set-syntax-quote *scheme-mode-syntax-table* #\#)
  (set-syntax-quote *scheme-mode-syntax-table* #\')
  (set-syntax-quote *scheme-mode-syntax-table* #\,)
  (set-syntax-quote *scheme-mode-syntax-table* #\`)
  (set-syntax-match *scheme-mode-syntax-table* #\( #\))
  (set-syntax-escape *scheme-mode-syntax-table* #\\)
  (set-syntax-start-multi-comment *scheme-mode-syntax-table* "#|")
  (set-syntax-end-multi-comment *scheme-mode-syntax-table* "|#"))

(defvar *scheme-mode-abbrev-table* nil)
(unless *scheme-mode-abbrev-table*
  (define-abbrev-table '*scheme-mode-abbrev-table*))

; completion
(defvar *scheme-completion-list* nil)
(defun scheme-completion ()
  (interactive)
  (or *scheme-completion-list*
      (setq *scheme-completion-list*
            (make-list-from-keyword-table *scheme-keyword-hash-table*))
      (return-from scheme-completion nil))
  (let ((opoint (point)))
    (when (skip-syntax-spec-backward "w_")
      (let ((from (point)))
        (goto-char opoint)
        (do-completion from opoint :list *scheme-completion-list*)))))

; add-indent
(defun scheme-add-indent ()
  (interactive)
  (save-excursion
    (goto-bol)
    (insert "\t"))
  (while (and (not (null (following-char)))
              (equal (following-char) #\TAB))
    (forward-char)))

; del-indent
(defun scheme-del-indent ()
  (interactive)
  (save-excursion
    (goto-bol)
    (let ((c (following-char)))
      (when (not (null c))
        (when (equal c #\TAB)
          (delete-char))))))

(defvar *scheme-mode-hook* nil)
(defun scheme-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Scheme")
  (setq buffer-mode 'scheme-mode)
  (use-keymap *scheme-mode-map*)
  (use-syntax-table *scheme-mode-syntax-table*)
  (and *scheme-keyword-file*
       (null *scheme-keyword-hash-table*)
       (setq *scheme-keyword-hash-table*
             (load-keyword-file *scheme-keyword-file* t)))
  (when *scheme-keyword-hash-table*
    (make-local-variable 'keyword-hash-table)
    (setq keyword-hash-table *scheme-keyword-hash-table*))
  (when *scheme-mode-abbrev-table*
    (setq *local-abbrev-table* *scheme-mode-abbrev-table*))
  (run-hooks '*scheme-mode-hook*))

(defun make-scheme-scratch ()
  (interactive)
  (switch-to-buffer *scheme-mode-scratch-buffer*)
  (scheme-mode)
  (make-local-variable 'need-not-save)
  (setf need-not-save t)
  (make-local-variable 'auto-save)
  (setf auto-save nil))
