(defvar run-test-suffixes '(".scm" ".rb" ".sh")
  "List of test file suffix.")

(defvar run-test-file "test/run-test"
  "Invoked file name by run-test.")

(defun flatten (lst)
  (cond ((null lst) '())
        ((listp (car lst))
         (append (flatten (car lst))
                 (flatten (cdr lst))))
        (t (cons (car lst) (flatten (cdr lst))))))

(defun run-test ()
  (interactive)
  (let ((test-file
         (find-if #'file-exists-p
                  (flatten
                   (mapcar (lambda (dir)
                             (mapcar (lambda (suffix)
                                       (concat dir run-test-file suffix))
                                     run-test-suffixes))
                           (list "./" "../" "../../" "../../../"))))))
    (if test-file
        (let ((current-directory (cadr (split-string(pwd)))))
          (cd (car (split-string test-file "\\/test\\/")))
          (compile (concat "test/" (file-name-nondirectory test-file)))
          (cd current-directory)))))

(define-key global-map "\C-c\C-t" 'run-test)
