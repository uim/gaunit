(defvar run-test-suffix '(".scm" ".rb")
  "List of test file suffix")

(defun run-test ()
  (interactive)
  (let ((test-file
         (find-if #'file-exists-p
                  (flatten
                   (mapcar (lambda (dir)
                             (mapcar (lambda (suffix)
                                       (concat dir "test/run-test" suffix))
                                     run-test-suffix))
                           (list "./" "../" "../../" "../../../"))))))
    (if test-file
        (let ((current-directory (cadr (split-string(pwd)))))
          (cd (car (split-string test-file "\\/test\\/")))
          (compile (concat "test/" (file-name-nondirectory test-file)))
          (cd current-directory)))))

(define-key global-map "\C-c\C-t" 'run-test)
