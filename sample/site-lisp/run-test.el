(require 'cl)

(defvar run-test-suffixes '(".scm" ".rb" ".sh")
  "List of test file suffix.")

(defvar run-test-file "test/run-test"
  "Invoked file name by run-test.")

(defvar run-test-search-directories '("./" "../" "../../" "../../../")
  "Searched directories when searching run-test-file.")

(defvar run-test-verbose-level-table '((0 . "-vs")
                                       (1 . "")
                                       (2 . "-vp")
                                       (3 . "-vn")
                                       (4 . "-vv"))
  "Passed argumets to run-test-file for set verbose level.")

(defun flatten (lst)
  (cond ((null lst) '())
        ((listp (car lst))
         (append (flatten (car lst))
                 (flatten (cdr lst))))
        (t (cons (car lst) (flatten (cdr lst))))))


(defun get-verbose-level-arg (num)
  (let ((elem (assoc num run-test-verbose-level-table)))
    (concat " "
            (if elem (cdr elem) ""))))

(defun run-test (&optional arg)
  (interactive "P")
  (let ((verbose-arg (get-verbose-level-arg (prefix-numeric-value arg)))
        (test-file
         (find-if #'file-exists-p
                  (flatten
                   (mapcar (lambda (dir)
                             (mapcar (lambda (suffix)
                                       (concat dir run-test-file suffix))
                                     run-test-suffixes))
                           run-test-search-directories)))))
    (if test-file
        (let ((current-directory (cadr (split-string(pwd)))))
          (cd (car (split-string test-file run-test-file)))
          (compile (concat
                    (concat "./"
                            (file-name-directory run-test-file))
                    (file-name-nondirectory test-file)
                    verbose-arg))
          (cd current-directory)))))

(define-key global-map "\C-c\C-t" 'run-test)
