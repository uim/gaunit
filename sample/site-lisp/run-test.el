(require 'cl)

(defvar run-test-suffixes '(".scm" ".rb" ".sh")
  "List of test file suffix.")

(defvar run-test-file-names '("test/run-test" "test/runner")
  "List of invoked file name by run-test.")

(defvar run-test-verbose-level-table '((0 . "-vs")
                                       (1 . "")
                                       (2 . "-vp")
                                       (3 . "-vn")
                                       (4 . "-vv"))
  "Passed argumets to run-test-file-names for set verbose level.")

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

(defun find-run-test-file-in-directory (directory filenames)
  (do ((fnames filenames (cdr fnames))
       (fname (concat directory (car filenames))
              (concat directory (car fnames))))
      ((or (file-exists-p fname)
           (null fnames))
       (if (file-exists-p fname)
           fname
         nil))))

(defun find-run-test-file (filenames)
  (let ((init-dir "./"))
    (do ((dir init-dir (concat dir "../"))
         (run-test-file (find-run-test-file-in-directory init-dir filenames)
                        (find-run-test-file-in-directory dir filenames)))
        ((or run-test-file (string= "/" (expand-file-name dir)))
         run-test-file))))

(defun find-test-files ()
  (mapcar (lambda (run-test-file)
            (let ((test-file (find-run-test-file
                              (mapcar (lambda (suffix)
                                        (concat run-test-file suffix))
                                      run-test-suffixes))))
              (if test-file
                  (cons run-test-file test-file)
                test-file)))
          run-test-file-names))

(defun run-test-if-find (test-file-infos verbose-arg)
  (cond ((null test-file-infos) nil)
        ((car test-file-infos)
         (let ((test-file-info (car test-file-infos)))
           (let ((current-directory (cadr (split-string(pwd))))
                 (run-test-file (car test-file-info))
                 (test-file (cdr test-file-info)))
             (cd (car (split-string test-file run-test-file)))
             (compile (concat
                       (concat "./"
                               (file-name-directory run-test-file))
                       (file-name-nondirectory test-file)
                       verbose-arg))
             (cd current-directory))))
        (t (run-test-if-find (cdr test-file-infos) verbose-arg))))

(defun run-test (&optional arg)
  (interactive "P")
  (run-test-if-find (find-test-files)
                    (get-verbose-level-arg (prefix-numeric-value arg))))

(define-key global-map "\C-c\C-t" 'run-test)
