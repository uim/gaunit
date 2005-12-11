(define-module test.unit.ui.gtk
  (extend test.unit.ui)
  (use gtk)
  (use srfi-2)
  (use file.util)
  (use util.queue)
  (use gauche.time)
  (use test.unit)
  (export <test-ui-gtk>))
(select-module test.unit.ui.gtk)
(gtk-init (with-module user *argv*))

(define *timeout-time* 10)

(define-class <test-ui-gtk> (<test-ui-base>)
  ((main-window :accessor main-window-of)
   (load-library-name-entry :accessor load-library-name-entry-of)
   (load-button :accessor load-button-of)
   (suite-name-entry :accessor suite-name-entry-of)
   (run-button :accessor run-button-of)
   (progress-bar :accessor progress-bar-of)
   (test-count-label :accessor test-count-label-of)
   (assertion-count-label :accessor assertion-count-label-of)
   (success-count-label :accessor success-count-label-of)
   (failure-count-label :accessor failure-count-label-of)
   (error-count-label :accessor error-count-label-of)
   (list-window :accessor list-window-of)
   (fault-list :accessor fault-list-of)
   (fault-detail-list :accessor fault-detail-list-of)
   (detail-window :accessor detail-window-of)
   (outer-detail-sub-panel :accessor outer-detail-sub-panel-of)
   (fault-detail-label :accessor fault-detail-label-of)
   (status-entry :accessor status-entry-of)
   (update-queue :accessor update-queue-of)
   (running :accessor running-of)
   (loaded-file :accessor loaded-file-of :init-value #f)
   (loaded-file-mtime :accessor loaded-file-mtime-of)
   (ran :accessor ran-of :init-value #f)))

(define-method initialize ((self <test-ui-gtk>) args)
  (next-method)
  (set! (main-window-of self) (make-main-window self)))

(define-macro (update! ui . body)
  `(enqueue! (update-queue-of ,ui)
             (lambda ()
               ,@body)))

(define (quit ui)
  (gtk-widget-destroy (main-window-of ui)))

(define (make-main-window ui)
  (let ((window (gtk-window-new GTK_WINDOW_TOPLEVEL))
        (accel (gtk-accel-group-new)))
    (g-signal-connect window "destroy"
                      (lambda _ (gtk-main-quit)))
    (gtk-widget-set-usize window 800 600)
    (gtk-window-set-policy window 1 1 0)
    (gtk-container-add window (make-main-panel ui))
    window))

(define (make-main-panel ui)
  (let ((panel (gtk-vbox-new #f 0)))
    (g-signal-connect panel "key_press_event"
                      (lambda (widget event)
                        (let1 kv (ref event 'keyval)
                          (cond
                           ((member kv (list GDK_Escape GDK_q))
                            (quit ui))
                           ((member kv (list GDK_r))
                            (gtk-button-clicked (run-button-of ui)))
                           ((member kv (list GDK_l))
                            (gtk-button-clicked (load-button-of ui)))))))
    (gtk-box-pack-start panel (make-load-panel ui) #f #f 0)
    (gtk-box-pack-start panel (make-suite-panel ui) #f #f 0)
    (gtk-box-pack-start panel (make-progress-panel ui) #f #f 0)
    (gtk-box-pack-start panel (make-info-panel ui) #f #f 0)
    (gtk-box-pack-start panel (make-list-panel ui) #f #f 0)
    (gtk-box-pack-start panel (make-detail-panel ui) #t #t 0)
    (gtk-box-pack-start panel (make-status-panel ui) #f #f 0)
    panel))

(define (make-load-panel ui)
  (let ((panel (gtk-hbox-new #f 10)))
    (gtk-container-set-border-width panel 10)
    (gtk-box-pack-start panel (gtk-label-new "Load:") #f #f 0)
    (gtk-box-pack-start panel (make-load-library-name-entry ui) #t #t 0)
    (gtk-box-pack-start panel (make-load-button ui) #f #f 0)
    panel))

(define (make-load-library-name-entry ui)
  (let ((entry (gtk-entry-new)))
    (gtk-entry-set-text entry (with-module user *program-name*))
    (set! (load-library-name-entry-of ui) entry)
    entry))

(define (make-load-button ui)
  (let ((button (gtk-button-new-with-label "Load")))
    (set! (load-button-of ui) button)
    (g-signal-connect button "clicked"
                      (lambda _
                        (reset-test-suites)
                        (load (gtk-entry-get-text
                               (load-library-name-entry-of ui)))
                        (output-status
                         ui
                         #`"Loaded ,(gtk-entry-get-text
                                      (load-library-name-entry-of ui))")
                        (reset-ui ui (gaunit-default-test-suite))
                        (if (symbol-bound? 'main (interaction-environment))
                          (begin
                            (quit ui)
                            (eval '(main (cons *program-name* *argv*))
                                  (interaction-environment)))
                          (rerun ui))))
    button))

(define (make-suite-panel ui)
  (let ((panel (gtk-hbox-new #f 10)))
    (gtk-container-set-border-width panel 10)
    (gtk-box-pack-start panel (gtk-label-new "Suite:") #f #f 0)
    (gtk-box-pack-start panel (make-suite-name-entry ui) #t #t 0)
    (gtk-box-pack-start panel (make-run-button ui) #f #f 0)
    panel))

(define (make-suite-name-entry ui)
  (let ((entry (gtk-entry-new)))
    (gtk-entry-set-editable entry #f)
    (set! (suite-name-entry-of ui) entry)
    entry))

(define (make-run-button ui)
  (let ((button (gtk-button-new-with-label "Run")))
    (set! (run-button-of ui) button)
    (g-signal-connect (run-button-of ui) "clicked"
                      (lambda _ (rerun ui)))
    button))

(define (make-progress-panel ui)
  (let ((panel (gtk-hbox-new #f 10)))
    (gtk-container-set-border-width panel 10)
    (gtk-box-pack-start panel (make-test-progress-bar ui) #t #t 0)
    panel))

(define (make-test-progress-bar ui)
  (let ((bar (gtk-progress-bar-new)))
    (set! (progress-bar-of ui) bar)
    (set-progress-bar-color! ui (make-green-color))
    bar))

(define (set-progress-bar-color! ui color)
  (gtk-widget-modify-bg (progress-bar-of ui)
                        GTK_STATE_PRELIGHT
                        color))

(define (make-color r g b)
  (let ((color (make <gdk-color>)))
    (slot-set! color 'red r)
    (slot-set! color 'green g)
    (slot-set! color 'blue b)
    color))

(define (make-green-color)
  (make-color #x0000 #xffff #x0000))

(define (make-red-color)
  (make-color #xffff #x0000 #x0000))

(define (make-info-panel ui)
  (let ((panel (gtk-hbox-new #f 0)))
    (gtk-container-set-border-width panel 10)
    (gtk-box-pack-start panel (gtk-label-new "Tests:") #f #f 0)
    (gtk-box-pack-start panel (make-count-label ui "test") #t #f 0)
    (gtk-box-pack-start panel (gtk-label-new "Assertions:") #f #f 0)
    (gtk-box-pack-start panel (make-count-label ui "assertion") #t #f 0)
    (gtk-box-pack-start panel (gtk-label-new "Successes:") #f #f 0)
    (gtk-box-pack-start panel (make-count-label ui "success") #t #f 0)
    (gtk-box-pack-start panel (gtk-label-new "Failures:") #f #f 0)
    (gtk-box-pack-start panel (make-count-label ui "failure") #t #f 0)
    (gtk-box-pack-start panel (gtk-label-new "Errors:") #f #f 0)
    (gtk-box-pack-start panel (make-count-label ui "error") #t #f 0)
    panel))

(define (make-count-label ui type)
  (let ((label (gtk-label-new "0")))
    (slot-set! ui (string->symbol #`",|type|-count-label") label)
    (gtk-label-set-justify label GTK_JUSTIFY_LEFT)
    label))

(define (make-list-panel ui)
  (let ((panel (gtk-hbox-new #f 0)))
    (gtk-box-pack-start panel (make-list-scrolled-window ui) #t #t 0)
    panel))

(define (make-list-scrolled-window ui)
  (let ((window (gtk-scrolled-window-new #f #f)))
    (set! (list-window-of ui) window)
    (gtk-scrolled-window-set-policy window
                                    GTK_POLICY_AUTOMATIC
                                    GTK_POLICY_AUTOMATIC)
    (gtk-widget-set-usize window -1 150)
    (gtk-scrolled-window-add-with-viewport window (make-fault-list ui))
    window))

(define (make-fault-list ui)
  (let ((list (gtk-list-new)))
    (set! (fault-list-of ui) list)
    (set! (fault-detail-list-of ui) '())
    (g-signal-connect list "select-child"
                      (lambda (lst item)
                        (show-fault ui
                                    (list-ref (fault-detail-list-of ui)
                                              (gtk-list-child-position lst item)))))
    (g-signal-connect list "unselect-child"
                      (lambda _
                        (clear-fault ui)))
    list))

(define (make-detail-panel ui)
  (let ((panel (gtk-hbox-new #f 0)))
    (gtk-box-pack-start panel (make-detail-scrolled-window ui) #t #t 0)
    panel))

(define (make-detail-scrolled-window ui)
  (let ((window (gtk-scrolled-window-new #f #f)))
    (set! (detail-window-of ui) window)
    (gtk-scrolled-window-set-policy window
                                    GTK_POLICY_AUTOMATIC
                                    GTK_POLICY_AUTOMATIC)
    (gtk-widget-set-usize window 400 -1)
    (gtk-scrolled-window-add-with-viewport window
                                           (make-outer-detail-sub-panel ui))
    window))

(define (make-outer-detail-sub-panel ui)
  (let ((panel (gtk-hbox-new #f 0)))
    (set! (outer-detail-sub-panel-of ui) panel)
    (gtk-box-pack-start panel (make-inner-detail-sub-panel ui) #f #f 0)
    panel))

(define (make-inner-detail-sub-panel ui)
  (let ((panel (gtk-hbox-new #f 0)))
    (gtk-box-pack-start panel (make-fault-detail-label ui) #f #f 0)
    panel))

(define (make-fault-detail-label ui)
  (let ((label (gtk-label-new "")))
    (set! (fault-detail-label-of ui) label)
    (gtk-label-set-line-wrap label #f)
    label))

(define (show-fault ui message)
  (raw-show-fault ui message))

(define (clear-fault ui)
  (raw-show-fault ui ""))

(define (raw-show-fault ui message)
  (let ((label (fault-detail-label-of ui)))
    (gtk-label-set-text label message))
  (let ((panel (outer-detail-sub-panel-of ui)))
    panel))

(define (make-status-panel ui)
  (let ((panel (gtk-hbox-new #f 0)))
    (gtk-box-pack-start panel (make-status-entry ui) #t #t 0)
    panel))

(define (make-status-entry ui)
  (let ((entry (gtk-entry-new)))
    (set! (status-entry-of ui) entry)
    (gtk-entry-set-editable entry #f)
    entry))

(define (string-inc-as-number string . options)
  (let-optionals* options ((inc-value 1))
    (number->string (+ inc-value (string->number string)))))

(define (count-up-label label)
  (gtk-label-set-text label (string-inc-as-number 
                             (gtk-label-get-text label))))
  

(define-method test-successed ((self <test-ui-gtk>) test)
  (count-up-label (success-count-label-of self)))

(define-method test-failed ((self <test-ui-gtk>) test message stack-trace)
  (let ((fault-list (fault-list-of self))
        (fault-item (gtk-list-item-new-with-label
                     (string-append #`",(error-line stack-trace)\n"
                                    #`",message in ,(name-of test)"))))
    (set-progress-bar-color! self (make-red-color))
    (count-up-label (failure-count-label-of self))
    (gtk-widget-show fault-item)
    (append-fault-detail-list! self #f (list stack-trace))
    (gtk-list-append-items fault-list (list fault-item))))

(define-method test-errored ((self <test-ui-gtk>) test err)
  (let* ((stack-trace (cddr (vm-get-stack-trace-lite)))
         (fault-list (fault-list-of self))
         (line (error-line (car stack-trace)))
         (fault-item (gtk-list-item-new-with-label
                      (string-append (if line
                                       #`",|line|\n"
                                       "")
                                     #`"Error occurred in ,(name-of test)"))))
    (append-fault-detail-list! self err stack-trace)
    (set-progress-bar-color! self (make-red-color))
    (count-up-label (error-count-label-of self))
    (gtk-widget-show fault-item)
    (gtk-list-append-items fault-list (list fault-item))))

(define (append-fault-detail-list! ui err stack-trace)
  (set! (fault-detail-list-of ui)
        (append (fault-detail-list-of ui)
                (list
                 (error-message err stack-trace :max-depth 5)))))

(define-method test-run ((self <test-ui-gtk>) test test-thunk)
  (update! self (test-thunk)))

(define-method test-start ((self <test-ui-gtk>) test)
  (update! self
           (let ((test-name (name-of test)))
             (output-status self #`"Running ,|test-name|..."))))

(define-method test-finish ((self <test-ui-gtk>) test)
  (update! self
           (let ((label (assertion-count-label-of self))
                 (bar (progress-bar-of self))
                 (assertion-number (assertion-number-of test)))
             (gtk-label-set-text label (string-inc-as-number
                                        (gtk-label-get-text label)
                                        assertion-number))
             (count-up-label (test-count-label-of self))
             (gtk-progress-set-value bar
                                     (+ 1 (gtk-progress-get-value bar))))))

(define-method test-case-setup ((self <test-ui-gtk>) test setup-thunk)
  (update! self (setup-thunk)))

(define-method test-case-teardown ((self <test-ui-gtk>) test teardown-thunk)
  (update! self (teardown-thunk)))

(define-method test-case-start ((self <test-ui-gtk>) test-case)
  #f)

(define-method test-case-finish ((self <test-ui-gtk>) test-case)
  #f)

(define-method test-suite-start ((self <test-ui-gtk>) test-suite)
  (set! (running-of self) #t)
  (gtk-timeout-add *timeout-time*
    (lambda ()
      (if (queue-empty? (update-queue-of self))
        (running-of self)
        (begin
          ((dequeue! (update-queue-of self)))
          #t))))
  (let ((window (main-window-of self)))
    (gtk-window-set-title window (name-of test-suite))
    (output-status self "Started.")
    (gtk-widget-show-all window))
  (unless (ran-of self)
    (set! (ran-of self) #t)
    (rerun self test-suite)
    (gtk-main)
    (exit 0) ; DIRTY!!
    ))

(define (update-loaded-file! ui)
  (unless (loaded-file-of ui)
    (set! (loaded-file-of ui)
          (gtk-entry-get-text
           (load-library-name-entry-of ui)))
    (set! (loaded-file-mtime-of ui)
          (file-mtime (loaded-file-of ui))))
  (when (< (loaded-file-mtime-of ui)
         (file-mtime (loaded-file-of ui)))
    (gtk-entry-set-text (load-library-name-entry-of ui)
                        (loaded-file-of ui))
    (gtk-button-clicked (load-button-of ui))))

(define-method test-suite-finish ((self <test-ui-gtk>) test-suite)
  (update! self
    (output-status self
                   #`"Finished in ,(operating-time-of test-suite) seconds."))
  (update! self
           (set! (running-of self) #f)))

(define (rerun ui . args)
  (let-optionals* args ((test-suite (gaunit-default-test-suite)))
    ;; (gtk-widget-activate (run-button-of ui))
    (update-loaded-file! ui)
    (soft-reset-test-suites (list test-suite))
    (reset-ui ui test-suite)
    (run-test ui test-suite)))

(define (run-test ui test-suite)
  (run test-suite :ui ui))

(define (output-status ui message)
  (gtk-entry-set-text (status-entry-of ui) message))

(define (reset-ui ui suite)
  (gtk-entry-set-text (suite-name-entry-of ui) (name-of suite))
  (gtk-progress-configure (progress-bar-of ui) 0 0 (test-number-of suite))
  (set! (fault-detail-list-of ui) '())
  (set! (update-queue-of ui) (make-queue))
  (gtk-list-clear-items (fault-list-of ui) 0 -1)
  (set-progress-bar-color! ui (make-green-color))
  (for-each (lambda (label-accessor)
              (gtk-label-set-text (label-accessor ui) "0"))
            (list test-count-label-of
                  assertion-count-label-of
                  success-count-label-of
                  failure-count-label-of
                  error-count-label-of)))

(set-default-test-ui! (make <test-ui-gtk>))

(provide "test/unit/ui/gtk")
