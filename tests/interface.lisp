;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:barker/tests/interface
  (:use #:cl #:5am)
  (:import-from #:barker/tests/suite
                #:suite)
  (:local-nicknames (#:pf #:barker/lib/platform)
                    (#:sp #:barker/base/swaybar-protocol)
                    (#:bk #:barker/base/bkblock)
                    (#:lr #:barker/listeners)
                    (#:br #:barker))
  (:export #:interface-suite)
  (:documentation "Tests for the interface layer."))

(in-package #:barker/tests/interface)


(def-suite interface-suite
  :description "Tests for code/main.lisp."
  :in suite)

(in-suite interface-suite)


;;; Listeners

(defun named-block (name &rest initargs)
  "Make a bkblock whose body is named NAME, passing INITARGS through."
  (apply #'make-instance 'bk:bkblock
         :body (make-instance 'sp:swblock :name name)
         initargs))

(test write-bar-empty
      "No blocks write an empty array line."
      (let ((out (make-string-output-stream)))
        (lr::write-bar '() out)
        (is (string= (format nil "[],~%") (get-output-stream-string out)))))

(test write-bar-keeps-order
      "Block bodies are written in order, followed by a comma."
      (let ((out (make-string-output-stream))
            (a (make-instance 'bk:bkblock
                              :body (make-instance 'sp:swblock
                                                   :full-text "a")))
            (b (make-instance 'bk:bkblock
                              :body (make-instance 'sp:swblock
                                                   :full-text "b"))))
        (lr::write-bar (list a b) out)
        (is (string= (format nil "[{\"full_text\":\"a\"},~
                                  {\"full_text\":\"b\"}],~%")
                     (get-output-stream-string out)))))

(test find-block-by-name
      "find-block returns the block whose body has that name."
      (let ((a (named-block "a"))
            (b (named-block "b")))
        (is (eq b (lr::find-block "b" (list a b))))))

(test find-block-unknown-name
      "find-block returns NIL when no block has that name."
      (is-false (lr::find-block "zzz" (list (named-block "a")))))

(test click-listener-routes-by-name
      "Each click goes to the block it names, in order."
      (let* ((log '())
             (count 0)
             (a (named-block "a"
                             :click-handler
                             (lambda (e body)
                               (push (list :a (sp:click-event-button e)) log)
                               body)))
             (b (named-block "b"
                             :click-handler
                             (lambda (e body)
                               (push (list :b (sp:click-event-button e)) log)
                               body)))
             (in (make-string-input-stream
                  (format nil "[~%{\"name\":\"a\",\"button\":1},~
                               ~%{\"name\":\"b\",\"button\":2},~%"))))
        (lr:click-listener (lambda () (incf count)) (list a b) in)
        (is (equal '((:a 1) (:b 2)) (reverse log)))
        (is (= 2 count))))

(test click-listener-ignores-unknown-and-unnamed
      "Clicks for unknown or unnamed blocks are skipped without error."
      (let* ((count 0)
             (a (named-block "a" :click-handler (lambda (e body)
                                                  (declare (ignore e))
                                                  body)))
             (in (make-string-input-stream
                  (format nil "[~%{\"name\":\"zzz\",\"button\":1},~
                               ~%{\"button\":1},~
                               ~%{\"name\":\"a\",\"button\":1},~%"))))
        (lr:click-listener (lambda () (incf count)) (list a) in)
        (is (= 1 count))))

(test event-source-listener-updates-per-line
      "Each line from an event source updates its block once."
      (let* ((count 0)
             (b (make-instance
                 'bk:bkblock
                 :event-source (lambda ()
                                 (make-string-input-stream
                                  (format nil "x~%y~%")))))
             (threads (lr:event-source-listener
                       (lambda () (incf count)) (list b))))
        (is (= 1 (length threads)))
        (mapc #'bt2:join-thread threads)
        (is (= 2 count))))

(test event-source-listener-skips-blocks-without-source
      "Blocks with no event source start no threads."
      (is (null (lr:event-source-listener
                 (lambda ()) (list (make-instance 'bk:bkblock))))))

(test update-listener-writes-on-notify
      "A notify makes the listener write the bar once."
      (let* ((n (pf:make-notifier))
             (out (make-string-output-stream))
             (b (make-instance 'bk:bkblock
                               :body (make-instance 'sp:swblock
                                                    :full-text "a")))
             (th (bt2:make-thread
                  (lambda () (lr:update-listener n (list b) out)))))
        (pf:notifier-notify n)
        (sleep 0.1)
        (bt2:destroy-thread th)
        (is (string= (format nil "[{\"full_text\":\"a\"}],~%")
                     (get-output-stream-string out)))))

(test write-bar-never-tears-a-body
      "Concurrent in-place updates are never half-applied on the bar."
      (let* ((body (make-instance 'sp:swblock :full-text "off" :urgent nil))
             (bb (make-instance
                  'bk:bkblock
                  :body body
                  :procedure (lambda (b)
                               (if (string= (sp:swblock-full-text b) "on")
                                   (setf (sp:swblock-full-text b) "off"
                                         (sp:swblock-urgent b) nil)
                                   (setf (sp:swblock-full-text b) "on"
                                         (sp:swblock-urgent b) t))
                               b)))
             (done nil)
             (torn 0)
             (flipper (bt2:make-thread
                       (lambda ()
                         (dotimes (i 100000)
                           (bk:do-procedure bb (lambda ())))
                         (setf done t)))))
        (loop until done
              do (let* ((line (with-output-to-string (out)
                                (lr::write-bar (list bb) out)))
                        (h (aref (com.inuoe.jzon:parse
                                  (string-right-trim '(#\, #\Newline) line))
                                 0)))
                   (unless (eq (string= (gethash "full_text" h) "on")
                               (and (gethash "urgent" h) t))
                     (incf torn))))
        (bt2:join-thread flipper)
        (is (zerop torn))))

(test write-bar-never-tears-a-clicked-body
      "Concurrent in-place click handlers are never half-applied on the bar."
      (let* ((body (make-instance 'sp:swblock :full-text "off" :urgent nil))
             (bb (make-instance
                  'bk:bkblock
                  :body body
                  :click-handler (lambda (event b)
                                   (declare (ignore event))
                                   (if (string= (sp:swblock-full-text b) "on")
                                       (setf (sp:swblock-full-text b) "off"
                                             (sp:swblock-urgent b) nil)
                                       (setf (sp:swblock-full-text b) "on"
                                             (sp:swblock-urgent b) t))
                                   b)))
             (done nil)
             (torn 0)
             (clicker (bt2:make-thread
                       (lambda ()
                         (dotimes (i 100000)
                           (bk:bkblock-handle-click bb nil (lambda ())))
                         (setf done t)))))
        (loop until done
              do (let* ((line (with-output-to-string (out)
                                (lr::write-bar (list bb) out)))
                        (h (aref (com.inuoe.jzon:parse
                                  (string-right-trim '(#\, #\Newline) line))
                                 0)))
                   (unless (eq (string= (gethash "full_text" h) "on")
                               (and (gethash "urgent" h) t))
                     (incf torn))))
        (bt2:join-thread clicker)
        (is (zerop torn))))

;;; Startup

(defun quietly-load-config ()
  "Call load-config with standard error discarded; return its blocks."
  (let ((*error-output* (make-broadcast-stream)))
    (br::load-config)))

(defun call-with-env (bindings thunk)
  "Call THUNK with environment BINDINGS, an alist of (NAME . VALUE).
A NIL value unsets NAME. Restore the old environment afterward."
  (let ((old (mapcar (lambda (b) (cons (car b) (sb-posix:getenv (car b))))
                     bindings)))
    (flet ((apply-env (alist)
             (dolist (b alist)
               (if (cdr b)
                   (sb-posix:setenv (car b) (cdr b) 1)
                   (sb-posix:unsetenv (car b))))))
      (apply-env bindings)
      (unwind-protect (funcall thunk)
        (apply-env old)))))

(defun call-with-config (text thunk)
  "Call THUNK with XDG_CONFIG_HOME set to a fresh temporary directory.
Write TEXT to barker/config.lisp there, or no file when TEXT is NIL.
Remove the directory afterward."
  (let* ((tmp (sb-posix:mkdtemp "/tmp/barker-XXXXXX"))
         (dir (format nil "~a/barker" tmp))
         (file (format nil "~a/config.lisp" dir)))
    (unwind-protect
         (progn
           (when text
             (ensure-directories-exist (format nil "~a/" dir))
             (with-open-file (s file :direction :output
                                :if-exists :supersede)
               (write-string text s)))
           (call-with-env `(("XDG_CONFIG_HOME" . ,tmp)) thunk))
      (when (probe-file file)
        (sb-posix:unlink file))
      (when (probe-file (format nil "~a/" dir))
        (sb-posix:rmdir dir))
      (sb-posix:rmdir tmp))))

(defun block-names (bkblocks)
  "Return the body names of BKBLOCKS, in order."
  (mapcar (lambda (bb) (sp:name-of (bk:bkblock-body bb))) bkblocks))

(test config-file-uses-xdg
      "XDG_CONFIG_HOME wins when it is set."
      (call-with-env '(("XDG_CONFIG_HOME" . "/x") ("HOME" . "/h"))
                     (lambda ()
                       (is (string= "/x/barker/config.lisp"
                                    (br::config-file))))))

(test config-file-falls-back-to-home
      "Without XDG_CONFIG_HOME the path is under $HOME/.config."
      (call-with-env '(("XDG_CONFIG_HOME" . nil) ("HOME" . "/h"))
                     (lambda ()
                       (is (string= "/h/.config/barker/config.lisp"
                                    (br::config-file))))))

(test config-file-empty-xdg-is-unset
      "An empty XDG_CONFIG_HOME is treated as unset."
      (call-with-env '(("XDG_CONFIG_HOME" . "") ("HOME" . "/h"))
                     (lambda ()
                       (is (string= "/h/.config/barker/config.lisp"
                                    (br::config-file))))))

(test default-block-is-named-barker
      "The default block is a bkblock labelled barker."
      (let ((b (br::default-block)))
        (is (typep b 'bk:bkblock))
        (is (string= "barker"
                     (sp:swblock-full-text (bk:bkblock-body b))))))

(test error-block-shows-condition-in-red
      "The error block carries the condition text and a red color."
      (let* ((c (make-condition 'simple-error
                                :format-control "boom"
                                :format-arguments nil))
             (body (bk:bkblock-body (br::error-block c))))
        (is (search "boom" (sp:swblock-full-text body)))
        (is (string= "#FF0000" (sp:swblock-color body)))))

(test write-header-writes-header-and-opens-array
      "The header line is followed by the opening of the infinite array."
      (let ((out (make-string-output-stream)))
        (br::write-header out)
        (is (string= (format nil "{\"version\":1,\"click_events\":true}~%[[],")
                     (get-output-stream-string out)))))

(test load-config-missing-file-gives-default
      "A missing config file gives the single default block."
      (call-with-config nil
                        (lambda ()
                          (is (equal '("barker")
                                     (block-names (br::load-config)))))))

(test load-config-reads-defblocks
      "Blocks defined in the config come back in definition order."
      (call-with-config "(defblock clock :interval 1) (defblock mail)"
                        (lambda ()
                          (is (equal '("clock" "mail")
                                     (block-names (br::load-config)))))))

(test load-config-restores-registry
      "load-config leaves the global registry as it found it."
      (let* ((bk:*blocks* (list (cons 'old :marker)))
             (before bk:*blocks*))
        (call-with-config "(defblock clock)" #'br::load-config)
        (is (eq before bk:*blocks*))))

(test load-config-error-gives-error-block
      "An error while loading gives a single error block."
      (call-with-config "(error \"boom\")"
                        (lambda ()
                          (let ((blocks (quietly-load-config)))
                            (is (equal '("config-error")
                                       (block-names blocks)))
                            (is (search "boom"
                                        (sp:swblock-full-text
                                         (bk:bkblock-body
                                          (first blocks)))))))))

(test load-config-reader-error-gives-error-block
      "A malformed config gives an error block, not a crash."
      (call-with-config "(defblock clock"
                        (lambda ()
                          (is (equal '("config-error")
                                     (block-names (quietly-load-config)))))))

(test load-config-sends-config-output-to-stderr
      "Output printed by the config never reaches standard output."
      (let ((out (make-string-output-stream))
            (err (make-string-output-stream)))
        (let ((*standard-output* out)
              (*error-output* err))
          (call-with-config "(princ \"noise\")" #'br::load-config))
        (is (string= "" (get-output-stream-string out)))
        (is (search "noise" (get-output-stream-string err)))))
