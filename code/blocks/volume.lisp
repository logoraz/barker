;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

;;; Port of gubar's volume-pipewire block.

(defpackage #:barker/blocks/volume
  (:use #:cl)
  (:import-from #:barker/base/swaybar-protocol
                #:swblock-full-text)
  (:import-from #:barker/base/bkblock
                #:defkind)
  (:import-from #:barker/blocks/support
                #:glyphs
                #:command-output
                #:guarded)
  (:local-nicknames (#:jzon #:com.inuoe.jzon))
  (:documentation "Volume block: default sink volume from pactl."))

(in-package #:barker/blocks/volume)


(defparameter *icons* (glyphs #xF057F #xF0580 #xF057E)
  "Nerd font volume icons, from quiet to loud.")

(defparameter *muted-icon* (aref (glyphs #xF0581) 0)
  "Nerd font icon shown while the sink is muted.")

(defun default-sink (pactl)
  "Return the parsed JSON object of the default sink, or NIL."
  (let ((sinks (jzon:parse (command-output pactl "-f" "json"
                                                   "list" "sinks")))
        (name (string-trim '(#\Space #\Newline)
                           (command-output pactl
                                                   "get-default-sink"))))
    (find name sinks
          :key (lambda (sink) (gethash "name" sink))
          :test #'equal)))

(defun sink-percent (sink)
  "Return the front-left volume of SINK as a whole percent."
  (parse-integer
   (gethash "value_percent"
            (gethash "front-left" (gethash "volume" sink)))
   :junk-allowed t))

(defun sink-text (sink)
  "Return the icon and volume percent of SINK as display text."
  (let* ((percent (sink-percent sink))
         (icon (if (gethash "mute" sink)
                   *muted-icon*
                   (aref *icons* (min (1- (length *icons*))
                                      (floor percent 34))))))
    (format nil "~a ~a" icon percent)))

(defkind volume (&key (pactl-command "pactl"))
  "Default sink volume, updated on signal 2 (SIGRTMIN+2)."
  (list :trigger 2
        :interval :persistent
        :procedure
        (guarded
         (lambda (body)
           (let ((sink (default-sink pactl-command)))
             (setf (swblock-full-text body)
                   (if sink (sink-text sink) "N/A"))
             body)))))
