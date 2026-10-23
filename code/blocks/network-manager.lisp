;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

;;; Port of gubar's network-manager block.

(defpackage #:barker/blocks/network-manager
  (:use #:cl)
  (:import-from #:barker/base/swaybar-protocol
                #:swblock-full-text)
  (:import-from #:barker/base/bkblock
                #:defkind)
  (:import-from #:barker/blocks/support
                #:glyphs
                #:command-output
                #:guarded)
  (:documentation "Network block: wifi or ethernet state from nmcli."))

(in-package #:barker/blocks/network-manager)


(defparameter *wifi-icons* (glyphs #xF091F #xF0922 #xF0925 #xF0928)
  "Nerd font wifi icons, from weak to strong signal.")

(defparameter *ethernet-icon* (aref (glyphs #xF0200) 0)
  "Nerd font icon shown while connected by ethernet.")

(defparameter *disconnected-icon* (aref (glyphs #xF092D) 0)
  "Nerd font icon shown while not connected.")

(defun split-lines (string)
  "Return the non-empty lines of STRING, or NIL if STRING is NIL."
  (when string
    (with-input-from-string (in string)
      (loop :for line = (read-line in nil)
            :while line
            :unless (string= line "")
              :collect line))))

(defun split-terse (line)
  "Split LINE, nmcli terse output, into fields on unescaped colons.
A backslash escapes the next character, so an SSID may contain colons."
  (let ((fields '())
        (field (make-string-output-stream))
        (escaped nil))
    (loop :for char :across line
          :do (cond (escaped (write-char char field)
                             (setf escaped nil))
                    ((char= char #\\) (setf escaped t))
                    ((char= char #\:)
                     (push (get-output-stream-string field) fields))
                    (t (write-char char field))))
    (push (get-output-stream-string field) fields)
    (nreverse fields)))

(defun ethernet-connected-p ()
  "Return true if an ethernet device is connected."
  (some (lambda (line)
          (destructuring-bind (&optional type state &rest rest)
              (split-terse line)
            (declare (ignore rest))
            (and (equal type "ethernet") (equal state "connected"))))
        (split-lines (command-output
                      "nmcli" "-t" "-f" "TYPE,STATE" "device" "status"))))

(defun active-wifi ()
  "Return (SSID SIGNAL) for the wifi network in use, or NIL."
  (dolist (line (split-lines
                 (command-output
                  "nmcli" "-t" "-f" "SSID,IN-USE,SIGNAL"
                  "device" "wifi" "list")))
    (destructuring-bind (&optional ssid in-use (strength "") &rest rest)
        (split-terse line)
      (declare (ignore rest))
      (when (equal in-use "*")
        (return (list ssid (parse-integer strength :junk-allowed t)))))))

(defun status-text (show-ssid)
  "Return the network state as display text; wifi wins over ethernet.
With SHOW-SSID, follow the wifi icon with the network name."
  (let ((wifi (active-wifi)))
    (cond (wifi
           (destructuring-bind (ssid strength) wifi
             (let ((icon (aref *wifi-icons*
                               (min (1- (length *wifi-icons*))
                                    (floor strength 26)))))
               (if show-ssid
                   (format nil "~a (~a)" icon ssid)
                   icon))))
          ((ethernet-connected-p) *ethernet-icon*)
          (t *disconnected-icon*))))

(defun monitor-stream ()
  "Return a stream of nmcli monitor events, or an empty stream on failure."
  (handler-case
      (sb-ext:process-output
       (sb-ext:run-program "nmcli" '("monitor")
                           :search t :wait nil
                           :input nil :output :stream :error nil))
    (error ()
      (make-string-input-stream ""))))

(defkind network-manager (&key ssid)
  "Network state, updated on every nmcli event. SSID shows the network."
  (list :interval :persistent
        :event-source #'monitor-stream
        :body (list :full-text (string (code-char #x2026)))
        :procedure
        (guarded
         (lambda (body)
           (setf (swblock-full-text body) (status-text ssid))
           body))))
