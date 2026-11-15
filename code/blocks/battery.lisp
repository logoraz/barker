(defpackage #:barker/blocks/battery
  (:use #:cl)
  (:import-from #:barker/base/swaybar-protocol
                #:swblock-full-text
                #:swblock-urgent)
  (:import-from #:barker/base/bkblock
                #:defkind)
  (:import-from #:barker/blocks/support
                #:glyphs
                #:guarded
                #:read-file-line)
  (:documentation "Battery block: charge level read from sysfs."))

(in-package #:barker/blocks/battery)


(defparameter *power-supply-path* "/sys/class/power_supply/"
  "Directory that lists the system's power supplies.")

(defparameter *icons*
  (glyphs #xF007A #xF007B #xF007C #xF007D #xF007E
          #xF007F #xF0080 #xF0081 #xF0082 #xF0079)
  "Nerd font battery icons, from nearly empty to full.")

(defparameter *charging-icon* (aref (glyphs #xF0084) 0)
  "Nerd font icon shown while the battery charges.")

(defun find-power-supply (prefix &optional (default ""))
  "Return the first power supply directory whose name contains PREFIX.
Names are tried in alphabetical order. When none matches, return the
path for PREFIX followed by DEFAULT."
  (let ((names (sort (mapcar (lambda (path)
                               (car (last (pathname-directory path))))
                             (directory
                              (concatenate 'string *power-supply-path* "*/")
                              :resolve-symlinks nil))
                     #'string<)))
    (concatenate 'string *power-supply-path*
                 (or (find-if (lambda (name) (search prefix name)) names)
                     (format nil "~a~a" prefix default)))))

(defun battery-label (level charging nerd-icons)
  "Return the label for LEVEL percent, CHARGING, with NERD-ICONS or text."
  (cond (charging (if nerd-icons *charging-icon* "CHRG:"))
        (nerd-icons (aref *icons* (min (1- (length *icons*))
                                       (floor level 10))))
        (t "BAT:")))

(defkind battery (&key (format "~a ~a%") nerd-icons bat ac)
  "Battery level every 3 seconds. BAT and AC are sysfs supply directories."
  (let ((capacity (concatenate 'string
                               (or bat (find-power-supply "BAT" 0))
                               "/capacity"))
        (online (concatenate 'string
                             (or ac (find-power-supply "AC"))
                             "/online")))
    (list :interval 3
          :body (list :full-text "N/A")
          :procedure
          (guarded
           (lambda (body)
             (let* ((level (parse-integer (read-file-line capacity)))
                    (charging (eql 1 (ignore-errors
                                      (parse-integer
                                       (read-file-line online))))))
               (setf (swblock-full-text body)
                     (format nil format
                             (battery-label level charging nerd-icons)
                             level)
                     (swblock-urgent body) (<= level 10))
               body))))))
