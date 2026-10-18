;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

;;; Port of gubar's xkb-layout block.
;;; Example binding within ~/.config/sway/config:
;;;   bindsym --release $mod+Control_R exec swaymsg input type:keyboard \
;;;     xkb_switch_layout next && pkill -SIGRTMIN+3 -n barker

(defpackage #:barker/blocks/xkb-layout
  (:use #:cl)
  (:local-nicknames (#:bk #:barker/base/bkblock)
                    (#:sp #:barker/base/swaybar-protocol)
                    (#:jzon #:com.inuoe.jzon)
                    (#:support #:barker/blocks/support))
  (:documentation "XKB layout block: the active keyboard layout."))

(in-package #:barker/blocks/xkb-layout)


(defun active-layout ()
  "Return (NAME INDEX) for the first input with an active layout, or NIL."
  (let ((inputs (jzon:parse (support:command-output "swaymsg" "-rt"
                                                    "get_inputs"))))
    (loop :for input :across inputs
          :for index = (gethash "xkb_active_layout_index" input)
          :when index
            :return (list (gethash "xkb_active_layout_name" input) index))))

(defun layout-alias (alias-pairs name index)
  "Return the alias for layout NAME or INDEX in ALIAS-PAIRS, else NAME."
  (or (cdr (assoc name alias-pairs :test #'equal))
      (cdr (assoc index alias-pairs :test #'equal))
      name))

(bk:defkind xkb-layout (&key alias-pairs)
  "Active layout, updated on signal 3 (SIGRTMIN+3).
ALIAS-PAIRS is an alist from layout name or index to display text."
  (list :trigger 3
        :interval :persistent
        :procedure
        (support:guarded
         (lambda (body)
           (destructuring-bind (name index) (active-layout)
             (setf (sp:swblock-full-text body)
                   (layout-alias alias-pairs name index))
             body)))))
