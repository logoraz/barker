(defpackage #:barker/lib/platform
  (:use #:cl)
  (:local-nicknames (#:sbc #:sb-concurrency)
                    (#:sba #:sb-alien)
                    (#:sbs #:sb-sys))
  (:export #:sigrtmin
           #:sigaction)
  (:export #:make-notifier
           #:notifier-notify
           #:notifier-wait)

  (:documentation "Platform Specifics: Signaling & Thread Notifications."))

(in-package #:barker/lib/platform)


;;; Signaling

(defun sigrtmin ()
  "Return the platform's first real-time signal number."
  (sba:alien-funcall
   (sba:extern-alien "__libc_current_sigrtmin"
                          (function sb-alien:int))))

(defun sigaction (signo handler)
  "Call HANDLER, a function of no arguments, when SIGNO arrives.
Replaces any handler already set for SIGNO. HANDLER runs in whichever
thread receives the signal, so keep it short and avoid taking locks."
  (sbs:enable-interrupt
   signo (lambda (signo info context)
           (declare (ignore signo info context))
           (funcall handler))))

;;; Notifications

(defstruct (notifier (:constructor make-notifier ()))
  "A wake-up channel: producers notify, a consumer waits."
  (mailbox (sbc:make-mailbox) :read-only t))

(defun notifier-notify (notifier)
  "Wake NOTIFIER's waiter. Safe to call from a signal handler."
  (sbc:send-message (notifier-mailbox notifier) t))

(defun notifier-wait (notifier)
  "Block until NOTIFIER is notified, then discard any extra wake-ups."
  (let ((mailbox (notifier-mailbox notifier)))
    (sbc:receive-message mailbox)
    (sbc:receive-pending-messages mailbox)
    t))
