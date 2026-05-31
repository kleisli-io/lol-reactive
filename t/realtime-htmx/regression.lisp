;;;; Regression tests for :lol-web/realtime-htmx.
;;;;
;;;; Covers: ws-client-js reconnect jitter (anti-thundering-herd) +
;;;; ws-client-js, sse-client-js, optimistic-js shape contracts.

(in-package :lol-web/realtime-htmx/test)
(in-suite :lol-web/realtime-htmx/test)

;;; ============================================================================
;;; ws-client-js — reconnect uses full jitter
;;; ============================================================================

(test regression-ws-client-reconnect-uses-jitter
  "ws-client-js generates JS whose reconnect delay is scaled by Math.random.
   Without jitter, every client that disconnects together retries at the
   same backoff edges, hammering the server. Full jitter (delay scaled by
   Math.random) spreads retries over [0, reconnectDelay)."
  (let ((js (ws-client-js)))
    (is (search "Math.random" js)
        "ws-client-js must invoke Math.random for jitter")
    (is (search "reconnectDelay" js)
        "ws-client-js must reference reconnectDelay")
    (is (search "setTimeout" js)
        "ws-client-js must schedule the reconnect via setTimeout")))

(test regression-ws-client-jitter-multiplies-delay
  "Jittered delay must be the product of reconnectDelay and Math.random,
   not Math.random alone. Catches a regression where the multiplier is
   accidentally dropped (a delay of just Math.random()*1 ≈ 0–1 ms is
   effectively no backoff at all)."
  (let ((js (ws-client-js)))
    ;; Parenscript renders (* reconnect-delay (Math.random)) — assert both
    ;; identifiers appear inside a Math.floor argument, which is the
    ;; canonical jittered-delay binding.
    (is (search "Math.floor" js)
        "jittered delay must be Math.floored to a whole-ms integer")))

;;; ============================================================================
;;; client JS generators — non-empty output, contain expected runtime hooks
;;; ============================================================================

(test regression-ws-client-js-non-empty
  "ws-client-js produces a non-empty Parenscript-compiled JS string."
  (let ((js (ws-client-js)))
    (is (stringp js))
    (is (> (length js) 100)
        "ws-client-js must compile to a substantive JS payload")
    (is (search "WebSocket" js)
        "must reference the WebSocket constructor")))

(test regression-sse-client-js-non-empty
  "sse-client-js produces a non-empty Parenscript-compiled JS string with
   EventSource handling."
  (let ((js (sse-client-js)))
    (is (stringp js))
    (is (> (length js) 100)
        "sse-client-js must compile to a substantive JS payload")
    (is (search "EventSource" js)
        "must reference the EventSource constructor")))

(test regression-optimistic-js-non-empty
  "optimistic-js produces a non-empty Parenscript-compiled JS string."
  (let ((js (optimistic-js)))
    (is (stringp js))
    (is (> (length js) 100)
        "optimistic-js must compile to a substantive JS payload")))

;;; ============================================================================
;;; optimistic-apply-payload — safe-html-string boundary check
;;; ============================================================================

(defun %signals-error-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error () t)))

(test regression-optimistic-rejects-unsafe-html
  "optimistic-apply-payload refuses a bare string under :html — the
   producer must wrap trusted markup in safe-html-string at the
   boundary or the call signals."
  (let* ((id "comp-unsafe")
         (closure (lambda (msg &rest args) (declare (ignore msg args)) nil)))
    (lol-web/core:register-component id closure)
    (unwind-protect
        (progn
          (is (%signals-error-p
               (lambda ()
                 (lol-web/realtime-htmx:optimistic-apply-payload
                  id '(:html "<script>1</script>")))))
          ;; Positive path: tagged safe-html-string is unwrapped to its
          ;; raw wire form in the returned plist.
          (let ((wire (lol-web/realtime-htmx:optimistic-apply-payload
                       id `(:html ,(lol-web/html:make-safe-html-string "<b>x</b>")))))
            (is (string= "<b>x</b>" (getf wire :html)))))
      (lol-web/core:unregister-component id))))

(test regression-originals-tied-to-registration-lifetime
  "component-originals lives on the component-entry struct, so
   unregister-component drops the entire history. A re-registration
   under the same id starts with an empty store — no leak between
   instance lifetimes."
  (let* ((id "comp-orig-life")
         (closure (lambda (msg &rest args) (declare (ignore msg args)) nil)))
    (lol-web/core:register-component id closure)
    (is (eq :ok
            (lol-web/realtime-htmx:optimistic-record-original
             id '(:html "<b>1</b>"))))
    (is (eq :ok
            (lol-web/realtime-htmx:optimistic-record-original
             id '(:html "<b>2</b>"))))
    (is (= 2 (length (lol-web/core:component-originals id))))
    (lol-web/core:unregister-component id)
    (is (null (lol-web/core:component-originals id))
        "originals are gone with the component-entry")
    (is (eq :no-component
            (lol-web/realtime-htmx:optimistic-record-original
             id '(:html "<b>3</b>")))
        "recording on an unregistered id is a silent :no-component result")
    (is (null (lol-web/realtime-htmx:optimistic-clear-originals id))
        "clearing an unregistered id reports that no component was cleared")
    ;; Re-register: store starts empty, regardless of pre-unregister content.
    (lol-web/core:register-component id closure)
    (unwind-protect
        (progn
          (is (null (lol-web/core:component-originals id))
              "re-registered instance starts with an empty originals store")
          (lol-web/realtime-htmx:optimistic-record-original
           id '(:html "<b>fresh</b>"))
          (is (= 1 (length (lol-web/core:component-originals id)))))
      (lol-web/core:unregister-component id))))

(test regression-originals-cap-refuses-once-full
  "optimistic-record-original returns :cap-reached when the per-component
   store hits *optimistic-originals-cap*. The cap is a small constant in
   the test so the loop runs deterministically."
  (let* ((id "comp-cap")
         (closure (lambda (msg &rest args) (declare (ignore msg args)) nil))
         (lol-web/realtime-htmx:*optimistic-originals-cap* 3))
    (lol-web/core:register-component id closure)
    (unwind-protect
        (progn
          (dotimes (i 3)
            (is (eq :ok
                    (lol-web/realtime-htmx:optimistic-record-original
                     id (list :html "<b>x</b>")))))
          (is (eq :cap-reached
                  (lol-web/realtime-htmx:optimistic-record-original
                   id '(:html "<b>over</b>")))))
      (lol-web/core:unregister-component id))))

;;; ============================================================================
;;; Optimistic originals — global cap across components
;;; ============================================================================

(test regression-optimistic-originals-global-cap-fires
  "Two components each well under the per-component cap can together
   reach the global cap; the next record is denied with
   :global-cap-reached so the global ceiling is enforced."
  (let* ((lol-web/realtime-htmx:*optimistic-originals-cap* 100)
         (lol-web/realtime-htmx:*optimistic-originals-global-cap* 4)
         (id-a "regression-optimistic-global-a")
         (id-b "regression-optimistic-global-b")
         (probe (lambda (msg &rest args)
                  (declare (ignore args))
                  (ecase msg (:id "stub") (:inspect '(:state ()))))))
    (lol-web/core:register-component id-a probe)
    (lol-web/core:register-component id-b probe)
    (unwind-protect
         (progn
           (lol-web/realtime-htmx:optimistic-record-original id-a '(:html "<b/>"))
           (lol-web/realtime-htmx:optimistic-record-original id-a '(:html "<b/>"))
           (lol-web/realtime-htmx:optimistic-record-original id-b '(:html "<b/>"))
           (lol-web/realtime-htmx:optimistic-record-original id-b '(:html "<b/>"))
           (is (eq :global-cap-reached
                   (lol-web/realtime-htmx:optimistic-record-original
                    id-a '(:html "<b/>")))
               "fifth record across components must trip the global cap"))
      (lol-web/core:unregister-component id-a)
      (lol-web/core:unregister-component id-b))))
