;;;; Regression tests for :lol-web/resources.
;;;;
;;;; The package exports nothing today — every symbol is internal — so
;;;; tests use `lol-web/resources::` qualifiers throughout. Coverage:
;;;; resource-state predicates, defresource macroexpansion contract,
;;;; cache key shape, and clear-resource-cache scope.

(in-package :lol-web/resources/test)
(in-suite :lol-web/resources/test)

;;; ============================================================================
;;; resource-state predicates dispatch on the :status keyword
;;; ============================================================================

(test regression-resource-state-predicates
  "The four predicates return T iff status matches their keyword."
  (let ((idle    (lol-web/resources::make-resource-state :status :idle))
        (loading (lol-web/resources::make-resource-state :status :loading))
        (success (lol-web/resources::make-resource-state :status :success
                                                         :data '(:k 1)))
        (err     (lol-web/resources::make-resource-state :status :error
                                                         :error "boom")))
    (is (lol-web/resources::resource-idle-p idle))
    (is (lol-web/resources::resource-loading-p loading))
    (is (lol-web/resources::resource-success-p success))
    (is (lol-web/resources::resource-error-p err))
    ;; Cross-checks: idle is not loading, etc.
    (is (not (lol-web/resources::resource-loading-p idle)))
    (is (not (lol-web/resources::resource-success-p err)))
    (is (not (lol-web/resources::resource-error-p success)))))

(test regression-with-resource-idle-branch-does-not-fetch-twice
  "with-resource renders loading for an idle state instead of calling the
   fetch function again from a dead fallback branch."
  (let ((calls 0))
    (flet ((fetch-probe ()
             (incf calls)
             (lol-web/resources::make-resource-state :status :idle)))
      (let ((html (lol-web/resources::with-resource (data (probe))
                    (declare (ignore data))
                    "done")))
        (is (= 1 calls))
        (is (search "Loading..." html))))))

(test regression-resource-state-accessors
  "Constructor stores data/error/timestamp/params and accessors retrieve
   them — guards against accidental slot renames during refactors."
  (let ((s (lol-web/resources::make-resource-state
            :status :success
            :data '(:user "alice")
            :timestamp 12345
            :params '(:id 7))))
    (is (eq :success (lol-web/resources::resource-state-status s)))
    (is (equal '(:user "alice") (lol-web/resources::resource-state-data s)))
    (is (= 12345 (lol-web/resources::resource-state-timestamp s)))
    (is (equal '(:id 7) (lol-web/resources::resource-state-params s)))))

;;; ============================================================================
;;; cache key shape — `name:params` so prefix-matching in
;;; clear-resource-cache works
;;; ============================================================================

(test regression-make-cache-key-shape
  "Cache keys start with `<name>:` so `clear-resource-cache <name>` can
   prefix-match. Changing this shape breaks targeted cache invalidation."
  (let ((k (lol-web/resources::make-cache-key 'user-data '(7))))
    (is (stringp k))
    (is (search "USER-DATA:" k)
        "key must start with the resource name followed by `:`")))

(test regression-clear-resource-cache-targets-prefix
  "Clearing a specific resource removes only its keys, leaving
   unrelated entries intact."
  ;; Seed two distinct resources in the cache.
  (lol-web/resources::set-cached-data 'alpha '(1) "A1")
  (lol-web/resources::set-cached-data 'alpha '(2) "A2")
  (lol-web/resources::set-cached-data 'beta  '(1) "B1")
  ;; Clear alpha only.
  (lol-web/resources::clear-resource-cache 'alpha)
  (is (null (lol-web/resources::get-cached-data 'alpha '(1)))
      "alpha/(1) should be evicted")
  (is (null (lol-web/resources::get-cached-data 'alpha '(2)))
      "alpha/(2) should be evicted")
  (is (string= "B1" (lol-web/resources::get-cached-data 'beta '(1)))
      "beta/(1) must survive — clear was prefix-scoped")
  ;; Cleanup: full clear after the test.
  (lol-web/resources::clear-resource-cache))

(test regression-clear-resource-cache-full-when-no-name
  "Calling clear-resource-cache with no argument empties the entire
   cache."
  (lol-web/resources::set-cached-data 'foo '(1) "f1")
  (lol-web/resources::set-cached-data 'bar '(1) "b1")
  (lol-web/resources::clear-resource-cache)
  (is (null (lol-web/resources::get-cached-data 'foo '(1))))
  (is (null (lol-web/resources::get-cached-data 'bar '(1))))
  (is (= 0 (getf (lol-web/resources::resource-cache-stats) :cached-items))))

;;; ============================================================================
;;; register-resource / get-resource-spec / list-resources round-trip
;;; ============================================================================

(test regression-resource-registry-roundtrip
  "register-resource stores a spec retrievable by get-resource-spec, and
   the resource appears in list-resources."
  (let ((spec (list :fetcher (lambda () 'data)
                    :cache :none)))
    (lol-web/resources::register-resource 'probe-resource spec)
    (is (eq spec (lol-web/resources::get-resource-spec 'probe-resource)))
    (is (member 'probe-resource (lol-web/resources::list-resources)))))

(test regression-fetch-resource-unknown-returns-error-state
  "Fetching an unregistered resource returns a state with :status :error
   rather than signaling — callers can render the error via the resource
   pipeline."
  (let ((state (lol-web/resources::fetch-resource 'definitely-not-registered)))
    (is (lol-web/resources::resource-error-p state))
    (is (search "not found"
                (lol-web/resources::resource-state-error state)))))

;;; ============================================================================
;;; *resource-cache* is bounded — request-derived keys cannot OOM
;;; ============================================================================

(test regression-resource-cache-bounded
  "*RESOURCE-CACHE* is a bounded LRU cache: cycling more distinct keys than
   its cap evicts rather than grows without limit. The timestamp folded into
   each (TIMESTAMP . DATA) value round-trips through get-cached-data."
  (let ((lol-web/resources::*resource-cache*
          (lol-web/core:make-bounded-cache :max-entries 4 :test 'equal)))
    (dotimes (i 20)
      (lol-web/resources::set-cached-data 'r (list i) i))
    (is (<= (lol-web/core:bounded-cache-count
             lol-web/resources::*resource-cache*)
            4)
        "cache must stay at or below its 4-entry cap after 20 distinct inserts")
    ;; Timestamp-folded value round-trips: a freshly set datum reads back.
    (lol-web/resources::set-cached-data 'r '(:fresh) 99)
    (is (eql 99 (lol-web/resources::get-cached-data 'r '(:fresh)))
        "get-cached-data must return the DATA half of the folded value")))
