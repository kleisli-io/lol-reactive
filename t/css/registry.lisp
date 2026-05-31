(in-package :lol-web/css/test)
(in-suite :lol-web/css/test)

;;; ============================================================================
;;; Registry mutation + lookup
;;; ============================================================================

(test registry-defcss-and-render
  "defcss registers a module that renders the rules it was defined with"
  (clear-css-registry)
  (defcss :test-buttons
    (".btn" (("padding" . "1rem"))))
  (let ((css (get-component-css :test-buttons)))
    (is (stringp css))
    (is (search ".btn" css))
    (is (search "padding: 1rem" css)))
  (clear-css-registry))

(test registry-clear-empties-everything
  "clear-css-registry empties both the hash-table and the load order"
  (clear-css-registry)
  (defcss :probe-a (".a" (("color" . "red"))))
  (defcss :probe-b (".b" (("color" . "blue"))))
  (is (= 2 (length (list-registered-css-components))))
  (clear-css-registry)
  (is (null (list-registered-css-components)))
  (is (null (get-css-module :probe-a))))

(test registry-load-order-is-registration-order
  "list-registered-css-components returns names in registration order"
  (clear-css-registry)
  (defcss :first  (".f" (("k" . "1"))))
  (defcss :second (".s" (("k" . "2"))))
  (defcss :third  (".t" (("k" . "3"))))
  (is (equal '(:first :second :third)
             (list-registered-css-components)))
  (clear-css-registry))

(test registry-generate-all-respects-load-order
  "generate-all-component-css emits each module's CSS in registration order"
  (clear-css-registry)
  (defcss :early (".early" (("z-index" . "1"))))
  (defcss :late  (".late"  (("z-index" . "9"))))
  (let ((all (generate-all-component-css)))
    (let ((early-pos (search ".early" all))
          (late-pos  (search ".late"  all)))
      (is (and early-pos late-pos))
      (is (< early-pos late-pos)
          "earlier-registered module appears earlier in output")))
  (clear-css-registry))

;;; ============================================================================
;;; Concurrency: registry lock serialises writes
;;; ============================================================================

(test registry-concurrent-defcss-no-duplicates-no-loss
  "Concurrent module registration produces exactly N entries with no duplicates.
   The (let ((tid tid)) …) shadow forces a fresh binding per LOOP iteration —
   without it, all threads would close over the same mutated tid cell and
   collapse onto a small subset of names."
  (clear-css-registry)
  (let* ((n-threads 16)
         (per-thread 25)
         (expected   (* n-threads per-thread))
         (threads
           (loop for tid from 0 below n-threads
                 collect (let ((tid tid))
                           (bordeaux-threads:make-thread
                            (lambda ()
                              (dotimes (i per-thread)
                                (lol-web/css::make-css-module
                                 (intern (format nil "MODULE-~D-~D" tid i) :keyword)
                                 (cons ".x" (list (cons "k" "v")))))))))))
    (dolist (th threads) (bordeaux-threads:join-thread th))
    (let ((load-order (list-registered-css-components)))
      (is (= expected (length load-order))
          "expected ~D modules registered, got ~D — concurrent push lost"
          expected (length load-order))
      (is (= expected (length (remove-duplicates load-order)))
          "load order has ~D unique entries vs ~D total — registry race"
          (length (remove-duplicates load-order)) (length load-order))))
  (clear-css-registry))

;;; ============================================================================
;;; Module messages
;;; ============================================================================

(test module-add-rule-and-render
  "A registered module accepts :add-rule and includes the rule in :render"
  (clear-css-registry)
  (let ((mod (lol-web/css::make-css-module :probe-add (cons ".base" '(("color" . "red"))))))
    (funcall mod :add-rule ".extra" '(("padding" . "1rem")))
    (let ((css (funcall mod :render)))
      (is (search ".base"  css))
      (is (search ".extra" css))
      (is (search "padding: 1rem" css))))
  (clear-css-registry))

(test regression-module-concurrent-add-rule-and-render
  "Per-module rule mutation is serialized while render reads snapshots."
  (clear-css-registry)
  (let* ((mod (lol-web/css::make-css-module
               :probe-concurrent-rules
               (cons ".base" '(("color" . "red")))))
         (n-threads 8)
         (per-thread 25)
         (threads
           (loop for tid from 0 below n-threads
                 collect (let ((tid tid))
                           (bordeaux-threads:make-thread
                            (lambda ()
                              (dotimes (i per-thread)
                                (funcall mod :add-rule
                                         (format nil ".t~D-~D" tid i)
                                         '(("padding" . "1rem")))
                                (funcall mod :render))))))))
    (dolist (th threads) (bordeaux-threads:join-thread th))
    (let ((inspect (funcall mod :inspect))
          (css (funcall mod :render)))
      (is (= (1+ (* n-threads per-thread)) (getf inspect :rule-count))
          "all concurrent :add-rule calls must be present")
      (is (search ".base" css))
      (is (search ".t0-0" css))
      (is (search ".t7-24" css))))
  (clear-css-registry))

(test module-unknown-message-errors
  "An unknown message signals an error rather than silently no-op"
  (clear-css-registry)
  (let ((mod (lol-web/css::make-css-module :probe-bad (cons ".x" '(("k" . "v"))))))
    (signals error (funcall mod :totally-unknown-message)))
  (clear-css-registry))

(test regression-media-selector-gated-by-safe-css-selector-p
  "The module renderer emits the @media block selector raw, so its safety
   rests on safe-css-selector-p at registration. A well-formed @media
   selector renders its raw header; a break-out @media string carrying
   `{`/`}`/`*/` is refused before it can enter the registry — at
   make-css-module construction and on a live module's :add-rule alike."
  (clear-css-registry)
  (unwind-protect
       (progn
         (let ((mod (make-css-module
                     :regression-media-ok
                     (cons "@media (min-width: 768px)"
                           (list (list ".x" '(("color" . "red"))))))))
           (is (search "@media (min-width: 768px) {" (funcall mod :render))
               "a safe @media selector renders its raw block header"))
         (signals unsafe-css-selector
           (make-css-module
            :regression-media-evil
            (cons "@media x { } body { background: url(evil) } /*"
                  (list (list ".x" '(("color" . "red")))))))
         (let ((mod (make-css-module :regression-media-addrule)))
           (signals unsafe-css-selector
             (funcall mod :add-rule "@media x } evil {" '()))))
    (clear-css-registry)))
