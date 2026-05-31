;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/HTMX; Base: 10 -*-
;;;; HTMX-style client runtime — composer + public API + boot
;;;;
;;;; The runtime is split across runtime/{config,swap,ajax,triggers}.lisp.
;;;; Each split file exports a defun returning a flat property-value pair list
;;;; that this composer splices into a single (ps:create ...) form via ps:ps*.
;;;;
;;;; Composer order matches source order to keep the compiled JS byte-stable
;;;; across the split. Adding a new cluster: write the pairs-helper, splice it
;;;; in below, and update the regression marker test.

(in-package :lol-web/htmx)

(defun htmx-runtime-public-api-pairs ()
  "Property-value pairs for the public htmx.* API + init. Stays here because
   init wires together every cluster (process-element, process-hx-on, setup-
   autocomplete) — the natural seam is the composer."
  (list
   ;; htmx.process(elt) - initialize htmx behavior on dynamically added content
   "process" '(lambda (elt)
                ;; Process the element itself if it has verb attributes
                (when (or (ps:chain elt (get-attribute "hx-get"))
                          (ps:chain elt (get-attribute "hx-post"))
                          (ps:chain elt (get-attribute "hx-put"))
                          (ps:chain elt (get-attribute "hx-delete")))
                  ((ps:@ *htmx* process-element) elt))
                ;; Process children with verb attributes
                (let ((children (ps:chain elt (query-selector-all
                                               "[hx-get], [hx-post], [hx-put], [hx-delete]"))))
                  (ps:chain children (for-each (ps:@ *htmx* process-element))))
                ;; Process hx-on-* on this element and all children
                ((ps:@ *htmx* process-hx-on) elt)
                (let ((all-children (ps:chain -array (from (ps:chain elt (get-elements-by-tag-name "*"))))))
                  (ps:chain all-children (for-each
                    (lambda (child)
                      (let ((attrs (ps:chain -array prototype slice (call (ps:chain child attributes)))))
                        (when (ps:chain attrs (some (lambda (attr)
                                                      (ps:chain (ps:@ attr name) (starts-with "hx-on")))))
                          ((ps:@ *htmx* process-hx-on) child))))))))

   ;; htmx.ajax(verb, path, target) - issue programmatic AJAX request
   "ajax" '(lambda (verb path target)
             (let ((target-el (if (stringp target)
                                  (ps:chain document (query-selector target))
                                  target)))
               (when target-el
                 ((ps:@ *htmx* issue-request) target-el verb path))))

   ;; htmx.trigger(elt, name, detail) - dispatch custom event on element
   "trigger" '(lambda (elt name detail)
                ((ps:@ *htmx* dispatch-event) name elt (or detail (ps:create))))

   ;; htmx.on(elt, event, fn) - add event listener, returns listener
   "on" '(lambda (elt event-name listener)
           (ps:chain elt (add-event-listener event-name listener))
           listener)

   ;; htmx.off(elt, event, fn) - remove event listener
   "off" '(lambda (elt event-name listener)
            (ps:chain elt (remove-event-listener event-name listener)))

   ;; htmx.onLoad(fn) - register callback for htmx:load events
   "onLoad" '(lambda (callback)
               (ps:chain document (add-event-listener "htmx:load"
                 (lambda (evt)
                   (callback (ps:@ evt detail elt))))))

   "init" '(lambda ()
            ;; Process hx-* elements (request verbs + hx-on-* handlers)
            (let ((htmx-elements (ps:chain document (query-selector-all
                                                     "[hx-get], [hx-post], [hx-put], [hx-delete]"))))
              (ps:chain htmx-elements (for-each (lambda (el)
                                                  (ps:chain *htmx* (process-element el))))))
            ;; Process hx-on-* on elements without request verbs
            ;; Scan all elements for any hx-on-* attribute (not just a hardcoded list)
            (let ((all-elements (ps:chain -array (from (ps:chain document (get-elements-by-tag-name "*"))))))
              (ps:chain all-elements (for-each
                (lambda (el)
                  ;; Skip if already processed via processElement (has verb attributes)
                  (unless (or (ps:chain el (get-attribute "hx-get"))
                              (ps:chain el (get-attribute "hx-post"))
                              (ps:chain el (get-attribute "hx-put"))
                              (ps:chain el (get-attribute "hx-delete")))
                    ;; Check if any attribute starts with hx-on
                    (let ((attrs (ps:chain -array prototype slice (call (ps:chain el attributes)))))
                      (when (ps:chain attrs (some (lambda (attr)
                                                    (ps:chain (ps:@ attr name) (starts-with "hx-on")))))
                        ((ps:@ *htmx* process-hx-on) el))))))))
            ;; Initialize autocomplete keyboard navigation
            (let ((ac-elements (ps:chain document (query-selector-all "[aria-autocomplete]"))))
              (ps:chain ac-elements (for-each
                (lambda (el)
                  (let ((el-id (ps:@ el id)))
                    (when el-id
                      ((ps:@ *htmx* setup-autocomplete)
                       el-id (+ "#" el-id "-results"))))))))
            (ps:chain console (log "(HTMX :status :loaded :version" (ps:@ *htmx* version) ")")))))

(defun htmx-runtime-js ()
  "Generate the HTMX-style client runtime via Parenscript.
   Composes pairs from runtime/config.lisp, runtime/swap.lisp, runtime/ajax.lisp,
   runtime/triggers.lisp, plus the public API defined here, into a single
   *htmx* object literal. Splices via ps:ps* + quasiquote to preserve the
   literal one-object-init shape — every property lands in one (ps:create ...).

   Returns a SAFE-HTML-STRING so html-page emits the script verbatim
   instead of escaping it."
  (lol-web/html:make-safe-html-string
   (parenscript:ps*
    `(defvar *htmx*
       (ps:create
        ,@(htmx-runtime-config-pairs)
        ,@(htmx-runtime-swap-pairs)
        ,@(htmx-runtime-ajax-pairs)
        ,@(htmx-runtime-triggers-pairs)
        ,@(htmx-runtime-public-api-pairs)))

    ;; Auto-initialize on DOMContentLoaded
    `(if (= (ps:@ document ready-state) "loading")
         (ps:chain document (add-event-listener "DOMContentLoaded"
                                                (ps:@ *htmx* init)))
         ((ps:@ *htmx* init)))

    ;; Lowercase alias for compatibility with standard htmx naming
    `(setf (ps:@ window htmx) *htmx*))))

;;; ============================================================================
;;; HTMX ATTRIBUTE HELPERS
;;; ============================================================================

(defun emit-hx-attrs (verb url target swap trigger)
  "Canonical scheme-guarded, value-escaped emitter for htmx verb attributes.
   Render `hx-<verb>=\"<url>\"` plus optional hx-target/hx-swap/hx-trigger
   pairs. URL is run through `safe-url` (rejects javascript:/data:/
   vbscript: schemes — returns NIL, suppressing the attribute via
   `~@[~]`) and then `escape-attribute`. TARGET/SWAP/TRIGGER are also
   `escape-attribute`d since they may carry caller-controlled data.
   hx-get/hx-post/hx-put/hx-delete and hx-morph all route through this so the
   scheme guard and escaping cannot drift between them."
  (let ((safe-url (lol-web/escape:escape-attribute
                   (lol-web/escape:safe-url url))))
    (format nil "~@[hx-~A=\"~a\"~]~@[ hx-target=\"~a\"~]~@[ hx-swap=\"~a\"~]~@[ hx-trigger=\"~a\"~]"
            (and safe-url verb) safe-url
            (lol-web/escape:escape-attribute target)
            (lol-web/escape:escape-attribute swap)
            (lol-web/escape:escape-attribute trigger))))

(defun hx-get (url &key target swap trigger)
  "Generate hx-get attribute string for cl-who. Unsafe URL schemes
   suppress the attribute; target/swap/trigger are attribute-escaped."
  (emit-hx-attrs "get" url target swap trigger))

(defun hx-post (url &key target swap trigger)
  "Generate hx-post attribute string for cl-who. Unsafe URL schemes
   suppress the attribute; target/swap/trigger are attribute-escaped."
  (emit-hx-attrs "post" url target swap trigger))

(defun hx-put (url &key target swap trigger)
  "Generate hx-put attribute string for cl-who. Unsafe URL schemes
   suppress the attribute; target/swap/trigger are attribute-escaped."
  (emit-hx-attrs "put" url target swap trigger))

(defun hx-delete (url &key target swap trigger)
  "Generate hx-delete attribute string for cl-who. Unsafe URL schemes
   suppress the attribute; target/swap/trigger are attribute-escaped."
  (emit-hx-attrs "delete" url target swap trigger))
