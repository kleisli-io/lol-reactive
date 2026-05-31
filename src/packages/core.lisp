;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; :lol-web/core — reactive primitives, component system, composition
;;;;   src/core/{signals,state,collections,components}.lisp
;;;;   src/composition/{props,context,children}.lisp

(in-package :cl-user)

(defpackage :lol-web/core
  (:use :cl :iterate)
  (:import-from :let-over-lambda
                :pandoriclet :with-pandoric
                :dlambda :defmacro!
                :symb)
  (:import-from :lol-web/crypto
                #:random-bytes-hex)
  (:export
   ;; signals.lisp
   #:*current-effect*
   #:make-signal
   #:make-effect
   #:make-computed
   #:batch
   #:with-lol-web-thread-safety
   #:with-reactive-context
   #:make-pandoric-signal
   ;; bounded-cache.lisp
   #:bounded-cache
   #:bounded-cache-p
   #:make-bounded-cache
   #:bounded-cache-count
   #:bounded-cache-get
   #:bounded-cache-set
   #:bounded-cache-remove
   #:bounded-cache-clear
   #:bounded-cache-keys
   ;; cycle-safe-printer.lisp
   #:with-cycle-safe-printer
   #:bounded-serialize
   #:*serialize-truncation-marker*
   ;; state.lisp
   #:make-store
   #:make-evolving-component
   #:make-component-factory
   #:*factory-registry*
   ;; collections.lisp
   #:make-reactive-list
   ;; components.lisp
   #:defcomponent
   #:with-component-state
   #:register-component
   #:unregister-component
   #:find-component
   #:component-principal-binding
   #:component-snapshots
   #:component-undo-stack
   #:component-redo-stack
   #:component-originals
   #:with-components-lock
   #:generate-component-id
   #:*components*
   ;; composition/props.lisp
   #:defcomponent-with-props
   #:with-props
   #:validate-props
   ;; composition/context.lisp
   #:defcontext
   #:defcontext-signal
   #:list-contexts
   #:get-context-info
   #:inspect-context
   #:inspect-all-contexts))
