;; Copyright © 2025 Kyuvi.
;; This file is published under version 3 of the GNU General General Public license.
;;
;; Module defining Gusts core procedures

(define-module (gust core)
  #:use-module ((guile)
    #:select ((define . def-) (define-public . def)
              (procedure? . prc?) (procedure? . fn?)
              (use-modules . use)
              (defined? . defd?)))
  #:export (fn defn defn-)
  #:re-export (def- def prc? fn? use defd?))

        ;;;; helper procedures ;;;;

(def (lambda-keys args-list)
  "(lambda-keys arg-list)
   Test if the list 'args-list' contains one or more of the keywords #:optn,
   #:optional, #:key, and #:rest."
  (if (list? args-list)
      (map (lambda (x) (if (memq x '(#:optn #:optional #:key #:rest)) x))
              args-list)
      '()))

(def (replace-optn! arg-list)
  "(replace-optn! arg-list)
   Test if 'arg-list' contains the #:optn keyword and replace it
   with the #:optional keyword if it does."
  (let ((opts (memq #:optn arg-list)))
    (if opts (set-car! opts #:optional))))

        ;;;; procedures ;;;;

(cond-expand
 (guile-2

  (define-syntax fn
    (lambda (stx)
      "(fn (params*) doc-string? exprs*)
       (prc (params*) doc-string? exprs*)
       Create an anonymous procedure using either lambda or lambda*
       depending on the contents of the 'params' list.
       The inculsion of one of the keywords #:optn (same as #:optional),
       #:optional, #:key, or #:rest in 'params' triggers the use of lambda*."
      (syntax-case stx ()
        ((fn params body ...)
         (let ((params-datum (syntax->datum (syntax params))))
           (if (null? (lambda-keys params-datum))
               (syntax (lambda params body ...))
               (begin (replace-optn! params-datum)
                      (syntax (lambda* params body ...)))))))))

 (export (fn . prc))

  (define-syntax defn-
    (syntax-rules ()
      "(defn- name (params*) doc-string? exprs*)
       Define a procedure 'name' using either lambda or lambda*
       depending on the contents of the 'params' list.
       The inculsion of one of the keywords #:optn (same as #:optional),
       #:optional, #:key or #:rest in 'params' triggers the use of lambda*."
      ((defn- name params body ...)
       (define name (fn params body ...)))))

  (define-syntax defn
    (syntax-rules ()
      "(defn name (params*) doc-string? exprs*)
       Define a public (exported) procedure 'name' using either lambda or lambda*
       depending on the contents of the 'params' list.
       The inculsion of one of the keywords #:optn (same as #:optional),
       #:optional, #:key or #:rest in 'params' triggers the use of lambda*."
      ((defn name params body ...)
       (define-public name (fn params body ...)))))
  )

 (guile

  ;; importing syncase doubles the start up time of guile 1 and syncase is not
  ;; as advancesd as the guile-2+ hygenic macro system and therefore not really
  ;; compatiable so define-macro is used instead.

  (use-modules (ice-9 optargs))

  (re-export lambda* let-optional let-optional* let-keywords let-keywords*)

  (define-macro (fn params . fdecl)
    "(fn (params*) doc-string? exprs*)
     (prc (params*) doc-string? exprs*)
     Create an anonymous procedure using either lambda or lambda*
     depending on the contents of the 'params' list.
     The inculsion of one of the keywords #:optn (same as #:optional),
     #:optional, #:key, or #:rest in 'params' triggers the use of lambda*."
    (let ((key-pred (lambda-keys params)))
      (if (null? key-pred)
          `(lambda ,params ,@fdecl)
          (begin (replace-optn! params)
                 `(lambda* ,params ,@fdecl)))))

  (def prc fn)

  (define-macro (defn- name params . fdecl)
    "(defn- name (params*) doc-string? exprs*)
     Define a procedure 'name' using either lambda or lambda*
     depending on the contents of the 'params' list.
     The inculsion of one of the keywords #:optn (same as #:optional),
     #:optional, #:key or #:rest in 'params' triggers the use of lambda*."
    `(define ,name (fn ,params ,@fdecl)))
  ;;
  (define-macro (defn name params . fdecl)
    "(defn name (params*) doc-string? exprs*)
     Define a public (exported) procedure 'name' using either lambda or lambda*
     depending on the contents of the 'params' list.
     The inculsion of one of the keywords #:optn (same as #:optional),
     #:optional, #:key or #:rest in 'params' triggers the use of lambda*."
    `(define-public ,name (fn ,params ,@fdecl)))

        ;;;; a working system for using syncase  ;;;;

  ;; (use-syntax (ice-9 syncase))
  ;;
  ;; (define-syntax fn
  ;;   (lambda (stx)
  ;;     "(fn (params*) doc-string? exprs*)
  ;;      (prc (params*) doc-string? exprs*)
  ;;      Create an anonymous procedure using either lambda or lambda*
  ;;      depending on the contents of the 'params' list.
  ;;      The inculsion of one of the keywords #:optn (same as #:optional),
  ;;      #:optional, #:key, or #:rest in 'params' triggers the use of lambda*."
  ;;     (syntax-case stx ()
  ;;       ((fn params body ...)
  ;;        (let ((params-datum (syntax->datum (syntax params))))
  ;;          (if (null? (lambda-keys params-datum))
  ;;              (syntax (lambda params body ...))
  ;;              (begin (replace-optn! params-datum)
  ;;                     (syntax (lambda* params body ...)))))))))

  ;; (define-syntax prc
  ;;   (syntax-rules ()
  ;;     ((_ . args) (fn . args))))

  ;; (export prc)

  ;; (define-syntax defn-
  ;;   (syntax-rules ()
  ;;     ((defn- name params body ...)
  ;;      (define name (fn params body ...)))))

  ;; (define-syntax defn
  ;;   (syntax-rules ()
  ;;     ((defn name params body ...)
  ;;      (define-public name (fn params body ...)))))
  ))
