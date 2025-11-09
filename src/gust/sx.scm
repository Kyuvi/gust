;; Copyright © 2025 Kyuvi.
;; This file is published under version 3 of the GNU General Public license.
;;
;; Gust syntax and aliases that work on syntax

(define-module (gust sx)
  #:export (defalias defrule))

(cond-expand
 (guile-2

  ;; TODO: not neccesary, move to (gust repl)?
  (define-syntax defalias
    (lambda (x)
      "(defalias new-name old-name docstring?)
    Set the (macro) definition of symbol 'new-name' to that of 'old-name'.
    Useful for aliasing (syntax transformer) macros."
      (syntax-case x ()
        ((defalias new-name old-name)
         ;; this does not copy the documentation, so we create a simple replacement
         (with-syntax ((alias-doc
                        (datum->syntax x
                          (string-append "A syntax alias for "
                                         (symbol->string
                                          (syntax->datum #'old-name))
                                         "."))))
                      #'(define-syntax new-name
                                (syntax-rules ()
                                  alias-doc
                                  ((_ . args) (old-name . args))))))
        ((defalias new-name old-name docstring)
         (string? (syntax->datum (syntax docstring)))
         #'(define-syntax new-name
                   (syntax-rules ()
                     docstring
                     ((_ . args) (old-name . args))))))))

  (define-syntax defrule
    (lambda (x)
      "(defrule keyword (#:rest pattern) docstring? template)
       Define 'keyword' as a new syntax-rules macro with one clause."
      (syntax-case x ()
        ((_ name (. pattern) template)
         #'(define-syntax name
             (syntax-rules ()
               ((_ . pattern) template))))
        ((_ name (. pattern) docstring template)
         (string? (syntax->datum #'docstring))
         #'(define-syntax name
             (syntax-rules ()
               docstring
               ((_ . pattern) template)))))))

  (re-export (syntax . sx)
             (define-syntax . defsx)
             (syntax-rules . sxrule)
             (syntax-case . sxcase))

  (export (defrule . defsx-rule)
          (defalias . defsx-alias))
  )

 (guile
  (use-syntax (ice-9 syncase))

  (define syntax->datum syntax-object->datum)

  (define datum->syntax datum->syntax-object)

  (define-syntax defalias
    (syntax-rules ()
      "(defalias new-name old-name)
       Set the (macro) definition of symbol 'new-name' to that of 'old-name'.
    Useful for aliasing macros."
      ((_ new-name old-name)
       (define-syntax new-name
         (syntax-rules ()
           ((_ . args) (old-name . args)))))))

  (defalias sx syntax)

  (defalias defsx define-syntax)

  (defalias sxrule syntax-rule)

  (defalias sxcase syntax-case)

  (defalias def-syntax-alias defalias)

  (define-syntax define-syntax-rule
    (lambda (x)
      "(define-syntax-rule (keyword #:rest pattern) template)
       Define 'keyword' as a new syntax-rules macro with one clause."
      (syntax-case x ()
        ((_ (name . pattern) template)
         (syntax (define-syntax name
                   (syntax-rules ()
                     ((_ . pattern) template))))))))

  (define-syntax defrule
    (lambda (x)
      "(defrule keyword (#:rest pattern) template)
       Define 'keyword' as a new syntax-rules macro with one clause."
      (syntax-case x ()
        ((_ name (. pattern) template)
         (syntax (define-syntax name
                  (syntax-rules ()
                    ((_ . pattern) template))))))))

  (defalias defsx-rule defrule)

  (define-syntax when
    (syntax-rules ()
      ((_ pred b1 ...)
       (if pred (begin b1 ...)))))

  (define-syntax unless
    (syntax-rules ()
      ((_ pred b1 ...)
       (if (not pred) (begin b1 ...)))))

  (re-export
   ;; syntax define-syntax syntax-rules syntax-case
   sc-macro define-syntax define-syntax-public
   eval-when fluid-let-syntax
   identifier-syntax let-syntax
   letrec-syntax syntax syntax-case syntax-rules
   with-syntax
   include

   sc-expand sc-expand3 install-global-transformer
   syntax-dispatch syntax-error bound-identifier=?
   datum->syntax-object free-identifier=?
   generate-temporaries identifier? syntax-object->datum
   void syncase

   )

  (export sx defsx sxrule sxcase
          defrule defsx-rule def-syntax-alias
          syntax->datum datum->syntax
          define-syntax-rule
          when unless)
  ))

;;               ;; update syntax-rules to accept documentation
;;               ;; (define-syntax syntax-rules
;;               ;;   (lambda (x)
;;               ;;     (syntax-case x ()
;;               ;;       ((_ (k ...) ((keyword . pattern) template) ...)
;;               ;;        (syntax (lambda (x)
;;               ;;                  (syntax-case x (k ...)
;;               ;;                    ((dummy . pattern) (syntax template))
;;               ;;                    ...))))
;;               ;;      ((_ (k ...) docstring ((keyword . pattern) template) ...)
;;               ;;       (string? (syntax->datum (syntax docstring)))
;;               ;;       (syntax (lambda (x)
;;               ;;                 docstring
;;               ;;                 (syntax-case x (k ...)
;;               ;;                   ((dummy . pattern) (syntax template))
;;               ;;                   ...)))))))
