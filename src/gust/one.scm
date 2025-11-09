;; Copyright © 2025 Kyuvi.
;; This file is published under version 3 of the GNU General Public license.
;;
;; Gust procedures and aliases found in guile-2+ but not found in guile-1 or
;; any modules, that are useful and can easily be implemented in guile-1

(define-module (gust wan))

(cond-expand
 (guile-2
  (display "\nNOTE: All procedures in (gust wan) are available from guile-2.\n"))

 (guile

  (define-public (identity x) x)

  (define-public (compose proc . rest)
    "Compose PROC with the procedures in REST, such that the last one in
REST is applied first and PROC last, and return the resulting procedure.
The given procedures must have compatible arity."
    (if (null? rest)
        proc
        (let ((g (apply compose rest)))
          (lambda args
            (call-with-values (lambda () (apply g args)) proc)))))

  (define-public (negate proc)
    "Return a procedure with the same arity as PROC that returns the `not'
of PROC's result."
    (lambda args
      (not (apply proc args))))

  (define-public (const value)
    "Return a procedure that accepts any number of arguments and returns
VALUE."
    (lambda _
      value))

  (define-public (and=> value procedure)
    "When VALUE is #f, return #f.  Otherwise, return (PROC VALUE)."
    (and value (procedure value)))

  (define call/cc call-with-current-continuation)

  ))
