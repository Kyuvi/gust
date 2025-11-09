;; Copyright © 2025 Kyuvi.
;; This file is published under version 3 of the GNU General Public license.
;;
;; Gust procedures and aliases that work on simple objects

(define-module (gust obj)
  #:use-module ((gust core) #:select (def)))

        ;;;; paedicates ;;;;

(def (false? obj)
  "(false? obj)
   Test if 'obj' is false."
   (eq? #f obj))

(def (true? obj)
  "(true? obj)
   Test if 'obj' is true."
  (eq? #t obj))

(def (void? obj)
  "(void? obj)
   Test if 'obj' is an #<unspecified> object."
  (cond-expand (guile-2 (unspecified? obj))
               (guile
                (let ((un (if #f #f)))
                  (if (eq? obj un) #t #f)))))

        ;;;; conversions ;;;;

(def (num obj)
  "(num obj)
   Corerce 'obj' into a number if possible, otherwise signals an error."
  (cond ((number? obj) obj)
        ((char? obj) (char->integer obj))
        ((string? obj) (string->number obj))
        (else (error "num: Incompatable object," obj))))

(def (str . args)
  "(str #:rest args)
   Concatenate all arguments, which should be printable objects, into a string.
   '#f' prints as 'false' and '#t' prints as 'true'.
    keywords print in the scheme format, '#:keyword' regardless of read-options"
  (with-output-to-string
    (lambda () (for-each (lambda (item) (display (cond ((true? item) "true")
                                                       ((false? item) "false")
                                                       (else item))))
                         args))))

(def (sym obj)
  "(sym obj)
   Coerces 'obj' into a symbol if possible, otherwise signals an error."
  (cond ((symbol? obj) obj)
        ((keyword? obj) (keyword->symbol obj))
        ((string? obj) (string->symbol obj))
        ((char? obj) (string->symbol (string obj)))
        (else (error "sym: Incompatable object:" obj))))

(def (keyword obj)
  "(keyword obj)
   Coerces 'obj' into a keyword if possible, otherwise signals an error."
  (cond ((keyword? obj) obj)
        ((symbol? obj) (symbol->keyword obj))
        ((or (string? obj) (char? obj))
         (symbol->keyword (sym obj)))
        ((number? obj)
         (symbol->keyword (sym (number->string obj))))
        (else (error "keyword: Incompatable object," obj))))

(def (name obj)
  "(name obj)
   Returns the name string of a string, symbol or keyword."
  (cond ((string? obj) obj)
        ((symbol? obj) (symbol->string obj))
        ((keyword? obj) (symbol->string (keyword->symbol obj)))
        (else (error "name: Incompatable object," obj))))
