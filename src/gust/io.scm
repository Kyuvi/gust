;; Copyright © 2025 Kyuvi.
;; This file is published under version 3 of the GNU General Public license.
;;
;; Gust procedures and aliases for input and output

(define-module (gust io)
  #:use-module ((gust core) #:select (def)))

(def nln newline)

(def (wr . args)
  "(wr #:rest args)
   Write all arguements to current output port, in a machine readable form."
  (if (pair? args)
      (let ((rest-args (cdr args)))
        (write (car args))
        (if (pair? rest-args)
            (begin (display #\space)
                   (apply wr rest-args))))))

(def (pr . args)
  "(pr #:rest args)
   Display all arguements on current output port formatted to be human readable."
  (if (pair? args)
      (let ((rest-args (cdr args)))
        (display (car args))
        (if (pair? rest-args)
            (begin (display #\space)
                   (apply pr rest-args))))))

(def (wrn . args)
  "(wrn #:rest args)
   Same as wr followed by newline."
  (apply wr args)
  (newline))

(def (prn . args)
  "(prn #:rest args)
   Same as pr followed by newline."
  (apply pr args)
  (newline))

(def (fmt fmt-str . args)
  "(fmt fmt-str #:rest args)
   Return 'fmt-str' with escapes replaced by the remaining arguments.
   See format for escape codes."
  (apply format #f fmt-str args))

(def (prfm fmt-str . args)
  "(prfm fmt-str #:rest args)
   Print 'fmt-str' to current-output-port with escapes replaced by arguments.
   See format for escape codes."
  (apply format #t fmt-str args))

(def printf prfm)

