;; Copyright © 2025 Kyuvi.
;; This file is published under version 3 of the GNU General Public license.
;;
;; Gust procdures and aliases for interacting with commands on the
;; operating systems

(define-module (gust os cmd)
  ;; #:use-module (gust gust)
  #:use-module ((gust core) #:select (def))
  #:use-module ((gust io) #:select (pr fmt))
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen))


(def (sh-cmd-str cmd)
  "(sh-cmd-str cmd)
   Get the output of 'cmd' as a single string."
  (let* ((port (open-input-pipe cmd))
         (out-str (read-string port)))
    (close-pipe port)
    out-str))

(def (sh-cmd-list cmd)
  "(sh-cmd-list cmd)
   Get the output of 'cmd' as a list of strings of each line."
  (string-split (sh-cmd-str cmd) #\newline))

(def (sh-cmd-out-err-str cmd)
  "(sh-cmd-out-err-str cmd)
   Returns a pair of the result string and error string  of 'cmd'."
  (let* ((err-cons (pipe))
         (port (with-error-to-port (cdr err-cons)
                 (lambda () (open-input-pipe cmd))))
         (_ (setvbuf (car err-cons) 'block
             (* 1024 1024 16)))
         (result (read-string port)))
    (close-port (cdr err-cons))
    ;; (values
    (cons
     result
     (read-string (car err-cons)))))

(def (yes-or-no? . args)
  "(yes-or-no? #:optional fmt-str #:rest args])
   Return #t or #f (or repeats) depending on input.
   fmt-str uses 'fomrat' escape code to print args."
  (let ((out-str (if (pair? args)
                     (apply fmt
                            (string-append (car args) " (y/yes or n/no) ")
                            (cdr args))
                     #f)))
    (when out-str (pr out-str))
    (let y-n-loop ()
      (case (string->symbol (string-trim-both (read-line)))
        ((y yes YES Yes) #t)
        ((n no NO No) #f)
        (else (pr (if out-str out-str "Please answer y/yes or n/no: "))
              (y-n-loop))))))

(def y-or-n? yes-or-no?)
