;; Copyright © 2025 Kyuvi.
;; This file is published under version 3 of the GNU General Public license.
;;
;; Gust procdures and aliases for interacting with the operating system.
;; These are also used by the gust gust module.

(define-module (gust os gos)
  #:use-module ((gust obj) #:select (str))
  #:use-module (gust core))

(cond-expand (guile-2

              (def os-sep file-name-separator-string)

              (def sys system*))

             (guile

              (def os-sep "/")

              (def (sys . args)
                   (error "sys: system* only available from guile-2+."))))

(defn sh (. args)
 "(sh #:rest args)
  Run 'args', after concatenating them into a string, using the 'system' function."
 (system (apply str args)))

(def pwd getcwd)

(def cd chdir)

(defn decode-time (#:optional (time (current-time)))
  "(decode-time #:optional time)
   Return a vector of the decoded (localtime) 'time'.
  'time' defaults to current-time."
  (localtime time))
