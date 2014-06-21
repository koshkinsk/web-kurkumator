#lang racket
; version 0.11

(require web-server/servlet
         web-server/servlet-env)

(require "kurkumator.rkt")
 
(define (my-app req)
  (let* ([bs (request-bindings req)]
         [kinput (if (exists-binding? 'ktext bs)
                   (extract-binding/single 'ktext bs)
                   #f)]
         [ktext (if (eq? kinput #f) ""
                    (kokoify-text kinput))]
         )
    (response/xexpr
     `(html (head (title "2017 kurkumator web-eblo"))
            (body (p (font ([size "4"]) "Веб-ебло куркуматора."))
                  (p (b ,ktext))
                  (form ([action "/kurkumator.rkt"])
                        (p "Копируй сюда свою пасту: ")
                        (TEXTAREA ([rows "4"] [cols "50"] [name "ktext"]) )
                        (p (input ([type "submit"] [value "Mamka ipal)))"])))
                        (p (font ([size "1"]) "Версия 0.11пук"))))))))
 
(serve/servlet my-app
               #:port 35007
               #:servlet-path "/kurkumator.rkt"
               #:log-file "kurkumator.log")

