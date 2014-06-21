#lang racket

(require web-server/servlet
         web-server/servlet-env)

(require "kurkumator.rkt")
 
(define (my-app req)

  (define rtxt #f)

  (let* ([bs (request-bindings req)]
         [kinput (if (exists-binding? 'ktext bs)
                   (extract-binding/single 'ktext bs)
                   #f)]
         [ktext (if (eq? kinput #f) ""
                    (kokoify-text kinput))]
         )
    (response/xexpr
     `(html (head (title "Modern kurkumator web 3.0 app"))
            (body (p "See kurkumator in action")
                  (p ,ktext)
                  (form ([action "/kurkumator.rkt"])
                        "Enter a number: "
                        (input ([type "text"] [name "ktext"] ))
                        (input ([type "submit"]))))))))
 
(serve/servlet my-app
               #:port 1337
               #:servlet-path "/kurkumator.rkt"
               #:log-file "kurkumator.log"
               )

