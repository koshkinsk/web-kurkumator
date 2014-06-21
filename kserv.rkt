#lang racket
; version 0.14

(require web-server/servlet
         web-server/servlet-env)

(require "kurkumator.rkt")

(define (emit-unfucked-text text)
  (let ([spl (string-split text "\n")])
    (append '(p)
            (for/fold ([rl null]) ([line spl])
              (append rl (list line '(br)))))))
 
(define (my-app req)
  (let* ([bs (request-bindings req)]
         [kinput (if (exists-binding? 'ktext bs)
                   (extract-binding/single 'ktext bs)
                   "")]
         [kmd (if (exists-binding? 'kmd bs)
                  (string->number (extract-binding/single 'kmd bs))
                   3)]
         [ktext (if (eq? kmd #f)
                    (begin
                      (set! kmd 3)
                      (format "Error: Ты, это давай КОКОКО отсюда с такими числами: ~a"  (extract-binding/single 'kmd bs)))
                    (if (eq? kinput #f) ""
                        (kokoify-text kinput kmd)))])
    (response/xexpr
     `(html (head (title "KaS (Kurkuma-as-service) (2017)"))
            (body (p (font ([size "4"]) "Веб-ебло куркуматора // Kurkuma As Service"))
                  (b ,(emit-unfucked-text ktext))
                  (form ([action "/kurkumator.rkt"])
                        (p "Копируй сюда свою пасту: ")
                        (TEXTAREA ([rows "4"] [cols "50"] [name "ktext"]) ,kinput )
                        (p "Zashquar magntitude: " (input ([type "number"] [min "1"] [max "20"] [name "kmd"] [value ,(number->string kmd)])))
                        (p (input ([type "submit"] [value "Mamka ipal)))"])))
                        (p (font ([size "1"]) "Версия 0.14пук"))))))))
 
(serve/servlet my-app
               #:port 35007
               #:listen-ip #f
               #:servlet-path "/kurkumator.rkt"
               #:log-file "kurkumator.log"
               #:command-line? #t)

