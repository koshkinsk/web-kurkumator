#lang racket

;; blind rewrite of authentic kurkumator
(provide kokoify-text html-unfuck)

;; from rosetta code 
(define (levenshtein a b)
  (define (ls0 a-index b-index)
    (cond [(or (= a-index -1) (= b-index -1)) (abs (- a-index b-index))]
          [else 
           (define a-char (string-ref a a-index))
           (define b-char (string-ref b b-index))
           (if (equal? a-char b-char)
               (ls (sub1 a-index) (sub1 b-index))
               (min (add1 (ls (sub1 a-index) b-index))
                    (add1 (ls a-index (sub1 b-index)))
                    (add1 (ls (sub1 a-index) (sub1 b-index)))))]))
  (define memo (make-hash))
  (define (ls a-i b-i)
    (hash-ref! memo (cons a-i b-i) (λ() (ls0 a-i b-i))))
  (ls (sub1 (string-length a)) (sub1 (string-length b))))
 
(define ko-list (list "азаза" "анархия" "анон" "анус" "аутизм" "аутист" "ебаный" "бан"  "батхерт" "биекция" "биткоин" "бнв" "бомбануло" "бомбит" "борщ" "бугурт" "будущее" "бухло" "быдло" "ватник" "ветеран" "вин" "внезапно" "гей" "гейос" "говно" "двач" "дедфуд" "десу" "диван" "для" "дрочить" "ебать"  "жопа" "жопу" "забери" "заблевал" "задрот" "запили" "затралел" "зафорсил" "зашквареный" "збс" "итт" "ипал" "кококо" "костыли" "комар" "крипто" "кукарек" "кун" "куркума" "лайк" "лалка" "ле" "лизнул" "линуск" "лисп" "личкрафт" "лойс" "лол" "лох" "лул" "лях" "маман" "мамка" "матан" "моар" "мюсли" "нано" "наркоман" "нассал" "нахуй" "нинужно" "ня" "няшмяш" "обосрал" "обосрался" "опущено" "отсос" "параша" "пердак" "пердолик" "петух" "петушок" "петушон" "пидон" "пидор" "пидорас" "пидораха" "пизда" "пиздолис" "писечка" "полизал" "полущ" "порст" "поссал" "потрачено" "потрачено" "ппц" "прост" "профит" "пруф" "прыщи" "пук" "пукан" "путин" "рабство" "разорвало" "рак" "рашка" "разъебу" "репост" "сасай" "сука" "свежо" "сиськи" "скатываешь" "скатывать" "скатываешь" "слил" "соси" "сосноль" "соснул" "спайс" "спали" "спалил" "сперма" "среньк" "сыч" "твою" "тебя" "трал" "транс" "трап" "тупой" "тред" "тян" "уау" "удобно" "уебать" "упоротый" "упороть" "успех" "фейл" "форс" "форсил" "форсить" "функциональщик" "хаскель" "хуйта" "хуле" "чат" "членодевка" "чухан" "шиндошс" "шкварить" "штоле"  "шлюха" "эпик"))

(define cons-string "бвгджзйклмнпрстфхцчшщ")
(define vowel-string "аеёиоуыэюя")

(define (remove-repeats s)
  (define-values (chars last)
    (for/fold ([chars null] [last #f])
      ([c (in-string s)] #:when (not (eqv? last c)))
      (values (cons c chars) c)))
  (list->string (reverse chars)))

(define (squeeze-word word)
  "Remove all vowels and squash repetetive consonants"
  (let* ([devow-rx (format "([~a])" vowel-string)]
         [squeeze-rx (format "([~a])\\1+" cons-string)]
         [devow (regexp-replace* devow-rx word "")])
    (remove-repeats devow)))

(define co-list (map squeeze-word ko-list))
(define co-ko-list (map cons co-list ko-list))

(define (find-nearest word [max-diff 10])
  (define res word)
  (define ldiff 1024)
  (for ([cw co-ko-list])
    (let ([cdiff (levenshtein (squeeze-word word) (car cw))])
      (when (and (> ldiff cdiff) (< cdiff max-diff))
        (set! res (cdr cw))
        (set! ldiff cdiff))))

  (when (char-upper-case? (car (string->list word)))
    (set! res (string-titlecase res)))
  
  res)

(define (kokoify-text text [max-diff 10])
  (define start 0)
  (define rstr "")
  (let loop ()
    (let ([rm (regexp-match-positions #rx"([а-яА-Я]+)" text start)])
      (when (not (eq? rm #f))
        (let* ([ns (car (car rm))]
               [ne (cdr (car rm))]
               [nw (find-nearest (substring text ns ne) max-diff)])
          (if (> (- ne ns) 3) ;; don't words shorter than 4 letters
              (set! rstr (string-append rstr (substring text start ns) nw))
              (set! rstr (string-append rstr (substring text start ns) (substring text ns ne)))
              )
          (set! start ne)
          (loop)))))
  (set! rstr (string-append rstr (substring text start)))

  rstr)

(define (html-unfuck text)
  (string-replace text "\n" "<br>"))
