#lang racket

(define (list-index lst e)
  (define (looping lst e i)
    (cond ((equal? (first lst) e) i)
          (else (looping (rest lst) e (+ i 1)))
          )
    )
  (looping lst e 0)
  )

(define words_de (file->list "German-Words.txt")) 
(define words_en (file->list "English-Words.txt"))
(define nouns_de (file->list "GermanNouns.txt"))
(define nouns_en (file->list "EnglishNouns.txt"))
(define verbs_de (file->list "Wortstämme-DE.txt"))
(define verbs_en (file->list "VerbsEnglish.txt"))

(define verbs_de_irr (file->list "TestIrrDe.txt"))
(define verbs_en_irr (file->list "TestIrrEng.txt"))

(define pronouns_de '("ich" "du" "er" "sie" "es" "wir" "ihr" "sie"))
(define pronouns_en '("I" "you" "he" "she" "it" "we" "you" "they"))
(define articles '("der" "die" "das"))

(define vowels '("a" "e" "i" "o" "u"))

(display "input / Eingabe: ")
(define input (read-line))
(define input-list '())
(define output null)
(define output-list '())
(define lang null)
(define article null)

(define pronoun '())


; achte auf Groß- und Kleinschreibung!
;(set! input (symbol->string input))
;(set! input (string-downcase input))
;(displayln input)
;(set! input (string->symbol input))

(define (get-lang lst)
  (define score 0)
  
  (define (langhelp lst)
    (cond [(not (empty? lst))
           (cond [(or (member (string->symbol (first lst)) words_de) (member (string->symbol (first lst)) nouns_de) (member (string->symbol (first lst)) verbs_de) (member (first lst) pronouns_de))
                  (set! score (+ score 1))
                  ]
                 
                 [(or (member (string->symbol (first lst)) words_en) (member (string->symbol (first lst)) nouns_en) (member (string->symbol (first lst)) verbs_en) (member (first lst) pronouns_en))
                  (set! score (- score 1))
                  ])
           
           (langhelp (rest lst))
           ])
    
    (cond [(positive? score)
           (set! lang "german")]
          [(negative? score)
           (set! lang "english")]
          [else (set! lang "undefined")]
          )
    )
  (langhelp lst)
  )


(define (clean-up-out str)
  (cond ((member #\_ (string->list str))
         (string-set! str (list-index (string->list str) #\_) #\ ) ;in_the_end --> in the_end --> in the end
         (clean-up-out output)
         ))
  (set! output  str)
  )

(define (get-article index)
  (cond [(< index 260)
         "der"]
        [(and (> index 259) (< index 519))
         "die"]
        [(> index 518)
         "das"]
        )
  )

(define (get-pronoun input)
  (cond [(member input pronouns_de) (set! output "pr=noun")
                                    (set! pronoun (list-ref pronouns_en (list-index pronouns_de input)))]
        [(member input pronouns_en) (set! output "pr=noun")
                                    (set! pronoun (list-ref pronouns_de (list-index pronouns_en input)))]
        )
  )


(define (get-verb input)

  (cond [(equal? lang "english")
         (cond [(member (string->symbol input) verbs_en)
                (set! output (symbol->string(list-ref verbs_de (list-index verbs_en (string->symbol input)))))
                (cond [(equal? pronoun "ich") (set! output (string-append output "e"))]
                      [(equal? pronoun "du") (set! output (string-append output "st"))]
                      [(equal? pronoun "er") (set! output (string-append output "t"))]
                      [(equal? pronoun "sie") (set! output (string-append output "t"))]
                      [(equal? pronoun "es") (set! output (string-append output "t"))]
                      [(equal? pronoun "wir") (set! output (string-append output "en"))]
                      [(equal? pronoun "ihr") (set! output (string-append output "t"))]
                      [(equal? pronoun "sie") (set! output (string-append output "en"))])
                ])
         ])

  
  (cond [(equal? pronoun "I")
         (cond [(member (string->symbol (substring input 0 (- (string-length input) 1))) verbs_de)
                (set! output (list-ref verbs_en (list-index verbs_de (string->symbol (substring input 0 (- (string-length input) 1))))))
                ])
         ])

  
  (cond [(equal? pronoun "you")
         (cond [(member (string->symbol (substring input 0 (- (string-length input) 1))) verbs_de)
                (set! output (list-ref verbs_en (list-index verbs_de (string->symbol (substring input 0 (- (string-length input) 1))))))
                ])
         (cond [(member (string->symbol (substring input 0 (- (string-length input) 2))) verbs_de)
                (set! output (list-ref verbs_en (list-index verbs_de (string->symbol (substring input 0 (- (string-length input) 2))))))
                ])
         ])
  (cond [(equal? pronoun "she")
         (cond [(member (string->symbol (substring input 0 (- (string-length input) 1))) verbs_de)
                (set! output (symbol->string (list-ref verbs_en (list-index verbs_de (string->symbol (substring input 0 (- (string-length input) 1)))))))
                (cond [(member (substring output (- (string-length output) 1)) vowels)
                       (set! output (string-append output "es"))]
                      [else (set! output (string-append output "s"))]
                      )
                ]
               [(member (string->symbol (substring input 0 (- (string-length input) 2))) verbs_de)
                (set! output (list-ref verbs_en (list-index verbs_de (string->symbol (substring input 0 (- (string-length input) 2))))))
                (set! pronoun "they")]
               )
         ])
  (cond [(or (equal? pronoun "he") (equal? pronoun "it"))
         (cond [(member (string->symbol (substring input 0 (- (string-length input) 1))) verbs_de)
                (set! output (symbol->string (list-ref verbs_en (list-index verbs_de (string->symbol (substring input 0 (- (string-length input) 1)))))))
                (cond [(member (substring output (- (string-length output) 1)) vowels)
                       (set! output (string-append output "es"))]
                      [else (set! output (string-append output "s"))]
                      )
                ])
         
         ])
  )

(define (get-irr-verb input)
  (cond [(equal? lang "english")
         (cond [(member input verbs_en_irr)
                (cond [(equal? pronoun "ich") (set! output (list-ref verbs_de_irr (list-index verbs_en_irr input)))]
                      [(equal? pronoun "du") (set! output (list-ref verbs_de_irr (+ (list-index verbs_en_irr input) 1 )))]
                      [(or (equal? pronoun "er") (equal? pronoun "sie") (equal? pronoun "es")) (set! output (list-ref verbs_de_irr (list-index verbs_en_irr input)))]
                      [(equal? pronoun "wir") (set! output (list-ref verbs_de_irr (+ (list-index verbs_en_irr input) 3 )))]
                      [(equal? pronoun "ihr") (set! output (list-ref verbs_de_irr (+ (list-index verbs_en_irr input) 4 )))]
                      [(equal? pronoun "sie") (set! output (list-ref verbs_de_irr (+ (list-index verbs_en_irr input) 5 )))])
                ])])

  (cond [(equal? lang "german")
        (cond [(member input verbs_de_irr)
               (cond [(equal? pronoun "I") (set! output (list-ref verbs_en_irr (list-index verbs_de_irr input)))]
                     [(equal? pronoun "you") (set! output (list-ref verbs_en_irr (+ (list-index verbs_de_irr input) 1 )))]
                     [(or (equal? pronoun "he") (equal? pronoun "she") (equal? pronoun "it")) (set! output (list-ref verbs_en_irr (+ (list-index verbs_de_irr input) 2 )))]
                     [(equal? pronoun "we") (set! output (list-ref verbs_en_irr (+ (list-index verbs_de_irr input) 3 )))]
                     [(equal? pronoun "you") (set! output (list-ref verbs_en_irr (+ (list-index verbs_de_irr input) 4 )))]
                     [(equal? pronoun "they") (set! output (list-ref verbs_en_irr (+ (list-index verbs_de_irr input) 5 )))])
               ])
        ]
  )
  )

(define (translate-help-de input)
  (cond ((member input words_de)
         (set! output (symbol->string (list-ref words_en (list-index words_de input))))
         (clean-up-out output)
         ))
  
  (cond [(member input nouns_de)   
         (set! output (symbol->string (list-ref nouns_en (list-index nouns_de input))))
         (clean-up-out output)
         ])
  )

(define (translate input counter)
  (cond [(eq? lang "german")
         (cond [(equal? (symbol->string input) (last input-list))
                (translate-help-de input)]
               [else (cond [(member (symbol->string input) articles)
                            (cond [(not (member (string->symbol (list-ref input-list (+ counter 1))) nouns_de))
                                   (translate-help-de input)
                                   ]
                                  [else (set! output "the")])
                            ]
                           [else (translate-help-de input)])
                     ])
         ])
  
  (cond [(eq? lang "english")
         (cond ((member input words_en)
                (set! output (symbol->string (list-ref words_de (list-index words_en input))))
                (clean-up-out output)
                ))

         (cond ((member input verbs_en_irr)
                (get-irr-verb input)
                ))
         
         (cond [(member input nouns_en)
                (set! article (get-article (list-index nouns_en input)))
                (cond [(not (empty? output-list))
                       (cond [(equal? (first output-list) "dem") (set! output-list (remove "dem" output-list))])
                       ])
                (set! output-list (cons article output-list))
                
                (set! output (symbol->string (list-ref nouns_de (list-index nouns_en input))))
                ])
         ])
  
  (get-pronoun (symbol->string input))
  (get-verb (symbol->string input))
  
  (set! output-list (cons output output-list))
  
  (set! output null)
  )


(define (loop input-list counter)
  (cond ((not (empty? input-list))
         (translate (string->symbol (first input-list)) counter)
         (loop (rest input-list) (+ counter 1))
         )
        )
  )


;(define (verb-get gender person numerus)
  
  

(define (print-lst lst)
  (cond ((> (length lst) 0)
         (cond [(equal? (first lst) "pr=noun") (display pronoun)]
               [else (display (first lst))])
         (display " ")
         (print-lst (rest lst))
         ))
  )

(set! input-list (string-split input)) 

(get-lang input-list)
(cond [(equal? lang "german") (display "Sprache: ")]
      [else (display "language: ")])
(displayln lang)

(loop input-list 0)
(set! output-list (reverse output-list))

(cond [(equal? lang "german") (display "Übersetzung: ")]
      [else (display "translation: ")])
(print-lst output-list)

;verben en->de (goes)
;Verben überarbeiten, dass Person u. Geschlecht von überall erkannt werden können
;Verben unregelmäßig
;Nomen Einzahl/Mehrzahl
;Kommentare

#|
wort_de -> Artikel = die -> in Liste der Nomen in Einzahl? -> nein -> Liste mit Wörtern in Mehrzahl
                                                           -> ja -> weiblich (außer ausnahmen, index muss abgeglichen werden ob m/w/n passt)
|#

;Alles nicht übersetzbare als Namen speichern + Feedback (tut mir leid, ich kan dieses Wort nicht übersetzen, du Arsch)
;Detect Pronoun fix