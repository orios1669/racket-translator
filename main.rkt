#lang racket

;writing function to get the index of element 'e' in list 'lst'
(define (list-index lst e)
  (define (looping lst e i)
    (cond ((equal? (first lst) e) i)
          (else (looping (rest lst) e (+ i 1)))
          )
    )
  (looping lst e 0)
  )

;loading all the word lists
(define words_de (file->list "German-Words.txt"))
(define words_en (file->list "English-Words.txt"))
(define nouns_de (file->list "GermanNouns.txt"))
(define nouns_en (file->list "EnglishNouns.txt"))
(define verbs_de (file->list "Wortstämme-DE.txt"))
(define verbs_en (file->list "VerbsEnglish.txt"))

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
(define pronoun null)

(define gender null)
(define person null)
(define numerus null)

(define end 0)

;get language of input
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

;remove underscores from output
(define (clean-up-out str)
  (cond ((member #\_ (string->list str))
         (string-set! str (list-index (string->list str) #\_) #\ ) ;in_the_end --> in the_end --> in the end
         (clean-up-out output)
         ))
  (set! output  str)
  )

;get correct article from noun
(define (get-article index ending)
  (cond [(not (equal? 0 ending))
         (cond [(equal? (get-article index 0) "die") (set! gender "female")]
               [(equal? (get-article index 0) "der") (set! gender "male")]
               [(equal? (get-article index 0) "das") (set! gender "neutral")]
               )
         (set! numerus "plural")
         (set! person 3)
         "die"]
        [else 
         (cond [(< index 260)
                (set! gender "male")
                (set! numerus "singular")
                (set! person 3)
                "der"]
               [(and (> index 259) (< index 519))
                (set! gender "female")
                (set! numerus "singular")
                (set! person 3)
                "die"]
               [(> index 518)
                (set! gender "neutral")
                (set! numerus "singular")
                (set! person 3)
                "das"]
               )
         ])
  )


(define (get-pronoun input counter)
  (cond [(member input pronouns_de)
         (set! output "pr=noun")
         (set! pronoun (list-ref pronouns_en (list-index pronouns_de input)))
         
         (cond [(equal? input "ich")
                (set! person 1)
                (set! numerus "singular")]
               [(equal? input "du")
                (set! person 2)
                (set! numerus "singular")]
               [(equal? input "er")
                (set! person 3)
                (set! numerus "singular")
                (set! gender "male")]
               [(equal? input "sie")
                (cond [(member (string->symbol (substring (list-ref input-list (+ counter 1)) 0 (- (string-length (list-ref input-list (+ counter 1))) 1))) verbs_de)
                       (set! person 3)
                       (set! numerus "singular")
                       (set! gender "female")
                       ]
                      [(member (string->symbol (substring (list-ref input-list (+ counter 1)) 0 (- (string-length (list-ref input-list (+ counter 1))) 2))) verbs_de)
                       (set! pronoun "they")
                       (set! numerus "plural")
                       (set! person 3)
                       ]
                      )
                ]
               [(equal? input "es")
                (set! person 3)
                (set! numerus "singular")
                (set! gender "neutral")]
               [(equal? input "wir")
                (set! person 1)
                (set! numerus "plural")]
               [(equal? input "ihr")
                (set! person 2)
                (set! numerus "plural")]
               )
         
         ]
        [(member input pronouns_en) 
         (cond [(equal? input "I")
                (set! person 1)
                (set! numerus "singular")]
               [(equal? input "you")
                (set! person 2)
                (set! numerus "singular")]
               [(equal? input "he")
                (set! person 3)
                (set! numerus "singular")
                (set! gender "male")]
               [(equal? input "she")
                (set! person 3)
                (set! numerus "singular")
                (set! gender "female")]
               [(equal? input "it")
                (set! person 3)
                (set! numerus "singular")
                (set! gender "neutral")]
               [(equal? input "we")
                (set! person 1)
                (set! numerus "plural")]
               [(equal? input "you")
                (set! person 2)
                (set! numerus "plural")]
               [(equal? input "they")
                (set! person 3)
                (set! numerus "plural")])
         
         (set! output "pr=noun")
         (set! pronoun (list-ref pronouns_de (list-index pronouns_en input)))]
        )
  )

(define (get-verb input)
  (cond [(equal? lang "english")
         (cond [(member (string->symbol input) verbs_en) (set! end 0)]
               [(member (string->symbol (substring input 0 (- (string-length input) 1))) verbs_en) (set! end 1)]
               [(member (string->symbol (substring input 0 (- (string-length input) 2))) verbs_en) (set! end 2)]
               )
         (cond [(member (string->symbol (substring input 0 (- (string-length input) end))) verbs_en)
                (set! output (symbol->string (list-ref verbs_de (list-index verbs_en (string->symbol (substring input 0 (- (string-length input) end)))))))
                (case numerus 
                  [("singular")
                   (case person
                     [(1) (set! output (string-append output "e"))]
                     [(2) (set! output (string-append output "st"))]
                     [(3) (set! output (string-append output "t"))]
                     )
                   ]
                  [("plural")
                   (case person
                     [(1) (set! output (string-append output "en"))]
                     [(2) (set! output (string-append output "t"))]
                     [(3) (set! output (string-append output "en"))]
                     )
                   ]
                  )
                ])
         ])
  
  (cond [(equal? lang "german")
         (case numerus 
           [("singular") 
            (case person 
              [(1) (cond [(member (string->symbol (substring input 0 (- (string-length input) 1))) verbs_de)
                          (set! output (list-ref verbs_en (list-index verbs_de (string->symbol (substring input 0 (- (string-length input) 1))))))
                          ])
                   ]
              [(2) (cond [(member (string->symbol (substring input 0 (- (string-length input) 2))) verbs_de)
                          (set! output (list-ref verbs_en (list-index verbs_de (string->symbol (substring input 0 (- (string-length input) 2))))))
                          ])
                   ]
              [(3) (cond [(member (string->symbol (substring input 0 (- (string-length input) 1))) verbs_de)
                          (set! output (symbol->string (list-ref verbs_en (list-index verbs_de (string->symbol (substring input 0 (- (string-length input) 1)))))))
                          (cond [(member (substring output (- (string-length output) 1)) vowels)
                                 (set! output (string-append output "es"))]
                                [else (set! output (string-append output "s"))]
                                )
                          ])
                   ]
              )
            ]
           [("plural")
            (case person
              [(1) (cond [(member (string->symbol (substring input 0 (- (string-length input) 2))) verbs_de)
                          (set! output (list-ref verbs_en (list-index verbs_de (string->symbol (substring input 0 (- (string-length input) 2))))))
                          ])
                   ]
              [(2) (cond [(member (string->symbol (substring input 0 (- (string-length input) 1))) verbs_de)
                          (set! output (list-ref verbs_en (list-index verbs_de (string->symbol (substring input 0 (- (string-length input) 1))))))
                          ])
                   ]
              [(3) (cond [(member (string->symbol (substring input 0 (- (string-length input) 2))) verbs_de)
                          (set! output (list-ref verbs_en (list-index verbs_de (string->symbol (substring input 0 (- (string-length input) 2))))))
                          ])
                   ]
              )
            ]
           )
         ])
  )

(define (translate-help-de input)
  (cond ((member input words_de)
         (set! output (symbol->string (list-ref words_en (list-index words_de input))))
         (clean-up-out output)
         ))
  
  
  (cond [(member (string->symbol (substring (symbol->string input) 0 (- (string-length (symbol->string input)) end))) nouns_de)
         (set! input (string->symbol (substring (symbol->string input) 0 (- (string-length (symbol->string input)) end))))
         (set! output (symbol->string (list-ref nouns_en (list-index nouns_de input))))
         (clean-up-out output)
         
         (cond [(equal? numerus "plural") (set! output (string-append output "s"))])
         ])
  )

(define (check-article-help ending counter)
  (displayln (list-ref input-list (+ counter 1)))
  (cond [(member (string->symbol (substring (list-ref input-list (+ counter 1)) 0 (string-length (list-ref input-list (+ counter 1))))) nouns_de)
         (cond [(and (> (list-index nouns_de (string->symbol (list-ref input-list (+ counter 1))))  259) (< (list-index nouns_de (string->symbol (list-ref input-list (+ counter 1))))  519))
                (set! gender "female")
                (set! numerus "singular")
                (set! person 3)
                ]
               [else (set! numerus "plural")
                     (set! person 3)])
         ])
  
  (cond [(not (equal? ending 0))
         (cond [(member (string->symbol (substring (list-ref input-list (+ counter 1)) 0 (- (string-length (list-ref input-list (+ counter 1))) ending))) nouns_de)
                (set! numerus "plural")
                (set! person 3)
                ])
         ])
  (set! end ending)
  )

(define (check-article input counter)
  (cond [(equal? input (or "der" "das"))
         (set! person 3)
         (set! numerus "singular")
         (cond [(equal? input "der") (set! gender "male")]
               [else (set! gender "neutral")])
         ]
        [(equal? input "die")
         (cond [(member (string->symbol (list-ref input-list (+ counter 1))) nouns_de) (check-article-help 0 counter)]
               [(member (string->symbol (substring (list-ref input-list (+ counter 1)) 0 (- (string-length (list-ref input-list (+ counter 1))) 1))) nouns_de) (check-article-help 1 counter)]
               [(member (string->symbol (substring (list-ref input-list (+ counter 1)) 0 (- (string-length (list-ref input-list (+ counter 1))) 2))) nouns_de) (check-article-help 2 counter)])
         ])
  )

(define (translate input counter)
  (cond [(eq? lang "german")
         (cond [(equal? (symbol->string input) (last input-list))
                (translate-help-de input)]
               [else (cond [(member (symbol->string input) articles)
                            (cond [(not (or (member (string->symbol (list-ref input-list (+ counter 1))) nouns_de) (member (string->symbol (substring (list-ref input-list (+ counter 1)) 0 (- (string-length (list-ref input-list (+ counter 1))) 1))) nouns_de)  (member (string->symbol (substring (list-ref input-list (+ counter 1)) 0 (- (string-length (list-ref input-list (+ counter 1))) 2))) nouns_de)))
                                   (translate-help-de input)
                                   ]
                                  [else (set! output "the")
                                        (check-article (symbol->string input) counter)]
                                  )
                            ]
                           [else (translate-help-de input)])
                     ])
         ])
  
  (cond [(eq? lang "english")
         (cond ((member input words_en)
                (set! output (symbol->string (list-ref words_de (list-index words_en input))))
                (clean-up-out output)
                ))
         
         (cond [(member input nouns_en) (set! end 0)]
               [(member (string->symbol (substring (symbol->string input) 0 (- (string-length (symbol->string input)) 1))) nouns_en) (set! end 1)]
               [(member (string->symbol (substring (symbol->string input) 0 (- (string-length (symbol->string input)) 2))) nouns_en) (set! end 2)])
         (cond [(member (string->symbol (substring (symbol->string input) 0 (- (string-length (symbol->string input)) end))) nouns_en)
                (set! article (get-article (list-index nouns_en (string->symbol (substring (symbol->string input) 0 (- (string-length (symbol->string input)) end)))) end))
                (cond [(not (empty? output-list))
                       (cond [(equal? (first output-list) "dem") (set! output-list (remove "dem" output-list))])
                       ])
                
                (set! output-list (cons article output-list))
                
                (cond [(equal? numerus "singular")
                       (set! output (symbol->string (list-ref nouns_de (list-index nouns_en (string->symbol (substring (symbol->string input) 0 (- (string-length (symbol->string input)) end)))))))
                       ]
                      [(equal? numerus "plural")
                       (cond [(or (equal? gender "male") (equal? gender "neutral"))
                              (set! output (string-append (symbol->string (list-ref nouns_de (list-index nouns_en (string->symbol (substring (symbol->string input) 0 (- (string-length (symbol->string input)) end)))))) "e"))
                              ]
                             [(equal? gender "female")
                              (set! output (symbol->string (list-ref nouns_de (list-index nouns_en (string->symbol (substring (symbol->string input) 0 (- (string-length (symbol->string input)) end)))))))
                              (cond [(member (substring output (- (string-length output) 1)) vowels)
                                     (set! output (string-append (symbol->string (list-ref nouns_de (list-index nouns_en (string->symbol (substring (symbol->string input) 0 (- (string-length (symbol->string input)) end)))))) "n"))
                                     ]
                                    [else
                                     (set! output (string-append (symbol->string (list-ref nouns_de (list-index nouns_en (string->symbol (substring (symbol->string input) 0 (- (string-length (symbol->string input)) end)))))) "en"))
                                     ]
                                    )
                              ])
                       ])
                ])
         ])
  
  (get-pronoun (symbol->string input) counter)
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

(newline)

(displayln gender)
(displayln numerus)
(displayln person)

;verben en->de (goes)  [LUIS]
;Verben überarbeiten, dass Person u. Geschlecht von überall erkannt werden können
;Verben unregelmäßig [LUIS]
;Kommentare