#lang racket
(require 2htdp/batch-io)


#|Función resaltador  que recibe como entrada tres parámetros.
El primer parámetro es el nombre del archivo que contiene las expresiones regulares.
El segundo parámetro es el nombre del archivo que contiene el código fuente a resaltar.
El tercer parámetro es el nombre de los documentos de HTML+CSS en donde se resalta el léxico del archivo especificado en el segundo parámetro.|#


(define (resaltador expt_txt data_txt out_html)
  (begin


    ;<<<--Data de entrada-->>>>
    (define datat1 (read-lines data_txt))
    ;<<<<<----Captura las expreciones regulares linea por linea->>>>>>
    (define lslex (read-lines expt_txt))

    ;Filtra la data para eliminar datos basura
    (define (filtering data ) (cond
                                [(empty? data) '()]
                                [(not(string=? (car data) "" )) (cons(car data) (filtering (cdr data))) ]
                                [(string=?(car data) "" ) (filtering (cdr data))]
                                )
      )

    (define datat (filtering datat1))

    ;(displayln datat)
    ;(displayln lslexs)
    ;(displayln lslex2)

    ;funcion que genera ids , los cuales posteriormente seran clases de css
    (define id-raw (sequence->list (in-inclusive-range 1 (+ 1 (length lslex)))))

    ;funcion que formatea los id's
    (define (format-id ids)
      (cond
        [(empty? ids) '()]
        [else (cons(number->string (car ids)) (format-id (cdr ids)))]
        )
      )
    (define id (format-id id-raw))

    ;Funcion que trae el ultimo elemento de una lista
    (define (lastElem list) (car (reverse list)))

    #|Simple lexer es la base del proyecto de una manera dinamica en la identificacion de expreciones
regulares desde una lista
|#
    (define (simple-lexer data lex)  (if (not(string=? data ""))
                                         (cond
                                           [(regexp-match lex data)
                                            (begin
                                              (string-length(car (regexp-match lex data))))]
                                           [else #f]
                                           )
                                         ""
                                         )
      )

    #|La funcion finder ayuiada a determinar cual va a ser la data su id y cual sera el salto que se tiene que dar para recorre la string|#
    (define (finder lstlex id data )
      (cond
        [(equal? lstlex '())' #f]
        [(not(equal?(simple-lexer data (car lstlex))#f)) (begin
                                                           (string-append (car (regexp-match (car lstlex) data))"«"(car id)"«"(number->string(simple-lexer data (car lstlex))))
                                                           )]
        [else (finder (cdr lstlex) (cdr id) data)]
        )
      )

    #|La funcion Jump retorna el salto que se tiene que dar para iterar en la string|#
    (define (jump s)(string->number(lastElem(regexp-split #px"«" s ))))
    (define (conversor s)(regexp-split #px"«" s ))

    (define greeted null)

    (define (greet name)
      (set! greeted (cons name greeted))
      (string-append "Hello, " name))

    #|La funcion Texter recorre la data en conjunto de sus identificadores , y al final de cada salto de linea coloca
un salto de linea para html en una sola linea|#

    (define (texter lstlex id data )
      (cond
        [(string=? data "") (begin
                              (greet "<br/>")
                              (reverse greeted))]
        [(not(equal? (finder lstlex id data)#f)) (begin
                                                   (greet(first(conversor (finder lstlex id data))))
                                                   (greet(second(conversor (finder lstlex id data))))
                                                   (texter lstlex id (substring data (jump (finder lstlex id data))))
                                                   ;(displayln(jump (finder lstlex id data)))

                                                   )]
        [else (begin
                (greet "err")
                ;(displayln (string-append (first data) "-> Caracter no reconocido " ))
                (greet (lastElem id))
                (texter lstlex id (substring data 1))
                )
              ]
        )
      )

    (define (texter-multiple lstlex id data) (cond
                                               [(equal? data '()) '()]
                                               [(not (equal? data '())) (cons (texter lslex id (car data)) (texter-multiple lstlex id (cdr data)))]
                                               ))

    (define greeted2 null)
    (define (greet2 name)(set! greeted2 (cons name greeted2)))
    #|La funcion texter-multiple2 realliza un appendder de las funciones anteriores|#
    (define (texter-multiple2 lstlex id data) (cond
                                                [(equal? data '()) '()]
                                                [(not (equal? data '())) (begin
                                                                           (greet2 (texter lslex id (car data)) )
                                                                           (texter-multiple lstlex id (cdr data))
                                                                           )
                                                                         ]
                                                ))
    #|alf o all final es la lista final despues de todo el proceso de busqueda|#
    (define alf (lastElem (texter-multiple2 lslex id datat)))




    (define html null)
    (define (html-push name)(set! html (cons name html)))

    #|hml-generato genera codigo html con el resultado de alf|#
    (define (hml-generator data [iscoment #f])(cond
                                                [(equal? data '()) html]
                                                [(string=? (car data) "<br/>")
                                                 (begin
                                                   (html-push (car data))
                                                   (hml-generator (cdr data) #f)
                                                   )]
                                                [(string=? (car data) "")
                                                 (hml-generator (cdr data))
                                                 ]
                                                [(not(string=? (car data) "<br/>"))
                                                 (begin

                                                   (html-push (string-append "<h1 class='s"(second data) "'>" (car data) "</h1>"))
                                                   (hml-generator (cddr data) ))
                                                 ]
                                                ))
    ;raww html es el html pre procesado
    (define html-raw(reverse(hml-generator alf)))

    ;la funcion member identifica si es un valor ya existente para poder generar colores diferentes irrepetibles
    (define (member? item seq)
      (sequence-ormap (lambda (x)
                        (equal? item x))
                      seq))

    #|hml-generato genera codigo html con el resultado de alf|#
    (define (random-list-h acc range len)
      (cond [(zero? len) acc]
            [else
             (let ((num (random range)))
               (cond [(member? num acc)
                      (random-list-h acc range len)]
                     [else
                      (random-list-h (cons num acc) range (sub1 len))]))]))

    (define (random-list range list-length)
      (random-list-h '() range list-length))

    ;se generan colores RGB totalmente diferentes
    (define (r n)(random-list 200 n))
    (define (g n)(random-list 200 n))
    (define (b n)(random-list 200 n))

    (define colorize null)

    (define (setcolorize name)
      (set! colorize (cons name colorize))
      )

    ;generador de colores rgb totalmente diferentes en formato css
    (define (color-id r g b [n 1])(cond
                                    [(empty? r) colorize ]
                                    [(not (empty? r)) (begin
                                                        (setcolorize(string-append ".s"(number->string n)"{color:rgb("(number->string(car r))","(number->string(car g))","(number->string(car b))");display:inline;padding-right: 10px;}"))
                                                        (color-id (cdr r) (cdr g) (cdr b) (+ n 1))
                                                        ) ]
                                    ))

    (define css-raw (color-id (r (+ (length id-raw) 1)) (g (+ (length id-raw) 1)) (b (+ (length id-raw) 1))))
    (define css-final (string-append "<style>"(string-join (map ~a css-raw) "")"</style>" ))
    (define html-final (string-append css-final (string-join (map ~a html-raw) "") ))

    ;se crea el archibo html con estilos
    (write-file out_html html-final)

    )
)

#|Función resaltador  que recibe como entrada tres parámetros.
El primer parámetro es el nombre del archivo que contiene las expresiones regulares.
El segundo parámetro es el nombre del archivo que contiene el código fuente a resaltar.
El tercer parámetro es el nombre de los documentos de HTML+CSS en donde se resalta el léxico del archivo especificado en el segundo parámetro.|#


(resaltador "./exp.txt" "./data.txt" "index.html")


