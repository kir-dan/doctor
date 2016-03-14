#lang scheme/base
(require racket/serialize)

(define (learning modelname textname flag)
	(define in (open-input-file textname))
	(define text (string-downcase (read-line in)))
	(close-input-port in)

	(define lst (text-to-lst text))

	(define ht (cond ((equal? flag "new") (begin
			(if (file-exists? modelname) (delete-file modelname) '())

			(make-hash)
		))

		((equal? flag "add") (begin
			(define in (open-input-file modelname))
			(define s-ht-in (read in))
			(close-input-port in)

			(deserialize s-ht-in)
		))

		(else (exit))
	))

	(parse-lst ht lst)

	(display "Learning complete\n")

	(define s-ht (serialize ht))

	(if (file-exists? modelname) (delete-file modelname) '())
	(define out (open-output-file modelname))
	(write s-ht out)
	(close-output-port out)
)

; PARSE-LST

(define (parse-lst ht lst)
	(cond ((null? (cdr lst)) ht)
		(else (begin (define pr (list (car lst) (car (cdr lst))))
			(if (hash-has-key? ht pr) (hash-set! ht pr (+ (hash-ref ht pr) 1)) (hash-set! ht pr 1))
			(parse-lst ht (cdr lst))))
	)
)

; TEXT-TO-LST

(define (my-char-punctuation? my-char)
	(or (char=? #\" my-char) (char=? #\, my-char) (char=? #\. my-char) (char=? #\! my-char) (char=? #\? my-char) (char=? #\- my-char)))
(define (text-to-lst line)
	(text-to-lst-utility line (make-string 0) 0))
(define (text-to-lst-utility line word bracket)
	(cond ((string=? line "") (if (not (string=? word "")) (list (string->symbol word)) '()))
		((and (char=? #\( (string-ref line 0)) (string=? word "")) (text-to-lst-utility (substring line 1) (make-string 0) (+ bracket 1)))
		((and (char=? #\( (string-ref line 0)) (not (string=? word ""))) (cons (string->symbol word) (text-to-lst-utility (substring line 1) (make-string 0) (+ bracket 1))))
		((and (> bracket 0) (not (char=? #\) (string-ref line 0)))) (text-to-lst-utility (substring line 1) (make-string 0) bracket))
		((and (> bracket 0) (char=? #\) (string-ref line 0))) (text-to-lst-utility (substring line 1) (make-string 0) (- bracket 1)))
		((or (char-alphabetic? (string-ref line 0)) (char-numeric? (string-ref line 0)) (char=? #\' (string-ref line 0))) (text-to-lst-utility (substring line 1) (string-append word (string (string-ref line 0))) bracket))
		((and (char=? #\- (string-ref line 0)) (not (string=? word ""))) (text-to-lst-utility (substring line 1) (string-append word (string (string-ref line 0))) bracket))
		((my-char-punctuation? (string-ref line 0)) (if (string=? word "") (cons (string->symbol (string (string-ref line 0))) (text-to-lst-utility (substring line 1) (make-string 0) bracket))
													(cons (string->symbol word) (cons (string->symbol (string (string-ref line 0))) (text-to-lst-utility (substring line 1) (make-string 0) bracket))) ))
		(else (if (string=? word "") (text-to-lst-utility (substring line 1) (make-string 0) bracket) (cons (string->symbol word) (text-to-lst-utility (substring line 1) (make-string 0) bracket))))))
