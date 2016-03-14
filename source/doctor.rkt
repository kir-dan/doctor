#lang scheme/base
(require racket/serialize)

(define (doctor modelname)
	(define in (open-input-file modelname))
	(define s-ht (read in))
	(close-input-port in)

	(define ht (hash->list (deserialize s-ht)))

	(define (visit-doctor name)
		(define (doctor-driver-loop name history)
			(define (reply user-response history)
				(define (change-person phrase)
					(many-replace '((i you) (me you) (you i) (am are) (are am) (my your) (your my)
									(i'm you're) (you're i'm) (your's mine)) phrase))

				(define user-list
					(give-user-list-true (give-user-list user-response '())))

				(define random-word
					(symbol->string (pick-random user-response))
				)

				(define (forward-vesna history)
					(make-forward ht ".")
				)

				(define (back-vesna history)
					(append (cdr (reverse (make-back ht "."))) (list '|.|))
				)

				(define (mix-vesna history)
					(make-mix ht random-word)
				)

				(define (qualifier history)
					(append
					(pick-random '((you seem to think)
									(you feel that)
									(why do you believe that)
									(why do you say that)
									(why do you think that))) (change-person (pick-random user-list))))

				(define (hedge history)
					(pick-random '((please go on)
									(many people have the same sorts of feelings)
									(many of my patients have told me the same thing)
									(please continue)
									(say anything else))))

				(define (earlier history)
					(append '(earlier you said that)
						(change-person (list-ref history (random (length history))))))

				(define keys
					(list
						(list '(depressed suicide) '(when you feel depressed |,| go out for ice cream) '(depression is a disease that can be treated))
						(list '(mother father parents) '(tell me more about your *) '(why do you feel that way about your * ?))))

				(define keylist
					(give-keylist keys))

				(define goodkeys
					(give-goodkeys keys user-response))

				(define perfectkeys ;without *
					(give-perfectkeys goodkeys))

				(define (key-words history)
					(pick-random perfectkeys))
				
				(define (good-strategy strategy user-response history keylist)
					(cond ((null? strategy) '())
						(else (if ((car (car strategy)) user-response history keylist random-word) 
							(cons (car strategy) (good-strategy (cdr strategy) user-response history keylist))
							(good-strategy (cdr strategy) user-response history keylist)))))

				(define strategy
					(list
						(list (lambda (user-response history keylist rword) (and (in-ht rword ht) (not (equal? rword ".")) (not (equal? rword "!")) (not (equal? rword "?")))) mix-vesna 10)
						(list (lambda (user-response history keylist rword) #t) forward-vesna 10)
						(list (lambda (user-response history keylist rword) #t) back-vesna 10)
						(list (lambda (user-response history keylist rword) (not (null? (find-keyword user-response keylist)))) key-words 4)
						(list (lambda (user-response history keylist rword) (not (null? (give-user-list-true (give-user-list user-response '()))))) qualifier 3)
						(list (lambda (user-response history keylist rword) #t) hedge 2)
						(list (lambda (user-response history keylist rword) (< 2 (length history))) earlier 5)))

				(define (sum_str gs)
					(if (null? gs) 0
					(+ (car (cdr (cdr (car gs)))) (sum_str (cdr gs)))))

				(define (exec-strategy gs user-response history)
					(if (null? gs) (hedge history)
					(if (prob (car (cdr (cdr (car gs)))) (sum_str gs))
						((car (cdr (car gs))) history)
						(exec-strategy (cdr gs) user-response history))))

				(exec-strategy (good-strategy strategy user-response history keylist) user-response history))

			(newline)
			(print '**)

			(let ((user-line (string-downcase (read-line (current-input-port)))))
				(cond ((equal? user-line "goodbye") 
						(printf "Goodbye, ~a!\n" name)
						(printf "See you next week")
						(newline)
						(doctor modelname))
					(else (print-reply (reply (line-to-response user-line) history))
						(doctor-driver-loop name (append history (give-user-list-true (give-user-list (line-to-response user-line) '())))))))
			)

		(printf "Hello, ~a!\n" name)
		(print '(what seems to be the trouble?))
		(doctor-driver-loop name '()))

	(print '(next!))
	(newline)
	(print '(Who are you?))
	(newline)
	(print '**)
	(let ((name (string-downcase (read-line (current-input-port)))))
		(if (equal? name "suppertime")
			(print '(Time to go home!))
			(visit-doctor name))))

(define (fifty-fifty)
	(= (random 2) 0))

(define (pick-random lst)
	(list-ref lst (random (length lst))))

(define (required-word replacement-pairs word)
	(define (loop i)
		(cond ((= (length replacement-pairs) i) word)
			((equal? (car (list-ref replacement-pairs i)) word) (cadr (list-ref replacement-pairs i)))
			(else (loop (+ i 1)))))
	(loop 0))

(define (many-replace replacement-pairs lst)
	(cond ((null? lst) '()) 
		(else
			(cons (required-word replacement-pairs (car lst))
				(many-replace replacement-pairs (cdr lst))))))

(define (required-keyword word keylist)
	(define (loop i)
		(cond ((= (length keylist) i) '())
			((equal? (list-ref keylist i) word) word)
			(else (loop (+ i 1)))))
	(loop 0))

(define (find-keyword phrase keylist)
	(cond ((null? phrase) '())
		(else (if (null? (required-keyword (car phrase) keylist)) (find-keyword (cdr phrase) keylist)
			(cons (required-keyword (car phrase) keylist) (find-keyword (cdr phrase) keylist))))))

(define (prob n1 n2)
	(< (random n2) n1))

(define (give-keylist keys)
	(cond ((null? keys) '())
		(else (append (car (car keys)) (give-keylist (cdr keys))))))

(define (check-word word phrase)
	(cond ((null? phrase) '())
		(else (if (equal? word (car phrase))
			word (check-word word (cdr phrase))))))

(define (check-key key phrase)
	(cond ((null? key) '())
		(else (if (null? (check-word (car key) phrase)) (check-key (cdr key) phrase)
			(cons (check-word (car key) phrase) (check-key (cdr key) phrase))))))

(define (give-key key phrase)
	(if (null? (check-key (car key) phrase))
		'() (cons (check-key (car key) phrase) (cdr key))))

(define (give-goodkeys keys phrase)
	(cond ((null? keys) '())
		(else (if (null? (give-key (car keys) phrase))
			(give-goodkeys (cdr keys) phrase) (cons (give-key (car keys) phrase) (give-goodkeys (cdr keys) phrase))))))

(define (word-replace words phrase)
	(cond ((null? words) '())
		(else (cons (many-replace (list (append '(*) (list (car words)))) phrase) (word-replace (cdr words) phrase)))))

(define (key-replace words phrases)
	(cond ((null? phrases) '())
		(else (append (word-replace words (car phrases)) (key-replace words (cdr phrases))))))

(define (give-perfectkeys keys)
	(cond ((null? keys) '())
		(else (append (key-replace (car (car keys)) (cdr (car keys))) (give-perfectkeys (cdr keys))))))

; USER-LIST

(define (give-user-list-true lst)
	(cond ((null? lst) '())
		(else (cons (reverse (car lst)) (give-user-list-true (cdr lst))))))

(define (give-user-list lst tmp-lst)
	(cond ((null? lst) (if (not (null? tmp-lst)) (list tmp-lst) '()))
		((or (equal? (car lst) '|.|) (equal? (car lst) '|!|)) (if (null? tmp-lst) (give-user-list (cdr lst) '()) (cons tmp-lst (give-user-list (cdr lst) '()))))
		((equal? (car lst) '|?|) (give-user-list (cdr lst) '()))
		(else (give-user-list (cdr lst) (cons (car lst) tmp-lst)))))

; LINE-TO-RESPONSE

(define (my-char-punctuation? my-char)
	(or (char=? #\" my-char) (char=? #\, my-char) (char=? #\. my-char) (char=? #\! my-char) (char=? #\? my-char) (char=? #\- my-char)))

(define (line-to-response line)
	(line-to-response-utility line (make-string 0) 0))

(define (line-to-response-utility line word bracket)
	(cond ((string=? line "") (if (not (string=? word "")) (list (string->symbol word)) '()))
		((and (char=? #\( (string-ref line 0)) (string=? word "")) (line-to-response-utility (substring line 1) (make-string 0) (+ bracket 1)))
		((and (char=? #\( (string-ref line 0)) (not (string=? word ""))) (cons (string->symbol word) (line-to-response-utility (substring line 1) (make-string 0) (+ bracket 1))))
		((and (> bracket 0) (not (char=? #\) (string-ref line 0)))) (line-to-response-utility (substring line 1) (make-string 0) bracket))
		((and (> bracket 0) (char=? #\) (string-ref line 0))) (line-to-response-utility (substring line 1) (make-string 0) (- bracket 1)))
		((or (char-alphabetic? (string-ref line 0)) (char-numeric? (string-ref line 0)) (char=? #\' (string-ref line 0))) (line-to-response-utility (substring line 1) (string-append word (string (string-ref line 0))) bracket))
		((and (char=? #\- (string-ref line 0)) (not (string=? word ""))) (line-to-response-utility (substring line 1) (string-append word (string (string-ref line 0))) bracket))
		((my-char-punctuation? (string-ref line 0)) (if (string=? word "") (cons (string->symbol (string (string-ref line 0))) (line-to-response-utility (substring line 1) (make-string 0) bracket))
													(cons (string->symbol word) (cons (string->symbol (string (string-ref line 0))) (line-to-response-utility (substring line 1) (make-string 0) bracket))) ))
		(else (if (string=? word "") (line-to-response-utility (substring line 1) (make-string 0) bracket) (cons (string->symbol word) (line-to-response-utility (substring line 1) (make-string 0) bracket))))))

; PRINT-REPLY

(define (print-reply lst)
	(if (and (my-char-punctuation? (string-ref (symbol->string (car lst)) 0)) (not (char=? #\" (string-ref (symbol->string (car lst)) 0))) (not (char=? #\- (string-ref (symbol->string (car lst)) 0)))) 
		(printf (symbol->string (car lst))) (printf (string-append " " (symbol->string (car lst)))))
	(if (not (null? (cdr lst))) (print-reply (cdr lst)) '()))

; MAKE-FORWARD

(define (firstword-filter ht word)
	(cond ((null? ht) '())
		(else (if (equal? (symbol->string (caaar ht)) word) (cons (car ht) (firstword-filter (cdr ht) word)) (firstword-filter (cdr ht) word))))
)

(define (secondword-filter ht word)
	(cond ((null? ht) '())
		(else (if (equal? (symbol->string (car (cdr (car (car ht))))) word) (cons (car ht) (secondword-filter (cdr ht) word)) (secondword-filter (cdr ht) word))))
)

(define (ht-sum lst)
	(cond ((null? lst) 0)
		(else (+ (cdr (car lst)) (ht-sum (cdr lst)))))
)

(define (fchoose-word lst sum last-word)
	(cond ((null? lst) last-word)
		(else (if (prob (cdr (car lst)) sum) (cdr (car (car lst))) (fchoose-word (cdr lst) sum last-word))))
)

(define (schoose-word lst sum last-word)
	(cond ((null? lst) last-word)
		(else (if (prob (cdr (car lst)) sum) (car (car (car lst))) (schoose-word (cdr lst) sum last-word))))
)

(define (make-forward ht word)
	(define lst (firstword-filter ht word))
	(define sum (ht-sum lst))
	;(define last-word (if (pair? lst) (cdr (car (car lst))) (cdr (car lst))))
	(define last-word (cdr (car (car lst))))

	(define new-word (car (fchoose-word lst sum last-word)))

	(if (or (equal? (symbol->string new-word) ".") (equal? (symbol->string new-word) "!") (equal? (symbol->string new-word) "?")) (list new-word) (cons new-word (make-forward ht (symbol->string new-word))))
)

(define (make-back ht word)
	(define lst (secondword-filter ht word))
	(define sum (ht-sum lst))
	;(define last-word (if (pair? lst) (car (car (car lst))) (car (car lst))))
	(define last-word (car (car (car lst))))

	(define new-word (schoose-word lst sum last-word))

	(if (or (equal? (symbol->string new-word) ".") (equal? (symbol->string new-word) "!") (equal? (symbol->string new-word) "?")) (list new-word) (cons new-word (make-back ht (symbol->string new-word))))
)

(define (make-mix ht word)
	(append (cdr (reverse (make-back ht word))) (list (string->symbol word)) (make-forward ht word))
)

; IN-HT

(define (in-ht word ht)
	(cond ((null? ht) #f)
		(else (if (equal? word (symbol->string (caaar ht))) #t (in-ht word (cdr ht)) )))
)
