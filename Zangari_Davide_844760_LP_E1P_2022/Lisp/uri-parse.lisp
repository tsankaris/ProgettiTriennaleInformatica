;;;; 844760 Davide Zangari
;;;; 869012 Andrea Cappone
;;;; 866194 Gaia Iori
;;;; -*-Mode: Lisp -*-
;;;; uri.lisp


;;; prende in input una uri-string e restituisce una lista uri-structure
(defun uri-parse (uri-string)
  (cond
   ((not (stringp uri-string))  nil)
   (t (uri uri-string))
   )
  )

;;; prende in input la lista uri-structure e restituisce lo scheme
(defun uri-scheme (uri-structure)
  (cond
   ((not (listp uri-structure))  nil)
   (t (first uri-structure))
   )
  )

;;; prende in input la lista uri-structure e restituisce lo userinfo
(defun uri-userinfo (uri-structure)
  (cond
   ((not (listp uri-structure))  nil)
   (t (second uri-structure))
   )
  )

;;; prende in input la lista uri-structure e restituisce l'host
(defun uri-host (uri-structure)
  (cond
   ((not (listp uri-structure))  nil)
   (t (third uri-structure))
   )
  )

;;; prende in input la lista uri-structure e restituisce la port
(defun uri-port (uri-structure)
  (cond
   ((not (listp uri-structure))  nil)
   (t (fourth uri-structure))
   )
  )

;;; prende in input la lista uri-structure e restituisce il path
(defun uri-path (uri-structure)
  (cond
   ((not (listp uri-structure))  nil)
   (t (fifth uri-structure))
   )
  )

;;; prende in input la lista uri-structure e restituisce la query
(defun uri-query (uri-structure)
  (cond
   ((not (listp uri-structure))  nil)
   (t (sixth uri-structure))
   )
  )

;;; prende in input la uri-structure e restituisce il fragment
(defun uri-fragment (uri-structure)
  (cond
   ((not (listp uri-structure))  nil)
   (t (seventh uri-structure))
   )
  )

;;; stampa la uri-structure
(defun uri-display (uri-structure &optional (stream t))
  (format stream "Scheme:		~S~%" (uri-scheme uri-structure))
  (format stream "Userinfo:	~S~%" (uri-userinfo uri-structure))
  (format stream "Host:		~S~%" (uri-host uri-structure))
  (format stream "Port:		~S~%" (uri-port uri-structure))
  (format stream "Path:		~S~%" (uri-path uri-structure))
  (format stream "Query:		~S~%" (uri-query uri-structure))
  (format stream "Fragment:	~S~%" (uri-fragment uri-structure))
  T
  )


;;; prende in input una stringa e ritorna la uri-structure se valida
(defun URI (u)
  (let (
	(u1 (URI1 u))
	(u2 (URI2 u))
	(s (get-scheme u))
	)
    (let (
	  (scheme (scheme s))
	  )
      (cond
       ((not (stringp u)) nil)
       ((not (member #\: (stl u))) nil) ;; ':' obbligatorio per lo scheme
       ((not scheme) nil) ;; scheme non valido
       ((null (third (stll u #\:))) (append (list scheme) '(nil) '(nil)
					    '(80) '(nil) '(nil) '(nil))) 
       ((or 
	 (equal s "mailto")
	 (equal s "tel")
	 (equal s "fax")
	 (equal s "news")
	 (equal s "zos")
	 ) (if u2 (append (list s) (list (first u2)) (list (second u2))
			  (list (third u2)) (list (fourth u2))
			  (list (fifth u2)) (list (sixth u2)))))
       (u1 (append (list s) (list (first u1)) (list (second u1))
		   (list (third u1)) (list (fourth u1)) (list (fifth u1))
		   (list (sixth u1)))) 
       (t nil)
       )
      )
    )
  )

;;; prende in input una stringa e se valida ritorna la uri-structure
(defun URI1 (u)
  (cond 
   ((not (stringp u)) nil)
   (t (uri1-syntax (llts (cdr (cdr (stll u #\:))))))
   )
  )

;;; prende in input la stringa e se valida ritorna la uri-structure
(defun URI2 (u)
  (let (
	(s (get-scheme u))
	(ps (llts (cdr (cdr (stll u #\:)))))
	)
    (cond 
     ((not (stringp u)) nil)
     ((equal s "mailto") (mailto ps))
     ((equal s "tel") (tel-fax ps))
     ((equal s "fax") (tel-fax ps))
     ((equal s "news") (news ps))
     ((equal s "zos") (zos ps))
     (t nil)
     )
    )
  )


;;; controlla che f sia un fragment
(defun fragment (f)
  (cond
   ((not (stringp f)) nil)
   ((> (length f) 0) f)
   (t nil)
   )
  )

;;; data una stringa URI ritorna solo lo scheme
(defun get-scheme (u)
  (let (
	(str (lts (car (stll u #\:))))
	)
    (cond
     ((not (stringp u)) nil)
     ((scheme str) str)
     (t "")
     )
    )
  )

;;; verifica solo che s sia scheme
(defun scheme (s)
  (cond
   ((not (stringp s)) nil)
   ((identificatore s) s)
   (t nil)
   )
  )

;;; verifica che u sia uno userinfo
(defun userinfo (u)
  (cond
   ((not (stringp u)) nil)
   ((identificatore u) u)
   (t nil)
   )
  )

;;; verifica che ds sia una port
(defun port (ds)
  (cond 
   ((not (stringp ds)) nil)
   ((equal ds "") 80)
   ((numero (stl ds)) (parse-integer ds))
   (t nil)
   )
  )

;;; verifica che la lista sia composta da soli caratteri numerici '(#\1 #\2 #\3)
(defun numero (l)
  (cond 
   ((not (listp l)) nil)
   ((null l) t)
   ((digit (car l)) (numero (cdr l)))
   (t nil)
   )
  )

;;; verifica che q sia una query valida
(defun query (q)
  (cond
   ((not (stringp q)) nil)
   ((equal q "") nil)
   ((every #'check-query (stl q)) q)
   (t nil)
   )
  )

;;; verifica che i sia un identificatore valido
(defun identificatore (i)
  (cond
   ((not (stringp i)) nil)
   ((equal i "") nil)
   (t (every #'check-identificatore (stl i)))
   )
  )

;;; verifica che i sia un identificatore-host
(defun identificatore-host (i)
  (cond
   ((not (stringp i)) nil)
   ((equal i "") nil)
   (t (every #'check-identificatore-host (stl i)))
   )
  )

;;; verifica che tf sia uno schema tel-fax
(defun tel-fax (tf)
  (cond
   ((not (stringp tf)) nil)
   ((userinfo tf) (append (list tf) '(nil) '(80)))
   (t nil)
   )
  )

;;; verifica che n sia uno schema news
(defun news (n)
  (cond
   ((not (stringp n)) nil)
   ((host n) (append '(nil) (list n) '(80)))
   (t nil)
   )
  )


;;; stl converte una stringa in lista, mantenendo i caratteri con #\X
;;; "ciao" --> '(#\c #\i #\a #\o)
(defun stl (s)
  (coerce s 'list)
  )

;;; lts converte una lista in stringa
;;; '(#\c #\i #\a #\o) --> "ciao"
(defun lts (l)
  (cond
   ((not (listp l)) "")
   (t (format nil "~{~A~}" l))
   )
  )

;;; llts converte una lista di liste in stringa
;;; '((a b c) #\C (d e) #\C (f g h)) --> "abcCdeCfgh"
(defun llts (l)
  (cond
   ((not (listp l)) "")
   (t (lts (lltl l)))
   )
  )

;;; converte una stringa s in lista di liste usando char c per
;;; separare le sottoliste 
;;; "abcCdeCfgh" --> '((a b c) #\C (d e) #\C (f g h))
(defun stll (s c)
  (cond
   ((not (and (stringp s) (characterp c))) nil)
   (t (l-list (stl s) nil nil c))
   )
  )

;;; modifica una lista x creando sottoliste separate da char c.
;;; le liste l ed s sono usati come parametri d'appoggio
;;; '(a b c #\C d e #\C f g h) --> '((a b c) #\C (d e) #\C (f g h))
(defun l-list (x l s c)
  (cond
   ((not (and (listp x) (listp l) (listp s) (characterp c))) nil)
   ((null x) (append l (if (null s) nil (list s))))
   ((eq (car x) c) (l-list (cdr x) (append l (if (null s) nil (list s))
					   (list c))
			   nil c))
   (t (l-list (cdr x) l (append s (list (car x))) c))
   )
  )

;;; converte una stringa s in una lista di liste usando c1 e c2 come separatori
;;; "abcC1deC2fgh" --> '((a b c) #\C1 (d e) #\C2 (f g h))
(defun stll-2 (s c1 c2)
  (cond
   ((not (and (stringp s) (characterp c1) (characterp c2))) nil)
   (t (l-list-2 (stl s) nil nil c1 c2))
   )
  )

;;; modifica una lista x creando sottoliste separate dai due char c1 c2
;;; le liste l ed s sono usati come parametri d'appoggio
;;; '(a b c #\C1 d e #\C2 f g h) --> '((a b c) #\C1 (d e) #\C2 (f g h))
(defun l-list-2 (x l s c1 c2)
  (cond
   ((not (and (listp x) (listp l) (listp s) (characterp c1) (characterp c2)))
    nil)
   ((null x) (append l (if (null s) nil (list s))))
   ((or (eq (car x) c1)
	(eq (car x) c2))
    (l-list-2 (cdr x)
	      (append l (if (null s) nil (list s)) (list (car x))) nil c1 c2))
   (t (l-list-2 (cdr x) l (append s (list (car x))) c1 c2))
   )
  )

;;; lltl converte una lista di liste in lista semplice
;;; '((a b c) d (e f g)) --> '(a b c d e f g)
(defun lltl (x)
  (cond
   ((null x) x)
   ((atom x) (list x))
   (t (append (lltl (car x)) (lltl (cdr x))))
   )
  )

;;; controlla che a sia un authority
(defun authority (a)
  (let (
	(l (stl a))
	)
    (cond 
     ((not (stringp a)) nil) ;; deve essere string
     ((not (and (equal (first l) #\/) (equal (second l) #\/))) nil)
     (t (check-authority (lts (cdr (cdr l)))))
     )
    )
  )

;;; controlla che l'authority a sia composto da userinfo facoltativo,
;;; host obbligatorio e port facoltativo
(defun check-authority (a)
  (let (
	(userinfo (userinfo-at a)) 
	(port (has-port a))
	(h0 (host a))
	(h1 (host (llts (cdr (cdr (stll a #\@))))))
	(h2 (host (lts (car (stll a #\:)))))
	(h3 (host (lts (third (stll-2 a #\@ #\:)))))
	)
    (cond
     ((not (stringp a)) nil)
     ((equal a "") nil)
     (h0 (append '(nil) (list h0) '(80)))
     ((and userinfo h1) (append (list userinfo) (list h1) '(80))) 
     ((and h2 port) (append '(nil) (list h2) (list port)))
     ((and userinfo h3 port) (append (list userinfo) (list h3) (list port)))
     (t nil)
     )
    )
  )

;;; verifica che lo userinfo u sia seguito da una @
(defun userinfo-at (u)
  (cond
   ((not (stringp u)) nil)
   ((equal u "") u) ;; userinfo facoltativo
   ((member #\@ (stl u)) (userinfo (lts (car (stll u #\@)))))
   (t nil)
   )
  )

;;; verifica che dopo i : ci sia una port
(defun has-port (p)
  (cond
   ((not (stringp p)) nil)
   ((equal p "") 80) ;; port facoltativa
   ((equal (last (stl p)) '(#\:)) nil) ;; se ha ':' deve  avere port
   ((member #\: (stl p)) (port (lts (third (stll p #\:)))))
   (t nil)
   )
  )

;;; verifica che ip sia un indirizzo ip valido
(defun indirizzo-ip (ip)
  (let (
	(ll (stll ip #\.))
	)
    (cond
     ((not (stringp ip)) nil) ;; deve  essere string
     ((not (= (length ll) 7)) nil)
     ((numero-ip ll) ip)
     (t nil)
     )
    )
  )

;;; verifica che le terne di ip siano separate da un punto
(defun numero-ip (ip)
  (cond
   ((not (listp ip)) nil) ;; deve  essere list
   ((terna-ip (car ip)) (punto-ip (cdr ip)))
   (t nil)
   )
  )

;;; verifica che ci sia una terna preceduta da un punto
(defun punto-ip (ip)
  (cond
   ((not (listp ip)) nil) ;; deve essere list
   ((null ip) t)
   ((eq #\. (car ip)) (numero-ip (cdr ip)))
   (t nil)
   )
  )

;;; verifica che la lista num sia una terna di ip valida
(defun terna-ip (num)
  (let (
	(fd (first num))
	(sd (second num))
	(td (third num))
	)
    (cond
     ((not (listp num)) nil)
     ((not (= (length num) 3)) nil)
     ((and 
       (digit fd)
       (digit sd)
       (digit td)
       (or 
	(char< fd #\2)
	(and 
	 (char= fd #\2)
	 (char< sd #\5)
	 )
	(and 
	 (char= sd #\5)
	 (char< td #\6)
	 )
	)
       )
      (lts num))
     (t nil)
     )
    )
  )

;;; ritorna T se d e' di tipo digit (char tra 0 e 9)
(defun digit (d)
  (member d '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
  )

;;; verifica che m sia uno schema mailto
(defun mailto (m)
  (let ((ll (stll m #\@)))
    (let (
	  (userinfo (list (lts (first ll))))
	  (host (list (if (null (third ll)) nil (lts (third ll)))))
	  )
      (cond 
       ((not (stringp m)) nil) ;; deve  essere string
       ((not (or (= (length ll) 1) (= (length ll) 3))) nil)
       ((check-mailto (first ll) (third ll)) (append userinfo host '(80)))
       (t nil)
       )
      )
    )
  )

;;; controlla che u sia uno userinfo e h sia un host facoltativo
(defun check-mailto (u h)
  (let (
	(us (lts u))
	(hs (lts h))
	)
    (cond
     ((not (and (listp u) (listp u))) nil)
     ((null u) nil)
     ((null h) (userinfo us))
     (t (and (userinfo us) (host hs)))
     )
    )
  )

;;; controlla che u sia uno schema di tipo uri1
(defun uri1-syntax (u)
  (let (
	(l (stl u))
	(auth1 (authority u))
	(poau1 (post-auth u))
	(auth2 (authority (llts (append '(#\/ #\/) (third (stll u #\/))))))
	(poau2 (post-auth (llts (cdr (cdr (cdr (stll u #\/)))))))
	)
    (cond 
     ((not (stringp u)) nil) ;; deve essere string
     (auth1 (append auth1 '(nil) '(nil) '(nil)))
     (poau1 (append '(nil) '(nil) '(80) poau1)) 
     ((and (equal (first l) #\/) (equal (second l) #\/) auth2 poau2)
      (append auth2 poau2)) 
     (t nil)
     )
    )
  )

;;; controlla che pa sia dopo l'authority 
(defun post-auth (pa)
  (let (
	(l (stl pa))
	)
    (cond 
     ((not (stringp pa)) nil) ;; deve essere string
     ((equal pa "") t) ;; post-auth facoltativo
     ((not (equal (car l) #\/)) nil) ;; deve iniziare con "/"
     (t (check-post-auth (lts (cdr l))))
     )
    )
  )

;;; controlla che pa sia composta da path, query e fragment facoltativi
(defun check-post-auth (pa)
  (let (
	(p (has-path pa))
	(q (has-query pa))
	(f (has-fragment pa))
	)
    (let (
	  (path (if (equal p "") '(nil) (list p)))
	  (query (if (equal q "") '(nil) (list q)))
	  (fragment (if (equal f "") '(nil) (list f)))
	  )
      (cond
       ((not (stringp pa)) nil)
       ((equal pa "") (append '(nil) '(nil) '(nil)))
       ((and p q f) (append path query fragment))
       (t nil)
       )
      )
    )
  )

;;; controlla che p abbia un path valido o che non lo abbia
(defun has-path (p)
  (let (
	(fc (car (stl p)))
	(p1 (path p))
	(p2 (path (lts (car (stll p #\?)))))
	(p3 (path (lts (car (stll p #\#)))))
	)
    (cond
     ((not (stringp p)) nil)
     ((equal p "") "")
     ((equal fc #\?) "")
     ((equal fc #\#) "")
     (p1 p1)
     ((and (member #\? (stl p)) p2) p2)
     ((and (member #\# (stl p)) p3) p3)
     (t nil)
     )
    )
  )

;;; controlla che q abbia una query valida o che non la abbia
(defun has-query (q)
  (let (
	(l (stl q))
	)
    (let (
	  (ll (stll (lts (cdr l)) #\#))
	  )
      (let (
	    (q1 (query (lts (car ll))))
	    (q2 (query (llts (cdr (cdr (stll (lts (car ll)) #\?))))))
	    )
	(cond
	 ((not (stringp q)) nil)
	 ((equal q "") "")
	 ((not (member #\? l)) "")
	 ((equal (car l) #\#) "")
	 ((and (equal (car l) #\?) q1) q1)
	 (t q2)
	 )
	)
      )
    )
  )

;;; controlla che f abbia un fragment valido o che non lo abbia
(defun has-fragment (f)
  (let (
	(f1 (fragment (lts (cdr (stl f)))))
	(f2 (fragment (llts (cdr (cdr (stll f #\#))))))
	)
    (cond
     ((not (stringp f)) nil)
     ((equal f "") "")
     ((not (member #\# (stl f))) "")
     ((and (equal (car (stl f)) #\#) f1) f1)
     (t f2)
     )
    )
  )

;;; controlla che u sia uno schema zos
(defun zos (u)
  (let (
	(l (stl u))
	(auth1 (authority u))
	(poau1 (post-auth-zos u))
	(auth2 (authority (llts (append '(#\/ #\/) (third (stll u #\/))))))
	(poau2 (post-auth-zos (llts (cdr (cdr (cdr (stll u #\/)))))))
	)
    (cond 
     ((not (stringp u)) nil) ;; deve essere string
     (auth1 (append auth1 '(nil) '(nil) '(nil)))
     (poau1 (append '(nil) '(nil) '(80) poau1))
     ((and (equal (first l) #\/) (equal (second l) #\/) auth2 poau2)
      (append auth2 poau2))
     (t nil)
     )
    )
  )

;;; controlla che pa sia dopo l'authority
(defun post-auth-zos (pa)
  (let (
	(l (stl pa))
	)
    (cond 
     ((not (stringp pa)) nil) ;; deve essere string
     ((equal pa "") t) ;; post-auth facoltativo
     ((not (equal (car l) #\/)) nil) ;; deve iniziare con "/"
     (t (check-post-auth-zos (lts (cdr l))))
     )
    )
  )

;;; controlla che pa abbia path obbligatorio, query e fragment facoltativi
(defun check-post-auth-zos (pa)
  (let (
	(p0 (path-zos pa))
	(p1 (path-zos (lts (car (stll pa #\?)))))
	(p2 (path-zos (lts (car (stll pa #\#)))))
	(q0 (has-query pa))
	(q1 (has-query (lts (car (stll pa #\#)))))
	(f (has-fragment pa))
	)
    (let (
	  (path0 (if (equal p0 "") '(nil) (list p0)))
	  (path1 (if (equal p1 "") '(nil) (list p1)))
	  (path2 (if (equal p2 "") '(nil) (list p2)))
	  (query0 (if (equal q0 "") '(nil) (list q0)))
	  (query1 (if (equal q1 "") '(nil) (list q1)))
	  (fragment (if (equal f "") '(nil) (list f)))
	  )
      (cond
       ((not (stringp pa)) nil)
       ((equal pa "") nil)
       (p0 (append path0 '(nil) '(nil)))
       ((and p1 q0) (append path1 query0 '(nil)))
       ((and p2 f) (append path2 '(nil) fragment))
       ((and p1 q1 f) (append path1 query1 fragment))
       (t nil)
       )
      )
    )
  )

;;; controlla che p sia un path zos valido
(defun path-zos (p)
  (let (
	(ll (stll-2 p #\( #\)))
	)
    (cond 
     ((not (stringp p)) nil) ;; deve essere string
     ((not (or (= (length ll) 1)
	       (and (= (length ll) 4)
		    (equal (second ll) #\() (equal (fourth ll) #\)))))
      nil)
     ((check-path-zos (first ll) (third ll)) p)
     (t nil)
     )
    )
  )

;;; controlla che i44 sia un id44 obbligatorio, e che i8 sia un id8 facoltativo
(defun check-path-zos (i44 i8)
  (let (
	(i44z (id44 i44))
	(i8z (id8 i8))
	)
    (cond
     ((not (and (listp i44) (listp i8))) nil)
     ((null i44) nil)
     ((null i8) i44z)
     (t (and i44z i8z))
     )
    )
  )

;;; controlla che la stringa i sia un id44
(defun id44 (i)
  (cond
   ((not (listp i))  nil)
   ((or (< (length i) 1) (> (length i) 44)) nil) ;; lunghezza errata
   ((not (alpha-char-p (car i))) nil) ;; non inizia con lettera
   ((equal (car (last i)) #\.) nil) ;; finisce con '.'
   ((check-id44 i) i)
   (t nil)
   )
  )

;;; controlla che la lista i sia un id44
(defun check-id44 (i)
  (cond
   ((not (listp i)) nil)
   ((null i) t)
   ((or (alphanumericp (car i)) (equal (car i) #\.)) (check-id44 (cdr i)))
   (t nil) ;; carattere non permesso
   )
  )

;;; controlla che la stringa i sia un id8
(defun id8 (i)
  (cond
   ((not (listp i))  nil)
   ((or (< (length i) 1) (> (length i) 8)) nil) ;; lunghezza errata
   ((not (alpha-char-p (car i))) nil) ;; non inizia con lettera
   ((check-id8 i) i)
   (t nil)
   )
  )

;;; controlla che la lista i sia un id8
(defun check-id8 (i)
  (cond
   ((not (listp i)) nil)
   ((null i) t)
   ((alphanumericp (car i)) (check-id8 (cdr i))) ;; solo alpha numerico
   (t nil) ;; carattere non permesso
   )
  )

;;; controlla che h sia un host valido
(defun host (h)
  (cond 
   ((not (stringp h)) nil) ;; deve essere string
   ((indirizzo-ip h) h)
   ((string-host (stll h #\.)) h)
   (t nil)
   )
  )

;;; controlla che h sia un identificatore-host valido
(defun string-host (h)
  (cond
   ((not (listp h)) nil) ;; deve essere list
   ((identificatore-host (lts (car h))) (punto-host (cdr h)))
   (t nil)
   )
  )

;;; controlla che h sia un identificatore-host preceduto dal punto
(defun punto-host (h)
  (cond
   ((not (listp h)) nil) ;; deve essere list
   ((null h) t)
   ((eq #\. (car h)) (string-host (cdr h)))
   (t nil)
   )
  )

;;; controlla che p sia un path valido
(defun path (p)
  (cond 
   ((not (stringp p)) nil) ;; deve essere string
   ((string-path (stll p #\/)) p)
   (t nil)
   )
  )

;;; controlla che p sia un identificatore valido
(defun string-path (p)
  (cond
   ((not (listp p)) nil) ;; deve essere list
   ((identificatore (lts (car p))) (slash-path (cdr p)))
   (t nil)
   )
  )

;;; controlla che p sia un identificatore preceduto da /
(defun slash-path (p)
  (cond
   ((not (listp p)) nil) ;; deve essere list
   ((null p) t)
   ((eq #\/ (car p)) (string-path (cdr p)))
   (t nil)
   )
  )

;;; controlla che c non sia un carattere proibito da identificatore
(defun check-identificatore (c)
  (if (member c '(#\? #\/ #\# #\@ #\:)) nil t)
  )

;;; controlla che c non sia un carattere proibito da identificatore-host
(defun check-identificatore-host (c)
  (if (member c '(#\? #\/ #\# #\@ #\: #\.)) nil t)
  )

;;; controlla che c non sia un carattere proibito da query
(defun check-query (c)
  (if (member c '(#\#)) nil t)
  )


;;;; end of file -- uri.lisp
