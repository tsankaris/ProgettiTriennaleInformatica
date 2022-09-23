;;;; 844760 Davide Zangari
;;;; 845273 Nicolas Guida
;;;; 845080 Mattia Campioni

;;;; -*-Mode: Lisp -*-
;;;; json-parse.lisp

(defun string-to-list (string)
  (cond
   ((not (stringp string))
    (error "argument is not a string!"))
   (T (map 'list #'char-code string))))

(defun rimuovi-spazi (json)
  (let ((testa (first json)))
    (cond
     ((or (equal testa (char-code '#\Space))
          (equal testa (char-code '#\Tab))
          (equal testa (char-code '#\Newline)))
      (rimuovi-spazi (rest json)))
     (T json))))

(defun json-parse (json)
  (let ((json-list (rimuovi-spazi(string-to-list json))))
    (cond
     ((equal (char-code '#\{) first json-list))
     (json-object (cdr json-list)))
    ((equal (char-code '#\[) (first json-list))
     (json-array (cdr json-list)))
    (T (error "argument is not correct")))))

(defun json-object (json)
  (let ((json-no-spazi (rimuovi-spazi json)))
    (cond
     ((and
       (equal (char-code '#\}) (first json-no-spazi))
      (null (cdr json-no-spazi)))
      '(json-obj))
     ((and
       (equal (char-code '#\}) (first json-no-spazi))
       (not (null (rimuovi-spazi (cdr json-no-spazi)))))
      (error "Syntax error"))
     (T (let ((json-return (json-members json-no-spazi NIL)))
          (cond
           ((null (rimuovi-spazi (second json-return)))
            (append '(json-obj) (first json-return)))
           (T (error "Syntax error"))))))))

(defun json-array (json)
  (let ((json-no-spazi (rimuovi-spazi json)))
    (cond
     ((and
       (equal (char-code '#\]) (first json-no-spazi))
       (null (cdr json-no-spazi)))
      '(json-array))
     ((and
       (equal (char-code '#\]) (first json-no-spazi))
       (not (null (rimuovi-spazi (cdr json-no-spazi)))))
      (error "Syntax error"))
     (T (let ((json-return (json-elements json-no-spazi NIL)))
          (cond
           ((null (rimuovi-spazi (second json-return)))
            (append '(json-array) (first json-return)))
           (T (error "Syntax error"))))))))

(defun json-members (json object)
  (let ((value (json-pairs json)))
    (let((parsed (append object (list (first value))))
        (to-be-parsed (rimuovi-spazi (second value))))
     (cond
      ((equal (char-code '#\}) (first to-be-parsed))
       (append (list parsed) (list (rest to-be-parsed))))
      ((equal (char-code '#\,) (first to-be-parsed))
       (json-members (rest to-be-parsed) parsed))
      (T (error "Syntax error"))))))

;;; json-elements (json object)
;; Gestisce il contenuto di un array chiamando json-value per ogni
;; elemento contenuto in esso.
(defun json-elements (json object)
  (let ((value (json-value json)))
    (let ((parsed (append object (list (first value))))
          (to-be-parsed (rimuovi-spazi (second value))))
      (cond
       ((equal (char-code '#\]) (first to-be-parsed))
        (append (list parsed) (list (rest to-be-parsed))))
       ((equal (char-code '#\,) (first to-be-parsed))
        (json-elements (rest to-be-parsed) parsed))
       (T (error "Syntax error"))))))

;;; json-pairs (json)
;; gestisce le
(defun json-pairs (json)
  (let ((json-no-spazi (rimuovi-spazi json)))
    (cond
     ((or
       (equal (char-code '#\") (first json-no-spazi))
       (equal (char-code '#\') (first json-no-spazi)))
      (let ((string-temp (rimuovi-spazi (second (json-string json-no-spazi)))))
        (cond
         ((equal (char-code '#\:) (first string-temp))
          (let ((valore (json-value (rimuovi-spazi (rest string-temp)))))
            (append
                 (list (append (list (first (json-string json-no-spazi)))
                               (list (first valore))))
                 (list (second valore)))))
         (T (error "Syntax error")))))
     (T (error "Syntax error")))))

;;; json-value (json)
;; Un json-value può essere una stringa, un numero, un json-object
;; oppure un array.
(defun json-value (json)
  (let ((json-no-spazi (rimuovi-spazi json)))
    (cond
     ((or
       (equal (char-code '#\") (first json-no-spazi))
       (equal (char-code '#\') (first json-no-spazi)))
      (json-string json-no-spazi))
     ((equal (char-code '#\{) (first json-no-spazi))
      (json-object-interno (rest json-no-spazi))
      ((equal (char-code '#\[) (first json-no-spazi))
       (json-array-interno (rest json-no-spazi)))
      ((or
        (and
         (>= (first json-no-spazi) 48)
         (<= (first json-no-spazi) 57))
        (equal (char-code '#\+) (first json-no-spazi))
        (equal (char-code '#\-) (first json-no-spazi)))
       (json-number json-no-spazi))
      (T (error "Syntax error")))))

;;; json-object-interno (json)
;;  funziona analoga a json-object con la differenza che gestisce
;; gli oggetti annidati.
(defun json-object-interno (json)
  (let ((json-no-spazi) (rimuovi-spazi json)))
  (cond
     ((equal (char-code '#\}) (first json-no-spazi)
             (append
        (list '(json-obj))
        (list (rest json-no-spazi))))
      (T (let ((member (json-members json-no-spazi NIL)))
          (append
           (list (append '(json-obj) (first member)))
           (list (second member))))))))

(defun json-array-interno (json)
  (let ((json-no-spazi (rimuovi-spazi json)))
    (cond
     ((equal (char-code '#\]) (first json-no-spazi))
       (append
        (list'(json-array))
        (list (rest json-no-spazi))))
      (T (let ((element (json-elements json-no-spazi NIL)))
          (append
           (list (append '(json-array) first element)))
          (list (second element))))))))

(defun json-string (json)
  (cond
   ((equal (char-code '#\") (first json))
    (json-string-dq (rest json) NIL))
   ((equal (char-code '#\') (first json))
    (json-string-sq (rest json) NIL))
   (T (error "Syntax error"))))

(defun json-string-dq (json var)
  (cond
   ((null json)
    (error "Syntax error"))
   ((equal (char-code '#\') (first json))
    (error "Sq not allowed"))
   ((equal (char-code '#\") (first json))
    (append
     (list (concatenate 'string var ""))
     (list (rest json))))
   (T
    (let ((carattere-char (string (code-char (first json)))))
      (json-string-dq (rest json)
                      (concatenate 'string var carattere-char))))))

(defun json-string-sq (json var)
  (cond
   ((null json)
    (error "Syntax error"))
   ((equal (char-code '#\") (first json))
    (error "Dq not allowed"))
   ((equal (char-code '#\') (first json))
    (append
     (list (concatenate 'string var ""))
     (list (rest json))))
   (T
    (let ((carattere-char (string (code-char (first json)))))
      (json-string-sq (rest json)
                          (concatenate 'string var carattere-char))))))

(defun json-number (json)
  (cond
   ((equal (char-code '#\+) (first json))
    (crea-numero-intero (rest json) "+"))
   ((equal (char-code '#\-) (first json))
    (crea-numero-intero (rest json) "-"))
   ((and
     (>= (first json) 48)
     (<= (first json) 57))
    (crea-numero-intero json NIL))
   (T (error "Syntax error"))))

(defun crea-numero-intero (json var)
  (cond
   ((null json)
    (error "Number syntax error"))
   ((and
     (>= (first json) 48)
     (<= (first json) 57))
    (let ((cifra (string (code-char (first json)))))
      (crea-numero-intero (rest json) (concatenate 'string var cifra))))
   ((equal (char-code '#\.) (first json))
    (let ((punto (string (code-char (first json)))))
      (crea-numero-float (rest json) (concatenate 'string var punto))))
   (T (append
       (list (parse-integer (concatenate 'string var "")))
       (list json)))))

(defun crea-numero-float (json var)
  (cond
   ((null json)
    (error "Number syntax error"))
   ((equal (char-code '#\.) (first json))
    (error "Not more than one point allowed"))
   ((and
     (>= (first json) 48)
     (<= (first json) 57))
    (let ((cifra (string (code-char (first json)))))
      (crea-numero-float (rest json) (concatenate 'string var cifra))))
   (T (append
       (list (parse-float (concatenate 'string var "")))
       (list json)))))

(defun controlla-indici (list)
  (cond ((null list) T)
        ((or
          (not (numberp (first list)))
          (< (first list) 0))
         NIL)
        (T (controlla-indici (rest list)))))
        
    

(defun json-access (json &optional key &rest index)
  (if (null key)
      json
    (json-access-object json key index)))

(defun json-access-object (json key &optional &rest index)
  (cond
   ((null json)
    (error "Key not found"))
   ((null key)
    (error "Key is null"))
   ((not (controlla-indici (flatten index)))
    (error "Indexes must be numerical numbers"))
   ((not (stringp key))
    (error "Key must be a string"))
   ((equal 'json-obj (first json))
    (json-access-object (rest json) key (flatten index)))
   ((equal 'json-array (first json))
    (json-access-array json key (flatten index)))
   ((and
     (equal (first (first json)) key)
     (not (null (first index))))
    (json-access-array (second (first json)) (flatten index)))
   (T (json-access-object (rest json) key (flatten index)))))
     
    

(defun flatten (x)
  (cond
   ((null x) x)
   ((atom x) (list x))
   (T (append (flatten (first x))
              (flatten (rest x))))))

(defun json-access-array (json &rest index)
  (let ((indici (flatten index)))
    (cond
     ((> (first indici) (- (length json) 1))
      (error "Index out of bounds"))
     ((equal 'json-array (first json))
      (json-access-array (rest json) indici))
     ((equal (rest indici) NIL)
      (cond
       ((equal (first indici) 0)
        (first json))
       (T (json-access-array (rest json) (- (first indici) 1)))))
     ((and
       (not (equal (rest indici) NIL))
       (not (atom (first json))))
      (cond
       ((equal (first indici) 0)
        (json-access-array (first json) (rest indici)))
       ((not (equal (first indici) 0))
        (json-access-array (rest json) (append (list (- (first indici) 1))
                                               (rest indici))))
       (T (error "Syntax error"))))
     (T (error "Syntax error")))))


;;;; end of file -- json-parse.lisp 