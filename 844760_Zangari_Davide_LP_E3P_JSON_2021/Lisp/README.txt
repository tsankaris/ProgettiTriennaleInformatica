Progetto universitario per l'insegnamento Linguaggi di Programmazione,
anno accademico 2020/2021.

Università degli Studi di Milano-Bicocca, laurea Triennale in Informatica.

Componenti del Gruppo di Progetto:
844760 Davide Zangari
845273 Nicolas Guida
845080 Mattia Campioni

Progetto: Costruire una libreria in Common Lisp e in Prolog che implementi un
 	  parser per il formato (semplificato) JSON.

Funzioni principali:
- json-parse (JSON) : effettua il parsing della stringa JSON scritta il
		      linguaggio json e restituisce l'oggetto json in sintassi Lisp.

- json-acces (JSON fileds) : prende come argomenti un oggett JSON in sintassi 
			     Lisp e una serie di campi. I campi sono formati da una chiave
			     e da un numero opzionale di indici numerici. Restituisce il valore
			     associato alla chiave se presente nell'oggetto JSON. Se vengono passati
			     anche gli indici restituisce il valore corrispondente che si trova
			     nell'array individuato dalla chiave key.

- json read (filename) : legge una stringa JSON scritta in sintassi json che si
			 trova all'interno di un file e ne effettua il parsing.

- json-dump (JSON filename) : accetta un oggetto JSON in sintassi Lisp e scrive
			      su file la stringa corrispondente in sintassi json.
