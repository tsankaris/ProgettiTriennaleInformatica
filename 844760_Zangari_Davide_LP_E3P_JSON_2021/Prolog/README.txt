Progetto universitario per l'insegnamento Linguaggi di Programmazione,
anno accademico 2020/2021.

Università degli Studi di Milano-Bicocca, laurea Triennale in Informatica.

Componenti del Gruppo di Progetto:
844760 Davide Zangari
845273 Nicolas Guida
845080 Mattia Campioni

Progetto: Costruire una libreria in Common Lisp e in Prolog che implementi un
 	  parser per il formato (semplificato) JSON.

Predicati principali:
- json_parse(JSONString, Object) : questo predicato risulta vero se si può
                                   effettuare il parsing di una JSONString in
				   sintassi json e unifica Object con l'oggetto
                                   json scritto in sintassi Prolog.

- json_access(JSON_obj, Fields, Result) : questo predicato risulta vero quando
                                          è possibile recuperare da una
                                          JSON-obj il valore associato al campo
                                          Fields. Fields può essere composto da
                                          un numero N>=0 corrispondente a un
                                          indice di un array json. Il risultato
                                          della ricerca viene unificato con
                                          Result.

- json_read(FileName, JSON) : legge una stringa in sintassi json da un file e 
                              questo predicato risulta vero se la stringa
                              rappresenta un oggetto JSON. Unifica JSON con
                              l'oggetto JSON in sintassi Prolog.

- json_dump(JSON, FileName) : scrive l'oggetto JSON su un file FileName in
                              sintassi json.