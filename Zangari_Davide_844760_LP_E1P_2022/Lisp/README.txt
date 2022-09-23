
%% LP_E1P_2022
%% PROGETTO: PARSING DI STRINGHE URI

Membri del gruppo:
866194 Gaia Iori
844760 Davide Zangari
869012 Andrea Cappone



%% FUNZIONI PRINCIPALI:
1) (defun uri-parse (uri-string))
   Accetta una stringa uri-string ("http://disco.unimib.it")
   e ritorna una lista uri-structure cosi' composta
   (scheme, userinfo, host, port, path, query, fragment)

2) (defun uri-display (uri-structure &optional (stream t)))
   Accetta la lista uri-structure e stampa in modo formattato le
   componenti del termine uri sullo stream opzionale.
   Esempio:
   (uri-display '("http" nil "disco.unimib.it" 80 nil nil nil))
   Scheme:	"http"
   Userinfo:	NIL
   Host:	"disco.unimib.it"
   Port:	80
   Path:	NIL
   Query:	NIL
   Fragment:	NIL
   T

3) (defun URI (u))
   Accetta una stringa u e verifica se fa parte della tipologia URI1 o
   URI2. Restituisce la uri-structure corrispondente.



%% AMBIENTE LISP USATO E TECNOLOGIE SFRUTTATE:
Abbiamo costruito il nostro progetto appoggiandoci all'ambiente LispWorks.



%% CHIARIMENTI SULLA STRUTTURA DI URI
In caso di porta mancante, la stessa viene impostata automaticamente a 80.
Questo viene fatto indipendentemente dallo schema, quindi si ritrova sempre.

Inoltre, abbiamo supposto che uri del tipo URI1 (senza schemi particolari)
potessero presentare un path vuoto, mentre uri con schema zos fossero
obbligati, in presenza dello slash (che segnala l'inizio del path), a
presentare un path non vuoto.
Sempre riguardo zos, sono accettati path contenenti blocchi id44 composti
con piu' punti di seguito.

Per tutti gli scheme speciali (mailto, news, tel, fax, zos) abbiamo
supposto che devono essere necessariamente scritti con sole lettere
minuscole. Se viene trovata anche solo 1 lettera maiuscola, lo scheme
non e' accettato come speciale, ma verifica la sintassi di URI1.

Visti i controlli fatti su blocchi del tipo identificatore e
identificatore-host, vengono accettati scheme che iniziano con caratteri
non esclusivamente alfabetici, e lo stesso per tutti gli altri pezzi che
sfruttano il blocco identificatore o identificatore-host.
Questi blocchi accettano anche stringhe contenenti solo caratteri
speciali (purche' non facciano parte dei caratteri proibiti). 
Ad esempio "dgs; ;ske;" o "; ;;" vengono accettati come userinfo, host,
path, query, fragment.

Infine, le funzioni relative all'ip presentano i controlli sulla
validita' del numero, e quindi non accettano ip non validi, o composti
da numeri che non abbiano 3 cifre. D'altra parte, la funzione host va ad
accettare ogni carattere (anche numerico), quindi accettera' al posto
del blocco ip cio' che non dovrebbe essere valido.



%% FUNZIONI PARTICOLARI USATE:
Per poter effettuare le conversioni tra stringhe e liste, abbiamo usato
una serie di funzioni particolari, di seguito elencate:
1) (defun stl (s)): tramite la funzione coerce convertiamo la stringa
   s in una lista
2) (defun lts (l)): tramite la funzione format convertiamo la lista
   l in una stringa

Per poter operare meglio sui diversi blocchi che compongono un URI,
abbiamo creato delle nostre funzioni particolari, di seguito elencate:
1) (defun llts (l)): converte una lista di liste in una semplice stringa
2) (defun stll (s c)): converte una stringa in una lista di liste,
   usando il carattere c come separatore
3) (defun l-list (x l s c)): modifica una lista x creando sottoliste
   separate dal carattere c. Le liste l ed s sono parametri d'appoggio
4) (defun stll-2 (s c1 c2)): converte una stringa in una lista di liste,
   usando i caratteri c1 e c2 come separatori
5) (defun l-list-2 (x l s c1 c2)): modifica una lista x creando sottoliste
   separate dai caratteri c1 e c2. Le liste l ed s sono parametri d'appoggio
6) (defun lltl (x)): appiattisce una lista, convertendo una lista di liste
   in una lista semplice



%% ESEMPI DI FUNZIONAMENTO:

uri-parse "http://disco.unimib.it"
("http" NIL "disco.unimib.it" 80 NIL NIL NIL)

uri-parse "mailto:John.Doe@example.com"
("mailto" "John.Doe" "example.com" 80 NIL NIL NIL)

uri-parse "http://www.ietf.org/rfc/rfc2396.txt"
("http" NIL "www.ietf.org" 80 "rfc/rfc2396.txt" NIL NIL)

uri-host (uri-parse "http://disco.unimib.it")
"disco.unimib.it"

uri-scheme (uri-parse "http://disco.unimib.it")
"http"
