
%% LP_E1P_2022
%% PROGETTO: PARSING DI STRINGHE URI

Membri del gruppo:
866194 Gaia Iori
844760 Davide Zangari
869012 Andrea Cappone



%% PREDICATI PRINCIPALI:
 1) uri_parse(URIString, URI)
    Accetta una stringa URIString ("http://disco.unimib.it") e un
    termine URI composto nel seguente modo:
    URI=uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment)
    e ritorna TRUE se URIString puo' essere scomposta in URI

2) uri_display(URI)
   Accetta il termine composto URI, con
   URI=uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment)
   e stampa in modo formattato le componenti del termine uri.
   Esempio:
   ?- uri_display(uri(http, [], 'disco.unimib.it', 80, [], [], [])).
           Scheme:http,
           Userinfo:[],
           Host:disco.unimib.it,
           Port:80,
           Path:[], 
           Query:[],
           Fragment:[]

3) uri_display(URI, Stream)
   Accetta il termine composto URI, con
   URI=uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment),
   e uno Stream, che verra' chiuso a fine funzione;
   stampa in modo formattato le componenti del termine uri sullo Stream.



%% AMBIENTE PROLOG USATO E TECNOLOGIE SFRUTTATE:
Abbiamo costruito il nostro progetto appoggiandoci all'ambiente
SWI-Prolog. Vista la possibilita' di utilizzare le DCG per l'implementazione
del problema, abbiamo deciso di sfruttarle.
In particolare per i controlli sulla struttura dei termini uri().



%% CHIARIMENTI SULLA STRUTTURA DI URI
In caso di porta mancante, abbiamo implementato un predicato (default_port/2)
che imposta automaticamente la stessa a 80. Questo viene fatto
indipendentemente dallo schema, quindi si ritrova sempre. 

Inoltre, abbiamo supposto che uri del tipo URI1 (senza schemi particolari)
potessero presentare path vuoto, mentre uri con schema zos fossero obbligati,
in presenza dello slash (che segnala l'inizio del path), a presentare
un path non vuoto.
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

Infine, i predicati relativi all'ip presentano i controlli sulla
validita' del numero, e quindi non accettano ip non validi, o composti
da numeri che non abbiano 3 cifre. D'altra parte, la funzione host va ad
accettare ogni carattere (anche numerico), quindi accettera' al posto
del blocco ip cio' che non dovrebbe essere valido.



%% FUNZIONI PARTICOLARI USATE:
Per poter elaborare le liste di caratteri e le conversioni in stringhe,
e per poter implementare le dcg correttamente, abbiamo usato una serie
di funzioni particolari, testate sull'ambiente SWI-Prolog,
di seguito elencate:
1) string_chars/2: per convertire le liste di caratteri in stringhe
2) phrase/2: per urilizzare le dcg
3) format/3: per stampare in modo formattato il termine uri()
4) atom_chars/2: per atomizzare una lista di caratteri
5) length/2: per i controlli su lunghezze di id44 e id8
6) char_type/2: per i controlli sui singoli caratteri
7) number_chars/2: per convertire una lista di caratteri in un numero



%% ESEMPI DI FUNZIONAMENTO:

?- uri_parse("http://disco.unimib.it", URI).
URI = uri(http, [], 'disco.unimib.it', 80, [], [], []).

?- uri_parse("mailto:John.Doe@example.com", URI).
URI = uri(mailto, 'John.Doe', 'example.com', 80, [], [], []).

?- uri_parse("http://www.ietf.org/rfc/rfc2396.txt", URI).
URI = uri(http, [], 'www.ietf.org', 80, 'rfc/rfc2396.txt', [], []).

?- uri_parse("http://disco.unimib.it", uri(_, _, Host, _, _, _, _)).
Host = 'disco.unimib.it'.

?- uri_parse("http://disco.unimib.it", uri(https, _, _, _, _, _, _)).
false.
