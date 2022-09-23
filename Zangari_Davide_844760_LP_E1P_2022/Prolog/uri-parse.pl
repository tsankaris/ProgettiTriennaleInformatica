
%% 866194 Gaia Iori, 844760 Davide Zangari, 869012 Andrea Cappone

%% Corso di Linguaggi di Programmazione, anno 2021/2022
%% Progetto: parsing di stringhe URI

% uri_parse/2 - ritorna true se la stringa
%  URIString corrisponde alla lista URI,
%  con URI=uri(Scheme, Userinfo, Host, Port, Path, Query, Fragm.)
uri_parse(URIString, URI) :-
    string_chars(URIString, K),
    phrase(uri(S, U, HI, PR, PH, Q, F), K),
    default_port(PR, PRD),
    URI = uri(S, U, HI, PRD, PH, Q, F).

% uri_display/1 stampa in modo formattato su output la lista URI,
%  con URI=uri(Scheme, Userinfo, Host, Port, Path, Query, Fragm.)
uri_display(URI) :-
    uriString(URI, Str),
    write(Str).

% uri_display/2 - stampa in modo formattato su Stream la lista URI
uri_display(URI, Stream) :-
    uriString(URI, Str),
    write(Stream, Str),
    close(Stream).

% uriString/2 - true se la stringa Str corrisponde alla versione
%  formattata della lista uri(), senza fare parsing
uriString(uri(S, U, HI, PR, PH, Q, F), Str) :-
    check_vuota(Sx, S), check_vuota(Ux, U),
    check_vuota(HIx, HI), check_vuota(PRx, PR),
    check_vuota(PHx, PH), check_vuota(Qx, Q),
    check_vuota(Fx, F),
    format(string(Str),
	   'Scheme:~a,
	   Userinfo:~a,
	   Host:~a,
	   Port:~a,
	   Path:~a,
	   Query:~a,
	   Fragment:~a',
	   [Sx, Ux, HIx, PRx, PHx, Qx, Fx]).

% check_vuota/2 - per convertire la stringa vuota nella stringa '[]'
check_vuota('[]', []) :- !.
check_vuota(N, N).

% uri/7 - corrispondente a URI1, true se Scheme,
%  Authority (Userinfo, Host, Port) e "post-Authority" (Path,
%  Query, Fragment) rispettano le regole di URI1.
uri(S, U, HI, PR, PH, Q, F) -->
    scheme(S), [':'], { controllaS(S) },
    auth(U, HI, PR), postauth(PH, Q, F), !.

uri(S, U, HI, PR, [], [], []) -->
    scheme(S), [':'], { controllaS(S) }, auth(U, HI, PR), !.

uri(S, [], [], [], PH, Q, F) -->
    scheme(S), [':'], { controllaS(S) }, postauth(PH, Q, F), !.

uri(S, [], [], [], [], [], []) --> scheme(S), [':'].

% uri/7 - corrispondente a URI2, true se i blocchi passati
%  rispettano le regole di URI2 e le regole dello schema in
%  particolare
uri(S, U, H, PR, PH, Q, F) -->
    scheme(S), [':'], schemesyn(S, U, H, PR, PH, Q, F), !.
uri(S, U, H, PR, PH, Q, F) -->
    scheme(S), [':'], { S=zos }, !, zos(U, H, PR, PH, Q, F) .

% schemesyn/7 - per indirizzare gli schemi speciali alla
%  loro regola speciale
schemesyn(S, U, H,[], [], [], []) --> { S=mailto }, !, mailto(U, H).
schemesyn(S, [], I, [], [], [], []) --> { S=news }, !, news(I).
schemesyn(S, U, [], [], [], [], []) --> { S=tel }, !, telfax(U).
schemesyn(S, U, [], [], [], [], []) --> { S=fax }, !, telfax(U).

% mailto/2 - costruisce il blocco mailto con Userinfo (U) e Host (H)
mailto(U, H) --> userinfo(U), ['@'], host_ip(H).
mailto(U, []) --> userinfo(U).

% news/1 - costruisce il blocco news con Host (H)
news(N) --> host_ip(N).

% telfax/1 - costruisce i blocchi tel e fax con Userinfo (U)
telfax(T) --> userinfo(T).

% auth/3 - costruisce il blocco authority con Userinfo (U),
%  Host (HI) e Port (PR)
auth(U, HI, PR) -->
    ['/'], ['/'], userinfo(U), ['@'],
    host_ip(HI), [':'], port(PR), !.

auth([], HI, PR) -->
    ['/'], ['/'], host_ip(HI), [':'], port(PR), !.

auth(U, HI, []) -->
    ['/'], ['/'], userinfo(U), ['@'], host_ip(HI), !.

auth([], HI, []) -->
    ['/'], ['/'], host_ip(HI), !.

% postauth/3 - costruisce il blocco post-authority con Path (PH),
%  Query (Q) e Fragment (F)
postauth(PH, Q, F) --> ['/'], path(PH), ['?'], query(Q), ['#'], fragment(F).

postauth(PH, Q, []) --> ['/'], path(PH), ['?'], query(Q), !.

postauth(PH, [], F) --> ['/'], path(PH), ['#'], fragment(F), !.

postauth(PH, [], []) --> ['/'], path(PH), !.

postauth([], Q, F) --> ['/'], ['?'], query(Q), ['#'], fragment(F), !.

postauth([], Q, []) --> ['/'], ['?'], query(Q), !.

postauth([], [], F) --> ['/'], ['#'], fragment(F), !.

postauth([], [], []) --> ['/'].

% postauth_zos/3 - costruisce il blocco post-authority relativo
%  allo schema zos, con Path relativo allo zos (PH),
%  Query (Q) e Fragment (F)
postauth_zos(PH, Q, F) -->
    ['/'], path_zos(PH), ['?'], query(Q), ['#'], fragment(F).

postauth_zos(PH, Q, []) --> ['/'], path_zos(PH), ['?'], query(Q), !.

postauth_zos(PH, [], F) --> ['/'], path_zos(PH), ['#'], fragment(F), !.

postauth_zos(PH, [], []) --> ['/'], path_zos(PH), !.

% zos/6 - costruisce l'uri relativa allo schema speciale zos,
%  con authority classica e post-authority speciale per lo schema
zos(U, H, PR, PH, Q, F) --> auth(U, H, PR), postauth_zos(PH, Q, F), !.

zos(U, H, PR, [], [], []) --> auth(U, H, PR), !.

zos([], [], [], PH, Q, F) --> postauth_zos(PH, Q, F), !.

% path_zos/1 - costruisce il blocco Path di un uri con schema zos
path_zos(P) -->
    id44_completo(C),
    ['('], id8_completo(K), [')'],
    { append([C, ['('], K, [')']] , Pl), atom_chars(P, Pl) }, !.
path_zos(P) --> id44_completo(C), { atom_chars(P, C) }.

% id44_completo/1 - costruisce l'intero id44 con controlli su
%  lunghezza e consistenza delle regole sintattiche
id44_completo(I) --> id44_alpha(I), { length(I, L), L=<44 }.

% id44_punto/1 - controlla la consistenza dei pezzi alfanumerici di id44
%  costruiti dividendoli in occorrenza del carattere '.'
id44_punto(I) -->
    id44(C), ['.'], id44_punto(Cs), { append([C, ['.'], Cs], I) }, !.
id44_punto(I) --> id44(I). %, { I=C }.

% id44_alpha/1 - controlla che il primo carattere sia alfabetico,
%  cosi che rispetti le regole sintattiche speciali, e possa proseguire
%  con caratteri alfanumerici e punti
id44_alpha(I) -->
    [C], { char_type(C, alpha) }, id44_punto(Cs), { I = [C | Cs] }, !.
id44_alpha(I) --> [C], { char_type(C, alpha), I = [C] }.

% id44/1 - controlla che i caratteri che lo compongono siano esclusivamente
%  di tipo alfanumerico, secondo le regole sintattiche speciali.
id44(I) --> [C], { char_type(C, alnum) }, id44(Cs), { I = [C | Cs] }, !.
id44(I) --> [C], { char_type(C, alnum), I = [C] }.

% id8_completo/1 - coastruisce l'intero id8 con controlli su lunghezza
%  e consistenza delle regole sintattiche
id8_completo(I) --> id8_alpha(I), { length(I, L), L=<8 }.

% id8_alpha/1 - controlla che il primo carattere di id8 sia alfabetico
%  seguito poi eventualmente da caratteri alfanumerici
id8_alpha(I) --> [C], { char_type(C, alpha) }, id8(Cs), { I = [C | Cs] }, !.
id8_alpha(I) --> [C], { char_type(C, alpha), I = [C] }.

% id8/1 - controlla che i caratteri che lo compongono siano esclusivamente
%  alfanumerici, secondo le regole sintatticge speciali
id8(I) --> [C], { char_type(C, alnum) }, id8(Cs), { I = [C | Cs] }, !.
id8(I) --> [C], { char_type(C, alnum), I = [C] }.

% scheme/1 - costruisce il blocco Scheme relativo a URI1 e URI2
scheme(S) --> id(C), { atom_chars(S, C) }.

% userinfo/1 - costruisce il blocco Userinfo relativo a URI1 e a
%  alcuni schemi speciali di URI2
userinfo(U) --> id(C), { atom_chars(U, C) }.

% host_ip/1 - costruisce il blocco Host, composto da un IP oppure da
%  uno o piu' host con le loro regole sintattiche
host_ip(H) --> ip(C), { atom_chars(H, C) }, !.
host_ip(H) --> hosts(C), { atom_chars(H, C) }.

% hosts/1 - controlla la sintassi della sequenza di uno o piu' hosts
hosts(H) --> idhost(C), ['.'], hosts(Cs), { append([C, ['.'], Cs] , H) }, !.
hosts(H) --> idhost(C), { H = C }.

% ip/1 - controlla la sintassi dell'ip, del tipo NNN.NNN.NNN.NNN
ip(B) -->
    blocco_ip(B1), ['.'],
    blocco_ip(B2), ['.'],
    blocco_ip(B3), ['.'],
    blocco_ip(B4),
    { append([B1, ['.'], B2, ['.'], B3, ['.'], B4], B) }.

% blocco_ip/1 - costruisce una terna NNN che compone l'ip,
%  con controlli sulla validita' del numero (0<NNN<256)
blocco_ip(B) -->
    digit(D1), digit(D2), digit(D3),
    { append([D1, D2, D3], Bl), number_chars(Bx, Bl),
      Bx =< 255, Bx >= 0, B=Bx }.

% port/1 - costruisce il blocco Port, convertendo la lista in numero
port(P) --> digits(D), { number_chars(P, D) }.

% default_port/2 - per restituire come porta di default 80 nel caso
%  in cui nello schema uri() non fosse passata una porta
default_port([], 80) :- !.
default_port(N, N).

% path/1 - costruisce il blocco Path relativo a URI1
path(P) --> paths(Cs), { atom_chars(P, Cs) }.

% paths/1 - controlla la sintassi del blocco path ricorsivamente
paths(P) --> id(C), ['/'], paths(Cs), { append([C, ['/'], Cs], P) }, !.
paths(P) --> id(C), { P=C }.

% query/1 - costruisce il blocco Query relativo a URI1 e schema zos
query(Q) --> queries(C), { atom_chars(Q, C) }.

% queries/1 - controlla la sintassi del blocco query ricorsivamente
queries(Q) --> [C], { controlla3(C) }, queries(Cs), { Q = [C | Cs] }, !.
queries(Q) --> [C], { controlla3(C), Q=[C] }.

% fragment/1 - costruisce il blocco Fragment relativo a URI1 e schema zos
fragment(F) --> fragments(C), { atom_chars(F, C) }.

% fragments/1 - controlla la sintassi del blocco fragment ricorsivamente
fragments(F) --> [C], fragments(Cs), { F = [C | Cs]}, !.
fragments(F) --> [C], { F=[C] }.

% digits/1 - usato nel blocco Port, controlla che la lista passata
%  contenga cifre (e rappresenti quindi un numero)
digits(D) --> [N], { char_type(N, digit) }, digits(Ns), {D = [N | Ns] }, !.
digits(D) --> [N], { char_type(N, digit), D = [N] }.

% digit/1 - controllo che il carattere passato sia una singola cifra,
%  usato nella costruzione di blocco_ip
digit(D) --> [N], { char_type(N, digit), D = [N] }.

% id/1 - controlla la sintassi dei pezzi di identificatore
id(I) --> [C], { controlla1(C) }, id(Cs), {I = [C | Cs] }, !.
id(I) --> [C], { controlla1(C), I=[C] }.

% idhost/1 - controlla la sintassi dei pezzi di identificatore
%  relativi agli host
idhost(I) --> [C], { controlla2(C) }, idhost(Cs), { I = [C | Cs] }, !.
idhost(I) --> [C], { controlla2(C), I=[C] }.

% controllaS/1 - controllo che lo schema relativo a URI1 non sia
%  uno degli schemi particolari
controllaS(S) :-
    S \== mailto,
    S \== news,
    S \== tel,
    S \== fax,
    S \== zos.

% controlla1/1 - controlla che il carattere rispetti le regole di
%  identificatore
controlla1(C) :-
    C \== '/',
    C \== '?',
    C \== '#',
    C \== '@',
    C \== ':'.

% controlla1/1 - controlla che il carattere rispetti le regole di
%  identificatore relativo agli host
controlla2(C) :-
    C \== '/',
    C \== '?',
    C \== '#',
    C \== '@',
    C \== ':',
    C \== '.'.

% controlla1/1 - controlla che il carattere rispetti le regole di
%  query, cioe' che non contenga il simbolo '#'
controlla3(C) :- C \== '#'.
