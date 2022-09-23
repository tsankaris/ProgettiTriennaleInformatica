%%%% 844760 Davide Zangari
%%%% 845273 Nicolas Guida
%%%% 845080 Mattia Campioni


%%%% -*- Mode: Prolog -*-
%%%% json-parse.pl

%%% json_parse(JSONString, Object).

json_parse(JSONString, Object) :-
   string_codes(JSONString, JSONUnicode),
   rimuovi_spazi(JSONUnicode, JSONUnicode1),
   json_object(JSONUnicode1, CharTemp, [], Object),
   rimuovi_spazi(CharTemp, CharFinal),
   vuoto(CharFinal).

json_parse(JSONString, Object) :-
   string_codes(JSONString, JSONUnicode),
   rimuovi_spazi(JSONUnicode, JSONUnicode1),
   json_array(JSONUnicode1, CharTemp, [], Object),
   rimuovi_spazi(CharTemp, CharFinal),
   vuoto(CharFinal).

%%char_code(Atom, Code). -integer denoting the character

controlla_spazi(X) :-
   char_code(' ', Y),
   X = Y.

controlla_spazi(X) :-
   char_code('\n', Y),
   X = Y.

controlla_spazi(X) :-
   char_code('\t', Y),
   X = Y.

rimuovi_spazi([], []).

rimuovi_spazi([X | Xs], Ys) :-
    controlla_spazi(X),
    rimuovi_spazi(Xs, Ys).

rimuovi_spazi([X | Xs], Ys) :-
    Ys = [X | Xs],
    !.

%%string_codes converte una stringa in una lista di char codes
primo_carattere(In, [X | Xs], Out) :-
    string_codes(In, [Y | _]),
    Y = X,
    Out = Xs.

%%json_object/4
json_object(CharIn, CharOut, ObjectIn, json_obj(ObjectOut)) :-
    primo_carattere("{",CharIn, CharTemp1),
    rimuovi_spazi(CharTemp1, CharTemp2),
    primo_carattere("}", CharTemp2, CharOut),
    ObjectIn = ObjectOut.

json_object(CharIn, CharOut, ObjectIn, json_obj(ObjectOut)) :-
    primo_carattere("{", CharIn, CharTemp1),
    rimuovi_spazi(CharTemp1, CharTemp2),
    json_members(CharTemp2, CharTemp3, ObjectIn, ObjectOut),
    rimuovi_spazi(CharTemp3, CharTemp4),
    primo_carattere("}", CharTemp4, CharOut).

json_members(CharIn, CharOut, ObjectIn, ObjectOut) :-
    json_pair(CharIn, CharTemp2, ObjectIn, ObjectTemp),
    rimuovi_spazi(CharTemp2, CharTemp3),
    primo_carattere(",", CharTemp3, CharTemp4),
    rimuovi_spazi(CharTemp4, CharTemp5),
    json_members(CharTemp5, CharOut, ObjectTemp, ObjectOut).

json_members(CharIn, CharOut, ObjectIn, ObjectOut) :-
    json_pair(CharIn, CharOut, ObjectIn, ObjectOut).

json_pair(CharIn, CharOut, ObjectIn, ObjectOut) :-
    json_string(CharIn, CharTemp1, Attributo),
    rimuovi_spazi(CharTemp1, CharTemp2),
    primo_carattere(":", CharTemp2, CharTemp3),
    rimuovi_spazi(CharTemp3, CharTemp4),
    json_value(CharTemp4, CharOut, Valore),
    append(ObjectIn, [(Attributo, Valore)], ObjectOut).

json_string(CharIn, CharOut, Stringa) :-
   primo_carattere("\'", CharIn, CharTemp1),
   crea_stringa_sq(CharTemp1, CharTemp2, StringOut),
   primo_carattere("\'", CharTemp2, CharOut),
   string_codes(Stringa, StringOut).

json_string(CharIn, CharOut, Stringa) :-
   primo_carattere("\"", CharIn, CharTemp1),
   crea_stringa_dq(CharTemp1, CharTemp2, StringOut),
   primo_carattere("\"", CharTemp2, CharOut),
   string_codes(Stringa, StringOut).
%%% fail nel caso in una stringa sq ci sia un dq
crea_stringa_sq([X | _], _, _) :-
   string_codes("\"", [H | _]),
   X = H,
   !,
   fail.

crea_stringa_sq([X | Xs], [X | Xs], []) :-
   string_codes("\'", [H | _]),
   X = H,
   !.
crea_stringa_sq([X | Xs], Zs, [X | Ys]) :-
   crea_stringa_sq(Xs, Zs, Ys).

%%% fail nel caso in una stringa dq ci sia un sq
crea_stringa_dq([X | _], _, _) :-
   string_codes("\'", [H | _]),
   X = H,
   !,
   fail.

crea_stringa_dq([X |  Xs], [X | Xs], []) :-
   string_codes("\"", [H | _]),
   X = H,
   !.
crea_stringa_dq([X | Xs], Zs, [X | Ys]) :-
   crea_stringa_dq(Xs, Zs, Ys).

json_value(CharIn, CharOut, Valore) :-
   json_string(CharIn, CharOut, Valore).

json_value(CharIn, CharOut, Valore) :-
   json_number(CharIn, CharOut, Valore).

json_value(CharIn, CharOut, Valore) :-
   json_object(CharIn, CharOut, [], Valore).

json_value(CharIn, CharOut, Valore) :-
   json_array(CharIn, CharOut, [], Valore).

json_number(CharIn, CharOut, Numero) :-
   primo_carattere("+", CharIn, CharTemp1),
   char_code('+', Plus),
   crea_numero(CharTemp1, CharTemp2, NumTemp1),
   non_vuoto(NumTemp1),
   append([Plus], NumTemp1, NumTemp2),
   primo_carattere(".", CharTemp2, CharTemp3),
   char_code('.', Punto),
   append(NumTemp2, [Punto], NumTemp3),
   crea_numero(CharTemp3, CharOut, NumTemp4),
   non_vuoto(NumTemp4),
   append(NumTemp3, NumTemp4, NumUnicode),
   number_codes(Numero, NumUnicode).

json_number(CharIn, CharOut, Numero) :-
   primo_carattere("+", CharIn, CharTemp1),
   char_code('+', Plus),
   crea_numero(CharTemp1, CharOut, NumTemp1),
   non_vuoto(NumTemp1),
   append([Plus], NumTemp1, NumUnicode),
   number_codes(Numero, NumUnicode).

json_number(CharIn, CharOut, Numero) :-
   primo_carattere("-", CharIn, CharTemp1),
   char_code('-', Minus),
   crea_numero(CharTemp1, CharTemp2, NumTemp1),
   non_vuoto(NumTemp1),
   append([Minus], NumTemp1, NumTemp2),
   primo_carattere(".", CharTemp2, CharTemp3),
   char_code('.', Punto),
   append(NumTemp2, [Punto], NumTemp3),
   crea_numero(CharTemp3, CharOut, NumTemp4),
   non_vuoto(NumTemp4),
   append(NumTemp3, NumTemp4, NumUnicode),
   number_codes(Numero, NumUnicode).

json_number(CharIn, CharOut, Numero) :-
   primo_carattere("-", CharIn, CharTemp1),
   char_code('-', Minus),
   crea_numero(CharTemp1, CharOut, NumTemp1),
   non_vuoto(NumTemp1),
   append([Minus], NumTemp1, NumUnicode),
   number_codes(Numero, NumUnicode).

json_number(CharIn, CharOut, Numero) :-
   crea_numero(CharIn, CharTemp1, NumTemp1),
   primo_carattere(".", CharTemp1, CharTemp2),
   char_code('.', Punto),
   append(NumTemp1, [Punto], NumTemp2),
   crea_numero(CharTemp2, CharOut, NumTemp3),
   append(NumTemp2, NumTemp3, NumUnicode),
   non_vuoto(NumUnicode),
   number_codes(Numero, NumUnicode).

json_number(CharIn, CharOut, Numero) :-
   crea_numero(CharIn, CharOut, NumUnicode),
   non_vuoto(NumUnicode),
   number_codes(Numero, NumUnicode).

%%%% crea_numero/3
crea_numero([X | Xs], [X | Xs], []) :-
   X < 48,
   !.

crea_numero([X | Xs], [X | Xs], []) :-
   X > 57,
   !.

crea_numero([X | Xs], Zs, [X | Ys]) :-
   crea_numero(Xs, Zs, Ys).

%%%% non_vuoto/1
non_vuoto(Lista) :-
   Lista \= [].

vuoto(Lista) :-
   Lista = [].

json_array(CharIn, CharOut, ObjectIn, json_array(ObjectOut)) :-
    primo_carattere("[", CharIn, CharTemp1),
    rimuovi_spazi(CharTemp1, CharTemp2),
    primo_carattere("]",CharTemp2, CharOut),
    ObjectIn = ObjectOut.

json_array(CharIn, CharOut, ObjectIn, json_array(ObjectOut)) :-
    primo_carattere("[", CharIn, CharTemp1),
    rimuovi_spazi(CharTemp1, CharTemp2),
    json_elements(CharTemp2, CharTemp3, ObjectIn, ObjectOut),
    rimuovi_spazi(CharTemp3, CharTemp4),
    primo_carattere("]", CharTemp4, CharOut).

json_elements(CharIn, CharOut, ObjectIn, ObjectOut) :-
    json_value(CharIn, CharTemp1, Valore),
    rimuovi_spazi(CharTemp1, CharTemp2),
    primo_carattere(",", CharTemp2, CharTemp3),
    rimuovi_spazi(CharTemp3, CharTemp4),
    append(ObjectIn, [Valore], ObjectTemp),
    json_elements(CharTemp4, CharOut, ObjectTemp, ObjectOut).

json_elements(CharIn, CharOut, ObjectIn, ObjectOut) :-
    json_value(CharIn, CharOut, ObjectTemp),
    append(ObjectIn, [ObjectTemp], ObjectOut).

%%%json_access(JSON_obj, Fields, Result).
json_access(X, void, X) :- !.

json_access(_, [], _) :-
   !,
   fail.

json_access(json_obj(), _, _) :-
   !,
   fail.

json_access(json_array(), _, _) :-
   !,
   fail.

json_access(JSON_obj, X, Result) :-
   estrai_coppie(JSON_obj, X, Result).

json_access(JSON_obj, [X], Result) :-
   estrai_coppie(JSON_obj, X, Result).

json_access(JSON_obj, [X | Xs], Result) :-
   estrai_coppie(JSON_obj, X, Out),
   json_access(Out, Xs, Result).

estrai_coppie(JSON_obj, Fields, Result) :-
   json_obj([X | Xs]) = JSON_obj,
   estrai_valore([X | Xs], Fields, Result).

estrai_coppie(JSON_obj, Fields, Result) :-
   json_array([X | Xs]) = JSON_obj,
   estrai_posizione([X | Xs], Fields, Result).

estrai_valore([], _, _) :-
   fail.

estrai_valore([(X, Y) | _], Fields, Result) :-
   string(Fields),
   X = Fields,
   Result = Y.

estrai_valore([_ | Xs], Fields, Result) :-
   string(Fields),
   estrai_valore(Xs, Fields, Result).

estrai_posizione([], [_], _) :-
   fail.

estrai_posizione([X | _], Fields, Result) :-
   number(Fields),
   Fields = 0,
   Result = X.

estrai_posizione([_ | Xs], Fields, Result) :-
   number(Fields),
   Y is Fields-1,
   estrai_posizione(Xs, Y, Result).

%%% json_read e json_dump
json_read(FileName, _) :-
   FileName = null,
   fail,
   !.

json_read(FileName, _) :-
exists_file(FileName) = false,
   fail,
   !.

json_read(FileName, JSON) :-
   exists_file(FileName),
   open(FileName, read, In),
   read_string(In, _, String),
   close(In),
   String = "",
   JSON = null,
   !.

json_read(FileName, JSON) :-
   exists_file(FileName),
   open(FileName, read, In),
   read_string(In, _, String),
   close(In),
   json_parse(String, JSON).

json_dump(JSON, FileName) :-
   to_string(JSON, "", Testo),
   scrivi_su_file(FileName, Testo).

scrivi_su_file(File, Text) :-
   open(File, write, Stream),
   write(Stream, Text),
   close(Stream).

to_string(json_obj([X | Xs]), StringIn, StringOut) :-
   string_concat(StringIn, "{", StringTemp1),
   to_string([X | Xs], StringTemp1, StringTemp2),
   string_concat(StringTemp2, "}", StringOut).

to_string(json_array([X | Xs]), StringIn, StringOut) :-
   string_concat(StringIn, "[", StringTemp1),
   to_string_array([X | Xs], StringTemp1, StringTemp2),
   string_concat(StringTemp2, "]", StringOut).

to_string(json_obj([]), StringIn, StringOut) :-
   string_concat(StringIn, "{", StringTemp1),
   string_concat(StringTemp1, "}", StringOut).

to_string(json_array([]), StringIn, StringOut) :-
   string_concat(StringIn, "[", StringTemp1),
   string_concat(StringTemp1, "]", StringOut).

to_string([], StringIn, StringOut) :-
   StringIn = StringOut,
   !.

to_string([(X, Y)], StringIn, StringOut) :-
   to_string(X, StringIn, StringTemp1),
   string_concat(StringTemp1, " : ", StringTemp2),
   to_string(Y, StringTemp2, StringOut),
   !.

to_string([(X, Y) | Zs], StringIn, StringOut) :-
   to_string(X, StringIn, StringTemp1),
   string_concat(StringTemp1, " : ", StringTemp2),
   to_string(Y, StringTemp2, StringTemp3),
   string_concat(StringTemp3, ", ", StringTemp4),
   to_string(Zs, StringTemp4, StringOut).

to_string(X, StringIn, StringOut) :-
   string(X),
   !,
   string_concat(StringIn, '"', StringTemp1),
   string_concat(StringTemp1, X, StringTemp2),
   string_concat(StringTemp2, '"', StringOut).

to_string(X, StringIn, StringOut) :-
   number(X),
   !,
   number_string(X, NumString),
   string_concat(StringIn, NumString, StringOut).

to_string_array([X], StringIn, StringOut) :-
   to_string(X, StringIn, StringOut),
   !.

to_string_array([X | Xs], StringIn, StringOut) :-
   Xs \= [],
   to_string(X, StringIn, StringTemp1),
   string_concat(StringTemp1, " , ", StringTemp2),
   to_string_array(Xs, StringTemp2, StringOut).













%%%% end of file -- json-parse.pl
