% search (Elem , List )

search(X , cons (X , _)).
search(X , cons (_ , Xs )) :- search (X , Xs ).

element(X, [X|_]).
element(X, [_|T]) :- element(X,T).

% search2 (Elem , List )
% looks for two consecutive occurrences of Elem

search2(X , cons(X , cons(X , _))).
search2(X , cons(_ , T )) :- search2(X , T).

% search_two (Elem , List )
% looks for two occurrences of Elem with any element in between !

search_two(X, cons(X, cons(_,cons(X, _)))).
search_two(X, cons(_,T)):- search_two(X,T).

% search_anytwo (Elem , List )
% looks for any Elem that occurs two times , anywhere
search_anytwo(X,cons(X, T)):- search(X,T).
search_anytwo(X,cons(_,T)):- search_anytwo(X,T).

%%% size (List , Size )
% Size will contain the number of elements in List ,
% written using notation zero , s( zero ), s(s( zero ))..

size(nil, zero).
size(cons(_,T), s(N)):- size(T,N).

% sum_list (List , Sum )

sum(X,zero,X).
sum(X,s(Y),s(Z)):- sum(X,Y,Z).

%sum_list(cons(zero,T),N):- sum_list(T,N).
%sum_list(cons(N,T),R):- sum(N,R,S), sum_list(T,S).
% Base case per sommare una lista vuota
sum_list(nil, zero).

% Clausola ricorsiva per sommare gli elementi della lista
sum_list(cons(N, T), R) :-
    sum_list(T, S),       % Calcola la somma della coda della lista
    sum(N, S, R).         % Somma l'elemento corrente N al risultato parziale S


% count (List , Element , NOccurrences )
% it uses count (List , Element , NOccurrencesSoFar , NOccurrences ) 
%NOccurrencesSoFar =finora
%tail recursive

count(List , E , N ):- count(List , E , zero , N).
count(nil , E , N , N).
count(cons(E , L) , E , N , M):- count(L , E , s( N) , M). %se la trovo -> +1
count(cons(E , L) , E2 , N , M):- E \= E2 , count(L , E2 , N , M ). %non è l'elemento

% max(List , Max )
% Max is the biggest element in List
% Suppose the list has at least one element

greater(s(_),zero).
greater(s(N),s(M)):- greater(N,M).

max(cons(N,T), M):- max(T,N,M).
max(nil,N,N).
max(cons(X,T), N, M):- 
	max(T,X,M),
	greater(X,N).
max(cons(X,T), N, M):- 
	max(T,N,M),
	greater(N,X).

% min - max (List ,Min , Max )
% Min is the smallest element in List
% Max is the biggest element in List
% Suppose the list has at least one element

minmax(cons(X,T), M, N):- minmax(T, X, X, M, N). %(tail, tempMin, tempMax, min, max)
minmax(nil,X1,X2,X1,X2).
minmax(cons(H,T), X1, X2, M, N):-
	minmax(T, H, X2, M,N), %H più piccolo di min
	greater(X1, H).
minmax(cons(H,T), X1, X2, M,N):-
	minmax(T, X1, H, M ,N), %più grande del max
	greater(H,X2).
minmax(cons(H,T), X1, X2, M, N):-
	minmax(T, X1, X2, M ,N), %tra i due
	greater(H,X1),
	greater(X2,H).
% Clausola ricorsiva: se H è uguale al minimo o massimo, continua senza modificare
minmax(cons(H, T), X1, X2, M, N) :-
    \+ greater(X1, H),        % H non è più piccolo dell'attuale minimo
    \+ greater(H, X2),        % H non è più grande dell'attuale massimo
    minmax(T, X1, X2, M, N).  % Continua senza cambiare né minimo né massimo


% same (List1 , List2 )
% are the two lists exactly the same ?

same(nil,nil).
same(cons(H,T1),cons(H,T2)):- same(T1,T2).


% all_bigger (List1 , List2 )
% all elements in List1 are bigger than those in List2 ,1 by 1
all_bigger(nil,nil).
all_bigger(cons(H1,T1),cons(H2,T2)):-
	all_bigger(T1,T2),
	greater(H1,H2).

% sublist (List1 , List2 )
% List1 should contain elements all also in List2
sublist(nil,List2).
sublist(cons(H,T),List2):-
	sublist(T,List2),
	search(H,List2).


% seq(N,E, List ) --> List is [E,E ,... ,E] with size N
% example : seq (s(s(s( zero ))), a, cons (a, cons (a, cons (a,nil )))).
%yes, fully relational

seq(zero , _ , nil ).
seq(s(N),E,cons(E,T)):-seq(N,E,T).

% seqR (N, List )
seqR(zero,cons(zero,nil)).
seqR(s(N), cons(s(N),T)):- seqR(N, T).

% seqR2 (N, List ) --> is [0 ,1 ,... ,N -1] %reverse dell'altra
last(nil, N, cons(N,nil)).
last(cons(H,T), N, cons(H,R)):- last(T,N,R).

seqR2(zero, nil).
seqR2(s(N), R) :- 
    seqR2(N, T),        % Costruisce la lista per N
    last(T, N, R).      % Aggiunge N alla fine della lista T

%l_map(+1)
%e.g. map_1(cons(s(zero), cons(zero, cons(s(s(s(zero))),nil))), 
%	cons(s(s(zero)), cons(s(zero), cons(s(s(s(s(zero)))),nil)))
map_1(nil, nil).
map_1(cons(N,T),cons(s(N),R)):- map_1(T,R).

%filter(>0)
%e.g. filter_0(cons(s(zero), cons(zero, cons(s(s(s(zero))),nil))), 
%	cons(s(zero)), cons(s(s(s(zero)))),nil))
filter_0(nil,nil).
filter_0(cons(N,T),cons(N,R)):-
	N \= zero, %con N != zero
	filter_0(T,R).
filter_0(cons(zero,T), R) :- %non includerlo
	filter_0(T,R).

%find=element/search

%drop_2
%e.g. drop_2(cons(s(zero), cons(zero, cons(s(s(s(zero))),nil))), 
%	cons(s(zero), nil))
equal(N,N).
drop_2(nil,nil).
drop_2(cons(H,T),cons(H,R)):-
	drop_2(T,R),
	size(cons(H,T),N),
	greater(N,s(s(zero))).
drop_2(cons(H,T),nil):-
	size(cons(H,T),N),
	equal(s(s(zero)),N).

%drop_while_0
%e.g. drop_while_0(cons(zero,cons(zero,cons(zero,cons(s(zero),nil)))),
% 	cons(s(zero),nil)).
drop_while_0(nil,nil).
drop_while_0(cons(zero,T), R):- drop_while_0(T,R).
drop_while_0(cons(H,T),cons(H,T)) :- 
	  H \= zero.

%partition_0
%e.g. partition_0(cons(s(s(zero)),cons(zero,cons(zero,cons(s(zero),nil)))),
%	cons(s(s(zero)), cons(s(zero)), nil)),
%	cons(zero, cons(zero, nil)).
partition_0(nil,nil,nil).
partition_0(cons(H,T),cons(H,R),Z):-
	partition_0(T, R, Z),
	greater(H,zero).
partition_0(cons(H,T),R, cons(H,Z)):-
	partition_0(T,R,Z),
	equal(H,zero).

%take_2
%e.g. take_2(cons(s(s(zero)),cons(zero,cons(zero,cons(s(zero),nil)))),
%	cons(s(s(zero)), cons(zero), nil))).

% Base case: empty list
take_2(nil, nil).

% Base case: one element list
take_2(cons(H, nil), cons(H, nil)).

% At least two elements
take_2(cons(H1, cons(H2, _)), cons(H1, cons(H2, nil))).