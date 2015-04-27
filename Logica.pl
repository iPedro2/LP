%minimo_e/2 - Recebe uma lista e um inteiro positivo e retorna true se esse inteiro for o minimo valor dessa lista (maximo 999)

minimo_e([],_) :- 1 is 2.
minimo_e([P|R],N) :-
					Xacc = 999,
					minimo_e([P|R],N,Xacc).
				
minimo_e([P|R],N,Xacc) :- 
							(P =< Xacc ->
								Xacc1 is P),
							Xacc1 is Xacc,
							minimo_e(R,N,Xacc1).
								

% lista_contem/2 - Recebe uma lista e um valor e verifica se esse valor se encontra na lista

lista_contem([],_) :- 1 is 2. % forca o retorno de false porque a lista e vazia
lista_contem([P|R],N) :-
						P is N ->				% condicao if
							P is N ;			% literais if
							lista_contem(R,N).	% literais else
						
% valores_unicos/1 - Recebe uma lista e devolve true se todos os seus elementos forem unicos.

valores_unicos([P|R]) :-
						X = [],
						valores_unicos([P|R],X).
						
valores_unicos([],_) :- 1 is 1.
valores_unicos([P|R],X) :-
						lista_contem(X,P) ->
							1 is 2 ;
							append([P],X,X1),
							valores_unicos(R,X1).


% e_tabuleiro/1 - Devolve true se o argumento for uma lista que contenha 9 numeros inteiros de 0 a 8 sem repeticoes. Essa lista e' a representacao de um tabuleiro.

e_tabuleiro(L) :-
					length(L,X),
					X is 9,
					valores_unicos(L).
