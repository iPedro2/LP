% da aula pratica: maximo/2 - Recebe uma lista e retorna o maior valor nela contido
maximo([P|R],M) :- maximo(R,P,M).				% predicado de entrada, recebe a lista e a variavel para devolver o valor
maximo([],M,M).									% se a lista for vazia, retorna o proprio valor
maximo([P|R],Macc,M) :-							% recebe uma lista com cabeca e resto, uma var de acumulacao e a var de retorno
						P > Macc,				% se a cabeca for maior que a var de acumulacao
						maximo(R,P,M).			% itera sobre o resto tendo como acumulacao a cabeca (o maior valor encontrado)
maximo([P|R],Macc,M) :-							% reit predicado anterior
						P =< Macc,				% se a cabeca for menor ou igual que a var de acumulacao
						maximo(R,Macc,M).		% itera sobre o resto com a mesma var de acumulacao (nao houve alteracao)

% minimo/2 - Recebe uma lista e retorna o menor valor nela contido
minimo([P|R],M) :- minimo(R,P,M).				% predicado de entrada, recebe uma lista e a var para devolver o valor
minimo([],M,M).									% se a lista for vazia, retorna o proprio valor
minimo([P|R],Macc,M) :-							% recebe uma lista com cabeca e resto, uma var de acumulacao e a var de retorno
						P < Macc,				% se a cabeca for menor que a var de acumulacao
						minimo(R,P,M).			% itera sobre o resto tenco como acumulacao a cabeca (o menor valor encontrado)
minimo([P|R],Macc,M) :-							% reit predicado anterior
						P >= Macc,				% se a cabeca for maior ou igual que a var de acumulacao
						minimo(R,Macc,M).		% itera sobre o resto com a mesma var de acumulacao (nao houve alteracao)							

% valores_unicos/1 - Recebe uma lista e devolve true se todos os seus elementos forem unicos.
valores_unicos([P|R]) :-
						X = [],
						valores_unicos([P|R],X).
						
valores_unicos([],_) :- true.
valores_unicos([P|R],X) :-
						lista_contem(X,P) ->
							false ;
							append([P],X,X1),
							valores_unicos(R,X1).

% e_tabuleiro/1 - Devolve true se o argumento for uma lista que contenha 9 numeros inteiros de 0 a 8 sem repeticoes. Essa lista e' a representacao de um tabuleiro.
e_tabuleiro(L) :-
					length(L,X),
					X is 9,
					valores_unicos(L).

% replace/4 - Recebe uma lista, um indice e um valor e substitui na lista o valor indicado pelo indice (indice comeca em 1)
replace(L, I, X, R) :-
						I1 is I - 1,
						Dummy =.. [dummy|L],
						J is I1 + 1,
						setarg(J, Dummy, X),
						Dummy =.. [dummy|R].
						
% print_transform/2 - Recebe duas listas que representam o estado actual do tabuleiro e o estado objectivo
print_transform(L1,L2) :-
								N is 1,
								print_transform(L1,L2,N).

print_transform([P1|R1],[P2|R2],N) :-
								%nl, write('com: '), writeln(N),
								((N =:= 1 ; N =:= 2 ; N =:= 7 ; N =:= 8 ; N =:= 13 ; N =:= 14) ->
									(P1 =\= 0 ->
										write(P1), write(' ') ;
										write('  ')) ;
								(N =:= 3 ; N =:= 15) ->
									(P1 =\= 0 ->
										write(P1), write('    ') ;
										write('     ')) ;
								(N =:= 9) ->
									(P1 =\= 0 ->
										write(P1), write(' -> ') ;
										write('  -> ')) ;
								(N =:= 4 ; N =:= 5 ; N =:= 10 ; N =:= 11 ; N =:= 16 ; N =:= 17) ->
									(P2 =\= 0 ->
										write(P2), write(' ') ;
										write('  ')) ;
								(N =:= 6 ; N =:= 12) ->
									(P2 =\= 0 ->
										writeln(P2) ;
										writeln(' ')) ;
								(N =:= 18) ->
									(P2 =\= 0 ->
										write(P2) ;
										write(' '))),
								((N =:= 1 ; N =:= 2 ; N =:= 3 ; N =:= 7 ; N =:= 8 ; N =:= 9 ; N =:= 13 ; N =:= 14 ; N =:= 15) ->
									(N1 is N + 1,
									%write('N1: '), writeln(N1),
									print_transform(R1,[P2|R2],N1)) ;
								(N =:= 4 ; N =:= 5 ; N =:= 6 ; N =:= 10 ; N =:= 11 ; N =:= 12 ; N =:= 16 ; N =:= 17) ->
									(N1 is N + 1,
									print_transform([P1|R1],R2,N1))),
								true.

print_single(L1) :-
					N is 1,
					print_single(L1,N).
print_single([P|R],N) :-
						((N =:= 1 ; N =:= 2 ; N =:= 4 ; N =:= 5 ; N =:= 7 ; N =:= 8) ->
							(P =\= 0 ->
								write(P), write(' ') ;
								write('  ')) ;
							(P =\= 0 ->
								writeln(P) ;
								writeln(' '))),
						N =\= 9 ->
							N1 is N + 1,
							print_single(R,N1) ;
							true.
			
% check_retry/2 - Recebe duas listas e verifica se sao iguais. Se nao forem, volta a invocar o resolve_manual/2
check_retry(L1,L2) :-
						print_single(L1),			% escreve no ecra a disposicao actual
						dif(L1,L2),					% verifica se as disposicoes sao diferentes
						resolve_manual2(L1,L2) ;	% se forem, prossegue com a resolucao
						writeln('Parabens!'),		% caso contrario, da a mensagem de parabens
						true.						% termina resolucao

% resolve_manual/2 - Recebe duas listas: uma com a disposicao inicial do tabuleiro e outra com a disposicao final.
resolve_manual(L1,L2) :-
						writeln('Transformacao desejada:'),
						%print_transform(L1,L2),
						resolve_manual2(L1,L2).

% resolve_manual2/2 - Para evitar escrever a disposicao final no ecra a cada movimentacao
resolve_manual2(L1,L2) :-
						writeln('Qual o seu movimento?'),
						read(Move),
						resolve_manual(L1,L2,Move).
						
% resolve_manual/3 - Para correspondencia dos varios movimentos
resolve_manual(L1,L2,'e') :-						% para a esquerda
							nth1(Zpos,L1,0),		% obtem a posicao do zero no tabuleiro original
							rm_esq(L1,L2,Zpos), !.	% invoca o predicado do movimento pretendido

resolve_manual(L1,L2,'d') :-						% para a direita
							nth1(Zpos,L1,0),		% obtem a posicao do zero no tabuleiro original
							rm_dir(L1,L2,Zpos), !.	% invoca o predicado do movimento pretendido

resolve_manual(L1,L2,'c') :-						% para cima
							nth1(Zpos,L1,0),		% obtem a posicao do zero no tabuleiro original
							rm_cim(L1,L2,Zpos), !.	% invoca o predicado do movimento pretendido

resolve_manual(L1,L2,'b') :-						% para baixo
							nth1(Zpos,L1,0),		% obtem a posicao do zero no tabuleiro original
							rm_bai(L1,L2,Zpos), !.	% invoca o predicado do movimento pretendido
							
resolve_manual(_,_,_) :- writeln('Movimento desconhecido (1)').

% move/4 - Recebe uma lista e os indices da peca a mover e do 'buraco' e retorna a lista com o movimento efectuado
move(L1,Ppos,Zpos,L2) :-
						nth1(Ppos,L1,Num),				% obtem o valor da peca que sera movida
						replace(L1,Ppos,0,L11),			% substitui a peca por 'buraco' (neste momento ha dois 'buracos')
						replace(L11,Zpos,Num,L2).		% substitui o 'buraco' antigo pelo valor da peca

rm_esq(L1,L2,3) :-
					writeln('Movimento ilegal (2a)'),	% para quando num movimento para a esquerda, o 'buraco' encontra-se a direita
					check_retry(L1,L2), !.
rm_esq(L1,L2,6) :-
					writeln('Movimento ilegal (2b)'),	% nenhuma peca pode ser movida para a esquerda, logo o movimento e ilegal
					check_retry(L1,L2), !.
rm_esq(L1,L2,9) :-
					writeln('Movimento ilegal (2c)'),
					check_retry(L1,L2), !.
rm_esq(L1,L2,Zpos) :-
						AoLado is Zpos + 1,				% posicao da peca que sera movida (a que esta a direita do 'buraco')
						move(L1,AoLado,Zpos,L11),		% realiza o movimento
						check_retry(L11,L2), !.			% verifica se ja se atingiu a solucao pretendida

rm_dir(L1,L2,1) :-
					writeln('Movimento ilegal (3a)'),	% para quando num movimento para a direita, o 'buraco' encontra-se a esquerda
					check_retry(L1,L2), !.
rm_dir(L1,L2,4) :-
					writeln('Movimento ilegal (3b)'),	% nenhuma peca pode ser movida para a direita, logo o movimento e ilegal
					check_retry(L1,L2), !.
rm_dir(L1,L2,7) :-
					writeln('Movimento ilegal (3c)'),
					check_retry(L1,L2), !.
rm_dir(L1,L2,Zpos) :- 
						AoLado is Zpos - 1,				% posicao da peca que sera movida (a que esta a esquerda do 'buraco')
						move(L1,AoLado,Zpos,L11),		% realiza o movimento
						check_retry(L11,L2), !.			% verifica se ja atingiu a solucao pretendida

rm_cim(L1,L2,7) :-
					writeln('Movimento ilegal (4a)'),	% para quando num movimento para cima, o 'buraco' encontra-se em baixo
					check_retry(L1,L2), !.
rm_cim(L1,L2,8) :-
					writeln('Movimento ilegal (4b)'),	% nenhuma peca pode ser movida para cima, logo o movimento e ilegal
					check_retry(L1,L2), !.
rm_cim(L1,L2,9) :-
					writeln('Movimento ilegal (4c)'),
					check_retry(L1,L2), !.
rm_cim(L1,L2,Zpos) :- 
						AoLado is Zpos + 3,				% posicao da peca que sera movida (a que esta por baixo do 'buraco')
						move(L1,AoLado,Zpos,L11),		% realiza o movimento
						check_retry(L11,L2), !.			% verifica se ja atingiu a solucao pretendida

rm_bai(L1,L2,1) :-
					writeln('Movimento ilegal (5a)'),	% para quando num movimento para baixo, o 'buraco' encontra-se em cima
					check_retry(L1,L2), !.
rm_bai(L1,L2,2) :-
					writeln('Movimento ilegal (5b)'),	% nenhuma peca pode ser movida para baixo, logo o movimento e ilegal
					check_retry(L1,L2), !.
rm_bai(L1,L2,3) :-
					writeln('Movimento ilegal (5c)'),
					check_retry(L1,L2), !.
rm_bai(L1,L2,Zpos) :- 
						AoLado is Zpos - 3,				% posicao da peca que sera movida (a que esta por cima do 'buraco')
						move(L1,AoLado,Zpos,L11),		% realiza o movimento
						check_retry(L11,L2), !.			% verifica se ja atingiu a solucao pretendida
						
% move_legal/4 - Afirma que a configuracao C2 e obtida da configuracao C1 fazendo o movimento M com a peca P
move_legal(C1,e,P,C2) :-
						nth1(Zpos,C1,0),
						P =:= Zpos + 1,
						move(C1,P,Zpos,Check),
						\+ dif(Check,C2), !.
						
move_legal(C1,d,P,C2) :-
						nth1(Zpos,C1,0),
						P =:= Zpos - 1,
						move(C1,P,Zpos,Check),
						\+ dif(Check,C2), !.
						
move_legal(C1,c,P,C2) :-
						nth1(Zpos,C1,0),
						P =:= Zpos + 3,
						move(C1,P,Zpos,Check),
						\+ dif(Check,C2), !.
						
move_legal(C1,b,P,C2) :-
						nth1(Zpos,C1,0),
						P =:= Zpos - 3,
						move(C1,P,Zpos,Check),
						\+ dif(Check,C2), !.

esta_na_fila([],_) :- false.
esta_na_fila([P|R],Valor) :-
							P =:= Valor, ! ;
							esta_na_fila(R,Valor).
	
add_val_fila(L,[],L) :- !.
add_val_fila(Fila,[P|R],Ret) :-
								append(Fila,[P],Ret2),
								add_val_fila(Ret2,R,Ret).
								
junta([], L, L).
junta([P | R], L1, [P | L2]) :- junta(R, L1, L2).

% para a procura cega, dada uma configuracao inicial, o programa deve gerar os sucessores dessa configuracao e testar, em largura, se algum deles
% coincide com a configuracao objectiva. Se sim, termina a computacao, se nao, gera os sucessores e repete o processo
% procura_cego/2
procura_cego(L1,L2) :-
						writeln('oh non!'),
						Fila = [],
						procura_cego(L1,Fila,L2).

procura_cego(L,_,L) :- writeln('terminou'), !.						
procura_cego(L1,Fila,L2) :-
						writeln('oui oui'),
						sucessores(L1,S),
						writeln(S),
						append(Fila,S,NvFila),
						writeln(NvFila).

% sucessores/2 - Dada uma configuracao do tabuleiro L, gera todos os sucessores possiveis (utilizado para a procura cega)
sucessores([],[]).
sucessores(L,S) :-
					pos_esq(L,S1),
					pos_dir(L,S2),
					pos_cim(L,S3),
					pos_bai(L,S4),
					append([],[S1],Sa),
					append(Sa,[S2],Sb),
					append(Sb,[S3],Sc),
					append(Sc,[S4],S).

pos_esq(L,S) :-
				nth1(Zpos,L,0),
				Pos is Zpos - 1,
				Pos > 0,
				Pos =\= 3,
				Pos =\= 6,
				move(L,Pos,Zpos,S), ! ; true.
				
pos_dir(L,S) :-
				nth1(Zpos,L,0),
				Pos is Zpos + 1,
				Pos =\= 4,
				Pos =\= 7,
				Pos < 10,
				move(L,Pos,Zpos,S), ! ; true.
				
pos_cim(L,S) :-
				nth1(Zpos,L,0),
				Pos is Zpos - 3,
				Pos > 0,
				move(L,Pos,Zpos,S), ! ; true.
				
pos_bai(L,S) :-
				nth1(Zpos,L,0),
				Pos is Zpos + 3,
				Pos < 10,
				move(L,Pos,Zpos,S), ! ; true.

