% Grupo 123, Pedro Bucho 69537, Joao Zeferino 76497

% transformacao/2
% transformacao(C1, C2) em que C1 e C2 sao configuracoes representadas por listas
transformacao([A, B, C, D, E, F, G, H, I], 
			  [J, K, L, M, N, O, P, Q, R]) :-
	  write('Transformacao desejada:'), nl, 
	  escreve(A), escreve(B), escreve(C),  
	  write('    '), 
	  escreve(J), escreve(K), escreve(L),nl, 
	  escreve(D), escreve(E), escreve(F), 
	  write(' -> '), 
	  escreve(M), escreve(N), escreve(O), nl,
	  escreve(G), escreve(H), escreve(I), 
	  write('    '), 
	  escreve(P), escreve(Q), escreve(R), nl, !.
	  
% escreve/1 e um predicado auxiliar de transformacao/2
% a primeira regra permite escrever uma configuracao
escreve([A, B, C, D, E, F, G, H, I]) :- escreve(A), escreve(B), escreve(C), nl,
										escreve(D), escreve(E), escreve(F), nl,
										escreve(G), escreve(H), escreve(I), nl.

escreve(S) :- S = 0, write('   ').
escreve(S) :- S < 10, write(' '), write(S), write(' ').

% escreve_solucao/1
% escreve_solucao(M) em que M e uma lista de movimentos e um movimento e um par (Mov, Peca)
escreve_solucao([(M, P) | []]) :- write('mova a peca '), 
								  write(P), 
								  traduz(M, Mp), 
								  write(Mp),
								  write('.'),
								  nl.

escreve_solucao([(M, P) | R]) :- write('mova a peca '), 
								 write(P), 
								 traduz(M, Mp), 
								 write(Mp),
								 nl, 
								 escreve_solucao(R).

% traduz/2 e um predicado auxiliar de escreve_solucao/1
traduz(c, ' para cima').
traduz(b, ' para baixo').
traduz(e, ' para a esquerda').
traduz(d, ' para a direita').

% minimo/2 - Recebe uma lista e retorna o menor valor nela contido
minimo([P|R],M) :- minimo(R,P,M).				% predicado de entrada, recebe uma lista e a var para devolver o valor
minimo([],M,M).									% se a lista for vazia, retorna o proprio valor
minimo([P|R],Macc,M) :-							% recebe uma lista com cabeca e resto, uma var de acumulacao e a var de retorno
						P < Macc,				% se a cabeca for menor que a var de acumulacao
						minimo(R,P,M).			% itera sobre o resto tenco como acumulacao a cabeca (o menor valor encontrado)
minimo([P|R],Macc,M) :-							% reit predicado anterior
						P >= Macc,				% se a cabeca for maior ou igual que a var de acumulacao
						minimo(R,Macc,M).		% itera sobre o resto com a mesma var de acumulacao (nao houve alteracao)

% minimo/2 - Recebe uma lista e retorna o menor valor nela contido
% Index corresponde ao indice do elemento com menor f
% Indice corresponde ao indice do elemento em comparacao
minimo_f([P|R],I,M) :-
					nth1(2,P,Valor),
					minimo_f(R,Valor,1,1,I,M).					% predicado de entrada, recebe uma lista e a variavel para devolver o valor
				  
minimo_f([],M,_,Index,Index,M).									% se a lista for vazia, retorna o proprio valor
minimo_f([P|R],Macc,Indice,_,I,M) :-							% recebe uma lista com cabeca e resto, uma variavel de acumulacao e a variavel de retorno
						nth1(2,P,Valor),						% obtem o valor de f
						Valor < Macc,							% se a cabeca for menor que a variavel de acumulacao
						NxIndice is Indice + 1,					% incrementa o indice de procura
						NxIndex is NxIndice,					% define o indice onde foi localizado o menor f
						minimo_f(R,Valor,NxIndice,NxIndex,I,M).	% itera sobre o resto tenco como acumulacao a cabeca (o menor valor encontrado)

minimo_f([P|R],Macc,Indice,Index,I,M) :-						% reit predicado anterior
						nth1(2,P,Valor),						% obtem o valor de f
						Valor >= Macc,							% se a cabeca for maior ou igual que a variavel de acumulacao
						NxIndice is Indice + 1,					% incrementa o indice de procura
						minimo_f(R,Macc,NxIndice,Index,I,M).	% itera sobre o resto com a mesma variavel de acumulacao (nao houve alteracao)

% replace/4 - Recebe uma lista, um indice e um valor e substitui na lista o valor indicado pelo indice (indice a contar a partir de 1)
replace(L, I, X, R) :-
						I1 is I - 1,
						Dummy =.. [dummy|L],
						J is I1 + 1,
						setarg(J, Dummy, X),
						Dummy =.. [dummy|R].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% RESOLVE_MANUAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% resolve_manual/2 - Recebe uma lista com a configuracao inicial e outra com a configuracao final e executa a resolucao manual
resolve_manual(L1,L2) :-
						transformacao(L1,L2),	% imprime a transformacao pretendida
						resolve_manual2(L1,L2).	% invoca o predicado da resolucao manual
						
resolve_manual2(L,L) :- 						% caso as listas sejam iguais
						writeln('Parabens!'),	% imprime a mensagem de parabens
						true, !.				% termina a execucao
						
resolve_manual2(L1,L2) :-
						writeln('Qual o seu movimento?'),	% pergunta ao utilizador o qual sera o seu movimento
						read(Move),							% recebe o input do utilizador
						mov_legal(L1,Move,_,L),				% verifica se o movimento pedido e legal e, se sim, realiza-o
						escreve(L),							% imprime a modificacao feita
						resolve_manual2(L,L2), ! ;			% invoca o resolve manual para verificar se a solucao ja foi atingida ou para pedir mais movimentos
															% se o movimento pedido for ilegal
						writeln('Movimento ilegal'),		% informa o utilizador
						resolve_manual2(L1,L2), !.			% invoca o resolve manual para verificar se a solucao ja foi atingida ou para pedir mais movimentos

% move/4 - Recebe uma lista e os indices da peca a mover e do zero e retorna a lista com o movimento efectuado
move(L1,Ppos,Zpos,L2) :-
						nth1(Ppos,L1,Num),			% obtem o valor da peca que sera movida
						replace(L1,Ppos,0,L11),		% substitui a peca por zero (neste momento ha dois 'buracos')
						replace(L11,Zpos,Num,L2).	% substitui o zero antigo pelo valor da peca


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MOV_LEGAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
						
% mov_legal/4 - Afirma que a configuracao C2 e obtida da configuracao C1 fazendo o movimento M com a peca P
mov_legal(C1,M,P,C2) :-
						M = c,									% para o movimento para cima
						nth1(Zpos,C1,0),						% obtem a posicao do zero
						Pos is Zpos + 3,						% obtem a posicao da peca a mover
						Pos < 10,								% se a posicao da peca a mover for valida
						get_at(C1,Pos,P),						% obtem o valor da peca
						move(C1,Pos,Zpos,C2) ;					% faz o movimento
						
						M = b,									% para o movimento para baixo
						nth1(Zpos,C1,0),						% obtem a posicao do zero
						Pos is Zpos - 3,						% obtem a posicao da peca a mover
						Pos > 0,								% se a posicao da peca a mover for valida
						get_at(C1,Pos,P),						% obtem o valor da peca
						move(C1,Pos,Zpos,C2) ;					% faz o movimento

						M = e,									% para o movimento para a esquerda
						nth1(Zpos,C1,0),						% obtem a posicao do zero
						Pos is Zpos + 1,						% obtem a posicao da peca a mover
						(Pos =\= 4 , Pos =\= 7 , Pos =\= 10),	% se a posicao da peca a mover for valida
						get_at(C1,Pos,P),						% obtem o valor da peca
						move(C1,Pos,Zpos,C2) ;					% faz o movimento

						M = d,									% para o movimento para a direita
						nth1(Zpos,C1,0),						% obtem a posicao do zero
						Pos is Zpos - 1,						% obtem a posicao da peca a mover
						(Pos =\= 0 , Pos =\= 3 , Pos =\= 6),	% se a posicao da peca a mover for valida
						get_at(C1,Pos,P),						% obtem o valor da peca
						move(C1,Pos,Zpos,C2).					% faz o movimento

% get_at/3 - Recebe uma lista e um indice (a contar a partir de 1) e retorna o valor contido nessa posicao
get_at(L,P,R) :- get_at(L,P,1,R), !.		% inicializa a procura
get_at([],_,_,_).							% para uma lista vazia retorna true
get_at([H|T],P,Acc,R) :-
					P =:= Acc,				% se a posicao pedida for igual a posicao actual
					R = H ;					% retorna o valor contido na cabeca da lista
											% se a posicao pedida nao for a actual
					Acc2 is Acc + 1,		% passa para a posicao seguinte
					get_at(T,P,Acc2,R).		% itera sobre a posicao seguinte

% addListaNV/3 - Recebe duas listas de listas e introduz o conteudo da segunda na primeira, excepto sublistas vazias
addListaNV(L,[],L) :- !.
addListaNV(Fila,[P|R],Ret) :-
								dif(P,[]),
								append(Fila,[P],Ret2),
								addListaNV(Ret2,R,Ret), ! ;
								addListaNV(Fila,R,Ret).

% first_fila/2 - Recebe uma fila, remove o primeiro elemento e retorna-o
first_fila([],_,[]) :- !.
first_fila([P|R],S,NvFila) :- 
								NvFila = R,
								S = P.

% del_ultimo/2 - Recebe uma lista e retorna-a com o ultimo elemento removido
del_ultimo([_], []) :- !.
del_ultimo([],[]) :- !.
del_ultimo([X|Xs], [X|WithoutLast]) :- del_ultimo(Xs, WithoutLast).

% divisao_lista/3 - Recebe duas listas L1 e L2 e retorna uma lista com os elementos de L1 que nao estao presentes em L2
divisao_lista([],_,[]) :- !.
divisao_lista(L1,L2,S) :- divisao_lista2(L1,L2,[],S), !.
divisao_lista2([],_,Acc,Acc) :- !.
divisao_lista2(L,[],_,L) :- !.
divisao_lista2([P|R],L2,Acc,S) :-
							(\+ member(P,L2)),				% se P nao estiver na lista L2
							append(Acc,[P],Acc2),			% adiciona P a S
							divisao_lista2(R,L2,Acc2,S), ! ;	% volta a correr para os elementos seguintes
							divisao_lista2(R,L2,Acc,S), !.	% volta a correr para os elementos seguintes

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% RESOLVE_CEGO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% resolve_cego/2 - Para a procura cega, dada uma configuracao inicial, o programa deve gerar os sucessores dessa configuracao e testar, em largura, se algum deles
% coincide com a configuracao objectiva. Se sim, termina a computacao, se nao, gera os sucessores e repete o processo
resolve_cego(L,L) :- transformacao(L,L), !.
resolve_cego(L1,L2) :-
						transformacao(L1,L2),
						dfs([],L1,[],[],L2), !.

dfs(_,L,_,Cam,L) :- escreve_solucao(Cam), !.
dfs(Pred,L1,Fe,Cam,L2) :-
				append([L1],Fe,NvFe),							% adiciona o no actual a lista de fechados
				append([L1],Pred,NvPred),						% adiciona o no actual a lista de predecessores
				sucessores(L1,S),								% obtem os sucessores do estado actual
				addListaNV([],S,S1),							% remove as sublistas vazias
				divisao_lista(S1,NvFe,S2),						% remove os sucessores que estao na lista de fechados
				(   dif(S2,[]) ->  								% se continuar a existir sucessores
					(   first_fila(S2,Proximo,_),				% obtem o primeiro sucessor gerado
						gera_caminho(L1,Proximo,Caminho),		% gera o caminho ate ele
						append(Cam,[Caminho],NvCam),			% adiciona o caminho a lista do caminho
						dfs(NvPred,Proximo,NvFe,NvCam,L2)) ;	% itera para o sucessor
																% caso nao tenham sobrado sucessores
					(   del_ultimo(Cam,NvCam),					% elimina o caminho ate ao no actual
						first_fila(Pred,Anterior,NvPred2),		% obtem o predecessor do no actual
						dfs(NvPred2,Anterior,NvFe,NvCam,L2))).	% itera para o no anterior

% sucessores/2 - Dada uma configuracao do tabuleiro L, gera todos os sucessores possiveis (utilizado para a procura cega)
sucessores([],[]).
sucessores(L,S) :-
					pos_cim(L,S1),
					pos_bai(L,S2),
					pos_esq(L,S3),
					pos_dir(L,S4),
					append([],[S1],Sa),
					append(Sa,[S2],Sb),
					append(Sb,[S3],Sc),
					append(Sc,[S4],S).

% pos_cim/2 - Recebe uma configuracao L e retorna o resultante de mover uma peca para baixo ou [] se nao for possivel
pos_cim(L,S) :-
				nth1(Zpos,L,0),
				Pos is Zpos + 3,
				Pos < 10,
				move(L,Pos,Zpos,S), ! ;
				S = [].
				
% pos_bai/2 - Recebe uma configuracao L e retorna o resultante de mover uma peca para cima ou [] se nao for possivel
pos_bai(L,S) :-
				nth1(Zpos,L,0),
				Pos is Zpos - 3,
				Pos > 0,
				move(L,Pos,Zpos,S), ! ;
				S = [].
				
% pos_esq/2 - Recebe uma configuracao L e retorna o resultante de mover uma peca para a direita ou [] se nao for possivel
pos_esq(L,S) :-
				nth1(Zpos,L,0),
				Pos is Zpos + 1,
				Pos =\= 4,
				Pos =\= 7,
				Pos < 10,
				move(L,Pos,Zpos,S), ! ;
				S = [].
				
% pos_dir/2 - Recebe uma configuracao L e retorna o resultante de mover uma peca para a esquerda ou [] se nao for possivel
pos_dir(L,S) :-
				nth1(Zpos,L,0),
				Pos is Zpos - 1,
				Pos > 0,
				Pos =\= 3,
				Pos =\= 6,
				move(L,Pos,Zpos,S), ! ;
				S = [].

% gera_caminho/3 (caminhos_sucessores) - Recebe uma configuracao e um seu sucessor e devolve um par para o caminho desse sucessor
gera_caminho([],_,[]) :- !.
gera_caminho(Orig,Suc,Cam) :-
						nth1(ZOrig,Orig,0),
						nth1(ZNv,Suc,0),
						Delta is ZNv - ZOrig,
						nth1(ZOrig,Suc,Peca),
						((Delta =:= -1 ->
							Cam = (d,Peca)) ;
						(Delta =:= 1 ->
							Cam = (e,Peca)) ;
						(Delta =:= -3 ->
							Cam = (b,Peca)) ;
						(Delta =:= 3 ->
							Cam = (c,Peca))),
						!.

listaVazia([]):- !.
% calcula_h/4 - Recebe a configuracao actual L1 e a configuracao objectiva L2 e d
calcula_h([],[],S,K) :- K is 8 - S.
calcula_h(L1,L2,K,S) :-
						listaVazia(L1), ! ;
						listaVazia(L2), ! ;
						calcula_h_aux(L1,L2,K,S).

calcula_h_aux([P1|R1],[P2|R2],K,S) :-
										(   P1 =:= 0 ->	   %evita que caso P1 e P2 =0 incremente 
											K1 = K,			% o numero de quadrados no sitio certo
											calcula_h(R1,R2,K1,S));
										(\+ dif(P1,P2) ->
											K1 is K + 1;
											K1 = K),
										calcula_h(R1,R2,K1,S).

expande_no(L1,L2,Ab,Fe,RetAb) :- % alterar L1 para passar a ser um no
								nth1(1,L1,Tabuleiro),
								sucessores(Tabuleiro,S),
								addListaNV([],S,S1),
								(dif(S1,[]) ->
									(   calcula_no(L1,L2,S1,N),
										divisao_lista(N,Ab,N1),
										divisao_lista(N1,Fe,N2),
										append(N2,Ab,RetAb)
										);
									RetAb = Ab).

% calcula_no/3 - Recebe um no actual L1, uma lista de sucessores S e retorna uma lista de nos N
calcula_no(_,_,[],[]).
calcula_no(L1,L2,S,N) :- calcula_no(L1,L2,S,[],N).
calcula_no(_,_,[],N,N).
% K e para a acumulacao dos nos
calcula_no(L1,L2,[P|R],K,N) :-
							nth1(1,L1,TabuleiroActual),					% obtem a configuracao do no actual
							gera_caminho(TabuleiroActual,P,Caminho),	% obtem o caminho desde o no actual ate ao primeiro sucessor
							nth1(3,L1,G),								% obtem o valor de g
							Gs = G + 1,									% calcula o novo valor de g
							calcula_h(P,L2,0,Hs),						% calcula o novo valor de h
							Fs is Gs + Hs,
							nth1(5,L1,CaminhoPercorrido),
							append(CaminhoPercorrido,[Caminho],CP),
							append(K,[[P,Fs,Gs,Hs,CP]],Ks),
							calcula_no(L1,L2,R,Ks,N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% RESOLVE_INFO_H %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% distancia de Hamming - numero de quadrados fora da posicao certa
resolve_info_h(L,L) :- transformacao(L,L), !.
resolve_info_h(L1,L2) :-
						transformacao(L1,L2),
						calcula_h(L1,L2,0,S),
						resolve_info_h(L1,[L1,S,0,S,[]],L2,[[L1,S,0,S,[]]],[]), !.

resolve_info_h(L,N,L,_,_) :-
								nth1(5,N,Caminho),
								escreve_solucao(Caminho), !.

resolve_info_h(_,_,L2,Ab,Fe) :-
								minimo_f(Ab,I,_),
								nth1(I,Ab,No),
								delete(Ab,No,NvAb),
								append(No,Fe,NvFe),
								expande_no(No,L2,NvAb,NvFe,RetAb),
								nth1(1,No,Tabuleiro),
								resolve_info_h(Tabuleiro,No,L2,RetAb,NvFe).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRANSFORMACAO_POSSIVEL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transformacao_possivel(C1,C2) :-
								translate(C1,C2,T),
								tp2(T,0,T2),
								Resto is T2 mod 2,
								Resto =:= 0, !.

tp2([],T,T).
tp2([P|R],Acc,T) :-
					percorre_elementos(P,R,0,T2),
					Acc2 is Acc + T2,
					tp2(R,Acc2,T).

percorre_elementos(_,[],T,T).
percorre_elementos(P,[R|S],Acc,T) :- 
							(   R < P ->  
							(   Acc2 is Acc + 1,
							percorre_elementos(P,S,Acc2,T)) ;

							(   percorre_elementos(P,S,Acc,T))).
							
converte([],T,T).
converte([P|R],Acc,T) :-
						(P =\= 0 ->		% nao e necessario fazer a conversao do zero porque nao sera utilizado
						(append(Acc,[P],Acc2),
						converte(R,Acc2,T)) ;

						converte(R,Acc,T)).

translate(C1,C2,T) :-
						converte(C2,[],M),
						translate2(C1,M,[],T).

translate2([],_,T,T).
translate2([P|R],M,Acc,T) :-
							(P =\= 0 ->
							(get_at(M,P,Valor),
							Valor2 is Valor + 10,
							append(Acc,[Valor2],Acc2),
							translate2(R,M,Acc2,T)) ;
							(translate2(R,M,Acc,T))).
							
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTES FORNECIDOS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

desafio(1) :- resolve_manual([1, 2, 3, 4, 5, 6, 7, 8, 0], [1, 0, 2, 4, 5, 3, 7, 8, 6]).
% input b d e b d

desafio(2) :- resolve_manual([0, 1, 3, 4, 2, 5, 7, 8, 6], [1, 2, 3, 4, 5, 6, 7, 8, 0]).
% input e c e c

desafio(3) :- resolve_cego([1, 2, 3, 4, 5, 6, 7, 8, 0], [1, 0, 2, 4, 5, 3, 7, 8, 6]).

desafio(4) :- resolve_cego([0, 1, 3, 4, 2, 5, 7, 8, 6], [1, 2, 3, 4, 5, 6, 7, 8, 0]).

desafio(5) :- resolve_info_h([1, 2, 3, 4, 5, 6, 7, 8, 0], [1, 0, 2, 4, 5, 3, 7, 8, 6]).

desafio(6) :- resolve_info_h([0, 1, 3, 4, 2, 5, 7, 8, 6], [1, 2, 3, 4, 5, 6, 7, 8, 0]).

