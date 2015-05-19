%%% transformacao/2
%%% transformacao(C1, C2) em que C1 e C2 sao configuracoes representadas por listas
%%% por exemplo
%%% ?- transformacao([1, 2, 3, 4, 5, 6, 7, 8, 0], [1, 0, 2, 4, 5, 3, 7, 8, 6]).
%%% Transformacao desejada:
%%%  1  2  3      1     2 
%%%  4  5  6  ->  4  5  3 
%%%  7  8         7  8  6 
%%% true .

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
      escreve(P), escreve(Q), escreve(R), nl,!.
      
%%% escreve/1 e um predicado auxiliar de transformacao/2
%%% a primeira regra permite escrever uma configuracao
%%% por exemplo
%%% ?- escreve([1, 2, 3, 4, 5, 6, 7, 8, 0]).
%%%  1  2  3 
%%%  4  5  6 
%%%  7  8    
%%% true .

escreve([A, B, C, D, E, F, G, H, I]) :- escreve(A), escreve(B), escreve(C), nl,
                                        escreve(D), escreve(E), escreve(F), nl,
                                        escreve(G), escreve(H), escreve(I), nl.

escreve(S) :- S = 0, write('   ').
escreve(S) :- S < 10, write(' '), write(S), write(' ').

%%% escreve_solucao/1
%%% escreve_solucao(M) em que M e uma lista de movimentos e um movimento e um par (Mov, Peca) 
%%% por exemplo
%%% ?- escreve_solucao([(b, 6), (b, 3), (d, 2)]).
%%% mova a peca 6 para baixo
%%% mova a peca 3 para baixo
%%% mova a peca 2 para a direita.
%%% true .

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


%%% traduz/2 e um predicado auxiliar de escreve_solucao/1

traduz(c, ' para cima').
traduz(b, ' para baixo').
traduz(e, ' para a esquerda').
traduz(d, ' para a direita').

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

% minimo/2 - Recebe uma lista e retorna o menor valor nela contido
% Index corresponde ao indice do elemento com menor f
% Indice corresponde ao indice do elemento em comparacao
minimo_f([P|R],I,M) :-
                  nth1(2,P,Valor),
                  minimo_f(R,Valor,1,1,I,M).				% predicado de entrada, recebe uma lista e a var para devolver o valor
minimo_f([],M,_,Index,Index,M).									% se a lista for vazia, retorna o proprio valor
minimo_f([P|R],Macc,Indice,_,I,M) :-							% recebe uma lista com cabeca e resto, uma var de acumulacao e a var de retorno
    					nth1(2,P,Valor),
						Valor < Macc,				% se a cabeca for menor que a var de acumulacao
    					NxIndice is Indice + 1,
    					NxIndex is NxIndice,
						minimo_f(R,Valor,NxIndice,NxIndex,I,M).			% itera sobre o resto tenco como acumulacao a cabeca (o menor valor encontrado)
minimo_f([P|R],Macc,Indice,Index,I,M) :-							% reit predicado anterior
    					nth1(2,P,Valor),
						Valor >= Macc,				% se a cabeca for maior ou igual que a var de acumulacao
    					NxIndice is Indice + 1,
						minimo_f(R,Macc,NxIndice,Index,I,M).		% itera sobre o resto com a mesma var de acumulacao (nao houve alteracao)

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
						transformacao(L1,L2),
						resolve_manual2(L1,L2).

% resolve_manual2/2 - Para evitar escrever a disposicao final no ecra a cada movimentacao
resolve_manual2(L1,L2) :-
						writeln('Qual o seu movimento?'),
						read(Move),
						resolve_manual(L1,L2,Move).
						
% resolve_manual/3 - Para correspondencia dos varios movimentos
resolve_manual(L1,L2,e) :-							% para a esquerda
							nth1(Zpos,L1,0),		% obtem a posicao do zero no tabuleiro original
							rm_esq(L1,L2,Zpos), !.	% invoca o predicado do movimento pretendido

resolve_manual(L1,L2,d) :-							% para a direita
							nth1(Zpos,L1,0),		% obtem a posicao do zero no tabuleiro original
							rm_dir(L1,L2,Zpos), !.	% invoca o predicado do movimento pretendido

resolve_manual(L1,L2,c) :-							% para cima
							nth1(Zpos,L1,0),		% obtem a posicao do zero no tabuleiro original
							rm_cim(L1,L2,Zpos), !.	% invoca o predicado do movimento pretendido

resolve_manual(L1,L2,b) :-							% para baixo
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
							\+ dif(P,Valor), ! ;
							esta_na_fila(R,Valor).

% add_val_fila/3 - Recebe uma lista de listas que representa uma fila de configuracoes e uma lista de listas com novas configuracoes e
% introdu-las na fila, excepto sublistas vazias
add_val_fila(L,[],L) :- !.
add_val_fila(Fila,[P|R],Ret) :-
								dif(P,[]),
								append(Fila,[P],Ret2),
								add_val_fila(Ret2,R,Ret), ! ;
								add_val_fila(Fila,R,Ret).

% first_fila/2 - Recebe uma fila, remove o primeiro elemento e retorna-o
first_fila([],_,[]) :- !.
first_fila([P|R],S,NvFila) :- 
								NvFila = R,
								S = P.
								
mete_no_fim(L,S) :-
					first_fila(L,Valor,L2),
					append(L2,[Valor],S).

% del_ultimo/2 - Recebe uma lista e retorna-a com o ultimo elemento removido	
del_ultimo([_], []) :- !.
del_ultimo([],[]) :- !.
del_ultimo([X|Xs], [X|WithoutLast]) :- del_ultimo(Xs, WithoutLast).

% divisao_lista/3 - Recebe duas listas L1 e L2 e retorna uma lista com os elementos de L1 que nao estao presentes em L2
divisao_lista([],_,[]) :- !.
divisao_lista(L1,L2,S) :- divisao_lista2(L1,L2,[],S),!.
divisao_lista2([],_,Acc,Acc) :- !.
divisao_lista2(L,[],_,L) :- !.
divisao_lista2([P|R],L2,Acc,S) :-
							(\+ esta_na_fila(L2,P)),		% se P nao estiver na lista L2
							append(Acc,[P],Acc2),			% adiciona P a S
							divisao_lista2(R,L2,Acc2,S),! ;	% volta a correr para os elementos seguintes
							divisao_lista2(R,L2,Acc,S),!.	% volta a correr para os elementos seguintes

% resolve_cego/2 - Para a procura cega, dada uma configuracao inicial, o programa deve gerar os sucessores dessa configuracao e testar, em largura, se algum deles
% coincide com a configuracao objectiva. Se sim, termina a computacao, se nao, gera os sucessores e repete o processo
resolve_cego(L,L) :- transformacao(L,L), !.
resolve_cego(L1,L2) :-
						transformacao(L1,L2),
						dfs(L1,[],[L1],[],L2), !.
%resolve_cego(L,L,_) :- escreve_solucao.
%resolve_cego(L1,L2,M) :- 
%						move_legal(L1,M,P,NewC),
 %   					\+ member(NewC,M),
  %  					resolve_cego(L1,L2,[L2|M]).
						
dfs(L,_,_,Cam,L) :- escreve_solucao(Cam),!.
dfs(L1,Ab,Fe,Cam,L2) :-
				sucessores(L1,S),				% gera os sucessores do estado actual
				add_val_fila([],S,S1),			% remove as sublistas vazias
				divisao_lista(S1,Fe,S2),		% elimina os sucessores ja visitados
				divisao_lista(S2,Ab,S3),		% elimina os sucessores na lista de abertos
    			(   dif(S3,[]) ->  				% caso tenha sucessores
                	(   append(S3,Ab,NvAb),
                        first_fila(NvAb,Proximo,NvAb2),
                    	cam_suc(L1,Proximo,Caminho),
                        append(Cam,[Caminho],NvCam)) ;
                        
                    (   first_fila(Ab,Proximo,NvAb2),	% caso os sucessores ja estejam na lista de abertos
                        del_ultimo(Cam,NvCam)			% ou fechados, ou nao tenha sucessores
                    )),
    			append(Fe,[Proximo],NvFe),
    			dfs(Proximo,NvAb2,NvFe,NvCam,L2).
								
descobre_no(L,Suc,Fechados,Final) :-
						first_fila(Suc,Primeiro,_),
    					divisao_lista([Primeiro],Fechados,Verificacao),
    					(   dif(Verificacao,[]) ->  
                        	(   resolve_cego(Primeiro,Fechados,Final),!) ;
                        	first_fila(Suc,_,SucSemPrimeiro)),
    					(   dif(SucSemPrimeiro,[]) ->  
                        	(   descobre_no(L,SucSemPrimeiro,Fechados,Final),!) ;
                        	(mete_no_fim(Fechados,NvFechados),
                             first_fila(NvFechados,Predecessor,_),
                             resolve_cego(Predecessor,NvFechados,Final),!)).
                            

% sucessores/2 - Dada uma configuracao do tabuleiro L, gera todos os sucessores possiveis (utilizado para a procura cega)
sucessores([],[]).
sucessores(L,S) :-
    				pos_bai(L,S1),
    				pos_cim(L,S2),
    				pos_dir(L,S3),
    				pos_esq(L,S4),
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
				move(L,Pos,Zpos,S), ! ;
				S = [].
				
pos_dir(L,S) :-
				nth1(Zpos,L,0),
				Pos is Zpos + 1,
				Pos =\= 4,
				Pos =\= 7,
				Pos < 10,
				move(L,Pos,Zpos,S), ! ;
				S = [].
				
pos_cim(L,S) :-
				nth1(Zpos,L,0),
				Pos is Zpos - 3,
				Pos > 0,
				move(L,Pos,Zpos,S), ! ;
				S = [].
				
pos_bai(L,S) :-
				nth1(Zpos,L,0),
				Pos is Zpos + 3,
				Pos < 10,
				move(L,Pos,Zpos,S), ! ;
				S = [].

% cam_suc/3 (caminhos_sucessores) - Recebe uma configuracao e um seu sucessor e devolve um par para o caminho desse sucessor
cam_suc([],_,[]) :- !.
cam_suc(Orig,Suc,Cam) :-
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

% cam_suc_lista/3 (caminhos_sucessores_lista) - Equivalente a cam_suc mas permite receber uma lista de sucessores para uma dada configuracao
cam_suc_lista(Orig,L,Cam) :- cam_suc_lista2(Orig,L,[],Cam).
cam_suc_lista2(_,[],Acc,Acc) :- !.
cam_suc_lista2(Orig,[P|R],Acc,Cam) :-
								cam_suc(Orig,P,Path),
								append(Acc,[Path],Acc2),
								cam_suc_lista2(Orig,R,Acc2,Cam).

cam_suc_lista_pre(Orig,Prefix,L,Cam) :- cam_suc_lista_pre2(Orig,Prefix,L,[],Cam).
cam_suc_lista_pre2(_,_,[],Acc,Acc) :- !.
cam_suc_lista_pre2(Orig,Prefix,[P|R],Acc,Cam) :-
    											(   dif(Prefix,[]) ->  
													append([],Prefix,Lpref) ;
                                                	write('')
                                                ),
												cam_suc(Orig,P,Path),
												append(Lpref,[Path],Appended),
												append(Acc,[Appended],Acc2),
												cam_suc_lista_pre2(Orig,Prefix,R,Acc2,Cam).

% sobreposicao/3 - Recebe duas listas e indica se as posicoes Pos em ambas 
% (a contar a partir de 1) sao iguais
% falta remover o 0, o 0 nao pode dar 1
sobreposicao(L,L,_) :- !.
sobreposicao(L1,L2,Pos) :-
						nth1(Pos,L1,Primeiro),
						nth1(Pos,L2,Segundo),
						\+ dif(Primeiro,Segundo).

%
listaVazia([]):- !.
%abs_sobr indica o numero de quadrados no sitio certo
calcula_h([],[],S,K) :- K is 8 - S.
calcula_h(L1,L2,K,S) :- listaVazia(L1),!;
    				  listaVazia(L2),!;
    				  calcula_h_aux(L1,L2,K,S).

calcula_h_aux([P1|R1],[P2|R2],K,S) :-  (   P1 =:= 0 ->       %evita que caso P1 e P2 =0 incremente 
                               			K1 = K,            % o numero de quadrados no sitio certo
                               		calcula_h(R1,R2,K1,S));
    						  		(\+ dif(P1,P2) ->
										K1 is K + 1;
									K1 = K),
									calcula_h(R1,R2,K1,S).

%abs_sobr([P1|R1],[P2|R2],S) :-
%    						( notZero(P1)-> (\+ dif(P1,P2) ->
%								S1 is S + 1; S1 = S);
%								S1 = S),
%							abs_sobr(R1,R2,S1).

expande_no(L1,L2,Ab,Fe,RetAb) :- % alterar L1 para passar a ser um no
    				nth1(1,L1,Tabuleiro),
    				sucessores(Tabuleiro,S),
    				add_val_fila([],S,S1),
    				(dif(S1,[]) ->
                        (   calcula_no(L1,L2,S1,N),
                            divisao_lista(N,Ab,N1),
                            divisao_lista(N1,Fe,N2),
                            append(N2,Ab,RetAb)
                            );
                    	RetAb = Ab
                    ).

% calcula_no/3 - Recebe um no actual L1, uma lista de sucessores S e retorna uma lista de nos N
calcula_no(_,_,[],[]).
calcula_no(L1,L2,S,N) :- calcula_no(L1,L2,S,[],N).
calcula_no(_,_,[],N,N).
% K e para a acumulacao dos nos
calcula_no(L1,L2,[P|R],K,N) :-
    						nth1(1,L1,TabuleiroActual),			% obtem a configuracao do no actual
    						cam_suc(TabuleiroActual,P,Caminho),	% obtem o caminho desde o no actual ate ao primeiro sucessor
    						nth1(3,L1,G),						% obtem o valor de g
    						Gs = G + 1,							% calcula o novo valor de g
    						calcula_h(P,L2,0,Hs),	% calcula o novo valor de h
    						Fs is Gs + Hs,
    						nth1(5,L1,CaminhoPercorrido),
    						append(CaminhoPercorrido,[Caminho],CP),
    						append(K,[[P,Fs,Gs,Hs,CP]],Ks),
                            calcula_no(L1,L2,R,Ks,N).

% distancia de Hamming - numero de quadrados fora da posicao certa
resolve_info_h(L,L) :- transformacao(L,L), !.
resolve_info_h(L1,L2) :-
						transformacao(L1,L2),
    					calcula_h(L1,L2,0,S),
    					resolve_info_h(L1,[L1,S,0,S,[]],L2,[[L1,S,0,S,[]]],[]),!.
resolve_info_h(L,N,L,_,_) :-
    							nth1(5,N,Caminho),
    							escreve_solucao(Caminho),!.
resolve_info_h(_,_,L2,Ab,Fe) :-
    							minimo_f(Ab,I,_),
    							nth1(I,Ab,No),
    							delete(Ab,No,NvAb),
    							append(No,Fe,NvFe),
    							expande_no(No,L2,NvAb,NvFe,RetAb),
    							nth1(1,No,Tabuleiro),
    							resolve_info_h(Tabuleiro,No,L2,RetAb,NvFe).

%no[C,F,G,H,M]:-.

notZero(A):- 0 \= A.

desafio(1) :- resolve_manual([1, 2, 3, 4, 5, 6, 7, 8, 0], [1, 0, 2, 4, 5, 3, 7, 8, 6]).
% input b d e b d

desafio(2) :- resolve_manual([0, 1, 3, 4, 2, 5, 7, 8, 6], [1, 2, 3, 4, 5, 6, 7, 8, 0]).
% input e c e c

desafio(3) :- resolve_cego([1, 2, 3, 4, 5, 6, 7, 8, 0], [1, 0, 2, 4, 5, 3, 7, 8, 6]).

desafio(4) :- resolve_cego([0, 1, 3, 4, 2, 5, 7, 8, 6], [1, 2, 3, 4, 5, 6, 7, 8, 0]).

desafio(5) :- resolve_info_h([1, 2, 3, 4, 5, 6, 7, 8, 0], [1, 0, 2, 4, 5, 3, 7, 8, 6]).

desafio(6) :- resolve_info_h([0, 1, 3, 4, 2, 5, 7, 8, 6], [1, 2, 3, 4, 5, 6, 7, 8, 0]).

