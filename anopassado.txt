%-----------------------------------------------------------------------------------------------------
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3
% Exercicio prático 1

%-----------------------------------------------------------------------------------------------------
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%-----------------------------------------------------------------------------------------------------
% SICStus PROLOG: definicoes iniciais

:- op(900,xfy,'::').

% Para os invariantes:

:- dynamic utente/4.
:- dynamic prestador/4.
:- dynamic cuidado/5.
:- dynamic recibo/9.

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado utente: IdUt,Nome,Idade,Morada -> {V,F}

utente(1,carlos,21,famalicao).
utente(2,diana,20,trofa).
utente(3,marcos,20,anais).
utente(4,vitor,20,guimaraes).
utente(5,antonio,6,povoadevarzim).
utente(6,rita,26,pontedelima).
utente(7,marta,66,guimaraes).
utente(8,paulo,16,barcelos).
utente(9,maria,43,barcelos).
utente(10,luis,51,vizela).

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado prestador: IdPrest,Nome,Especialidade,Instituicao -> {V,F}

prestador(1, pres1, pediatria, csjoane).
prestador(2, pres2, cardiologia, hospitalbraga).
prestador(3, pres3, cirurgia, hospitalbraga).
prestador(4, pres4, ginecologia, hospitalbraga).
prestador(5, pres5, neurologia, hsmm).
prestador(6, pres6, psiquiatria, hsog).
prestador(7, pres7, oftamologia, htrofa).
prestador(8, pres8, reumatologia, htrofa).
prestador(9, pres9, psiquiatria, hospitalbraga).

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado cuidado: Data,IdUt,IdPrest,Descricao,Custo -> {V,F}

cuidado(01-02-2017, 1, 6, hipnose, 15).
cuidado(13-02-2017, 3, 4, papanico, 30).
cuidado(13-02-2017, 2, 5, cerebrotroca, 30).
cuidado(14-02-2017, 2, 7, olhoterapia, 7).
cuidado(20-03-2017, 4, 2, pacemaker, 20).
cuidado(02-04-2017, 7, 4, ovariologia, 5).
cuidado(03-04-2017, 3, 5, neuroterapia, 25).
cuidado(20-04-2017, 6, 7, retinoterapia, 35).
cuidado(22-05-2017, 6, 2, cardioterapia, 55).
cuidado(04-06-2017, 2, 2, cardiograma, 99).
cuidado(15-06-2017, 1, 1, terapiafala, 5).
cuidado(18-06-2017, 2, 8, reumatomagrafia, 350).
cuidado(18-06-2017, 2, 9, cbt, 10).

% //////////////////////////////////////////////// Ponto 1 ///////////////////////////////////////////
%-----------------------------------------------------------------------------------------------------
% Invariante que năo permite a inserçăo de conhecimento de um utente com um id já existente
% uso _ quando o dado năo é importante e năo quero saber dele

+utente(Id, N, I, M) :: (solucoes(Id, utente(Id,_,_,_), S),
						comprimento(S,L),
						L==1).

%-----------------------------------------------------------------------------------------------------
% Invariante que năo permite a inserçăo de conhecimento de um prestador com um id já existente

+prestador(Id, N, E, I) :: (solucoes(Id, prestador(Id,_,_,_), S),
							comprimento(S, L),
							L == 1).

%-----------------------------------------------------------------------------------------------------
% Invariante que năo permite a inserçăo de conhecimento de um cuidado quando o id do utente/prestador
% năo existem na base de conhecimento

+cuidado(Dat,U,P,D,C) :: (solucoes(P, prestador(P, _, _, _), LisP),
						comprimento(LisP, NumP),
						NumP == 1,
						solucoes(U, utente(U, _, _, _), LisU),
						comprimento(LisU, NumU),
						NumU == 1).

% //////////////////////////////////////////////// Ponto 2 ///////////////////////////////////////////
%-----------------------------------------------------------------------------------------------------
% Invariante que năo permite a remocao de conhecimento de um utente năo presente na base de conhecimento
% e com id associado a cuidado

-utente(Id, N, I, M) :: (solucoes(Id, utente(Id,N,I,M), Uts),
						comprimento(Uts, Lu),
						Lu==1,
						solucoes(Id, cuidado(_,Id,_,_,_), Cuids),
						comprimento(Cuids, Lc),
						Lc == 0).

%-----------------------------------------------------------------------------------------------------
% Invariante que năo permite a remocao de conhecimento de um prestador năo presente na base de
% conhecimento e com o id associado a cuidado

-prestador(Id, N, E, I) :: (solucoes(Id, prestador(Id,_,_,_), Prests),
							comprimento(Prests, Lp),
							Lp == 1,
							solucoes(Id, cuidado(_,_,Id,_,_), Cuids),
							comprimento(Cuids, Lc),
							Lc == 0).

%-----------------------------------------------------------------------------------------------------
% Invariante que năo permite a remocao de conhecimento de um cuidado năo presente na base de conhecimento

-cuidado(Dat, U, P, D, C) :: (solucoes((Dat,U,P,D,C), cuidado(Dat,U,P,D,C), Cuids),
							comprimento(Cuids, L),
							L == 1).

% //////////////////////////////////////////////// Ponto 3 ///////////////////////////////////////////
%-----------------------------------------------------------------------------------------------------
% Extensao do predicado utentesPNome: Nome,Lis -> {V,F}

utentesPNome(Nome, Lis) :-
	solucoes((IdUt,Nome,Idade,Morada), utente(IdUt,Nome,Idade,Morada), Lis).

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado utentesPIdade: Idade,Lis -> {V,F}

utentesPIdade(Idade, Lis) :-
	solucoes((IdUt,Nome,Idade,Morada), utente(IdUt,Nome,Idade,Morada), Lis).

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado utentesPMorada: Morada,Lis -> {V,F}

utentesPMorada(Morada,Lis) :-
	solucoes((IdUt,Nome,Idade,Morada), utente(IdUt,Nome,Idade,Morada), Lis).

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado utentesPId: Idade,Lis -> {V,F}

utentesPId(IdUt, Lis) :-
	solucoes((IdUt,Nome,Idade,Morada), utente(IdUt,Nome,Idade,Morada), Lis).

% //////////////////////////////////////////////// Ponto 4 ///////////////////////////////////////////
% ----------------------------------------------------------------------------------------------------
% Extensao do predicado instituicoes: Resultado -> {V,F}

instituicoes(Resultado) :-
	solucoes(Instituicao, prestador(_,_,_,Instituicao), Insts),
	removeDup(Insts,Resultado).

% //////////////////////////////////////////////// Ponto 5 ///////////////////////////////////////////
% ----------------------------------------------------------------------------------------------------


% Extensao do predicado cuidadosPInstituicao: Inst,Result -> {V,F}

cuidadosPInstituicao(Inst,List) :-
	solucoes(IdPrest,prestador(IdPrest,Nome,Esp,Inst),L),
	cuidadosPInstituicaoAux(L,List).


% Extensao do predicado cuidadosPInstituicaoAux: L,Result -> {V,F}

cuidadosPInstituicaoAux([],[]).
cuidadosPInstituicaoAux([H|T],List) :-
	cuidadosDePrestador(H,Aux1),
	cuidadosPInstituicaoAux(T,Aux2),
	concatena(Aux1,Aux2,List).


% ----------------------------------------------------------------------------------------------------
% Extensao do predicado cuidadosDePrestador: IdPrest,R -> {V,F}

cuidadosDePrestador(IdPrest,R):-
	solucoes((Data,IdUt,IdPrest,Desc,Custo),cuidado(Data,IdUt,IdPrest,Desc,Custo),R).


% ----------------------------------------------------------------------------------------------------

% Extensao do predicado cuidadosPCidade: Cidade,Result -> {V,F}

cuidadosPCidade(Cidade,List) :-
	solucoes(U, utente(U,_,_,Cidade), Uts),
	getCuidadosPCidadeAux(Uts,ListCuid),
	removeDup(ListCuid, List).

% Extensao do predicado cuidadosPCidadeAux: L,Resultado -> {V,F}
% Recebe lista de IDs de utentes
% Obtém lista de instituições desses utentes

getCuidadosPCidadeAux([], []).
getCuidadosPCidadeAux([IdUtente | T], [Descricao | Resto]) :-
	cuidado(_, IdUtente, _, Descricao, _),
	getCuidadosPCidadeAux(T, Resto).


% ----------------------------------------------------------------------------------------------------
% Extensao do predicado cuidadosPDatas: Data,Result -> {V,F}

cuidadosPDatas(Data,List) :-
	solucoes((Data,Ut,Prest,Desc,Cust), cuidado(Data,Ut,Prest,Desc,Cust), List).


% //////////////////////////////////////////////// Ponto 6 ///////////////////////////////////////////
% ----------------------------------------------------------------------------------------------------
% Extensao do predicado utentesDePrestador: IdPrest,Lis -> {V,F}

utentesDePrestador(IdPrest,Lis):-
	solucoes(IdPrest,prestador(IdPrest,Nome,Esp,Inst),Aux1),
	utentesDePrestadorAux(Aux1,Aux2),
	utentesPListaId(Aux2,Lis).

utentesDePrestadorAux([],[]).
utentesDePrestadorAux([H|T],Lis):-
	solucoes(IdUt,cuidado(_,IdUt,H,_,_),Aux1),
	utentesDePrestadorAux(T,Aux2),
	concatena(Aux1,Aux2,Aux3),
	removeDup(Aux3,Lis).

utentesPListaId([],[]).
utentesPListaId([H|T],Lis):-
	utentesPId(H,Aux1),
	utentesPListaId(T,Aux2),
	concatena(Aux1,Aux2,Lis).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado utentesDeEspecialidade: Esp,Lis -> {V,F}

utentesDeEspecialidade(Esp,Lis):-
	solucoes(IdPrest,prestador(IdPrest,Nome,Esp,Inst),Aux1),
	utentesDePrestadorAux(Aux1,Aux2),
	utentesPListaId(Aux2,Lis).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado utentesDeInstituicao: Inst,Lis -> {V,F}

utentesDeInstituicao(Inst,Lis):-
	solucoes(IdPrest,prestador(IdPrest,Nome,Esp,Inst),Aux1),
	utentesDePrestadorAux(Aux1,Aux2),
	utentesPListaId(Aux2,Lis).

% //////////////////////////////////////////////// Ponto 7 ///////////////////////////////////////////
% ----------------------------------------------------------------------------------------------------
% Extensao do predicado cuidadosDeUtentes: IdUt,R -> {V,F}

cuidadosDeUtentes(IdUt,R):-
	solucoes((Data,IdUt,IdPrest,Desc,Custo),cuidado(Data,IdUt,IdPrest,Desc,Custo),R).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado cuidadosDeInstituicao: IdUt,R -> {V,F}

cuidadosDeInstituicao(Inst,Lis):-
	solucoes(IdPrest,prestador(IdPrest,Nome,Esp,Inst),Aux),
	cuidadosDeInstituicaoAux(Aux,Lis).

cuidadosDeInstituicaoAux([],[]).
cuidadosDeInstituicaoAux([H|T],Lis):-
	cuidadosDePrestador(H,Aux1),
	cuidadosDeInstituicaoAux(T,Aux2),
	concatena(Aux1,Aux2,Lis).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado cuidadosDePrestador: IdPrest,R -> {V,F}

cuidadosDePrestador(IdPrest,R):-
	solucoes((Data,IdUt,IdPrest,Desc,Custo),cuidado(Data,IdUt,IdPrest,Desc,Custo),R).

% //////////////////////////////////////////////// Ponto 8 ///////////////////////////////////////////
% Enunciado: Determinar todas as instituições/prestadores a que um utente já recorreu;

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getInstPrestByUtente: Id,Resultado -> {V,F}
%
% getInstPrestByUtente(IdUtente, R)
%   IdUtente: ID do utente
%   R: Lista de pares (Instituição, IdPrestador)
%
% Obtém IDs de prestadores que estão registados em cuidados do utente
% Usa predicado adicional para obter (instituição, prestador) a partir de IDs de prestadores
getInstPrestByUtente(IdUtente, R) :-
	solucoes(IdPrestador, cuidado(_, IdUtente, IdPrestador, _, _), S),
	removeDup(S, Resultado),
	getInstPrestByPrestadores(Resultado, R).

% Extensao do predicado getInstPrestByPrestadores: L,Resultado -> {V,F}
%
% Recebe lista de IDs de prestadores
% Obtém lista de (Instituicao, IdPrestador) desses prestadores
getInstPrestByPrestadores([], []).
getInstPrestByPrestadores([IdPrestador | T], [(Instituicao, IdPrestador) | Resto]) :-
	prestador(IdPrestador, _, _, Instituicao),
	getInstPrestByPrestadores(T, Resto).

% //////////////////////////////////////////////// Ponto 9 ///////////////////////////////////////////
% Enunciado: Calcular o custo total dos cuidados de saúde por utente/especialidade/prestador/datas;

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getTotalByUtente: IdUtente, Total -> {V,F}
getTotalByUtente(IdUtente, T) :-
	solucoes(Custo, cuidado(_, IdUtente, _, _, Custo), S),
	somatorio(S, T).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getTotalByPrestador: IdPrestador, Total -> {V,F}
getTotalByPrestador(IdPrestador, T) :-
	solucoes(Custo, cuidado(_, _, IdPrestador, _, Custo), S),
	somatorio(S, T).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getTotalByPrestadores: IdPrestador[], Total -> {V,F}
getTotalByPrestadores([], 0).
getTotalByPrestadores([IdPrestador | L], T) :-
	getTotalByPrestador(IdPrestador, Custo),
	getTotalByPrestadores(L, RestoDoTotal),
	T is RestoDoTotal + Custo.

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getTotalByData: Data, Total -> {V,F}
getTotalByData(Data, T) :-
	solucoes(Custo, cuidado(Data, _, _, _, Custo), S),
	somatorio(S, T).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getTotalByDatas: Data[], Total -> {V,F}
getTotalByDatas([], 0).
getTotalByDatas([Data | L], T) :-
	getTotalByData(Data, Custo),
	getTotalByDatas(L, RestoDoTotal),
	T is RestoDoTotal + Custo.

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getTotalByEspecialidade: Especialidade, Total -> {V,F}
getTotalByEspecialidade(Especialidade, T) :-
	solucoes(IdPrest, prestador(IdPrest, _, Especialidade, _), Prestadores),
	getTotalByPrestadores(Prestadores, T).

% ////////////////////////////////////////////// Ponto Extra /////////////////////////////////////////
%-----------------------------------------------------------------------------------------------------
% Extensao do predicado recibo: IdRecibo, IdUt, NomeUt, Morada, Especialidade, Instituicao, Data, Descricao, Custo -> {V,F}

recibo(1, 3, marcos, anais, ginecologia, hospitalbraga, 13-02-2017, papanico, 30).
recibo(2, 4, vitor, guimaraes, cardiologia, hospitalbraga, 20-03-2017, pacemaker, 20).

%-----------------------------------------------------------------------------------------------------
% Invariante que năo permite a inserçăo de conhecimento de um recibo com um id já existente
% uso _ quando o dado năo é importante e năo quero saber dele

+recibo(Id, U, N, M, E, I, Dat, D, C) :: (solucoes(Id, recibo(Id,_,_,_,_,_,_,_,_), R),
					comprimento(R,Lr),
					Lr==1,
					solucoes((U,N,M), utente(U,N,_,M), Uts),
					comprimento(Uts,Lu),
					Lu==1,
					solucoes((P,E,I), prestador(P,_,E,I), Pres),
					comprimento(Pres, Lp),
					Lp>=1,
					solucoes((Dat,U,P,D,C), cuidado(Dat,U,P,D,C), Cuids),
					comprimento(Cuids, Lc),
					Lc==1).

%-----------------------------------------------------------------------------------------------------
% Invariante que não permite a remoção de qualquer recibo, uma vez que a informação
% financeira nunca pode ser eliminada. Anti-fraude.

-recibo(Id, U, N, M, E, I, Dat, D, C) :: fail.

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado utentesQGastaramMaisQX: Valor,Resultado -> {V,F}

utentesQGastaramMaisQX(Valor,R1) :-
	solucoes((U,C), recibo(_,U,_,_,_,_,_,_,C), Resultado),
	utentesQGastaramMaisQXAux(Resultado,Valor,R),
	removeDup(R,R1).

% Extensao do predicado utentesQGastaramMaisQXAux: Lista,Valor,Resultado -> {V,F}

utentesQGastaramMaisQXAux([],Valor,[]).
utentesQGastaramMaisQXAux([(IdUt,Custo)|T],Valor,L):-
	Custo=<Valor,utentesQGastaramMaisQXAux(T,Valor,L).
utentesQGastaramMaisQXAux([(IdUt,Custo)|T],Valor,[IdUt|L]):-
	Custo>Valor,utentesQGastaramMaisQXAux(T,Valor,L).

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado listarUtentesMaisFreq: Resultado -> {V,F}

listarUtentesMaisFreq(Resultado) :-
	solucoes(IdUt, utente(IdUt,_,_,_), R),
	solucoes(IdUt, recibo(_,IdUt,_,_,_,_,_,_,_), R2),
	listarUtentesMaisFreqAux(R,R2,R3),
	ordenarDecresc(R3,Resultado).

% Extensao do predicado listarUtentesMaisFreqAux: Utentes,Resultado -> {V,F}
% Esta funcao pega em cada elemento da 1Ş lista e verifica quantas vezes aparece na segunda, no fim devolve um par com cada elemento e o nr de vezes q apareceu.

listarUtentesMaisFreqAux([],L,[]).
listarUtentesMaisFreqAux([H|T],L,[(H,Q)|R]):-
	quantosTem(H,L,Q),listarUtentesMaisFreqAux(T,L,R).

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado gastoRegistadoPorUtente: Utente, Resultado -> {V,F}

gastoRegistadoPorUtente(U,R) :-
	solucoes(C, recibo(_,U,_,_,_,_,_,_,C), Custos),
	somatorio(Custos, R).

% ////////////////////////////////////////// Predicados Extra /////////////////////////////////////////
% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado inserir: Termo -> {V,F}
% mesmo que EVOLUCAO

inserir(Termo) :-
	solucoes(Invariante, +Termo::Invariante, Lista),
	insercao(Termo),
	teste(Lista).

% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado remover: Termo -> {V,F}

remover(Termo) :-
	solucoes(Invariante, -Termo::Invariante, Lista),
	teste(Lista),
	remocao(Termo).

% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado insercao: Termo -> {V,F}

insercao(T) :-
	assert(T).
insercao(T) :-
	retract(T),!,fail.

% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado remocao: Termo -> {V,F}

remocao(T) :-
	retract(T).
remocao(T) :-
	assert(T),!,fail.

% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado teste: Lista -> {V,F}

teste([]).
teste([I|L]) :-
	I,
	teste(L).

% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado solucoes: X,Y,Z -> {V,F}

solucoes(X,Y,Z) :-
	findall(X,Y,Z).

% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado comprimento: Lista, Resultado -> {V,F}

comprimento(X,Z):-
	length(X,Z).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado somatorio: lista, resultado -> {V,F}

somatorio([], 0).
somatorio([X|Y], R) :-
	somatorio(Y,G),
	R is X+G.

% Extensao do predicado nao: Q -> {V,F}

nao(Q):- Q, !, fail.
nao(Q).

% Extensao do predicado quantosTem:A,Lista,Resultado  -> {V,F}

quantosTem(A,[],0).
quantosTem(A,[H|T],Resultado):-
	(A == H), quantosTem(A,T,R), Resultado is R+1.
quantosTem(A,[H|T],Resultado):-
	(A \= H), quantosTem(A,T,Resultado).

% Extensao do predicado pertence: X,L -> {V,F}

pertence(X,[]):- fail.
pertence(X,[X|T]):- X==X.
pertence(X,[H|T]):-
	X\=H,
	pertence(X,T).

% Extensao do predicado removeDup: L,R -> {V,F}

removeDup([],[]).
removeDup([X|T],R):-
	pertence(X,T),
	removeDup(T,R).
removeDup([X|T],[X|R]):-
	nao(pertence(X,T)),
	removeDup(T,R).

% Extensao do predicado ordenarDecresc: L,Resultado -> {V,F}

ordenarDecresc([X],[X]).
ordenarDecresc([X|Y],T):-
	ordenarDecresc(Y,R),
	insereOrdenado(X,R,T).

% Extensao do predicado insereOrdenado: X,L,Resultado  -> {V,F}

insereOrdenado((X1,Y1),[],[(X1,Y1)]).
insereOrdenado((X1,Y1),[(X2,Y2)|Z],[(X1,Y1)|[(X2,Y2)|Z]]):-
	Y1>Y2.
insereOrdenado((X1,Y1), [(X2,Y2)|Z], [(X2,Y2)|R2]) :-
	Y1=<Y2,
	insereOrdenado((X1,Y1),Z,R2).

% Extensao do predicado concatena(L1,L2,L3)->{V,F}

concatena([],L2,L2).
concatena([X|L1],L2,[X|L]) :-
	concatena(L1,L2,L).
