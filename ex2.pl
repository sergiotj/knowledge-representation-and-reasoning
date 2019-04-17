%-----------------------------------------------------------------------------------------------------
% SRCR
% TP2

%-----------------------------------------------------------------------------------------------------
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%-----------------------------------------------------------------------------------------------------
% SICStus PROLOG: definicoes iniciais

:-op(500, fx, '+').
:-op(500, fx, '-').
:-op(400, xfx, '::').
:- dynamic '-'/1.
:- dynamic '+'/1.
:- dynamic '::'/2.


% descrição da estrutura do conhecimento

%utente(IdUt, Nome, Idade, Cidade).
%servico(IdServ, Descricao, Instituicao, Cidade, Capacidade).
%consulta(Data, IdUt, IdServ, Custo).

:- dynamic idUtAtual/1.
:- dynamic utente/4.
:- dynamic idServAtual/1.
:- dynamic servico/4.
:- dynamic consulta/4.
:- dynamic filho/2.

% dados iniciais

idUtAtual(1).
idServAtual(1).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
% 					Representaçao de conhecimento perfeito positivo
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% Extensao do predicado utente: IdUt, Nome, Idade, Cidade -> {V,F,D}
utente(rosalina,34,braga).
utente(maria,23,porto).
utente(jose,23,lisboa).
utente(joana,24,braga).
utente(joao,45,coimbra).

% Extensao do predicado servico: IdServ, Descricao, Instituicao, Cidade, Capacidade -> {V,F,D}
servico(serv1, csjoane      , guimaraes, 3).
servico(serv2, hospitalbraga, braga    , 7).
servico(serv3, hospitalluz  , braga    , 4).
servico(serv4, hospitalluz  , guimaraes, 5).
servico(serv5, uhfamalicao  , famalicao, 2).
servico(serv6, hsantamaria  , porto    , 3).
servico(serv7, htrofa       , braga    , 1).
servico(serv8, htrofa       , braga    , 6).
servico(serv9, hospitalbraga, braga    , 3).

% Extensao do predicado consulta: Data, IdUt, IdServ, Custo -> {V,F,D}
consulta(2015-11-20,1,2,22).
consulta(2016-11-20,1,2,25).
consulta(2016-09-13,3,5,30).
consulta(2017-02-28,4,9,45).


% ////////////////////////////////////////////////////////////////////////////////////////////////////
% 					Representaçao de conhecimento perfeito negativo
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% Definições das negações fortes (não é verdadeiro ^ não é desconhecido)

-utente(Id,N,I,C) :-
					nao(utente(Id,N,I,C)),
					nao(excecao(utente(Id,N,I,C))).

-servico(Id,D,I,C,Cap) :-
					nao(servico(Id,D,I,C,Cap),
					nao(excecao(servico(Id,D,I,C,Cap)).

-consulta(D,IdU,IdServ,C) :-
					nao(consulta(D,IdU,IdServ,C)),
					nao(excecao(consulta(D,IdU,IdServ,C)).

% Negação explícita

-utente(32,joaquim,65,braganca).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
% 					Representar casos de conhecimento imperfeito incerto
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% ----------------------------------------------------------------------------------------------------
% Desconhece-se a cidade do utente com o id=15, que é a Margarida com 19 anos.

utente(15, margarida, 19, i1).

excecao(utente(A,B,C,D)) :-
	utente(A,B,C,i1).

% ----------------------------------------------------------------------------------------------------
% Não se sabe qual a descrição do serviço 11 e a sua capacidade, que é o hospital da covilhã.

servico(11, i2, hospitalcovilha, covilha, i3).

excecao(servico(A, B, C, D, E)) :-
	servico(A, i2, C, D, i3).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
% 					Representar casos de conhecimento imperfeito impreciso
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% ----------------------------------------------------------------------------------------------------
% Não se sabe se a utente com id=11, com o nome Inês, residente na cidade de Lisboa, tem 23 ou 24 anos.

excecao(utente(11,ines,23,lisboa)).
excecao(utente(11,ines,24,lisboa)).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
% 					Representar casos de conhecimento imperfeito interdito
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% ////////////////////////////////////////////////////////////////////////////////////////////////////
% Manipular invariantes que designem restrições à inserção e à remoção de conhecimento do sistema
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% ////////////////////////////////////////////////////////////////////////////////////////////////////
% ////////////////////////////////////// Predicados importantes //////////////////////////////////////
% ////////////////////////////////////////////////////////////////////////////////////////////////////


% operadores 'e' e 'ou'
:- op(600, 'yfx', [ e, ou ]).
:- dynamic e/2.
:- dynamic ou/2.

% dupla negação
si(-(-Q), R) :-
    si(Q, R), !.

% lei de De Morgan (A ou B) <=> -(-A e -B)
si(Q1 ou Q2, R) :-
    si(-(-Q1 e -Q2), R), !.

si(Q1 e Q2, verdadeiro) :-
    si(Q1, verdadeiro),
    si(Q2, verdadeiro).
si(Q1 e _, falso) :-
    si(Q1, falso), !.
si(_ e Q2, falso) :-
    si(Q2, falso).
si(Q1 e Q2, desconhecido) :-
    si(Q1, desconhecido),
    nao(si(Q2, falso)), !.
si(Q1 e Q2, desconhecido) :-
    si(Q2, desconhecido),
    nao(si(Q1, falso)).

si(Q, verdadeiro) :-
    atomico(Q),
    Q.
si(Q, falso) :-
    atomico(Q),
    -Q.
si(Q, desconhecido) :-
    atomico(Q),
    nao(Q),
    nao(-Q).


% ////////////////////////////////////////////////////////////////////////////////////////////////////
% 										Evolução do Conhecimento
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% ////////////////////////////////////////// Predicados Extra ////////////////////////////////////////
% ----------------------------------------------------------------------------------------------------

nao(X) :-
    X, !, fail;
    true.

pertence(X, [X | _]).
pertence(X, [Y | T]) :-
    pertence(X, T).

comprimento([], 0).
comprimento([_|L], N2) :-
    comprimento(L, N),
    N2 is N + 1.

sum([], 0).
sum([X | L], S) :-
    sum(L, S2),
    S is S2 + X.

solucoes(F, Q, R) :-
    findall(F, Q, R).

removeDups([],[]).
removeDups([H|T],R) :-
	pertence(H,T),
	removeDups(T,R).
removeDups([H|T],[H|R]) :-
	nao(pertence(H,T)),
	removeDups(T,R).

atomico(Q) :-
    Q \= _ e _,
    Q \= _ ou _,
    Q \= -(-_).

% ////////////////////////////////////// FIM DO FICHEIRO /////////////////////////////////////////////
