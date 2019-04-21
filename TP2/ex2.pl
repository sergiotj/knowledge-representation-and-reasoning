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
:- dynamic excecao/1.
:- dynamic interdito/1.
:- dynamic excecaoInc/1.
:- dynamic incerto/1.

% dados iniciais

idUtAtual(1).
idServAtual(1).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                   Representaçao de conhecimento perfeito positivo
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
%                   Representaçao de conhecimento perfeito negativo
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% Definições das negações fortes (não é verdadeiro ^ não é desconhecido)

-utente(Id,N,I,C) :-
                    nao(utente(Id,N,I,C)),
                    nao(excecao(utente(Id,N,I,C))),
                    nao(excecaoInc(utente(Id,N,I,C))).

-servico(Id,D,I,C,Cap) :-
                    nao(servico(Id,D,I,C,Cap)),
                    nao(excecao(servico(Id,D,I,C,Cap))),
                    nao(excecaoInc(servico(Id,D,I,C,Cap))).

-consulta(D,IdU,IdServ,C) :-
                    nao(consulta(D,IdU,IdServ,C)),
                    nao(excecao(consulta(D,IdU,IdServ,C))),
                    nao(excecaoInc(consulta(D,IdU,IdServ,C))).

% Negação explícita

-utente(32,joaquim,65,braganca).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                   Representar casos de conhecimento imperfeito incerto
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% ----------------------------------------------------------------------------------------------------
% Desconhece-se a cidade do utente com o id=15, que é a Margarida com 19 anos.

utente(15, margarida, 19, i1).

excecaoInc(utente(A,B,C,D)) :-
    utente(A,B,C,i1).

% ----------------------------------------------------------------------------------------------------
% Não se sabe qual a descrição do serviço 11 e a sua capacidade, que é o hospital da covilhã.

servico(11, i2, hospitalcovilha, covilha, i3).

excecaoInc(servico(A, B, C, D, E)) :-
    servico(A, i2, C, D, i3).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                   Representar casos de conhecimento imperfeito impreciso
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% ----------------------------------------------------------------------------------------------------
% Não se sabe se a utente com id=11, com o nome Inês, residente na cidade de Lisboa, tem 23 ou 24 anos.

excecaoInc(utente(11,ines,23,lisboa)).
excecaoInc(utente(11,ines,24,lisboa)).

% ----------------------------------------------------------------------------------------------------
% Desconhece-se se o servico com id=10, serv10 do Hospital de Santa Maria no Porto tem capacidade 4 ou
% capacidade 5
excecaoInc(servico(10,serv10, hsantamaria, porto, 4)).
excecaoInc(servico(10,serv10, hsantamaria, porto, 5)).

% ----------------------------------------------------------------------------------------------------
% Não se sabe se a consulta prestada em 25-12-2018, pelo servico com id=8 ao utente com id=3
% teve um custo de 10€ ou um custo de 15€
excecaoInc(consulta(25-12-2018, 3, 8, 10) ).
excecaoInc(consulta(25-12-2018, 3, 8, 15) ).

% ----------------------------------------------------------------------------------------------------
% Desconhece-se se a consulta prestada pelo servico com id=5 ao utente com id=8 foi
% no dia 01 de Abril ou no dia 02 de Abril, e se o custo foi 20€ ou 30€.
excecaoInc(consulta(01-04-2019, 8, 5, 20)).
excecaoInc(consulta(02-04-2019, 8, 5, 20)).
excecaoInc(consulta(01-04-2019, 8, 5, 30)).
excecaoInc(consulta(02-05-2019, 8, 5, 30)).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                   Representar casos de conhecimento imperfeito interdito
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% ----------------------------------------------------------------------------------------------------
% Não se pode saber qual o IdUt da consulta realizada em 2019-01-30, prestada pelo serviço com id = 3
% com o custo 25€

consulta(2019-01-30, i5, 3, 25).
excecao(consulta(A, B, C, D)) :-
	cuidado(A, i5, C, D).
nulointerdito(i5).
+consulta(A, B, C, D) ::    (solucoes(
                                        (A, Interdito, C, D),
                                        (consulta(2019-01-30, Interdito, 3, 25), nao(nulointerdito(Interdito))),
                                        List
								),
                 		  		comprimento(List, N),
                 		  		N == 0
                 		  	).

% ----------------------------------------------------------------------------------------------------
% Nunca se poderá saber, a partir do recibo com id=4, em que instituição foi prestado um peeling químico,
% na área da dermatologia, ao utente com id=7 e intitulado Marta, em 04-12-2017, com o custo de 30€.
recibo( 4, 7, marta, guimaraes, dermatologia, i6, 04-12-2017, peelingquimico, 30 ).

% Nunca se poderá saber qual a capacidade, a partir do serviço com id = 15, que corresponde ao servico
% com descricao serv15, do hospitallisboa, na cidade de Lisboa
servico(serv15, hospitallisboa, lisboa, i6).

excecao(servico(A, B, C, D)) :-
	servico(A, B, C, i6).
nulointerdito(i6).
+recibo(A, B, C, D) :: ( solucoes(
                                    (A, B, C, Interdito),
                                    (servico(serv15, hospitallisboa, lisboa, Interdito), nao(nulointerdito(Interdito))),
                                    List
                        ),
                        comprimento(List, N),
                        N == 0
                        ).

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
%                                       Evolução do Conhecimento
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% predicado evolucao
evolucao( Termo ) :-
    solucoes( Invariante,+Termo::Invariante,Lista ),
    insercao( Termo ),
    teste( Lista ).

insercao( Termo ) :-
    assert( Termo ).
insercao( Termo ) :-
    retract( Termo ),!,fail.

teste( [] ).
teste( [R|LR] ) :-
    R,
    teste( LR ).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                                     Evolução - Tipo Incerto
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% Incerto no nome do utente
evolucao(utente(Id,_,I,C),i,nome) :-
    evolucao(utente(Id,i,I,C)).

% Incerto na idade do utente
evolucao(utente(Id,N,_,C),i,idade) :-
    evolucao(utente(Id,N,i,C)).

% Incerto na cidade do utente
evolucao(utente(Id,N,I,_),i,cidade) :-
    evolucao(utente(Id,N,I,i)).

% -----
% Incerto na descrição do serviço
evolucao(servico(Id,_,I,C,Cap),i,desc) :-
    evolucao(servico(Id,i,I,C,Cap)).

% Incerto na instituição do serviço
evolucao(servico(Id,D,_,C,Cap),i,inst) :-
    evolucao(servico(Id,D,i,C,Cap)).

% Incerto na cidade do serviço
evolucao(servico(Id,D,I,_,Cap),i,cidade) :-
    evolucao(servico(Id,D,I,i,Cap)).

% -----
% Incerto na data da consulta
evolucao(consulta(_,IdUt,IdServ,C),i,data):-
    evolucao(consulta(i,IdUt,IdServ,C)).

% Incerto no custo da consulta
evolucao(consulta(D,IdUt,IdServ,_),i,custo):-
    evolucao(consulta(D,IdUt,IdServ,i)).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                                     Evolução - Tipo Impreciso
% ////////////////////////////////////////////////////////////////////////////////////////////////////

evolucaoImpre(_,_,[]).

% Impreciso na cidade do utente
evolucaoImpre(utente(Id,N,I),cidade,[H|T]) :-
    nao(utente(Id,_,_,_)),
    insercao(excecao(utente(Id,N,I,H))),
    evolucaoImpre(utente(Id,N,I),cidade,T).

% Impreciso na idade do utente
evolucaoImpre(utente(Id,N,C),idade,[H|T]) :-
    nao(utente(Id,_,_,_)),
    insercao(excecao(utente(Id,N,H,C))),
    evolucaoImpre(utente(Id,N,C),idade,T).

% -----
% Impreciso na descrição do serviço
evolucaoImpre(servico(Id,N,C,Cap),desc,[H|T]) :-
    nao(servico(Id,_,_,_,_)),
    insercao(excecao(servico(Id,H,N,C,Cap))),
    evolucaoImpre(servico(Id,N,C,Cap),desc,T).

% Impreciso na cidade do serviço
evolucaoImpre(servico(Id,D,N,Cap),cidade,[H|T]) :-
    nao(servico(Id,_,_,_,_)),
    insercao(excecao(servico(Id,D,N,H,Cap))),
    evolucaoImpre(servico(Id,D,N,Cap),cidade,T).

% -----
% Impreciso na data da consulta
evolucaoImpre(consulta(IdUt,IdServ,C),data,[H|T]) :-
    nao(consulta(_,IdUt,IdServ,_)),
    insercao(excecao(consulta(H,IdUt,IdServ,C))),
    evolucaoImpre(consulta(IdUt,IdServ,C),data,T).

% Impreciso na custo da consulta
evolucaoImpre(consulta(D,IdUt,IdServ),custo,[H|T]) :-
    nao(consulta(_,IdUt,IdServ,_)),
    insercao(excecao(consulta(D,IdUt,IdServ,H))),
    evolucaoImpre(consulta(D,IdUt,IdServ),custo,T).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                                     Evolução - Tipo Interdito
% ////////////////////////////////////////////////////////////////////////////////////////////////////
% Teste de palavra interdita
interdito(interdito).

% Interdito no nome do utente
evolucao(utente(Id,N,I,C),nome) :-
    evolucao(utente(Id,N,I,C)),
    interdito(N).

% Interdito na idade do utente
evolucao(utente(Id,N,I,C),idade) :-
    evolucao(utente(Id,N,I,C)),
    interdito(I).

% Interdito na cidade do utente
evolucao(utente(Id,N,I,C),cidade) :-
    evolucao(utente(Id,N,I,C)),
    interdito(C).

% -----
% Interdito na descrição do serviço
evolucao(servico(Id,D,I,C,Cap),desc) :-
    evolucao(servico(Id,D,I,C,Cap)),
    interdito(D).

% Interdito na instituição do serviço
evolucao(servico(Id,D,I,C,Cap),inst) :-
    evolucao(servico(Id,D,I,C,Cap)),
    interdito(I).

% Interdito na cidade do serviço
evolucao(servico(Id,D,I,C,Cap),cidade) :-
    evolucao(servico(Id,D,I,C,Cap)),
    interdito(C).

% -----
% Interdito na data da consulta
evolucao(consulta(D,IdUt,IdServ,C),data) :-
    evolucao(consulta(D,IdUt,IdServ,C)),
    interdito(D).

% Interdito no custo da consulta
evolucao(consulta(D,IdUt,IdServ,C),custo) :-
    evolucao(consulta(D,IdUt,IdServ,C)),
    interdito(C).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                                       Involução do Conhecimento
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% Predicado involucao para conhecimento perfeito
involucao(Termo) :-
    nao(excecao(Termo)),
    nao(excecaoInc(Termo)),
    solucoes(Invariante,-Termo::Invariante, Lista),
    remove(Termo),
    teste(Lista).

remove(T) :-
    retract(T).
remove(T) :-
    assert(T), !, fail.

retrocesso(T) :-
    T,
    remove(T),
    solucoes(I, -T::I, LInv),
    teste(LInv), !.

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                                     Involução - Tipo Incerto
% ////////////////////////////////////////////////////////////////////////////////////////////////////
incerto(incerto).

% Incerteza na nome do utente
excecaoInc(utente(Id,_,Idade,Cidade)) :-
    utente(Id,incerto,Idade,Cidade).

% Incerteza na idade do utente
excecaoInc(utente(Id,Nome,_,Cidade)) :-
    utente(Id,Nome,incerto,Cidade).

% Incerteza na cidade do utente
excecaoInc(utente(Id,Nome,Idade,_)) :-
    utente(Id,Nome,Idade,incerto).

% -----
% Incerteza na descrição do serviço
excecaoInc(servico(Id,_,Instituicao,Cidade,Capacidade)) :-
    servico(Id,incerto,Instituicao,Cidade,Capacidade).

% Incerteza na instituição do serviço
excecaoInc(servico(Id,Descricao,_,Cidade,Capacidade)) :-
    servico(Id,Descricao,incerto,Cidade,Capacidade).

% Incerteza na cidade do serviço
excecaoInc(servico(Id,Descricao,Instituicao,_,Capacidade)) :-
    servico(Id,Descricao,Instituicao,incerto,Capacidade).

% -----
% Incerteza na data da consulta
excecaoInc(consulta(_,IdUt,IdServ,Custo)) :-
    consulta(incerto,IdUt,IdServ,Custo).

% Incerteza no custo da consulta
excecaoInc(consulta(Data,IdUt,IdServ,_)) :-
    consulta(Data,IdUt,IdServ,incerto).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                                     Involução - Tipo Impreciso
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% Predicado involucao para conhecimento incerto e impreciso
involucaoII(Termo) :-
    excecaoInc(Termo),
    solucoes(Invariante,-Termo::Invariante, Lista),
    remove(excecaoInc(Termo)),
    teste(Lista).

impreciso(impreciso).

% Conhecimento impreciso na nome do utente
excecaoInc(utente(Id,_,Idade,Cidade)) :-
    utente(Id,impreciso,Idade,Cidade).

% Conhecimento impreciso na idade do utente
excecaoInc(utente(Id,Nome,_,Cidade)) :-
    utente(Id,Nome,impreciso,Cidade).

% Conhecimento impreciso na cidade do utente
excecaoInc(utente(Id,Nome,Idade,_)) :-
    utente(Id,Nome,Idade,impreciso).

% -----
% Conhecimento impreciso na descrição do serviço
excecaoInc(servico(Id,_,Instituicao,Cidade,Capacidade)) :-
    servico(Id,impreciso,Instituicao,Cidade,Capacidade).

% Conhecimento impreciso na instituição do serviço
excecaoInc(servico(Id,Descricao,_,Cidade,Capacidade)) :-
    servico(Id,Descricao,impreciso,Cidade,Capacidade).

% Conhecimento impreciso na cidade do serviço
excecaoInc(servico(Id,Descricao,Instituicao,_,Capacidade)) :-
    servico(Id,Descricao,Instituicao,impreciso,Capacidade).

% -----
% Conhecimento impreciso na data da consulta
excecaoInc(consulta(_,IdUt,IdServ,Custo)) :-
    consulta(impreciso,IdUt,IdServ,Custo).

% Conhecimento impreciso no custo da consulta
excecaoInc(consulta(Data,IdUt,IdServ,_)) :-
    consulta(Data,IdUt,IdServ,impreciso).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                                     Involução - Tipo Interdito
% ////////////////////////////////////////////////////////////////////////////////////////////////////

involucaoInterdito(Termo) :-
    excecaoInc(Termo),
    solucoes(Invariante,-Termo::Invariante, Lista),
    remove(Termo),
    teste(Lista).

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
