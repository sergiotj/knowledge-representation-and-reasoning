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
:- dynamic servico/5.
:- dynamic consulta/4.
:- dynamic excecao/1.
:- dynamic interdito/1.
:- dynamic excecaoInc/1.
:- dynamic incerto/1.

% dados iniciais

idUtAtual(6).
idServAtual(10).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                   Representaçao de conhecimento perfeito positivo
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% Extensao do predicado utente: IdUt, Nome, Idade, Cidade -> {V,F,D}
utente(1,rosalina,34,braga).
utente(2,maria,23,porto).
utente(3,jose,23,lisboa).
utente(4,joana,24,braga).
utente(5,joao,45,coimbra).

% Extensao do predicado servico: IdServ, Descricao, Instituicao, Cidade, Capacidade -> {V,F,D}
servico(1,serv1, csjoane      , guimaraes, 3).
servico(2,serv2, hospitalbraga, braga    , 7).
servico(3,serv3, hospitalluz  , braga    , 4).
servico(4,serv4, hospitalluz  , guimaraes, 5).
servico(5,serv5, uhfamalicao  , famalicao, 2).
servico(6,serv6, hsantamaria  , porto    , 3).
servico(7,serv7, htrofa       , braga    , 1).
servico(8,serv8, htrofa       , braga    , 6).
servico(9,serv9, hospitalbraga, braga    , 3).

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

excecaoInc(utente(11,ines,23,lisboa) ).
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
% Desconhece-se se a consulta no dia 1 de Abril ao utente com id=8 foi
% prestada pelo servico com id = 5 ou pelo servico com id = 7, e se o custo foi 20€ ou 30€.
excecaoInc(consulta(01-04-2019, 8, 5, 20)).
excecaoInc(consulta(02-04-2019, 8, 7, 20)).
excecaoInc(consulta(01-04-2019, 8, 5, 30)).
excecaoInc(consulta(02-05-2019, 8, 7, 30)).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                   Representar casos de conhecimento imperfeito interdito
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% ----------------------------------------------------------------------------------------------------
% Não se pode saber qual o IdUt da consulta realizada em 2019-01-30, prestada pelo serviço com id = 3
% com o custo 25€

consulta(2019-01-30, i5, 3, 25).
excecao(consulta(A, B, C, D)) :-
	consulta(A, i5, C, D).
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
% Nunca se poderá saber qual a capacidade, a partir do serviço com id = 15, que corresponde ao servico
% com descricao serv15, do hospitallisboa, na cidade de Lisboa
servico(15, serv15, hospitallisboa, lisboa, i6).

excecao(servico(A, B, C, D, E)) :-
	servico(A, B, C, D, i6).
nulointerdito(i6).
+servico(A, B, C, D, E) :: ( solucoes(
                                    (A, B, C, D, Interdito),
                                    (servico(15, serv15, hospitallisboa, lisboa, Interdito), nao(nulointerdito(Interdito))),
                                    List
                        ),
                        comprimento(List, N),
                        N == 0
                        ).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
% Manipular invariantes que designem restrições à inserção e à remoção de conhecimento do sistema
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% Inserção:
%% Não permitir dois utentes com o mesmo ID

+utente(Id,Nome,I,C) :: (solucoes( Id, (utente(Id, _, _, _)), S ),
                      comprimento( S,N ),
				              N == 1).

%% Não permitir dois serviços com o mesmo ID;
%% não permitir dois serviços com a mesma descrição no mesmo hospital.

+servico(Id,D,I,C,Cap) :: (solucoes( Id, (servico(Id, _, _, _, _)), S1 ),
                           comprimento( S1,N1 ),
     				               N1 == 1,
                           solucoes( (Id,D,I), (servico(_, D, I, _, _)), S2 ),
                           comprimento( S2,N2 ),
 				                   N2 == 1).


%% Não permitir duas consultas com o mesmo IdU e IdServ na mesma data;
%% não permitir uma consulta com um IdU não existente;
%% não permitir uma consulta com um IdServ não existente.

+consulta(D,IdU,IdServ,C) :: (solucoes( (D,IdU,IdServ), (consulta(D, IdU, IdServ, _)), S1 ),
                              comprimento( S1,N1 ),
        				              N1 == 1,
                              solucoes( IdU, (utente(IdU, _, _, _)), S2 ),
                              comprimento( S2,N2 ),
        				              N2 == 1,
                              solucoes( IdServ, (servico(IdServ, _, _, _, _)), S3 ),
                              comprimento( S3,N3 ),
  				                    N3 == 1).


% Remoção:
%% Não permitir remover um utente que tenha consultas associadas.

-utente(Id,Nome,I,C) :: (solucoes( IdServ, (consulta(_, Id, IdServ, _)), S ),
                        comprimento( S,N ),
  				              N == 0).


%% Não permitir remover um serviço que tenha consultas associadas.


-servico(Id,D,I,C,Cap) :: (solucoes( Data, (consulta(Data, IdU, Id, _)), S ),
                          comprimento( S,N ),
    				              N == 0).

%% Não há restrições à remoção de consultas.

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

nextIdUt(Id) :-
    retrocesso(idUtAtual(Id)),
    Id2 is Id + 1,
    evolucao(idUtAtual(Id2)).

nextIdServ(Id) :-
    retrocesso(idServAtual(Id)),
    Id2 is Id + 1,
    evolucao(idServAtual(Id2)).

teste( [] ).
teste( [R|LR] ) :-
    R,
    teste( LR ).

excecaoInc(utente(Id,Nome,Idade,Cidade)) :-
    utente(Id,incerto,Idade,Cidade).

excecaoInc(utente(Id,Nome,Idade,Cidade)) :-
    utente(Id,Nome,incerto,Cidade).

excecaoInc(utente(Id,Nome,Idade,Cidade)) :-
    utente(Id,Nome,Idade,incerto).

excecaoInc(servico(Id,Descricao,Instituicao,Cidade,Cap)) :-
    utente(Id,incerto,Instituicao,Cidade,Cap).

excecaoInc(servico(Id,Descricao,Instituicao,Cidade,Cap)) :-
    utente(Id,Descricao,incerto,Cap).

excecaoInc(servico(Id,Descricao,Instituicao,Cidade,Cap)) :-
    utente(Id,Descricao,Instituicao,incerto,Cap).

excecaoInc(consulta(Data,IdUt,IdServ,Custo)) :-
    consulta(incerto,IdUt,IdServ,Custo).

excecaoInc(consulta(Data,IdUt,IdServ,Custo)) :-
    consulta(Data,IdUt,IdServ,incerto).

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

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                                     Evolução - Tipo Incerto
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% Incerto no nome do utente
evolucao(utente(_,I,C),incerto,nome) :-
    nextIdUt(Id),
    evolucao(excecaoInc(utente(Id,incerto,I,C))).

% Incerto na idade do utente
evolucao(utente(N,_,C),incerto,idade) :-
    nextIdUt(Id),
    evolucao(excecaoInc(utente(Id,N,incerto,C))).

% Incerto na cidade do utente
evolucao(utente(N,I,_),incerto,cidade) :-
    nextIdUt(Id),
    evolucao(excecaoInc(utente(Id,N,I,incerto))).

% -----
% Incerto na descrição do serviço
evolucao(servico(_,I,C,Cap),incerto,desc) :-
    nextIdServ(Id),
    evolucao(excecaoInc(servico(Id,incerto,I,C,Cap))).

% Incerto na instituição do serviço
evolucao(servico(D,_,C,Cap),incerto,inst) :-
    nextIdServ(Id),
    evolucao(excecaoInc(servico(Id,D,incerto,C,Cap))).

% Incerto na cidade do serviço
evolucao(servico(D,I,_,Cap),incerto,cidade) :-
    nextIdServ(Id),
    evolucao(excecaoInc(servico(Id,D,I,incerto,Cap))).

% -----
% Incerto na data da consulta
evolucao(consulta(_,IdUt,IdServ,C),incerto,data):-
    evolucao(excecaoInc(consulta(incerto,IdUt,IdServ,C))).

% Incerto no custo da consulta
evolucao(consulta(D,IdUt,IdServ,_),incerto,custo):-
    evolucao(excecaoInc(consulta(D,IdUt,IdServ,incerto))).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                                     Evolução - Tipo Impreciso
% ////////////////////////////////////////////////////////////////////////////////////////////////////

evolucaoImpre(_,_,[]).
evolucaoImpreAux(_,_,[]).

% Impreciso na cidade do utente
evolucaoImpre(utente(_,N,I),cidade,[H|T]) :-
    nextIdUt(Id),
    insercao(excecaoInc(utente(Id,N,I,H))),
    evolucaoImpreAux(utente(Id,N,I),cidade,T).

evolucaoImpreAux(utente(Id,N,I),cidade,[H|T]) :-
    insercao(excecaoInc(utente(Id,N,I,H))),
    evolucaoImpreAux(utente(Id,N,I),cidade,T).

% Impreciso na idade do utente
evolucaoImpre(utente(_,N,C),idade,[H|T]) :-
    nextIdUt(Id),
    insercao(excecaoInc(utente(Id,N,H,C))),
    evolucaoImpreAux(utente(Id,N,C),idade,T).

evolucaoImpreAux(utente(Id,N,C),idade,[H|T]) :-
    insercao(excecaoInc(utente(Id,N,H,C))),
    evolucaoImpreAux(utente(Id,N,C),idade,T).

% -----
% Impreciso na descrição do serviço
evolucaoImpre(servico(_,N,C,Cap),desc,[H|T]) :-
    nextIdServ(Id),
    insercao(excecaoInc(servico(Id,H,N,C,Cap))),
    evolucaoImpreAux(servico(Id,N,C,Cap),desc,T).

evolucaoImpreAux(servico(Id,N,C,Cap),desc,[H|T]) :-
    insercao(excecaoInc(servico(Id,H,N,C,Cap))),
    evolucaoImpreAux(servico(Id,N,C,Cap),desc,T).

% Impreciso na cidade do serviço
evolucaoImpre(servico(_,D,N,Cap),cidade,[H|T]) :-
    nextIdServ(Id),
    insercao(excecaoInc(servico(Id,D,N,H,Cap))),
    evolucaoImpreAux(servico(Id,D,N,Cap),cidade,T).

evolucaoImpreAux(servico(Id,D,N,Cap),cidade,[H|T]) :-
    insercao(excecaoInc(servico(Id,D,N,H,Cap))),
    evolucaoImpreAux(servico(Id,D,N,Cap),cidade,T).

% -----
% Impreciso na data da consulta
evolucaoImpre(consulta(IdUt,IdServ,C),data,[H|T]) :-
    nao(consulta(_,IdUt,IdServ,_)),
    insercao(excecaoInc(consulta(H,IdUt,IdServ,C))),
    evolucaoImpre(consulta(IdUt,IdServ,C),data,T).

% Impreciso na custo da consulta
evolucaoImpre(consulta(D,IdUt,IdServ),custo,[H|T]) :-
    nao(consulta(_,IdUt,IdServ,_)),
    insercao(excecaoInc(consulta(D,IdUt,IdServ,H))),
    evolucaoImpre(consulta(D,IdUt,IdServ),custo,T).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                                     Evolução - Tipo Interdito
% ////////////////////////////////////////////////////////////////////////////////////////////////////
% Teste de palavra interdita
interdito(interdito).

% Interdito no nome do utente
evolucao(utente(N,I,C),nome) :-
    nextIdUt(Id),
    evolucao(utente(Id,N,I,C)),
    interdito(N).

% Interdito na idade do utente
evolucao(utente(N,I,C),idade) :-
    nextIdUt(Id),
    evolucao(utente(Id,N,I,C)),
    interdito(I).

% Interdito na cidade do utente
evolucao(utente(N,I,C),cidade) :-
    nextIdUt(Id),
    evolucao(utente(Id,N,I,C)),
    interdito(C).

% -----
% Interdito na descrição do serviço
evolucao(servico(D,I,C,Cap),desc) :-
    nextIdServ(Id),
    evolucao(servico(Id,D,I,C,Cap)),
    interdito(D).

% Interdito na instituição do serviço
evolucao(servico(D,I,C,Cap),inst) :-
    nextIdServ(Id),
    evolucao(servico(Id,D,I,C,Cap)),
    interdito(I).

% Interdito na cidade do serviço
evolucao(servico(D,I,C,Cap),cidade) :-
    nextIdServ(Id),
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
    retrocesso(Termo),
    teste(Lista).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                                     Involução - Tipo Incerto e Impreciso
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% Predicado involucao para conhecimento incerto e impreciso para utentes
involucaoIIUtente(Termo) :-
    excecaoInc(Termo),
    solucoes(Invariante,-Termo::Invariante, Lista),
    getexcecoesU(Termo,X),
    solucoes(X,X,List),
    removeListas(List),
    teste(Lista).

getexcecoesU(utente(Id,Nome,Idade,Morada),excecaoInc(utente(IdUt,_,_,_))) :-
    IdUt = Id,
    excecaoInc(utente(IdUt,_,_,_)).

% Predicado involucao para conhecimento incerto e impreciso para serviços
involucaoIIServico(Termo) :-
    excecaoInc(Termo),
    solucoes(Invariante,-Termo::Invariante, Lista),
    getexcecoesS(Termo,X),
    solucoes(X,X,List),
    removeListas(List),
    teste(Lista).

getexcecoesS(servico(IdServ,Descricao,Instituicao,Cidade,Capacidade),excecaoInc(servico(IdServico,_,_,_,_))) :-
    IdServico = IdServ,
    excecaoInc(servico(IdServico,_,_,_,_)).

% Predicado involucao para conhecimento incerto e impreciso para consultas
involucaoIIConsulta(Termo) :-
    excecaoInc(Termo),
    solucoes(Invariante,-Termo::Invariante, Lista),
    getexcecoesC(Termo,X),
    solucoes(X,X,List),
    removeListas(List),
    teste(Lista).

getexcecoesC(consulta(Data,IdUt,IdServ,Custo),X) :-
    excecaoInc(consulta(Data,IdUt,_,_)).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                                     Involução - Tipo Interdito
% ////////////////////////////////////////////////////////////////////////////////////////////////////

involucaoInterdito(Termo) :-
    excecao(Termo), % corrigido agora
    solucoes(Invariante,-Termo::Invariante, Lista),
    retrocesso(Termo),
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

retrocesso(T) :-
    T,
    remove(T),
    solucoes(I, -T::I, LInv),
    teste(LInv), !.

remove(T) :-
    retract(T).
remove(T) :-
    assert(T), !, fail.

removeListas([]).
removeListas([H|T]) :-
    retrocesso(H),
    removeListas(T).

atomico(Q) :-
    Q \= _ e _,
    Q \= _ ou _,
    Q \= -(-_).

% ////////////////////////////////////// FIM DO FICHEIRO /////////////////////////////////////////////
