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
:- dynamic nulointerdito/1.
:- dynamic incerto/1.

% dados iniciais

idUtAtual(9).
idServAtual(13).
idConsultaAtual(5).

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
                    nao(excecao(utente(Id,N,I,C))).

-servico(Id,D,I,C,Cap) :-
                    nao(servico(Id,D,I,C,Cap)),
                    nao(excecao(servico(Id,D,I,C,Cap))).

-consulta(D,IdU,IdServ,C) :-
                    nao(consulta(D,IdU,IdServ,C)),
                    nao(excecao(consulta(D,IdU,IdServ,C))).

% Negação explícita

-utente(35,joaquim,65,braganca).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                   Representar casos de conhecimento imperfeito incerto
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% ----------------------------------------------------------------------------------------------------
% Desconhece-se a cidade do utente com o id=7, que é a Margarida com 19 anos.

utente(7, margarida, 19, i1).

excecao(utente(A,B,C,D)) :-
    utente(A,B,C,i1).

% ----------------------------------------------------------------------------------------------------
% Não se sabe qual a descrição do serviço 10 e a sua capacidade, que é o hospital da covilhã.

servico(10, i2, hospitalcovilha, covilha, i3).

excecao(servico(A, B, C, D, E)) :-
    servico(A, i2, C, D, i3).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                   Representar casos de conhecimento imperfeito impreciso
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% ----------------------------------------------------------------------------------------------------
% Não se sabe se a utente com id=8, com o nome Inês, residente na cidade de Lisboa, tem 23 ou 24 anos.

excecao(utente(8,ines,23,lisboa)).
excecao(utente(8,ines,24,lisboa)).

% ----------------------------------------------------------------------------------------------------
% Desconhece-se se o servico com id=11, serv10 do Hospital de Santa Maria no Porto tem capacidade 4 ou
% capacidade 5
excecao(servico(11,serv10, hsantamaria, porto, 4)).
excecao(servico(11,serv10, hsantamaria, porto, 5)).

% ----------------------------------------------------------------------------------------------------
% Não se sabe se a consulta prestada em 25-12-2018, pelo servico com id=8 ao utente com id=3
% teve um custo de 10€ ou um custo de 15€
excecao(consulta(25-12-2018, 3, 8, 10) ).
excecao(consulta(25-12-2018, 3, 8, 15) ).

% ----------------------------------------------------------------------------------------------------
% Desconhece-se se a consulta no dia 1 de Abril ao utente com id=8 foi
% prestada pelo servico com id = 5 ou pelo servico com id = 7, e se o custo foi 20€ ou 30€.
excecao(consulta(01-04-2019, 8, 5, 20)).
excecao(consulta(01-04-2019, 8, 7, 20)).
excecao(consulta(01-04-2019, 8, 5, 30)).
excecao(consulta(01-04-2019, 8, 7, 30)).

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
+consulta(A, B, C, D) ::
    (solucoes( (A, Interdito, C, D), (consulta(2019-01-30, Interdito, 3, 25), nao(nulointerdito(Interdito))), List),
     comprimento(List, N),
     N == 0).

% ----------------------------------------------------------------------------------------------------
% Nunca se poderá saber qual a capacidade, a partir do serviço com id = 12, que corresponde ao servico
% com descricao serv15, do hospitallisboa, na cidade de Lisboa
servico(12, serv15, hospitallisboa, lisboa, i6).

excecao(servico(A, B, C, D, E)) :-
    servico(A, B, C, D, i6).
nulointerdito(i6).
+servico(A, B, C, D, E) ::
    (solucoes( (A, B, C, D, Interdito), (servico(12, serv15, hospitallisboa, lisboa, Interdito), nao(nulointerdito(Interdito))), List),
     comprimento(List, N),
     N == 0).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
% Manipular invariantes que designem restrições à inserção e à remoção de conhecimento do sistema
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% Inserção:
%% Não permitir dois utentes com o mesmo ID

+utente(Id,Nome,I,C) :: (solucoes( Id, (utente(Id, _, _, _)), S ),
                         comprimento( S,N ),
                         N == 1).

% Não permitir dois serviços com o mesmo ID;

+servico(Id,D,I,C,Cap) :: (solucoes( Id, (servico(Id, _, _, _, _)), S1 ),
                           comprimento( S1,N1 ),
                           N1 == 1).

% Não permitir dois serviços com a mesma descrição no mesmo hospital.
+servico(Id,D,I,C,Cap) :: (solucoes( (Id,D,I), (servico(_, D, I, _, _)), S2 ),
                           comprimento( S2,N2 ),
                           N2 == 1).


% Não permitir duas consultas com o mesmo IdU e IdServ na mesma data;
+consulta(D,IdU,IdServ,C) :: (solucoes( (D,IdU,IdServ), (consulta(D, IdU, IdServ, _)), S1 ),
                              comprimento( S1,N1 ),
                              N1 == 1).

% Não permitir uma consulta com um IdU não existente;
+consulta(D,IdU,IdServ,C) :: (solucoes( IdU, (utente(IdU, _, _, _)), S2 ),
                              comprimento( S2,N2 ),
                              N2 == 1).

% Não permitir uma consulta com um IdServ não existente.
+consulta(D,IdU,IdServ,C) :: (solucoes( IdServ, (servico(IdServ, _, _, _, _)), S3 ),
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

% negação
si(-Q, verdadeiro) :-
    si(Q, falso), !.
si(-Q, falso) :-
    si(Q, verdadeiro), !.

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
    involucao(idUtAtual(Id)),
    Id2 is Id + 1,
    evolucao(idUtAtual(Id2)).

nextIdServ(Id) :-
    involucao(idServAtual(Id)),
    Id2 is Id + 1,
    evolucao(idServAtual(Id2)).

teste( [] ).
teste( [R|LR] ) :-
    R,
    teste( LR ).

excecao(utente(Id,Nome,Idade,Cidade)) :-
    utente(Id,incerto,Idade,Cidade).

excecao(utente(Id,Nome,Idade,Cidade)) :-
    utente(Id,Nome,incerto,Cidade).

excecao(utente(Id,Nome,Idade,Cidade)) :-
    utente(Id,Nome,Idade,incerto).

excecao(servico(Id,Descricao,Instituicao,Cidade,Cap)) :-
    utente(Id,incerto,Instituicao,Cidade,Cap).

excecao(servico(Id,Descricao,Instituicao,Cidade,Cap)) :-
    utente(Id,Descricao,incerto,Cap).

excecao(servico(Id,Descricao,Instituicao,Cidade,Cap)) :-
    utente(Id,Descricao,Instituicao,incerto,Cap).

excecao(consulta(Data,IdUt,IdServ,Custo)) :-
    consulta(incerto,IdUt,IdServ,Custo).

excecao(consulta(Data,IdUt,IdServ,Custo)) :-
    consulta(Data,IdUt,IdServ,incerto).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                                         Registo de entidades
% ////////////////////////////////////////////////////////////////////////////////////////////////////

registarUtente(Nome, Idade, Cidade) :-
    nextIdUt(Id),
    evolucao(utente(Id, Nome, Idade, Cidade)).

registarServico(Descricao, Instituicao, Cidade, Capacidade) :-
    nextIdServ(Id),
    evolucao(servico(Id, Descricao, Instituicao, Custo, Capacidade)).

registarConsulta(Data, IdUt, IdServ, Custo) :-
    evolucao(consulta(Data, IdUt, IdServ, Custo)).

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
    evolucao(excecao(utente(Id,incerto,I,C))).

% Incerto na idade do utente
evolucao(utente(N,_,C),incerto,idade) :-
    nextIdUt(Id),
    evolucao(excecao(utente(Id,N,incerto,C))).

% Incerto na cidade do utente
evolucao(utente(N,I,_),incerto,cidade) :-
    nextIdUt(Id),
    evolucao(excecao(utente(Id,N,I,incerto))).

% -----
% Incerto na descrição do serviço
evolucao(servico(_,I,C,Cap),incerto,desc) :-
    nextIdServ(Id),
    evolucao(excecao(servico(Id,incerto,I,C,Cap))).

% Incerto na instituição do serviço
evolucao(servico(D,_,C,Cap),incerto,inst) :-
    nextIdServ(Id),
    evolucao(excecao(servico(Id,D,incerto,C,Cap))).

% Incerto na cidade do serviço
evolucao(servico(D,I,_,Cap),incerto,cidade) :-
    nextIdServ(Id),
    evolucao(excecao(servico(Id,D,I,incerto,Cap))).

% -----
% Incerto na data da consulta
evolucao(consulta(_,IdUt,IdServ,C),incerto,data):-
    evolucao(excecao(consulta(incerto,IdUt,IdServ,C))).

% Incerto no custo da consulta
evolucao(consulta(D,IdUt,IdServ,_),incerto,custo):-
    evolucao(excecao(consulta(D,IdUt,IdServ,incerto))).

% Incerto no id do utente
evolucao(consulta(D,_,IdServ,C),incerto,utente):-
    evolucao(excecao(consulta(D,incerto,IdServ,C))).

% Incerto no id do serviço
evolucao(consulta(D,IdUt,_,C),incerto,servico):-
    evolucao(excecao(consulta(D,IdUt,incerto,C))).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                                     Evolução - Tipo Impreciso
% ////////////////////////////////////////////////////////////////////////////////////////////////////

evolucaoImpreAux(_,_,[]).

% Impreciso na cidade do utente
evolucaoImpre(utente(_,N,I),cidade,[H|T]) :-
    nextIdUt(Id),
    insercao(excecao(utente(Id,N,I,H))),
    evolucaoImpreAux(utente(Id,N,I),cidade,T).

evolucaoImpreAux(utente(Id,N,I),cidade,[H|T]) :-
    insercao(excecao(utente(Id,N,I,H))),
    evolucaoImpreAux(utente(Id,N,I),cidade,T).

% Impreciso na idade do utente
evolucaoImpre(utente(_,N,C),idade,[H|T]) :-
    nextIdUt(Id),
    insercao(excecao(utente(Id,N,H,C))),
    evolucaoImpreAux(utente(Id,N,C),idade,T).

evolucaoImpreAux(utente(Id,N,C),idade,[H|T]) :-
    insercao(excecao(utente(Id,N,H,C))),
    evolucaoImpreAux(utente(Id,N,C),idade,T).

% -----
% Impreciso na descrição do serviço
evolucaoImpre(servico(_,I,C,Cap),desc,[H|T]) :-
    nextIdServ(Id),
    insercao(excecao(servico(Id,H,I,C,Cap))),
    evolucaoImpreAux(servico(Id,I,C,Cap),desc,T).

evolucaoImpreAux(servico(Id,I,C,Cap),desc,[H|T]) :-
    insercao(excecao(servico(Id,H,I,C,Cap))),
    evolucaoImpreAux(servico(Id,I,C,Cap),desc,T).

% Impreciso na cidade do serviço
evolucaoImpre(servico(D,I,_,Cap),cidade,[H|T]) :-
    nextIdServ(Id),
    insercao(excecao(servico(Id,D,I,H,Cap))),
    evolucaoImpreAux(servico(Id,D,I,Cap),cidade,T).

evolucaoImpreAux(servico(Id,D,I,Cap),cidade,[H|T]) :-
    insercao(excecao(servico(Id,D,I,H,Cap))),
    evolucaoImpreAux(servico(Id,D,I,Cap),cidade,T).

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

% Impreciso no utente da consulta
evolucaoImpre(consulta(Data,IdServ,C),utente,[H|T]) :-
    nao(consulta(Data,_,IdServ,_)),
    insercao(excecao(consulta(Data,H,IdServ,C))),
    evolucaoImpre(consulta(Data,IdServ,C),utente,T).

% Impreciso no servico da consulta
evolucaoImpre(consulta(D,IdUt,Custo),servico,[H|T]) :-
    nao(consulta(_,IdUt,_,Custo)),
    insercao(excecao(consulta(D,IdUt,H,Custo))),
    evolucaoImpre(consulta(D,IdUt,Custo),servico,T).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                                     Evolução - Tipo Interdito
% ////////////////////////////////////////////////////////////////////////////////////////////////////
% Teste de palavra interdita
interdito(interdito).

% Interdito no nome do utente
evolucao(utente(Nome,Idade,Cidade),nome) :-
    nextIdUt(Id),
    evolucao(utente(Id,Nome,Idade,Cidade)),
    assert(nulointerdito(Nome)),
    assert(
        +utente(A, B, C, D) ::
            (solucoes( Interdito, (utente(Id, Interdito, Idade, Cidade), nao(nulointerdito(Interdito))), List),
             comprimento(List, N),
             N == 0)
    ).

% Interdito na idade do utente
evolucao(utente(Nome, Idade, Cidade),idade) :-
    nextIdUt(Id),
    evolucao(utente(Id,Nome,Idade,Cidade)),
    assert(nulointerdito(Idade)),
    assert(
        +utente(A, B, C, D) ::
            (solucoes( Interdito, (utente(Id, Nome, Interdito, Cidade), nao(nulointerdito(Interdito))), List),
             comprimento(List, N),
             N == 0)
    ).

% Interdito na cidade do utente
evolucao(utente(Nome,Idade,Cidade),cidade) :-
    nextIdUt(Id),
    evolucao(utente(Id,Nome,Idade,Cidade)),
    assert(nulointerdito(Cidade)),
    assert(
        +utente(A, B, C, D) ::
            (solucoes( Interdito, (utente(Id, Nome, Idade, Interdito), nao(nulointerdito(Interdito))), List),
             comprimento(List, N),
             N == 0)
    ).

% -----
% Interdito na descrição do serviço
evolucao(servico(Descricao,Instituicao,Cidade,Capacidade),descricao) :-
    nextIdServ(Id),
    evolucao(servico(Id,Descricao,Instituicao,Cidade,Capacidade)),
    assert(nulointerdito(Descricao)),
    assert(
        +servico(A, B, C, D, E) ::
            (solucoes( Interdito, (servico(Id, Interdito, Instituicao, Cidade, Capacidade), nao(nulointerdito(Interdito))), List),
             comprimento(List, N),
             N == 0)
    ).

% Interdito na instituição do serviço
evolucao(servico(Descricao,Instituicao,Cidade,Capacidade),instituicao) :-
    nextIdServ(Id),
    evolucao(servico(Id,Descricao,Instituicao,Cidade,Capacidade)),
    assert(nulointerdito(Instituicao)),
    assert(
        +servico(A, B, C, D, E) ::
            (solucoes( Interdito, (servico(Id, Descricao, Interdito, Cidade, Capacidade), nao(nulointerdito(Interdito))), List),
             comprimento(List, N),
             N == 0)
    ).

% Interdito na cidade do serviço
evolucao(servico(Descricao,Instituicao,Cidade,Capacidade),cidade) :-
    nextIdServ(Id),
    evolucao(servico(Id,Descricao,Instituicao,Cidade,Capacidade)),
    assert(nulointerdito(Cidade)),
    assert(
        +servico(A, B, C, D, E) ::
            (solucoes( Interdito, (servico(Id, Descricao, Instituicao, Interdito, Capacidade), nao(nulointerdito(Interdito))), List),
             comprimento(List, N),
             N == 0)
    ).

% -----
% Interdito na data da consulta
evolucao(consulta(Data,IdUt,IdServ,Custo),data) :-
    evolucao(consulta(Data,IdUt,IdServ,Custo)),
    assert(nulointerdito(Data)),
    assert(
        +consulta(A, B, C, D) ::
            (solucoes( Interdito, (consulta(Interdito, IdUt, IdServ, Custo), nao(nulointerdito(Interdito))), List),
             comprimento(List, N),
             N == 0)
    ).

% Interdito no custo da consulta
evolucao(consulta(Data,IdUt,IdServ,Custo),custo) :-
    evolucao(consulta(Data,IdUt,IdServ,Custo)),
    assert(nulointerdito(Custo)),
    assert(
        +consulta(A, B, C, D) ::
            (solucoes( Interdito, (consulta(Data, IdUt, IdServ, Interdito), nao(nulointerdito(Interdito))), List),
             comprimento(List, N),
             N == 0)
    ).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                                       Involução do Conhecimento
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% Predicado involucao para conhecimento perfeito
involucao(Termo) :-
    nao(excecao(Termo)),
    solucoes(Invariante,-Termo::Invariante, Lista),
    teste(Lista),
    retract(Termo).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                                     Involução - Tipo Incerto e Impreciso
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% Predicado involucao para conhecimento incerto e impreciso para utentes
involucaoIIUtente(Termo) :-
    excecao(Termo),
    solucoes(Invariante,-Termo::Invariante, Is),
    getexcecoesU(Termo,Excecoes),
    teste(Is),
    removeLista(Excecoes).

getexcecoesU(utente(Id,_,_,_), L) :-
    solucoes(excecao(utente(Id,A,B,C)), excecao(utente(Id,A,B,C)), L).

% Predicado involucao para conhecimento incerto e impreciso para serviços
involucaoIIServico(Termo) :-
    excecao(Termo),
    solucoes(Invariante,-Termo::Invariante, Is),
    getexcecoesS(Termo,Excecoes),
    teste(Is),
    removeLista(Excecoes).

getexcecoesS(servico(Id,_,_,_,_), L) :-
    solucoes(excecao(servico(Id,A,B,C,D)), excecao(servico(Id,A,B,C,D)), L).

% Predicado involucao para conhecimento incerto e impreciso para consultas
involucaoIIConsulta(Termo) :-
    excecao(Termo),
    solucoes(Invariante,-Termo::Invariante, Is),
    getexcecoesC(Termo,Excecoes),
    teste(Is),
    removeLista(Excecoes).

getexcecoesC(consulta(_,IdU,IdS,_), L) :-
    solucoes(excecao(consulta(Data,IdU,IdS,Custo)), excecao(consulta(Data,IdU,IdS,Custo)), L).

% ////////////////////////////////////////////////////////////////////////////////////////////////////
%                                     Involução - Tipo Interdito
% ////////////////////////////////////////////////////////////////////////////////////////////////////

involucaoInterdito(Termo) :-
    excecao(Termo),
    solucoes(Invariante,-Termo::Invariante, Lista),
    teste(Lista),
    retract(Termo).

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

removeLista([]).
removeLista([H|T]) :-
    retract(H),
    removeLista(T).

atomico(Q) :-
    Q \= _ e _,
    Q \= _ ou _,
    Q \= -(-_).

% ////////////////////////////////////// FIM DO FICHEIRO /////////////////////////////////////////////
