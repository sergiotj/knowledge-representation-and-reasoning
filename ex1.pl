% descrição da estrutura do conhecimento

%utente(IdUt, Nome, Idade, Cidade).
%servico(IdServ, Descricao, Instituicao, Cidade).
%consulta(Data, IdUt, IdServ, Custo).

:- dynamic idUtAtual/1.
:- dynamic utente/4.
:- dynamic servico/4.
:- dynamic consulta/4.

% relações auxiliares

nao(X) :-
    X, !, fail;
    true.

max([X], X).
max([X | L], M) :-
    max(L, M),
    M >= X.
max([X | L], X) :-
    max(L, M),
    X >= M.

solucoes(F, Q, R) :-
    Q, assert(tmp(F)), fail.
solucoes(F, Q, R) :-
    construir([], R).

construir(L, R) :-
    retract(tmp(X)), !,
    construir([X | L], R).
construir(R, R).

% operações

% Registar utentes, serviços e consultas;

idUtAtual(1).

nextIdUt(Id) :-
    idUtAtual(Id),
    retract(idUtAtual(Id)),
    Id2 is Id + 1,
    assert(idUtAtual(Id2)).

%nextIdUt(NextId) :-
%    solucoes(Id, utente(Id, _, _, _), Ids),
%    ((max(Ids, MaxId),
%     NextId is MaxId + 1);
%     Ids = [], % se não houver nenhum utente, o novo id será 1
%     NextId is 1).

registarUtente(Nome, Idade, Cidade) :-
    nao(utente(_, Nome, Idade, Cidade)),
    nextIdUt(NextIdUt),
    assert(utente(NextIdUt, Nome, Idade, Cidade)).

nextIdServ(NextId) :-
    solucoes(Id, servico(Id, _, _, _), Ids),
    ((max(Ids, MaxId),
     NextId is MaxId + 1);
     Ids = [],
     NextId is 1).

registarServico(Descricao, Instituicao, Cidade) :-
    nao(servico(_, Descricao, Instituicao, Cidade)),
    nextIdServ(NextId),
    assert(servico(NextId, Descricao, Instituicao, Cidade)).

registarConsulta(Data, IdUt, IdServ, Custo) :-
    nao(consulta(Data, IdUt, IdServ, Custo)),
    utente(IdUt, _, _, _),
    servico(IdServ, _, _, _),
    assert(consulta(Data, IdUt, IdServ, Custo)).

% Remover utentes, serviços e consultas;

removerUtente(utente(Id, Nome, Idade, Cidade)) :-
    utente(Id, Nome, Idade, Cidade),
    retract(utente(Id, Nome, Idade, Cidade)).

% Identificar as instituições prestadoras de serviços; Sergio
% Identificar utentes/serviços/consultas por critérios de seleção; Joel 
% Identificar serviços prestados por instituição/cidade/datas/custo; Miguel
% Identificar os utentes de um serviço/instituição; Tiago e Joel
% Identificar serviços realizados por utente/instituição/cidade; Tiagos
servicosInstituicao(Instituicao, R) :-
	solucoes((IdServ,Descricao,Instituicao,Cidade), servico(IdServ,Descricao,Instituicao,Cidade),R).




% Calcular o custo total dos cuidados de saúde por utente/serviço/instituição/data. Alex
