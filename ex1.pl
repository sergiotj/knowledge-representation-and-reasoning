% descrição da estrutura do conhecimento

%utente(IdUt, Nome, Idade, Cidade).
%servico(IdServ, Descricao, Instituicao, Cidade).
%consulta(Data, IdUt, IdServ, Custo).

:- dynamic idUtAtual/1.
:- dynamic utente/4.
:- dynamic idServAtual/1.
:- dynamic servico/4.
:- dynamic consulta/4.

% dados iniciais

init :-
    registarUtente(a1,b2,c1),
    registarUtente(a2,b1,c2),
    registarUtente(a3,b2,c4),
    registarServico(a1,b2,c1),
    registarServico(a2,b1,c2),
    registarServico(a3,b2,c4),
    registarConsulta(d1, 1, 1, 1.0),
    registarConsulta(d1, 1, 2, 1.5),
    registarConsulta(d2, 1, 3, 2.3).

% relações auxiliares

nao(X) :-
    X, !, fail;
    true.

pertence(X, [X | _]).
pertence(X, [Y | T]) :-
    pertence(X, T).

max([X], X).
max([X | L], M) :-
    max(L, M),
    M >= X.
max([X | L], X) :-
    max(L, M),
    X >= M.

sum([], 0).
sum([X | L], S) :-
    sum(L, S2),
    S is S2 + X.

list([]).
list([_|_]).

cons([], L, L).
cons([H1|T1], L, [H1|L2]) :-
    cons(T1, L, L2).

flatten([], []).
flatten([H|T], L) :-
    list(H),
    flatten(H, H2),
    flatten(T, T2),
    cons(H2, T2, L).
flatten([H|T], [H|T2]) :-
    nao(list(H)),
    flatten(T, T2).

solucoes(F, Q, R) :-
    Q, assert(tmp(F)), fail.
solucoes(F, Q, R) :-
    construir([], R).

construir(L, R) :-
    retract(tmp(X)), !,
    construir([X | L], R).
construir(R, R).

forEach(Var, Set, Format, P, R) :-
    pertence(Var, Set),
    P,
    assert(tmp(Format)),
    fail.
forEach(_, _, Format, _, R) :-
    construir([], R).

% operações

% Registar utentes, serviços e consultas;

idUtAtual(1).

nextIdUt(Id) :-
    idUtAtual(Id),
    retract(idUtAtual(Id)),
    Id2 is Id + 1,
    assert(idUtAtual(Id2)).

idServAtual(1).

nextIdServ(Id) :-
    idServAtual(Id),
    retract(idServAtual(Id)),
    Id2 is Id + 1,
    assert(idServAtual(Id2)).

registarUtente(Nome, Idade, Cidade) :-
    nao(utente(_, Nome, Idade, Cidade)),
    nextIdUt(NextIdUt),
    assert(utente(NextIdUt, Nome, Idade, Cidade)).

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

removerServico(servico(Id, Descricao, Instituicao, Cidade)) :-
    servico(Id, Descricao, Instituicao, Cidade),
    retract(servico(Id, Descricao, Instituicao, Cidade)).

removerConsulta(consulta(Data, IdUt, IdServ, Custo)) :-
    consulta(Data, IdUt, IdServ, Custo),
    retract(consulta(Data, IdUt, IdServ, Custo)).

% Identificar as instituições prestadoras de serviços;

% Identificar utentes/serviços/consultas por critérios de seleção;

% Identificar serviços prestados por instituição/cidade/datas/custo;

% Identificar os utentes de um serviço/instituição;

% Identificar serviços realizados por utente/instituição/cidade;

% Calcular o custo total dos cuidados de saúde por utente/serviço/instituição/data.

custoUtente(utente(IdUt, _, _, _), Custo) :-
    solucoes(C, consulta(_, IdUt, _, C), Custos),
    sum(Custos, Custo).
    
custoServico(servico(IdServ, _, _, _), Custo) :-
    solucoes(C, consulta(_, _, IdServ, C), Custos),
    sum(Custos, Custo).

custoInstituicao(Instituicao, Custo) :-
    solucoes(IdServ, servico(IdServ, _, Instituicao, _), Ids),
    forEach(Id, Ids, C, consulta(_, _, Id, C), CCs),
    flatten(CCs, Custos),
    sum(Custos, Custo).

custoData(D, Custo) :-
    solucoes(C, consulta(D, _, _, C), Custos),
    sum(Custos, Custo).
