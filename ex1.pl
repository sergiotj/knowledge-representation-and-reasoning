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

utente(1,ambrosio,18,braga).
utente(2,maria,23,porto).
utente(3,jose,23,lisboa).
utente(4,joana,24,braga).

servico(1, serv1, csjoane, guimaraes).
servico(2, serv2, hospitalbraga, braga).
servico(3, serv3, hospitalluz, braga).
servico(4, serv4, hospitalluz, guimaraes).
servico(5, serv5, uhfamalicao, famalicao).
servico(6, serv6, hsantamaria, porto).
servico(7, serv7, htrofa, braga).
servico(8, serv8, htrofa, braga).
servico(9, serv9, hospitalbraga, braga).

consulta(2015-11-20,1,2,22).
consulta(2016-11-20,1,2,25).
consulta(2016-09-13,3,5,30).
consulta(2017-02-28,4,9,45).

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

construir(L, R) :-
    retract(tmp(X)), !,
    construir([X | L], R).
construir(R, R).

solucoes(F, Q, R) :-
    Q, assert(tmp(F)), fail.
solucoes(F, Q, R) :-
    construir([], R).

forEach(Var, Set, Format, P, R) :-
    pertence(Var, Set),
    P,
    assert(tmp(Format)),
    fail.
forEach(_, _, Format, _, R) :-
    construir([], R).

removeDups([],[]).
removeDups([H|T],R) :-
	pertence(H,T),
	removeDups(T,R).
removeDups([H|T],[H|R]) :-
	nao(pertence(H,T)),
	removeDups(T,R).

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

listarInstituicoes(Result) :-
    solucoes(Inst, servico(_,_,Inst,_), L),
    removeDups(L, Result).

% Identificar utentes/serviços/consultas por critérios de seleção;

	% Extensao do predicado utentes: Nome,Idade,Cidade,LR -> {V,F}

utentes(Nome, Idade, Cidade, LR) :-
	solucoes(utente(IdUt,Nome,Idade,Cidade), utente(IdUt,Nome,Idade,Cidade), LR).

	% Extensao do predicado servicos: Instituicao,Cidade,LR -> {V,F}

servicos(Instituicao,Cidade,LR) :-
	solucoes(servico(IdServ,Descricao,Instituicao,Cidade), servico(IdServ,Descricao,Instituicao,Cidade), LR).

    % Extensao do predicado consultas: Data,IdUt,IdServ,LR -> {V,F}

consultas(Data,IdUt,IdServ,LR) :-
	solucoes((Data,IdUt,IdServ,Custo), consulta(Data,IdUt,IdServ,Custo), LR).

% Identificar serviços prestados por instituição/cidade/datas/custo; Miguel

% Identificar os utentes de um serviço/instituição; Tiago e Joel

% Identificar serviços realizados por utente/instituição/cidade;

servicosUtente(IdUt, R) :-
	solucoes(servico(IdServ,Descricao,Instituicao,Cidade), (consulta(Data,IdUt,IdServ,Custo),servico(IdServ,Descricao,Instituicao,Cidade)),L),
	removeDups(L,R).

servicosInstituicao(Instituicao, R) :-
	solucoes(servico(IdServ,Descricao,Instituicao,Cidade), servico(IdServ,Descricao,Instituicao,Cidade),L),
	removeDups(L,R).

servicosCidade(Cidade, R) :-
	solucoes(servico(IdServ,Descricao,Instituicao,Cidade), servico(IdServ,Descricao,Instituicao,Cidade),L),
	removeDups(L,R).

% Calcular o custo total dos cuidados de saúde por utente/serviço/instituição/data. Alex

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
