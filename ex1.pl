% descrição da estrutura do conhecimento

%utente(IdUt, Nome, Idade, Cidade).
%servico(IdServ, Descricao, Instituicao, Cidade).
%consulta(Data, IdUt, IdServ, Custo).

:- dynamic idUtAtual/1.
:- dynamic utente/4.
:- dynamic servico/4.
:- dynamic consulta/4.

% povoação

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

pertence(X,[]) :- fail.
pertence(X,[X|T]) :- X==X.
pertence(X,[H|T]) :-
	X\=H,
	pertence(X,T).

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
	solucoes((IdServ,Descricao,Instituicao,Cidade), (consulta(Data,IdUt,IdServ,Custo),servico(IdServ,Descricao,Instituicao,Cidade)),R).

servicosInstituicao(Instituicao, R) :-
	solucoes((IdServ,Descricao,Instituicao,Cidade), servico(IdServ,Descricao,Instituicao,Cidade),R).

servicosCidade(Cidade, R) :-
	solucoes((IdServ,Descricao,Instituicao,Cidade), servico(IdServ,Descricao,Instituicao,Cidade),R).

% Calcular o custo total dos cuidados de saúde por utente/serviço/instituição/data. Alex
