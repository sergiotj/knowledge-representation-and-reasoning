% descrição da estrutura do conhecimento

%utente(IdUt, Nome, Idade, Cidade).
%servico(IdServ, Descricao, Instituicao, Cidade).
%consulta(Data, IdUt, IdServ, Custo).

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

% operações

% Registar utentes, serviços e consultas;

nextIdUt(NextId) :-
    findall(Id, utente(Id, _, _, _), Ids),
    ((max(Ids, MaxId),
     NextId is MaxId + 1);
     Ids = [], % se não houver nenhum utente, o novo id será 1
     NextId is 1).

registarUtente(Nome, Idade, Cidade) :-
    nao(utente(_, Nome, Idade, Cidade)),
    nextIdUt(NextIdUt),
    assert(utente(NextIdUt, Nome, Idade, Cidade)).

% Remover utentes, serviços e consultas;
% Identificar as instituições prestadoras de serviços;
% Identificar utentes/serviços/consultas por critérios de seleção;
% Identificar serviços prestados por instituição/cidade/datas/custo;
% Identificar os utentes de um serviço/instituição;
% Identificar serviços realizados por utente/instituição/cidade;
% Calcular o custo total dos cuidados de saúde por utente/serviço/instituição/data.
