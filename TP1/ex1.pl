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

init :-
    registarUtente(rosalina,34,braga),
    registarUtente(maria,23,porto),
    registarUtente(jose,23,lisboa),
    registarUtente(joana,24,braga),
    registarUtente(joao,45,coimbra),

    registarServico(serv1, csjoane      , guimaraes, 3),
    registarServico(serv2, hospitalbraga, braga    , 7),
    registarServico(serv3, hospitalluz  , braga    , 4),
    registarServico(serv4, hospitalluz  , guimaraes, 5),
    registarServico(serv5, uhfamalicao  , famalicao, 2),
    registarServico(serv6, hsantamaria  , porto    , 3),
    registarServico(serv7, htrofa       , braga    , 1),
    registarServico(serv8, htrofa       , braga    , 6),
    registarServico(serv9, hospitalbraga, braga    , 3),

    registarConsulta(2015-11-20,1,2,22),
    registarConsulta(2016-11-20,1,2,25),
    registarConsulta(2016-09-13,3,5,30),
    registarConsulta(2017-02-28,4,9,45).

% relações auxiliares

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

% invariantes

% IdUt é chave primária utente
+utente(IdUt, Nome, Idade, Cidade) ::
    (solucoes(IdUt, utente(IdUt, _, _, _), Ids),
     comprimento(Ids, C), C < 2).

% IdServ é chave primária utente
+servico(IdServ, Descricao, Instituicao, Cidade) ::
    (solucoes(IdServ, servico(IdServ, _, _, _, _), Ids),
     comprimento(Ids, C), C < 2).

% Existe no máximo 1 serviço com a mesma descrição, prestado pela mesma
% instituição, na mesma cidade,
+servico(IdServ, Descricao, Instituicao, Cidade, Capacidade) ::
    (solucoes(Id, servico(Id, Descricao, Instituicao, Cidade, _), Ids),
     comprimento(Ids, C), C < 2).

% IdUt é chave estrangeira, refere-se a um utente
+consulta(Data, IdUt, IdServ, Custo) ::
    (utente(IdUt, _, _, _)).

% IdServ é chave estrangeira, refere-se a um serviço
+consulta(Data, IdUt, IdServ, Custo) ::
    (servico(IdServ, _, _, _, _)).

+consulta(Data, IdUt, IdServ, Custo) ::
    (servico(IdServ, _, _, _, Capacidade),
     solucoes(IdServ, consulta(Data, _, IdServ, _), Ids),
     comprimento(Ids, C), C =< Capacidade).

+consulta(Data, _, _, _) ::
    (verificarData(Data)).

% predicados evolucao e involucao, e seus auxiliares

teste([]).
teste([I|L]) :-
    I, teste(L).

insere(T) :-
    assert(T).
insere(T) :-
    retract(T), !, fail. % insucesso

evolucao(T) :-
    solucoes(I, +T::I, LInv),
    insere(T),
    teste(LInv), !. % sucesso automático se invariantes se manterem

remove(T) :-
    retract(T).
remove(T) :-
    assert(T), !, fail.

retrocesso(T) :-
    T, % previne insucesso do predicado retract por não conseguir retirar T
    remove(T),
    solucoes(I, -T::I, LInv),
    teste(LInv), !. % sucesso automático se invariantes se manterem

% operações

% Registar utentes, serviços e consultas;

nextIdUt(Id) :-
    retrocesso(idUtAtual(Id)),
    Id2 is Id + 1,
    evolucao(idUtAtual(Id2)).

nextIdServ(Id) :-
    retrocesso(idServAtual(Id)),
    Id2 is Id + 1,
    evolucao(idServAtual(Id2)).

registarUtente(Nome, Idade, Cidade) :-
    nextIdUt(NextIdUt),
    evolucao(utente(NextIdUt, Nome, Idade, Cidade)).

registarServico(Descricao, Instituicao, Cidade, Capacidade) :-
    nextIdServ(NextId),
    evolucao(servico(NextId, Descricao, Instituicao, Cidade, Capacidade)).

registarConsulta(Data, IdUt, IdServ, Custo) :-
    evolucao(consulta(Data, IdUt, IdServ, Custo)).

% Remover utentes, serviços e consultas;

removerUtente(utente(Id, Nome, Idade, Cidade)) :-
    retrocesso(utente(Id, Nome, Idade, Cidade)).

removerServico(servico(Id, Descricao, Instituicao, Cidade)) :-
    retrocesso(servico(Id, Descricao, Instituicao, Cidade)).

removerConsulta(consulta(Data, IdUt, IdServ, Custo)) :-
    retrocesso(consulta(Data, IdUt, IdServ, Custo)).

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

  % Identificar serviços por Instituição
listarServicosInstituicao(Instituicao,LR) :-
  solucoes(IdServ,servico(IdServ,_,Instituicao,_,_),LR).

  % Identificar serviços por Cidade
listarServicosCidade(Cidade,LR) :-
  solucoes(IdServ,servico(IdServ,_,_,Cidade,_),L).

  % Identificar serviços por Datas
listarServicosData(Data,LR) :-
  verificarData(Data),
  solucoes(IdServ,consulta(Data,_,IdServ,_),L),
  removeDups(L,LR).

verificarData(Y-M-D) :-
  integer(Y),
  integer(M),
  integer(D),
  Y > 0,
  M >= 1,
  M =< 12,
  D >= 1,
  D =< 31.

% listar serviços por custo
listarServicosCusto(Custo,LR) :-
  solucoes(IdServ,consulta(_,_,IdServ,Custo),L),
  removeDups(L,LR).



% Identificar os utentes de um serviço/instituição; Tiago e Joel

utentesServico(IdServ, R) :-
	solucoes(utente(IdUt,Nome,Idade,Cidade), (consulta(Data,IdUt,IdServ,Custo),utente(IdUt,Nome,Idade,Cidade)),L),
	removeDups(L,R).

utentesInstituicao(Instituicao, R) :-
	solucoes(utente(IdUt,Nome,Idade,Cidade), (servico(IdServ,_,Instituicao,_),consulta(_,IdUt,IdServ,_),utente(IdUt,Nome,Idade,Cidade)),L),
	removeDups(L,R).


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
    solucoes(C, (servico(IdServ, _, Instituicao, _), consulta(_, _, IdServ, C)), Custos),
    sum(Custos, Custo).

custoData(D, Custo) :-
    solucoes(C, consulta(D, _, _, C), Custos),
    sum(Custos, Custo).
