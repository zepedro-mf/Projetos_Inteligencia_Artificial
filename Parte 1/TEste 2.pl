% =========================================================
% Inteligência Artificial em Engenharia Biomédica
% Trabalho de Grupo - Parte 1
% Tema: Representação de Conhecimento sobre Tensão Arterial
% =========================================================

:- dynamic (paciente/4).
:- dynamic (consulta/7).
:- dynamic (tensao/6).

:- dynamic ('-'/1).
:- dynamic excecao/1.
:- dynamic interdito/1.

:- op(900, xfy, '::').
:- op(900, xfy, 'e').
:- op(900, xfy, 'ou').

% =========================================================
%             1. SISTEMA DE INFERÊNCIA
% =========================================================

si(Questao, verdadeiro) :- Questao.
si(Questao, falso) :- -Questao.
si(Questao, desconhecido) :- nao(Questao), nao(-Questao).

nao(Questao) :- Questao, !, fail.
nao(_).

comprimento([], 0).
comprimento([_|L], N) :- comprimento(L, N1), N is N1 + 1.

% =========================================================
%             2. PRESSUPOSTO DO MUNDO FECHADO
% =========================================================

-paciente(Id,Nome,DataNasc,Sexo) :-
    nao(paciente(Id,Nome,DataNasc,Sexo)),
    nao(excecao(paciente(Id,Nome,DataNasc,Sexo))).

-consulta(Id,Data,Pac,Idade,Dia,Sis,Pulso) :-
    nao(consulta(Id,Data,Pac,Idade,Dia,Sis,Pulso)),
    nao(excecao(consulta(Id,Data,Pac,Idade,Dia,Sis,Pulso))).

-tensao(Id,Class,SistInf,SistSup,DiastInf,DiastSup) :-
    nao(tensao(Id,Class,SistInf,SistSup,DiastInf,DiastSup)),
    nao(excecao(tensao(Id,Class,SistInf,SistSup,DiastInf,DiastSup))).

% =========================================================
%             3. CONHECIMENTO PERFEITO
% =========================================================

paciente(p001, ana, (12,5,1985), f).
paciente(p002, bruno, (3,8,1972), M).
paciente(p003, carla, (27,2,1990), f).

consulta(c001, (1,10,2025), p001, 40, 78, 122, 70).
consulta(c002, (1,10,2025), p002, 53, 92, 160, 75).
consulta(c003, (1,10,2025), p003, 35, 80, 120, 65).

tensao(ta01, baixa, 0, 90, 0, 60).
tensao(ta02, normal, 90, 129, 60, 84).
tensao(ta03, elevada, 130, 139, 85, 89).
tensao(ta04, hipertensao, 140, 999, 90, 999).

% =========================================================
%             4. CONHECIMENTO NEGATIVO
% =========================================================

% Tensões sistólicas não podem ser menores que diastólicas
-tensao(_,_,SistInf,_,DiastInf,_) :- SistInf < DiastInf.

% =========================================================
%             5. CONHECIMENTO IMPERFEITO
% =========================================================

% INCERTO: pulsação não registada
consulta(c004, (3,10,2025), p001, 40, 85, 125, desconhecido).
excecao(consulta(Id,Data,Pac,Idade,Dia,Sis,Pulso)) :-
    consulta(Id,Data,Pac,Idade,Dia,Sis,desconhecido).

% IMPRECISO: valores de tensão flutuaram
excecao(consulta(c005,(4,10,2025),p002,53,91,158,74)).
excecao(consulta(c005,(4,10,2025),p002,53,93,162,76)).

% INTERDITO: paciente sem número de utente conhecido
paciente(num_desconhecido, dario, (15,7,1980), M).
excecao(paciente(Id,Nome,Data,Sexo)) :- paciente(num_desconhecido,Nome,Data,Sexo).
interdito(num_desconhecido).

+paciente(Id,Nome,Data,Sexo) ::
    (findall(Id, paciente(Id,Nome,Data,Sexo), S),
     comprimento(S,N),
     N == 1).

% =========================================================
%             6. EVOLUÇÃO DO CONHECIMENTO
% =========================================================

evolucao(Termo) :-
    findall(Invariante, +Termo::Invariante, Lista),
    insercao(Termo),
    validar(Lista).

insercao(Termo) :- assert(Termo).
insercao(Termo) :- retract(Termo),!,fail.

validar([]).
validar([R|LR]) :- R, validar(LR).

% =========================================================
%             7. INVOLUÇÃO DO CONHECIMENTO
% =========================================================

involucao(Termo) :-
    findall(Invariante, -Termo::Invariante, Lista),
    remocao(Termo),
    validar(Lista).

remocao(Termo) :- retract(Termo).
remocao(Termo) :- assert(Termo),!,fail.

% Exemplo de invariante para remoção:
-consulta(Id,_,_,_,_,_,_) :: (
    findall(Id, consulta(Id,_,_,_,_,_,_), S),
    comprimento(S,N),
    N == 0).

% =========================================================
%             8. INVARIANTES DE INSERÇÃO
% =========================================================

+consulta(Id,Data,Pac,Idade,Dia,Sis,Pulso) ::
    (findall(Pac, paciente(Pac,_,_,_), L),
     comprimento(L,N),
     N == 1).

% =========================================================
%             9. RACIOCÍNIO: CLASSIFICAÇÃO E RISCO
% =========================================================

% Determinar classificação da tensão arterial
classificar_tensao(Pac, Classe) :-
    consulta(_,_,Pac,_,Dia,Sis,_),
    tensao(_,Classe,SistInf,SistSup,DiastInf,DiastSup),
    Sis >= SistInf, Sis =< SistSup,
    Dia >= DiastInf, Dia =< DiastSup.

% Avaliação de risco baseado na classificação e histórico
avaliar_risco(Pac, baixo) :-
    classificar_tensao(Pac, normal), !.

avaliar_risco(Pac, moderado) :-
    classificar_tensao(Pac, elevada), !.

avaliar_risco(Pac, alto) :-
    findall(Classe, classificar_tensao(Pac, Classe), L),
    member(hipertensao, L), !.

avaliar_risco(_, desconhecido).

% =========================================================
%             10. CONSULTAS E RELATÓRIOS
% =========================================================

% Relatório detalhado
relatorio_paciente_detalhado(Pac) :-
    paciente(Pac, Nome, DataNasc, Sexo),
    (classificar_tensao(Pac, Classe) -> true ; Classe = 'indeterminado'),
    (avaliar_risco(Pac, Risco) -> true ; Risco = 'indeterminado'),
    
    format('=== RELATORIO MEDICO DETALHADO ===~n', []),
    format('Paciente: ~w~n', [Nome]),
    format('Data Nascimento: ~w~n', [DataNasc]),
    format('Sexo: ~w~n', [Sexo]),
    format('Classificação Tensão: ~w~n', [Classe]),
    format('Nível de Risco: ~w~n', [Risco]),
    format('--- Histórico de Consultas ---~n', []),
    listar_consultas(Pac).

% Listar consultas de um paciente
listar_consultas(Pac) :-
    findall((Data, Dia, Sis, Pulso), consulta(_, Data, Pac, _, Dia, Sis, Pulso), L),
    (L = [] -> 
        format('  Sem consultas registadas~n', [])
    ;
        format('Consultas do paciente ~w:~n', [Pac]),
        listar_consultas_aux(L)
    ).

listar_consultas_aux([]).
listar_consultas_aux([(Data,Dia,Sis,Pulso)|T]) :-
    (Pulso == desconhecido ->
        format('  Data: ~w - TA: ~w/~w - Pulsação: não registada~n', [Data, Sis, Dia])
    ;
        format('  Data: ~w - TA: ~w/~w - Pulsação: ~w~n', [Data, Sis, Dia, Pulso])
    ),
    listar_consultas_aux(T).
