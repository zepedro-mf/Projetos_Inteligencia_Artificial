% =========================================================================
% Projeto Final: Tensão Arterial (P1, P2, P3 integrados)
% Ficheiro: projeto_final.pl
% Encoding: UTF-8
% Uso: consult('projeto_final.pl').  Depois: ?- demo.
% =========================================================================

:- op(100, xfy, ::).           % operador para invariantes
:- dynamic paciente/5.
:- dynamic consulta/7.
:- dynamic tensao_arterial/6.
:- dynamic neg/1.              % conhecimento negativo explícito
:- dynamic nulo/1.             % marcador de nulos (incerto/impreciso/interdito)

% evitar warnings sobre cláusulas não contiguas
:- discontiguous paciente/5.
:- discontiguous consulta/7.
:- discontiguous tensao_arterial/6.
:- discontiguous neg/1.
:- discontiguous nulo/1.
:- discontiguous (+)/1.
:- discontiguous (-)/1.

% --------------------------
% Definição dos nulos (P3)
% --------------------------
nulo(incerto).
nulo(interdito).
nulo(impreciso).

% --------------------------
% P1 — DOMÍNIO
% --------------------------
% paciente(ID, Nome, DataNasc, Sexo, Morada)
paciente(p001, 'Ana Silva', '1985-05-20', feminino, 'Rua A').
paciente(p002, 'Joao Santos', '1970-11-10', masculino, 'Av. B').
paciente(p003, 'Maria Alves', '1995-03-15', feminino, 'Local C').
% exemplo de incerto
paciente(p004, 'Carlos', incerto, masculino, 'Rua D').                % Data incerta
% exemplo de interdito
paciente(p005, 'Sofia', '1990-01-01', feminino, interdito(morada)).   % Morada interdito

% consulta(IDconsulta, Data, IDpaciente, Idade, Diastolica, Sistolica, Pulsacao)
consulta(c001, '2025-10-01', p001, 40, 80, 120, 75).
consulta(c002, '2025-10-15', p001, 40, 85, 135, 80).
consulta(c003, '2025-10-01', p002, 54, 100, 165, 88).
% consulta com imprecisões
consulta(c004, '2025-11-01', p004, 35, impreciso(75,80), impreciso(115,125), 70).

% tensao_arterial(ID, ClasseAtom, SistMin, SistMax, DiastMin, DiastMax)
tensao_arterial(ta_optima,       optima,     0, 120, 0, 80).
tensao_arterial(ta_normal,       normal,     120, 130, 80, 85).
tensao_arterial(ta_normal_alta,  normal_alta,130, 140, 85, 90).
tensao_arterial(ta_hipert1,      hipertensao1,140,160, 90,100).
tensao_arterial(ta_hipert2,      hipertensao2,160,180,100,110).
tensao_arterial(ta_hipert3,      hipertensao3,180,10000,110,10000).
tensao_arterial(ta_hipotensao,   hipotensao,  0, 90,   0, 60).

% --------------------------
% P1 — Regras de classificação
% --------------------------
% classificar_leitura/3: se valores numéricos, devolve uma classe
classificar_leitura(Sist, Diast, Classe) :-
    number(Sist), number(Diast),
    tensao_arterial(_, Classe, SI, SS, DI, DS),
    Sist >= SI, Sist < SS,
    Diast >= DI, Diast < DS.

% -----------------------------------------
% P3 — Análise intervalar (tratamento impreciso)
% -----------------------------------------
% classificacao_intervalar/3: trata imprecisos e mistos
classificacao_intervalar(Syst, Diast, Classe) :-
    Syst = impreciso(Smin, Smax),
    Diast = impreciso(Dmin, Dmax),
    ( (Smin >= 140 ; Dmin >= 90) -> Classe = hipertensao_certeza
    ; (Smax >= 140 ; Dmax >= 90) -> Classe = hipertensao_possivel
    ; Classe = indeterminado ).

classificacao_intervalar(Syst, Diast, Classe) :-
    Syst = impreciso(Smin, Smax),
    number(Diast),
    ( (Smin >= 140 ; Diast >= 90) -> Classe = hipertensao_certeza
    ; (Smax >= 140 ; Diast >= 90) -> Classe = hipertensao_possivel
    ; Classe = indeterminado ).

classificacao_intervalar(Syst, Diast, Classe) :-
    Diast = impreciso(Dmin, Dmax),
    number(Syst),
    ( (Syst >= 140 ; Dmin >= 90) -> Classe = hipertensao_certeza
    ; (Syst >= 140 ; Dmax >= 90) -> Classe = hipertensao_possivel
    ; Classe = indeterminado ).

% delega para classificar_leitura quando ambos são números
classificacao_intervalar(Syst, Diast, Classe) :-
    classificar_leitura(Syst, Diast, Classe).

% --------------------------
% P1 — Funções estatísticas
% --------------------------
% media das últimas N leituras sistólicas (se existirem)
media_sistolica(ID_P, N, Media) :-
    findall(S, (consulta(_, _, ID_P, _, _, S, _), number(S)), ListaS),
    take_last(N, ListaS, Ultimas),
    Ultimas \= [],
    sum_list(Ultimas, Soma),
    length(Ultimas, L),
    Media is Soma / L.

take_last(N, List, Last) :-
    length(List, L),
    ( L =< N -> Last = List
    ; length(Last, N), append(_, Last, List)
    ).

% --------------------------
% Coerência de leituras
% --------------------------
coerente_leitura(Sist, Diast) :-
    ( number(Sist), number(Diast) -> Sist > Diast
    ; % se alguma é imprecisa, verificar limites
      (Sist = impreciso(Smin,Smax) -> Smin =< Smax ; true),
      (Diast = impreciso(Dmin,Dmax) -> Dmin =< Dmax ; true)
    ).

% --------------------------
% P2 — RACIONCÍNIO E NEGAÇÃO
% --------------------------
% conhecimento negativo explícito
neg(historial_doenca(p001, diabetes)).   % p001 NÃO tem historial de diabetes

% hipertensao_registada/1: considera imprecisões via classificacao_intervalar
hipertensao_registada(ID_P) :-
    consulta(_, _, ID_P, _, Diast, Sist, _),
    ( number(Diast), number(Sist) -> (Diast >= 90 ; Sist >= 140)
    ; classificacao_intervalar(Sist, Diast, C),
      (C = hipertensao_certeza ; C = hipertensao_possivel)
    ).

diagnostico_risco(ID_P, risco_cardiovascular_elevado) :-
    hipertensao_registada(ID_P),
    \+ diagnostico_risco(ID_P, hipotensao_cronica).

diagnostico_risco(ID_P, hipotensao_cronica) :-
    \+ hipertensao_registada(ID_P),
    findall(C, (consulta(C,_,ID_P,_,D,S,_), number(D), number(S), classificar_leitura(S,D,hipotensao)), L),
    length(L, N), N >= 2.

% si/2: meta-predicado determinístico para verdadeiro/falso/desconhecido
si(Questao, verdadeiro) :- call(Questao), !.
si(Questao, falso) :- neg(Questao), !.
si(Questao, falso) :- \+ call(Questao), !.
si(Questao, desconhecido) :- \+ call(Questao), \+ neg(Questao).

% --------------------------
% P3 — INVARIANTES FORMAIS E EVOLUÇÃO/INVOLUÇÃO
% --------------------------
% Invariante: ID de paciente único
+paciente(ID,_,_,_,_) :: (
    findall(ID, paciente(ID,_,_,_,_), L),
    length(L, N),
    N =:= 1
).

% Invariante: não inserir paciente se existir negação forte
+paciente(ID,_,_,_,_) :: (
    \+ neg(paciente(ID,_,_,_,_))
).

% Invariante: proteger valores interditos — não permitir revelar substituindo
+paciente(ID, _, _, Sexo, _) :: (
    findall((N,D,M), paciente(ID,N,D,Sexo,M), L),
    forall(member((N0,D0,M0), L),
           \+ (N0 == interdito ; D0 == interdito ; M0 == interdito))
).

% Invariante: não remover paciente se existirem consultas
-paciente(ID,_,_,_,_) :: (
    \+ consulta(_,_,ID,_,_,_,_)
).

% Invariante para consulta: paciente deve existir
+consulta(_,_,ID,_,_,_,_) :: (
    ( paciente(ID,_,_,_,_) ; \+ neg(paciente(ID,_,_,_,_)) )
).

% Invariante de coerência para consulta
+consulta(_,_,_,_,Diast,Sist,_) :: (
    ( number(Diast), number(Sist) -> Sist > Diast ; true )
).

% mecanismo genérico de evolução/involução com verificação de invariantes
evolucao(Termo) :-
    findall(Inv, +Termo::Inv, ListaInv),
    assertz(Termo),
    ( teste(ListaInv) -> true
    ; retract(Termo), fail
    ).

involucao(Termo) :-
    findall(Inv, -Termo::Inv, ListaInv),
    retract(Termo),
    ( teste(ListaInv) -> true
    ; assertz(Termo), fail
    ).

teste([]).
teste([R|Rs]) :- R, teste(Rs).

% wrappers para API
inserir_paciente(ID, Nome, Data, Sexo, Morada) :-
    evolucao(paciente(ID, Nome, Data, Sexo, Morada)).

remover_paciente(ID) :-
    paciente(ID, _, _, _, _),
    involucao(paciente(ID, _, _, _, _)).

inserir_consulta(ID, Data, PID, Idade, Diast, Sist, Puls) :-
    evolucao(consulta(ID, Data, PID, Idade, Diast, Sist, Puls)).

remover_consulta(ID) :-
    consulta(ID, Data, PID, _, Diast, Sist, Puls),
    involucao(consulta(ID, Data, PID, _, Diast, Sist, Puls)).

% --------------------------
% Utilitários (lista, demo, relatórios)
% --------------------------
listar_pacientes :-
    writeln('--- Pacientes ---'),
    forall(paciente(ID, Nome, Data, Sexo, Morada),
           format('~w | ~w | ~w | ~w | ~w~n', [ID, Nome, Data, Sexo, Morada])).

listar_consultas :-
    writeln('--- Consultas ---'),
    forall(consulta(ID, Data, PID, Idade, Diast, Sist, Puls),
           format('~w | Paciente: ~w | Data: ~w | Idade: ~w | ~w/~w | Puls: ~w~n', 
                  [ID, PID, Data, Idade, Sist, Diast, Puls])).

% relatório simples de um paciente (histórico + classificação por consulta)
relatorio_paciente(ID_P) :-
    paciente(ID_P, Nome, _, _, _),
    format('Relatório — ~w (~w)~n', [Nome, ID_P]),
    findall((DataC,Idade,Diast,Sist,Class),
            ( consulta(_, DataC, ID_P, Idade, Diast, Sist, _),
              ( classificacao_intervalar(Sist, Diast, Class)
              ; (number(Sist), number(Diast), classificar_leitura(Sist, Diast, Class))
              )
            ),
            L),
    ( L = [] -> writeln('Sem consultas registadas.')
    ; forall(member((DataC,Idade,Diast,Sist,Class), L),
             format('~w | Idade: ~w | Tensão: ~w/~w | Class: ~w~n', 
                    [DataC, Idade, Sist, Diast, Class]))
    ).

% demo automatizada
demo :-
    writeln('=== Demo Projeto Tensão Arterial ==='), nl,
    listar_pacientes, nl,
    listar_consultas, nl,
    writeln('Testes de classificação:'),
    classificacao_intervalar(impreciso(150,160), impreciso(95,110), C1),
    format('Intervalar (150-160 / 95-110) -> ~w~n', [C1]),
    classificacao_intervalar(impreciso(125,145), impreciso(70,88), C2),
    format('Intervalar (125-145 / 70-88) -> ~w~n', [C2]),
    ( inserir_paciente(px, 'Novo', '2000-01-01', masculino, 'Lugar') -> writeln('Inseriu px') ; writeln('Falha inserir px') ),
    ( inserir_paciente(p001, 'Duplicado', '1990-01-01', masculino, 'Lugar') -> writeln('Inseriu duplicado (ERRO)') ; writeln('Erro esperado: ID duplicado') ),
    ( remover_paciente(p001) -> writeln('Removido p001 (ERRO: deveria falhar por consultas)') ; writeln('Remoção p001 bloqueada por consultas (OK)') ),
    writeln('=== Fim Demo ===').

