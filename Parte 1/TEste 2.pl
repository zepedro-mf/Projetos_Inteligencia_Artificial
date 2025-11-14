% =========================================================
% Inteligência Artificial em Engenharia Biomédica
% TRABALHO DE GRUPO - TEst-2 CONSOLIDADO (Integração de Módulo P2)
% Versão: IDs Alfanuméricos (p001, c001, etc.)
% =========================================================

% =========================================================
% SUPRESSÃO DE WARNINGS (adicionar no início do ficheiro)
% =========================================================

:- discontiguous paciente/5.
:- discontiguous consulta/7.
:- discontiguous tensao/6.
:- discontiguous excecao/1.
:- discontiguous (-)/1.

% Desativar warnings de variáveis singleton
:- style_check(-singleton).

% --------------------------------------------
% 0. Directives and Operators (Atualizados)
% --------------------------------------------

:- dynamic (paciente/5). 
:- dynamic (consulta/7).
:- dynamic (tensao/6).

:- dynamic ('-'/1).
:- dynamic excecao/1.
:- dynamic interdito/1.
:- dynamic negativo/1.

:- op(900, xfy, '::').
:- op(900, xfy, 'e').
:- op(900, xfy, 'ou').

% --------------------------------------------
% 1. SISTEMA DE INFERÊNCIA
% --------------------------------------------

% Negação por Falha (de Módulo P2.pl)
nao(Questao) :- Questao, !, fail.
nao(_).

% Verificação de interditos - trata como desconhecido
e_interdito(paciente(Id,_,_,_,_)) :- interdito(Id).
e_interdito(consulta(Id,_,_,_,_,_,_)) :- interdito(Id).
e_interdito(tensao(Id,_,_,_,_,_)) :- interdito(Id).

% Extensao do meta-predicado si: Questao, Resposta -> {V,F,D}
si( Questao, desconhecido) :- e_interdito(Questao), !.
si( Questao, verdadeiro):- Questao.
si( Questao, falso):- -Questao.
si( Questao, desconhecido) :- nao(Questao), nao(-Questao).

% Extensao do meta-predicado siC: Questao1, Questao2, Resposta -> {V,F,D}
siC( Q1, Q2, verdadeiro) :- 
    si(Q1, verdadeiro), 
    si(Q2, verdadeiro).
siC( Q1, Q2, falso) :- 
    si(Q1, verdadeiro), 
    si(Q2, falso);
    si(Q1, falso), 
    si(Q2, verdadeiro);
    si(Q1, falso), 
    si(Q2, falso).
siC( Q1, Q2, desconhecido) :- nao(siC(Q1, Q2, verdadeiro)), nao(siC(Q1, Q2, falso)).

% Extensao do meta-predicado siD: Questao1, Questao2, Resposta -> {V,F,D}
siD( Q1, Q2, verdadeiro) :- 
    si(Q1, verdadeiro); 
    si(Q2, verdadeiro).
siD( Q1, Q2, falso) :- 
    si(Q1, falso), 
    si(Q2, falso).
siD( Q1, Q2, desconhecido) :- nao(siD(Q1, Q2, verdadeiro)), nao(siD(Q1, Q2, falso)).

% --------------------------------------------
% 2. COMPRIMENTO / AUXILIAR
% --------------------------------------------

comprimento([], 0).
comprimento([_|T], N) :-
    comprimento(T, N1),
    N is N1 + 1.

% =========================================================
%             3. PRESSUPOSTO DO MUNDO FECHADO (CWA)
% =========================================================

% CWA para paciente (Atualizado para aridade /5)
-paciente(Id,Nome,DataNasc,Sexo,Morada) :-
    nao(paciente(Id,Nome,DataNasc,Sexo,Morada)),
    nao(excecao(paciente(Id,Nome,DataNasc,Sexo,Morada))).

% CWA para consulta
-consulta(Id,Data,Pac,Idade,Dia,Sis,Pulso) :-
    nao(consulta(Id,Data,Pac,Idade,Dia,Sis,Pulso)),
    nao(excecao(consulta(Id,Data,Pac,Idade,Dia,Sis,Pulso))).

% CWA para tensao
-tensao(Id,Class,SistInf,SistSup,DiastInf,DiastSup) :-
    nao(tensao(Id,Class,SistInf,SistSup,DiastInf,DiastSup)),
    nao(excecao(tensao(Id,Class,SistInf,SistSup,DiastInf,DiastSup))).

% Conhecimento perfeito negativo explícito (IDs não foram alterados aqui, pois são externos)
-paciente(636237854, jose, (10,2,1969), masculino, cascais).
-paciente(325544694, ricardo, (5,5,2005), masculino, faro).
-consulta(_, (25,12,2023), _, _, _, _, _).
-consulta(cons999, (14,2,2024), _, _, _, _, _).

% =========================================================
%             4. CONHECIMENTO PERFEITO
% =========================================================


paciente(p001, 'Ana Silva', date(1980,5,10), 'feminino', 'Rua A, 12').
paciente(p002, 'João Costa', date(1975,2,20), 'masculino', 'Rua B, 45').
paciente(p003, 'Maria Dias', date(1990,7,1), 'feminino', 'Rua C, 9').
paciente(p004, 'Carlos Lima', date(1966,11,15), 'masculino', 'Av. D, 10').
paciente(p005, 'Daniela Faria', date(1993,8,22), 'feminino', 'Trav. E, 1').
paciente(p006, 'Eduardo Gomes', date(1959,3,30), 'masculino', 'Lg. F, 2').
paciente(p007, 'Filipa Henriques', date(1989,4,12), 'feminino', 'Pr. G, 15').
paciente(p008, 'Gonçalo Isidro', date(1977,1,7), 'masculino', 'Beco H, 3').
paciente(p009, 'Inês Janeiro', date(1996,10,25), 'feminino', 'Estrada I, 7').

% consulta(IdConsulta, Data, IdPaciente, Idade, Diastolica, Sistolica, Pulsacao)
consulta(c001, (9,1,2024), p001, 38, 82, 125, 72). % TA Normal
consulta(c002, (10,1,2024), p004, 58, 92, 148, 76). % Hipertensão Grau 1
consulta(c003, (11,1,2024), p005, 31, 76, 118, 65). % TA Ótima
consulta(c004, (12,1,2024), p002, 51, 88, 138, 74). % TA Normal Alta
consulta(c005, (15,1,2024), p006, 65, 105, 168, 82). % Hipertensão Grau 2
consulta(c006, (16,1,2024), p007, 35, 84, 128, 70). % TA Normal
consulta(c007, (17,1,2024), p008, 48, 95, 152, 78). % Hipertensão Grau 1
consulta(c008, (18,1,2024), p003, 55, 79, 119, 66). % TA Ótima
consulta(c009, (19,1,2024), p009, 28, 58, 88, 62). % Hipotensão


% tensao(Id, Class, SistInf, SistSup, DiastInf, DiastSup)
tensao(ta01, hipotensao, 0, 90, 0, 60).
tensao(ta02, otima, 90, 120, 60, 80).
tensao(ta03, normal, 120, 130, 80, 85).
tensao(ta04, normal_alta, 130, 140, 85, 90).
tensao(ta05, hipertensao_grau1, 140, 160, 90, 100).
tensao(ta06, hipertensao_grau2, 160, 180, 100, 110).
tensao(ta07, hipertensao_grau3, 180, 300, 110, 300).

% =========================================================
%             5. CONHECIMENTO NEGATIVO
% =========================================================

% Negação para consistência: tensão sistólica deve ser sempre superior à diastólica
-tensao(_,_,SistInf,_,DiastInf,_) :- SistInf < DiastInf.

% =========================================================
%             6. CONHECIMENTO IMPERFEITO
% =========================================================

% INCERTO: pulsação não registada (Atualização do ID do paciente e consulta)
consulta(c_incerto_001, (3,10,2025), p001, 40, 85, 125, desconhecido).
excecao(consulta(Id,Data,Pac,Idade,Dia,Sis,Pulso)) :-
    consulta(Id,Data,Pac,Idade,Dia,Sis,desconhecido).

% INTERDITO: paciente sem número de utente conhecido
paciente(num_desconhecido, dario, (15,7,1980), masculino, 'desconhecido').
excecao(paciente(Id,Nome,Data,Sexo,Morada)) :- 
    paciente(num_desconhecido,Nome,Data,Sexo,Morada),
    Id == num_desconhecido.
interdito(num_desconhecido).

% INVARIANTES 
+paciente(Id,Nome,Data,Sexo,Morada) ::
    (findall(Id, paciente(Id,_,_,_,_), S),
     comprimento(S,N),
     N == 0).  

+consulta(Id,_,_,_,_,_,_) ::
    (findall(Id, consulta(Id,_,_,_,_,_,_), S),
     comprimento(S,N),
     N == 0).  

+consulta(_,_,_,_,Dia,Sis,Pulso) ::
    (Dia > 40, Dia < 200,
     Sis > 60, Sis < 300,
     Pulso > 30, Pulso < 200,
     Sis > Dia).  % Sistólica > Diastólica

% =========================================================
%             9. RACIOCÍNIO: CLASSIFICAÇÃO E RISCO
% =========================================================

% --- Classificação de Tensão Arterial (Nova regra de Módulo P2.pl) ---
classificar_ta(Sistolica, Diastolica, Res) :-
    tensao(_, Res, SistolicaInf, SistolicaSup, DiastolicaInf, DiastolicaSup),
    Sistolica >= SistolicaInf,
    Sistolica =< SistolicaSup,
    Diastolica >= DiastolicaInf,
    Diastolica =< DiastolicaSup, !.

% Classificação para um paciente (Adaptado para usar a nova classificar_ta/3)
classificar_tensao(Pac, Classe) :-
    consulta(_,_,Pac,_,Diastolica,Sistolica,_),
    classificar_ta(Sistolica, Diastolica, Classe).

% --- Verificação de Hipertensão (Nova regra de Módulo P2.pl) ---
tem_hipertensao(IdPac) :-
    consulta(_,_,IdPac,_,Diastolica,Sistolica,_),
    (classificar_ta(Sistolica, Diastolica, hipertensao_grau1);
     classificar_ta(Sistolica, Diastolica, hipertensao_grau2);
     classificar_ta(Sistolica, Diastolica, hipertensao_grau3)).

% --- Avaliação de Risco (Adaptado de TEst-2 para as novas Classes) ---

avaliar_risco(Pac, baixo) :-
    (classificar_tensao(Pac, otima); classificar_tensao(Pac, normal)), !.

avaliar_risco(Pac, moderado) :-
    classificar_tensao(Pac, normal_alta), !.

avaliar_risco(Pac, alto) :-
    tem_hipertensao(Pac), !.

avaliar_risco(_, desconhecido).

% =========================================================
%             10. CONSULTAS E RELATÓRIOS
% =========================================================

% Relatório detalhado (Atualizado para paciente/5)
relatorio_paciente_detalhado(Pac) :-
    paciente(Pac, Nome, DataNasc, Sexo, Morada),
    (classificar_tensao(Pac, Classe) -> true ; Classe = 'indeterminado'),
    (avaliar_risco(Pac, Risco) -> true ; Risco = 'indeterminado'),
    
    format('=== RELATORIO MEDICO DETALHADO ===~n', []),
    format('Paciente: ~w~n', [Nome]),
    format('Data Nascimento: ~w~n', [DataNasc]),
    format('Sexo: ~w~n', [Sexo]),
    format('Morada: ~w~n', [Morada]),
    format('Classificação Tensão: ~w~n', [Classe]),
    format('Nível de Risco: ~w~n', [Risco]),
    format('--- Histórico de Consultas ---~n', []),
    listar_consultas(Pac).

% Listar consultas de um paciente 
listar_consultas(Pac) :-
    findall((Data, Diastolica, Sistolica, Pulso), consulta(_, Data, Pac, _, Diastolica, Sistolica, Pulso), L),
    (L = [] -> 
        format('  Sem consultas registadas~n', [])
    ;
        format('Consultas do paciente ~w:~n', [Pac]),
        listar_consultas_aux(L)
    ).

% Auxiliar de listagem
listar_consultas_aux([]).
listar_consultas_aux([(Data, Dia, Sis, Pulso)|T]) :-
    format('  Data: ~w | TA Diastólica: ~w | TA Sistólica: ~w | Pulsação: ~w~n', [Data, Dia, Sis, Pulso]),
    listar_consultas_aux(T).

listar_pacientes :-
    findall((Id,Nome,DataNasc,Sexo,Morada), paciente(Id,Nome,DataNasc,Sexo,Morada), L),
    ( L = [] ->
        format('Sem pacientes registados~n', [])
    ;
        format('--- Lista de Pacientes ---~n', []),
        listar_pacientes_aux(L)
    ).

listar_pacientes_aux([]).
listar_pacientes_aux([(Id,Nome,DataNasc,Sexo,Morada)|T]) :-
    format('ID: ~w | Nome: ~w | Data Nasc: ~w | Sexo: ~w | Morada: ~w~n', [Id,Nome,DataNasc,Sexo,Morada]),
    listar_pacientes_aux(T).

% ------------------------------------------------
% 11. EVOLUÇÃO / INVOLUÇÃO (inserção / remoção segura com invariantes)
% ------------------------------------------------

% evolucao(+Termo) : insere Termo se todos os invariantes +Termo::Inv forem satisfeitos
evolucao(Termo) :-
    findall(Inv, (+Termo::Inv), ListaInv),
    testar(ListaInv),
    assertz(Termo).

% involucao(+Termo) : remove Termo se todos os invariantes -Termo::Inv (se existirem) forem satisfeitos
involucao(Termo) :-
    findall(Inv, (-Termo::Inv), ListaInv),
    testar(ListaInv),
    retract(Termo).

% testar(+ListaInvariantes) : avalia cada invariante (cada Inv é um goal)
testar([]). 
testar([Inv|R]) :-
    call(Inv),
    testar(R).

% ------------------------------------------------
% 12. ATUALIZAÇÃO DE PACIENTES
% ------------------------------------------------

% atualizar_paciente(+IdPaciente, +NovoNome, +NovaData, +NovoSexo, +NovaRua)
% Permite atualizar dados de um paciente existente
atualizar_paciente(Id, NovoNome, NovaData, NovoSexo, NovaRua) :-
    paciente(Id, _Nome, _Data, _Sexo, _Rua),
    retract(paciente(Id, _Nome, _Data, _Sexo, _Rua)),
    assertz(paciente(Id, NovoNome, NovaData, NovoSexo, NovaRua)).

% atualizar_nome_paciente(+IdPaciente, +NovoNome)
atualizar_nome_paciente(Id, NovoNome) :-
    paciente(Id, _Nome, Data, Sexo, Rua),
    retract(paciente(Id, _Nome, Data, Sexo, Rua)),
    assertz(paciente(Id, NovoNome, Data, Sexo, Rua)).

% atualizar_data_paciente(+IdPaciente, +NovaData)
atualizar_data_paciente(Id, NovaData) :-
    paciente(Id, Nome, _Data, Sexo, Rua),
    retract(paciente(Id, Nome, _Data, Sexo, Rua)),
    assertz(paciente(Id, Nome, NovaData, Sexo, Rua)).

% atualizar_morada_paciente(+IdPaciente, +NovaMorada)
atualizar_morada_paciente(Id, NovaMorada) :-
    paciente(Id, Nome, Data, Sexo, _Rua),
    retract(paciente(Id, Nome, Data, Sexo, _Rua)),
    assertz(paciente(Id, Nome, Data, Sexo, NovaMorada)).



