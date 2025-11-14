
% ============================================
% MÓDULO P2 — RACIOCÍNIO E NEGAÇÃO
% ============================================

:- dynamic paciente/5.
:- dynamic consulta/7.
:- dynamic negativo/1.

% --------------------------------------------
% 0. Base de Dados
% --------------------------------------------
% paciente(IdPaciente, Nome, DataNascimento, Sexo, Morada)
paciente(1002, 'Ana Silva', date(1980,5,10), 'masculino', 'Rua A, 12').
paciente(1005, 'João Costa', date(1975,2,20), 'masculino', 'Rua B, 45').
paciente(1009, 'Maria Dias', date(1990,7,1), 'feminino', 'Rua C, 9').
paciente(1010, 'Pedro Santos', date(1996,3,15), 'masculino', 'Rua D, 7').

% consulta(IdConsulta, Data, IdPaciente, Idade, Diastolica, Sistolica, Pulsacao)
consulta(cons_2024_002, (9,1,2024), 1002, 38, 82, 125, 72). % TA Normal
consulta(cons_2024_003, (10,1,2024), 1003, 58, 92, 148, 76). % Hipertensão Grau 1
consulta(cons_2024_004, (11,1,2024), 1004, 31, 76, 118, 65). % TA Ótima
consulta(cons_2024_005, (12,1,2024), 1005, 51, 88, 138, 74). % TA Normal Alta
consulta(cons_2024_006, (15,1,2024), 1006, 65, 105, 168, 82). % hipertensão Grau 2
consulta(cons_2024_007, (16,1,2024), 1007, 35, 84, 128, 70). % TA Normal
consulta(cons_2024_008, (17,1,2024), 1008, 48, 95, 152, 78). % Hipertensão Grau 1
consulta(cons_2024_009, (18,1,2024), 1009, 55, 79, 119, 66). % TA Ótima
consulta(cons_2024_010, (19,1,2024), 1010, 28, 58, 88, 62). % Hipotensão
consulta(cons_2024_011, (20,1,2024), 1009, 28, 58, 88, 62).
consulta(cons_2024_012, (19,2,2024), 1009, 28, 58, 88, 62).
consulta(cons_2024_013, (19,1,2023), 1009, 28, 58, 88, 62).

% tensao_arterial(IdTA, Classificacao, SistolicaInf, SistolicaSup, DiastolicaInf, DiastolicaSup)
tensao_arterial(ta01, hipotensao, 0, 90, 0, 60).
tensao_arterial(ta02, otima, 90, 120, 60, 80).
tensao_arterial(ta03, normal, 120, 130, 80, 85).
tensao_arterial(ta04, normal_alta, 130, 140, 85, 90).
tensao_arterial(ta05, hipertensao_grau1, 140, 160, 90, 100).
tensao_arterial(ta06, hipertensao_grau2, 160, 180, 100, 110).
tensao_arterial(ta07, hipertensao_grau3, 180, 300, 110, 300).


% --------------------------------------------
% 1. Negação por falha
% --------------------------------------------
nao(Questao) :- Questao, !, fail.
nao(_).


% --------------------------------------------
% 2. Conhecimento negativo
% --------------------------------------------
% Pressuposto Mundo Fechado
-paciente(IdPac,Nome,(Dia,Mes,Ano),Sexo,Morada) :- 
    nao(paciente(IdPac,Nome,(Dia,Mes,Ano),Sexo,Morada)),
    nao(excecao(paciente(IdPac,Nome,(Dia,Mes,Ano),Sexo,Morada))).

-consulta(IdConsulta,(Dia,Mes,Ano),IdPac,Idade,Diastolica,Sistolica,Pulsacao) :- 
    nao(consulta(IdConsulta,(Dia,Mes,Ano),IdPac,Idade,Diastolica,Sistolica,Pulsacao)),
    nao(excecao(consulta(IdConsulta,(Dia,Mes,Ano),IdPac,Idade,Diastolica,Sistolica,Pulsacao))).

% Conhecimento perfeito negativo explícito
-paciente(636237854, jose, (10,2,1969), masculino, cascais). % Paciente que faleceu
-paciente(325544694, ricardo, (5,5,2005), masculino, faro). % Paciente que mudou que clínica
-consulta(_, (25,12,2023), _, _, _, _, _). % Dia de greve dos funcionários da clínica
-consulta(cons999, (14,2,2024), _, _, _, _, _). % Dia em que o Médico adoeceu         


% --------------------------------------------
% 3. Sistemas de inferência
% --------------------------------------------
% Extensao do meta-predicado si: Questao, Resposta -> {V,F,D}
si( Questao, verdadeiro):- Questao.
si( Questao, falso):- -Questao.
si( Questao, desconhecido) :- nao(Questao), nao(-Questao).

% Extensao do meta-predicado siC: Questao1, Questao2, Resposta -> {V,F,D}
siC( Q1, Q2, verdadeiro) :- 
	si(Q1, verdadeiro), 
	si(Q2, verdadeiro).
siC( Q1, Q2, falso) :- 
	si(Q1, verdadeiro), 
	si(Q2, falso).
siC( Q1, Q2, desconhecido) :- 
	si( Q1, verdadeiro), 
	si( Q2, desconhecido).
siC( Q1, Q2, falso) :- 
	si( Q1, falso),
	si( Q2, verdadeiro).
siC( Q1, Q2, falso) :-
	si( Q1, falso),
	si( Q2, falso).
siC( Q1, Q2, falso) :-
	si( Q1, falso),
	si( Q2, desconhecido).
siC( Q1, Q2, desconhecido) :-
	si( Q1, desconhecido),
	si( Q2, verdadeiro).
siC( Q1, Q2, falso) :-
	si( Q1, desconhecido),
	si( Q2, falso).
siC( Q1, Q2, desconhecido) :-
	si( Q1, desconhecido),
	si( Q2, desconhecido).


% Extensao do meta-predicado siD: Questao1, Questao2, Resposta -> {V,F,D}
siD( Q1, Q2, verdadeiro) :-
	si( Q1, verdadeiro),
	si( Q2, verdadeiro).
siD( Q1, Q2, verdadeiro) :-
	si( Q1, verdadeiro),
	si( Q2, falso).
siD( Q1, Q2, verdadeiro) :-
	si( Q1, verdadeiro),
    si( Q2, desconhecido).
siD( Q1, Q2, verdadeiro) :-
	si( Q1, falso),
	si( Q2, verdadeiro).
siD( Q1, Q2, falso) :-
	si( Q1, falso),
	si( Q2, falso).
siD( Q1, Q2, desconhecido) :-
	si( Q1, falso),
	si( Q2, desconhecido).
siD( Q1, Q2, verdadeiro) :-
	si( Q1, desconhecido),
	si( Q2, verdadeiro).
siD( Q1, Q2, desconhecido) :-
	si( Q1, desconhecido),
	si( Q2, falso).
siD( Q1, Q2, desconhecido) :-
	si( Q1, desconhecido),
	si( Q2, desconhecido).


% --------------------------------------------
% Classificação pontual da tensão arterial
% --------------------------------------------
classificar_ta(Sistolica, Diastolica, Res) :-
    tensao_arterial(_, Classificacao, SistolicaInf, SistolicaSup, DiastolicaInf, DiastolicaSup),
    Sistolica >= SistolicaInf,
    Sistolica =< SistolicaSup,
    Diastolica >= DiastolicaInf,
    Diastolica =< DiastolicaSup,
    Res = Classificacao.


% ----------------------------------------------------
% Classificação da tensão arterial de um paciente
% ----------------------------------------------------
classificar_ta_paciente_aux(IdPac, ConsultasClassificadas) :-
    findall((Data, Classificacao),
            (consulta(_, Data, IdPac, _, Diastolica, Sistolica, _),
             classificar_ta(Sistolica, Diastolica, Classificacao)),
            Consultas),
    ordenar_consultas_por_data(Consultas, ConsultasClassificadas).

classificar_ta_paciente(IdPac) :-
    classificar_ta_paciente_aux(IdPac, ConsultasClassificadas),
    (ConsultasClassificadas = [] -> 
        format('Paciente ~w: Sem consultas registadas~n', [IdPac])
    ;
        paciente(IdPac, Nome, _, _, _),
        format('Classificação TA de ~w (ID: ~w):~n', [Nome, IdPac]),
        listar_classificacoes_aux(ConsultasClassificadas)
    ).

listar_classificacoes_aux([]).
listar_classificacoes_aux([(Data, Classificacao)|T]) :-
    format('  Data: ~w | Classificação: ~w~n', [Data, Classificacao]),
    listar_classificacoes_aux(T).

% -----------------------------------------------
% Diagnósticos específicos de tensão arterial
% -----------------------------------------------
pacientes_hipertensos(Pacientes) :-
    findall(Nome, 
            (consulta(_,_,IdPac,_,Diastolica,Sistolica,_),
             paciente(IdPac,Nome,_,_,_),
             (classificar_ta(Sistolica, Diastolica, hipertensao_grau1);
              classificar_ta(Sistolica, Diastolica, hipertensao_grau2);
              classificar_ta(Sistolica, Diastolica, hipertensao_grau3))), 
            Pacientes).

pacientes_normal(Pacientes) :-
    findall(Nome, 
            (consulta(_,_,IdPac,_,Diastolica,Sistolica,_),
             paciente(IdPac,Nome,_,_,_),
             (classificar_ta(Sistolica, Diastolica, otima);
              classificar_ta(Sistolica, Diastolica, normal))), 
            Pacientes).

pacientes_normal_alta(Pacientes) :-
    findall(Nome, 
            (consulta(_,_,IdPac,_,Diastolica,Sistolica,_),
             paciente(IdPac,Nome,_,_,_),
             classificar_ta(Sistolica, Diastolica, normal_alta)), 
            Pacientes).

pacientes_hipotensos(Pacientes) :-
    findall(Nome, 
            (consulta(_,_,IdPac,_,Diastolica,Sistolica,_),
             paciente(IdPac,Nome,_,_,_),
             classificar_ta(Sistolica, Diastolica, hipotensao)), 
            Pacientes).

tem_ta_normal_ou_otima(IdPac) :-
    consulta(_,_,IdPac,_,Diastolica,Sistolica,_),
    (classificar_ta(Sistolica, Diastolica, otima);
     classificar_ta(Sistolica, Diastolica, normal)).
 
% --------------------------------------------
% Listar consultas do paciente por ordem cronológica
% --------------------------------------------

% Predicado principal: listar consultas ordenadas por data (mais recente primeiro)
listar_consultas(Pac) :-
    findall((Data, Diastolica, Sistolica, Pulso), 
            consulta(_, Data, Pac, _, Diastolica, Sistolica, Pulso), 
            Consultas),
    (Consultas = [] -> 
        format('  Sem consultas registadas~n', [])
    ;
        ordenar_consultas_por_data(Consultas, ConsultasOrdenadas),
        format('Consultas do paciente ~w (do mais recente):~n', [Pac]),
        listar_consultas_aux(ConsultasOrdenadas)
    ).

% Ordenar lista de consultas por data (mais recente primeiro)
ordenar_consultas_por_data(Consultas, Ordenadas) :-
    ordenar_consultas_aux(Consultas, [], Ordenadas).

ordenar_consultas_aux([], Ordenada, Ordenada).
ordenar_consultas_aux([H|T], Accum, Ordenada) :-
    inserir_por_data(H, Accum, NovoAccum),
    ordenar_consultas_aux(T, NovoAccum, Ordenada).

% Inserir uma consulta na posição correta (ordenado por data)
inserir_por_data(Consulta, [], [Consulta]).
inserir_por_data(Consulta, [H|T], [Consulta, H|T]) :-
    consulta_mais_recente(Consulta, H).
inserir_por_data(Consulta, [H|T], [H|T1]) :-
    \+ consulta_mais_recente(Consulta, H),
    inserir_por_data(Consulta, T, T1).

% Verificar se a Consulta1 é mais recente que Consulta2
consulta_mais_recente((Data1, _, _, _), (Data2, _, _, _)) :-
    data_mais_recente(Data1, Data2).

% Comparar duas datas - retorna true se Data1 é mais recente que Data2
data_mais_recente((D1, M1, A1), (D2, M2, A2)) :-
    (A1 > A2 -> true;
     A1 =:= A2, M1 > M2 -> true;
     A1 =:= A2, M1 =:= M2, D1 > D2).

% Auxiliar de listagem (mantém o formato original)
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

% --------------------------------------------
% Versão com classificação da TA
% --------------------------------------------

% Listar consultas com classificação da TA
listar_consultas_com_classificacao(Pac) :-
    findall((Data, Diastolica, Sistolica, Pulso), 
            consulta(_, Data, Pac, _, Diastolica, Sistolica, Pulso), 
            Consultas),
    (Consultas = [] -> 
        format('  Sem consultas registadas~n', [])
    ;
        ordenar_consultas_por_data(Consultas, ConsultasOrdenadas),
        format('Consultas do paciente ~w: ~n', [Pac]),
        listar_consultas_com_class_aux(ConsultasOrdenadas)
    ).

% Auxiliar de listagem com classificação
listar_consultas_com_class_aux([]).
listar_consultas_com_class_aux([(Data, Dia, Sis, Pulso)|T]) :-
    classificar_ta(Sis, Dia, Classificacao),
    format('  Data: ~w | TA: ~w/~w | Pulsação: ~w | Classificação: ~w~n', 
           [Data, Sis, Dia, Pulso, Classificacao]),
    listar_consultas_com_class_aux(T).

% --------------------------------------------
% Funções auxiliares úteis
% --------------------------------------------

% Última consulta de um paciente
ultima_consulta_paciente(Pac, Data, Diastolica, Sistolica, Pulso) :-
    findall((Data, Diastolica, Sistolica, Pulso), 
            consulta(_, Data, Pac, _, Diastolica, Sistolica, Pulso), 
            Consultas),
    Consultas \= [],
    ordenar_consultas_por_data(Consultas, [(Data, Diastolica, Sistolica, Pulso)|_]).

% Primeira consulta de um paciente
primeira_consulta_paciente(Pac, Data, Diastolica, Sistolica, Pulso) :-
    findall((Data, Diastolica, Sistolica, Pulso), 
            consulta(_, Data, Pac, _, Diastolica, Sistolica, Pulso), 
            Consultas),
    Consultas \= [],
    ordenar_consultas_por_data(Consultas, ConsultasOrdenadas),
    last(ConsultasOrdenadas, (Data, Diastolica, Sistolica, Pulso)).