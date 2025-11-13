
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
% 4. Regras de diagnóstico da tensão arterial
% --------------------------------------------
% Classificação pontual da tensão arterial
classificar_ta(Sistolica, Diastolica, Res) :-
    tensao_arterial(_, Classificacao, SistolicaInf, SistolicaSup, DiastolicaInf, DiastolicaSup),
    Sistolica >= SistolicaInf,
    Sistolica =< SistolicaSup,
    Diastolica >= DiastolicaInf,
    Diastolica =< DiastolicaSup,
    Res = Classificacao.

% Classificação da tensão arterial de um paciente
classificar_ta_paciente(IdPac, Res) :-
    consulta(_,(_,_,_),IdPac,_, Diastolica, Sistolica, _),
    Sistolica >= SistolicaInf,
    Sistolica =< SistolicaSup,
    Diastolica >= DiastolicaInf,
    Diastolica =< DiastolicaSup,
    Res = Classificacao.

% Diagnóstico específico de hipertensao grau 1, 2 e 3
pacientes_hipertensos(Pacientes) :-
    findall(Nome, 
            (consulta(_,_,IdPac,_,Diastolica,Sistolica,_),
             paciente(IdPac,Nome,_,_,_),
             (classificar_ta(Sistolica, Diastolica, hipertensao_grau1);
              classificar_ta(Sistolica, Diastolica, hipertensao_grau2);
              classificar_ta(Sistolica, Diastolica, hipertensao_grau3))), 
            Pacientes).

% Diagnóstico específico de tensão arterial normal e otima
pacientes_normal(Pacientes) :-
    findall(Nome, 
            (consulta(_,_,IdPac,_,Diastolica,Sistolica,_),
             paciente(IdPac,Nome,_,_,_),
             (classificar_ta(Sistolica, Diastolica, otima);
              classificar_ta(Sistolica, Diastolica, normal))), 
            Pacientes).

% Diagnóstico específico de tensão arterial normal alta
pacientes_normal_alta(Pacientes) :-
    findall(Nome, 
            (consulta(_,_,IdPac,_,Diastolica,Sistolica,_),
             paciente(IdPac,Nome,_,_,_),
             classificar_ta(Sistolica, Diastolica, normal_alta)), 
            Pacientes).

% Diagnóstico específico de hipotensao
pacientes_hipotensos(Pacientes) :-
    findall(Nome, 
            (consulta(_,_,IdPac,_,Diastolica,Sistolica,_),
             paciente(IdPac,Nome,_,_,_),
             classificar_ta(Sistolica, Diastolica, hipotensao)), 
            Pacientes).

% Verificação de condicoes normais
tem_ta_normal_ou_otima(IdPac) :-
    consulta(_,_,IdPac,_,Diastolica,Sistolica,_),
    (classificar_ta(Sistolica, Diastolica, otima);
     classificar_ta(Sistolica, Diastolica, normal)).
tem_ta_otima(IdPac) :-
    consulta(_,_,IdPac,_,Diastolica,Sistolica,_),
    classificar_ta(Sistolica, Diastolica, otima).

tem_ta_normal(IdPac) :-
    consulta(_,_,IdPac,_,Diastolica,Sistolica,_),
    classificar_ta(Sistolica, Diastolica, normal).

tem_ta_normal_alta(IdPac) :-
    consulta(_,_,IdPac,_,Diastolica,Sistolica,_),
    classificar_ta(Sistolica, Diastolica, normal_alta).

% Verificacao de hipertensao
tem_hipertensao(IdPac) :-
    consulta(_,_,IdPac,_,Diastolica,Sistolica,_),
    (classificar_ta(Sistolica, Diastolica, hipertensao_grau1);
     classificar_ta(Sistolica, Diastolica, hipertensao_grau2);
     classificar_ta(Sistolica, Diastolica, hipertensao_grau3)).

tem_hipertensao_grau1(IdPac) :-
    consulta(_,_,IdPac,_,Diastolica,Sistolica,_),
    classificar_ta(Sistolica, Diastolica, hipertensao_grau1).

tem_hipertensao_grau2(IdPac) :-
    consulta(_,_,IdPac,_,Diastolica,Sistolica,_),
    classificar_ta(Sistolica, Diastolica, hipertensao_grau2).

tem_hipertensao_grau3(IdPac) :-
    consulta(_,_,IdPac,_,Diastolica,Sistolica,_),
    classificar_ta(Sistolica, Diastolica, hipertensao_grau3).

% Verificacao de hipotensao
tem_hipotensao(IdPac) :-
    consulta(_,_,IdPac,_,Diastolica,Sistolica,_),
    classificar_ta(Sistolica, Diastolica, hipotensao).