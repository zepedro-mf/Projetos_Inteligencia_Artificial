
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
paciente(p1, 'Ana Silva', date(1980,5,10), 'masculino', 'Rua A, 12').
paciente(p2, 'João Costa', date(1975,2,20), 'masculino', 'Rua B, 45').
paciente(p3, 'Maria Dias', date(1990,7,1), 'feminino', 'Rua C, 9').

% consulta(IdConsulta, Data, IdPaciente, Idade, Diastolica, Sistolica, Pulsacao, Peso, Altura, Temperatura)
consulta(c1, date(2025,10,10), p1, 45, 80, 120, 58, 48, 165, 38).  % tensão normal, bradicardia, magreza, febre
consulta(c2, date(2025,10,12), p2, 50, 95, 150, 103, 90, 180, 36).  % hipertensão, taquicardia, sobrepeso, temperatura normal
consulta(c3, date(2025,10,15), p3, 34, 55, 85, 65, 75, 16, 34.5).   % hipotensão, pulsação normal, peso normal, hipotermia

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
% 4. Regras de diagnóstico
% --------------------------------------------
% Classificação da Tensão Arterial usando si/2
classificar_ta(Sistolica, Diastolica, Res) :-
    tensao_arterial(_, Classificacao, SistolicaInf, SistolicaSup, DiastolicaInf, DiastolicaSup),
    Sistolica >= SistolicaInf,
    Sistolica =< SistolicaSup,
    Diastolica >= DiastolicaInf,
    Diastolica =< DiastolicaSup,
    Res = Classificacao.

% Cálculo com verificação de coerência
calcular_classificacao_ta(IdPac, Classificacao) :-
    consulta(_,(_,_,_),IdPac,_, Diastolica, Sistolica, _),
    classificar_ta(Sistolica, Diastolica, Classificacao).

% Diagnósticos específicos usando negação
pacientes_hipertensos(Pacientes) :-
    findall(Nome, 
            (consulta(_,_,IdPac,_,Diastolica,Sistolica,_),
             paciente(IdPac,Nome,_,_,_),
             (Diastolica > 90; Sistolica > 140)), 
            Pacientes).

% Verificação de condição específica
tem_ta_otima(IdPac) :-
    consulta(_,_,IdPac,_,Diastolica,Sistolica,_),
    classificar_ta(Sistolica, Diastolica, otima).



% Classificação de tensão arterial pontual
classifica_tensao(S, D, Classificacao) :-
    (S >= 90, S =< 120, D >= 60, D =< 79) -> Classificacao = normal
    ; (S >= 120 ; D >= 90) -> Classificacao = hipertensao
    ; (S < 90 ; D < 60) -> Classificacao = hipotensao
    .

% Classificação da tensão arterial de um paciente
classifica_tensao_paciente(IdConsulta, Classificacao) :-
    consulta(IdConsulta, _, _, _, D, S, _, _, _, _),
    ( (S >= 90, S =< 120, D >= 60, D =< 79) -> Classificacao = normal
    ; (S >= 120 ; D >= 90) -> Classificacao = hipertensao
    ; (S < 90 ; D < 60) -> Classificacao = hipotensao
    ).

% Classificação pontual de um pulso
classifica_pulso(Pulso, Classificacao) :-
    ( Pulso >= 60, Pulso =< 100 -> Classificacao = normal
    ; Pulso < 60 -> Classificacao = bradicardia
    ; Pulso > 100 -> Classificacao = taquicardia
    ).

% Classificação do pulso de um paciente
classifica_pulso_paciente(IdConsulta, Classificacao) :-
    consulta(IdConsulta, _, _, _, _, _, Pulso, _, _, _),
    ( Pulso >= 60, Pulso =< 100 -> Classificacao = normal
    ; Pulso < 60 -> Classificacao = bradicardia
    ; Pulso > 100 -> Classificacao = taquicardia
    ).

% Determinação pontual do IMC
determinar_imc(Peso, Altura, Classificacao) :-
    AlturaM is Altura / 100,
    IMC is Peso / (AlturaM * AlturaM),
    write('IMC: '), write(IMC), nl,
    ( IMC < 18.5 -> Classificacao = magreza
    ; (IMC >= 18.5, IMC < 24.9) -> Classificacao = normal
    ; (IMC >= 25, IMC < 29.9) -> Classificacao = sobrepeso
    ; IMC >= 30 -> Classificacao = obesidade
    ).

% Determinação do IMC de um paciente
determinar_imc_paciente(Paciente, Classificacao) :-
    consulta(_, _, Paciente, _, _, _, _, Peso, Altura, _),
    AlturaM is Altura / 100,
    IMC is Peso / (AlturaM * AlturaM),
    write('IMC: '), write(IMC), nl,
    ( IMC < 18.5 -> Classificacao = magreza
    ; (IMC >= 18.5, IMC < 24.9) -> Classificacao = normal
    ; (IMC >= 25, IMC < 29.9) -> Classificacao = sobrepeso
    ; IMC >= 30 -> Classificacao = obesidade
    ).

% Avaliação pontual da temperatura corporal
classifica_temperatura(Temp, Classificacao) :-
    ( Temp >= 36.5, Temp =< 37.5 -> Classificacao = normal
    ; Temp > 37.5 -> Classificacao = febre
    ; Temp < 35 -> Classificacao = hipotermia
    ). 

% Avaliação da temperatura corporal de um paciente
classifica_temperatura_paciente(IdConsulta, Classificacao) :-
    consulta(IdConsulta, _, _, _, _, _, _, _, _, Temp),
    ( Temp >= 36.5, Temp =< 37.5 -> Classificacao = normal
    ; Temp > 37.5 -> Classificacao = febre
    ; Temp < 35 -> Classificacao = hipotermia
    ).