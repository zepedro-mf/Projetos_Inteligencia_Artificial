
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
paciente(p1, 'Ana Silva', date(1980,5,10), f, 'Rua A, 12').
paciente(p2, 'João Costa', date(1975,2,20), m, 'Rua B, 45').
paciente(p3, 'Maria Dias', date(1990,7,1), f, 'Rua C, 9').

% consulta(IdConsulta, Data, IdPaciente, Idade, Diastolica, Sistolica, Pulsacao, Peso, Altura, Temperatura)
consulta(c1, date(2025,10,10), p1, 45, 80, 120, 58, 48, 165, 38).  % tensão normal, bradicardia, magreza, febre
consulta(c2, date(2025,10,12), p2, 50, 95, 150, 103, 90, 180, 36).  % hipertensão, taquicardia, sobrepeso, temperatura normal
consulta(c3, date(2025,10,15), p3, 34, 55, 85, 65, 75, 16, 34.5).   % hipotensão, pulsação normal, peso normal, hipotermia


% --------------------------------------------
% 1. Negação por falha (negation as failure)
% --------------------------------------------
nao(Questao) :- Questao, !, fail.
nao(_).                


% --------------------------------------------
% 2. Sistema de inferência trivalorado (si/2)
% --------------------------------------------
si(Questao, verdadeiro) :- Questao, !.
si(Questao, falso) :- negativo(Questao), !.
si(_, desconhecido).


% --------------------------------------------
% 3. Regras de diagnóstico
% --------------------------------------------

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