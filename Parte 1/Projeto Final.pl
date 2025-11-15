% =========================================================
% SUPRESSÃO DE WARNINGS
% =========================================================

:- discontiguous paciente/5.
:- discontiguous consulta/7.
:- discontiguous tensao/6.
:- discontiguous excecao/1.
:- discontiguous (-)/1.

% Desativar warnings de variáveis singleton
:- style_check(-singleton).

% =========================================================
% 0. CONFIGURAÇÃO INICIAL E OPERADORES
% =========================================================

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

% =========================================================
% 4. CONHECIMENTO PERFEITO
% =========================================================

% paciente(IdPaciente, Nome, DataNascimento, Sexo, Morada)
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
consulta(c001, (20,3,2024), p003, 38, 82, 125, 72). % TA Normal

% tensao_Arterial(Id, Class, SistInf, SistSup, DiastInf, DiastSup)
tensao_arterial(ta01, hipotensao, 0, 90, 0, 60).
tensao_arterial(ta02, otima, 90, 120, 60, 80).
tensao_arterial(ta03, normal, 120, 130, 80, 85).
tensao_arterial(ta04, normal_alta, 130, 140, 85, 90).
tensao_arterial(ta05, hipertensao_grau1, 140, 160, 90, 100).
tensao_arterial(ta06, hipertensao_grau2, 160, 180, 100, 110).
tensao_arterial(ta07, hipertensao_grau3, 180, 300, 110, 300).

% =========================================================
% 2. COMPRIMENTO / AUXILIAR
% =========================================================

comprimento([], 0).
comprimento([_|T], N) :-
    comprimento(T, N1),
    N is N1 + 1.

% =========================================================
% 1. NEGAÇÃO POR FALHA
% =========================================================

nao(Questao) :- Questao, !, fail.
nao(_).

% =========================================================
% 3. CONHECIMENTO NEGATIVO COMPLETO
% =========================================================

% PRESSUPOSTO DO MUNDO FECHADO (PMF)
-paciente(Id,Nome,DataNasc,Sexo,Morada) :-
    nao(paciente(Id,Nome,DataNasc,Sexo,Morada)),
    nao(excecao(paciente(Id,Nome,DataNasc,Sexo,Morada))).

-consulta(Id,Data,Pac,Idade,Dia,Sis,Pulso) :-
    nao(consulta(Id,Data,Pac,Idade,Dia,Sis,Pulso)),
    nao(excecao(consulta(Id,Data,Pac,Idade,Dia,Sis,Pulso))).

-tensao_arterial(Id,Class,SistInf,SistSup,DiastInf,DiastSup) :-
    nao(tensao_arterial(Id,Class,SistInf,SistSup,DiastInf,DiastSup)),
    nao(excecao(tensao_arterial(Id,Class,SistInf,SistSup,DiastInf,DiastSup))).

% CONHECIMENTO NEGATIVO EXPLÍCITO
-paciente(636237854, jose, (10,2,1969), masculino, cascais).
-paciente(325544694, ricardo, (5,5,2005), masculino, faro).
-consulta(_, (25,12,2023), _, _, _, _, _).
-consulta(cons999, (14,2,2024), _, _, _, _, _).
-tensao_arterial(_,_,SistInf,_,DiastInf,_) :- SistInf < DiastInf.

% =========================================================
% 1. SISTEMA DE INFERÊNCIA
% =========================================================

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

% ===================================================================
% 7. REGRAS DE DIAGNÓSTIVO A PARTIR DA ANÁLISE DA TENSÃO ARTERIAL
% ===================================================================

classificar_ta(Sistolica, Diastolica, Res) :-
    tensao_arterial(_, Classificacao, SistolicaInf, SistolicaSup, DiastolicaInf, DiastolicaSup),
    Sistolica >= SistolicaInf,
    Sistolica =< SistolicaSup,
    Diastolica >= DiastolicaInf,
    Diastolica =< DiastolicaSup,
    Res = Classificacao.

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
        (paciente(IdPac, Nome, _, _, _) -> 
            format('Classificação TA de ~w (ID: ~w):~n', [Nome, IdPac])
        ;
            format('Classificação TA do paciente ID ~w:~n', [IdPac])
        ),
        listar_classificacoes_aux(ConsultasClassificadas)
    ).

listar_classificacoes_aux([]).
listar_classificacoes_aux([(Data, Classificacao)|T]) :-
    format('  Data: ~w | Classificação: ~w~n', [Data, Classificacao]),
    listar_classificacoes_aux(T).

ordenar_consultas_por_data(Consultas, Ordenadas) :-
    ordenar_consultas_aux(Consultas, [], Ordenadas).

ordenar_consultas_aux([], Ordenada, Ordenada).
ordenar_consultas_aux([H|T], Accum, Ordenada) :-
    inserir_por_data(H, Accum, NovoAccum),
    ordenar_consultas_aux(T, NovoAccum, Ordenada).

inserir_por_data(Consulta, [], [Consulta]).
inserir_por_data(Consulta, [H|T], [Consulta, H|T]) :-
    consulta_mais_recente(Consulta, H).
inserir_por_data(Consulta, [H|T], [H|T1]) :-
    \+ consulta_mais_recente(Consulta, H),
    inserir_por_data(Consulta, T, T1).

consulta_mais_recente((Data1, _, _, _), (Data2, _, _, _)) :-
    data_mais_recente(Data1, Data2).

data_mais_recente((D1, M1, A1), (D2, M2, A2)) :-
    (A1 > A2 -> true;
     A1 =:= A2, M1 > M2 -> true;
     A1 =:= A2, M1 =:= M2, D1 > D2).

avaliar_risco_paciente(IdPac) :-
    classificar_ta_paciente_aux(IdPac, ConsultasClassificadas),
    (ConsultasClassificadas = [] -> 
        format('Paciente ~w: Sem consultas registadas~n', [IdPac])
    ;
        (paciente(IdPac, Nome, _, _, _) -> 
            format('Classificação TA e Risco de ~w:~n', [Nome, IdPac])
        ;
            format('Classificação TA e Risco do paciente ID ~w:~n', [IdPac])
        ),
        format('~-60s~n', ['']),  % Linha separadora
        format('  Data          | Classificação    | Risco~n'),
        format('~-60s~n', ['']),
        listar_classificacoes_com_risco_aux(ConsultasClassificadas)
    ).

listar_classificacoes_com_risco_aux([]).
listar_classificacoes_com_risco_aux([(Data, Classificacao)|T]) :-
    avaliar_risco_por_classificacao(Classificacao, Risco),
    format('  ~-13w | ~-16w | ~w~n', [Data, Classificacao, Risco]),
    listar_classificacoes_com_risco_aux(T).

avaliar_risco_por_classificacao(Classificacao, baixo) :-
    (Classificacao = otima; Classificacao = normal).

avaliar_risco_por_classificacao(normal_alta, moderado).
avaliar_risco_por_classificacao(hipotensao, moderado).

avaliar_risco_por_classificacao(Classificacao, alto) :-
    (Classificacao = hipertensao_grau1;
     Classificacao = hipertensao_grau2;
     Classificacao = hipertensao_grau3).

avaliar_risco_por_classificacao(_, desconhecido).

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

% =========================================================
% 8. CONSULTAS E RELATÓRIOS
% =========================================================

relatorio_paciente_detalhado(Pac) :-
    paciente(Pac, Nome, DataNasc, Sexo, Morada),
    (classificar_ta_paciente(Pac, Classe) -> true ; Classe = 'indeterminado'),
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

listar_consultas_aux([]).
listar_consultas_aux([(Data, Dia, Sis, Pulso)|T]) :-
    classificar_ta(Sis, Dia, Classificacao),
    format('  Data: ~w | TA Diastólica: ~w | TA Sistólica: ~w | Pulsação: ~w | Classificação: ~w~n', 
           [Data, Dia, Sis, Pulso, Classificacao]),
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

% =========================================================
% 6. CONHECIMENTO IMPERFEITO
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
% 9. EVOLUÇÃO / INVOLUÇÃO
% =========================================================

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

% =========================================================
% 10. ATUALIZAÇÃO DE PACIENTES
% =========================================================

atualizar_paciente(Id, NovoNome, NovaData, NovoSexo, NovaRua) :-
    paciente(Id, _Nome, _Data, _Sexo, _Rua),
    retract(paciente(Id, _Nome, _Data, _Sexo, _Rua)),
    assertz(paciente(Id, NovoNome, NovaData, NovoSexo, NovaRua)).

atualizar_nome_paciente(Id, NovoNome) :-
    paciente(Id, _Nome, Data, Sexo, Rua),
    retract(paciente(Id, _Nome, Data, Sexo, Rua)),
    assertz(paciente(Id, NovoNome, Data, Sexo, Rua)).

atualizar_data_paciente(Id, NovaData) :-
    paciente(Id, Nome, _Data, Sexo, Rua),
    retract(paciente(Id, Nome, _Data, Sexo, Rua)),
    assertz(paciente(Id, Nome, NovaData, Sexo, Rua)).

atualizar_morada_paciente(Id, NovaMorada) :-
    paciente(Id, Nome, Data, Sexo, _Rua),
    retract(paciente(Id, Nome, Data, Sexo, _Rua)),
    assertz(paciente(Id, Nome, Data, Sexo, NovaMorada)).