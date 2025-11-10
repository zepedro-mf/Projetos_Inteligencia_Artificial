% =========================================
% MÓDULO P1 — DOMÍNIO (Pacientes, Consultas, Tensões)
% =========================================


%=========================================================================

%                                 DADOS FALSOS


% paciente(ID, Nome, Idade, Sexo, Cidade)
% Dados aleatórios sobre pacientes para serem chamados mais tarde
paciente(p1, 'Alice Silva', 35, feminino, 'Lisboa').
paciente(p2, 'Bruno Costa', 52, masculino, 'Porto').
paciente(p3, 'Carla Mendes', 68, feminino, 'Coimbra').
paciente(p4, 'Daniel Rocha', 41, masculino, 'Braga').
paciente(p5, 'Eva Martins', 27, feminino, 'Faro').
paciente(p6, 'Filipe Sousa', 59, masculino, 'Aveiro').
paciente(p7, 'Gabriela Pinto', 73, feminino, 'Setúbal').
paciente(p8, 'Hugo Almeida', 46, masculino, 'Leiria').
paciente(p9, 'Inês Carvalho', 31, feminino, 'Viseu').
paciente(p10, 'João Ferreira', 64, masculino, 'Évora').
% (adicionar mais depois até 10)

% consulta(ID, Data, PacienteID, Medico, Sistólica, Diastólica)
% ID = identificador da consulta
% Data = data da consulta
% Medico = quem realizou a consulta
% S, D e P são medições feitas durante a consulta
% =========================================
% Dados das Consultas (20 no total)
% =========================================

consulta(c1,  '2025-03-10', p1, dr_silva, 120, 80, 72).   
consulta(c2,  '2025-06-15', p1, dr_silva, 135, 85, 75).   
consulta(c3,  '2025-03-11', p2, dr_mendes, 150, 95, 85).  
consulta(c4,  '2025-09-12', p2, dr_mendes, 145, 90, 82).  
consulta(c5,  '2025-03-12', p3, dr_lima, 90, 55, 70).     
consulta(c6,  '2025-07-20', p3, dr_lima, 100, 65, 74).    
consulta(c7,  '2025-03-15', p4, dr_mendes, 130, 85, 76).  
consulta(c8,  '2025-04-18', p4, dr_silva, 160, 100, 82).  
consulta(c9,  '2025-03-18', p5, dr_silva, 115, 78, 70).   
consulta(c10, '2025-05-22', p5, dr_oliveira, 118, 79, 71).
consulta(c11, '2025-03-20', p6, dr_lima, 162, 102, 88).   
consulta(c12, '2025-07-25', p6, dr_silva, 155, 95, 83).   
consulta(c13, '2025-10-02', p6, dr_mendes, 138, 89, 79).  
consulta(c14, '2025-03-22', p7, dr_mendes, 145, 92, 88).  
consulta(c15, '2025-09-05', p7, dr_mendes, 142, 91, 90).  
consulta(c16, '2025-03-25', p8, dr_silva, 105, 68, 65).  
consulta(c17, '2025-07-30', p8, dr_lima, 98, 58, 60).     
consulta(c18, '2025-03-26', p9, dr_lima, 95, 55, 60).     
consulta(c19, '2025-06-01', p9, dr_silva, 102, 66, 68).   
consulta(c20, '2025-03-30', p10, dr_mendes, 138, 85, 78). 


%==========================================================================

% classificacao(Tipo, Sistólica, Diastólica)
% Usa as medições para verificar se estão dentro de certos critérios
% para determinar o tipo de pressão arterial
classificacao(hipotensao, S, D) :- S < 100; D < 60.
classificacao(normal, S, D) :- S >= 100, S < 140, D >= 60, D < 90.
classificacao(hipertensao, S, D) :- S >= 140; D >= 90.

% regra de coerência -> para garantir que não houve erros significativos durante a medição
coerente(S, D) :- S > D.

% classificar consulta
% para descobrir o tipo de resultado de uma consulta específica
classificar_consulta(ID, Categoria) :-
    consulta(ID, _, _, _, S, D),
    classificacao(Categoria, S, D).

% consulta válida (sistólica > diastólica)
consulta_valida(ID) :-
    consulta(ID, _, _, _, S, D),
    coerente(S, D).

% Encontrar todas as consultas de um determinado paciente
% consultas_por_paciente('Alice Silva', ID).
consultas_por_paciente(NomePaciente, IDConsulta) :-
    paciente(PID, NomePaciente, _, _, _),
    consulta(IDConsulta, _, PID, _, _, _).

% Encontrar todas as consultas de um determinado Médico
% consultas_por_medico(dr_mendes, ID).
consultas_por_medico(Medico, IDConsulta) :-
    consulta(IDConsulta, _, _, Medico, _, _).

% Encontrar todos os pacientes dentro de um certo intervalo de idades
% pacientes_por_idade(30, 50, Nome).
pacientes_por_idade(IdadeMin, IdadeMax, Nome) :-
    paciente(_, Nome, Idade, _, _),
    Idade >= IdadeMin,
    Idade =< IdadeMax.


% AGORA ESTES SÃO OS PEDIDOS QUE PRECISO DE FAZER AO PROLOG PARA OBTER UMA FUNÇÃO/RESPOSTA ESPECÍFICA

%   Mostrar todos os pacientes ==============>     consulta(ID, Data, PID, Medico, S, D, P).	
%   Classificar uma consulta ===============>	     classificar_consulta(c1, Categoria).	
%   Classificar todas as consultas ==========>	     classificar_consulta(ID, Categoria).	
%   Validar medições ========================>      consulta_valida(ID).	
%   Consultas por paciente ================> 	 consultas_por_paciente('Alice Silva', ID).	
%   Consultas por Médico ==================>      consultas_por_medico(dr_mendes, ID).	
%   Pacientes por intervalo de idade =======>      pacientes_por_idade(30, 60, Nome).
