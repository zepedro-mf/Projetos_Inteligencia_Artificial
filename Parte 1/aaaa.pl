% ============================================
% MÓDULO P1 — DOMÍNIO (Pacientes, Consultas, Tensões)
% ============================================
% • Definir predicados principais (paciente/5, consulta/7, tensao_arterial/6)
% • Inserir exemplos iniciais de pacientes e consultas
% • Criar regras simples de classificação (normal/hipertensão/hipotensão)
% • Garantir coerência das leituras (ex: sistólica > diastólica)

% ============================================
% MÓDULO P2 — RACIOCÍNIO E NEGAÇÃO
% ============================================
% • Implementar negação por falha (not/nao)
% • Representar conhecimento negativo explícito
% • Meta-predicado si(Questao, Resposta) com respostas:
%     → verdadeiro
%     → falso
%     → desconhecido
% • Regras de diagnóstico clínico usando a base do P1

% ============================================
% MÓDULO P3 — CONHECIMENTO IMPERFEITO + EVOLUÇÃO
% ============================================
% • Implementar três tipos de valores nulos:
%     - incerto
%     - impreciso (intervalos)
%     - interdito (com proteção para não ser revelado)
% • Definir invariantes (ex: IDs únicos, impossibilidade de mudar interdito)
% • Criar mecanismos de:
%     → evolucao(Termo)
%     → involucao(Termo)
%     com verificação antes de alterar o conhecimento
