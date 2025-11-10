% =========================================================================
% MÓDULO P3 — CONHECIMENTO IMPERFEITO E EVOLUÇÃO
% =========================================================================
% - Suporta 3 tipos de nulos:
%     incerto         -> valor desconhecido
%     impreciso(L,H)  -> intervalo/imprecisão (ex.: idade/imprecisão de pressão)
%     interdito       -> existe mas não pode ser revelado/substituído
% - Invariantes para:
%     * ID único
%     * Proibir contraditório (+facto vs -facto)
%     * Proteger campos interditos (não permitem revelação/substituição)
%     * Coerência sistólica/diastólica (sistólica > diastólica quando ambos numéricos)
% - evolucao/1 e involucao/1 com verificação de invariantes
% - Exemplos e testes
% =========================================================================

:- module(modulo_p3,
    [
      nulo/1,
      evolucao/1,
      involucao/1,
      inserir_paciente/5,
      remover_paciente/1,
      inserir_consulta/7,
      remover_consulta/1,
      demo_p3/0
    ]).

:- dynamic paciente/5.
:- dynamic '-paciente'/5.
:- dynamic consulta/7.
:- dynamic '-consulta'/7.
:- dynamic tensao_arterial/6.
:- dynamic nulo/1.

% adição: declarar operador :: para invariantes (melhora legibilidade/portabilidade)
:- op(100, xfy, ::).

% -------------------------------------------------------------------------
% (1) Definir os valores nulos permitidos
% -------------------------------------------------------------------------
% Os identificadores simbólicos usados para denotar tipos de nulos
nulo(incerto).
nulo(impreciso).
nulo(interdito).

% -------------------------------------------------------------------------
% (2) Exemplos iniciais (base de teste)
% -------------------------------------------------------------------------
% pacientes: paciente(ID, Nome, DataNasc, Sexo, Morada)
% - Uma entrada perfeita
paciente(p1, 'Ana Silva', '1985-06-10', feminino, 'Braga').

% - campo DataNasc incerto (não sabemos)
paciente(p2, 'Miguel Santos', incerto, masculino, 'Guimarães').

% - campo Morada interdito (existe mas não pode ser revelado)
paciente(p3, 'Paciente X', '1979-03-03', feminino, interdito).

% - paciente com campo impreciso (podemos usar impreciso(D1,D2) para data/idade)
%  aqui usamos uma convenção: se a data de nascimento for imprecisa, representamos
%  algo como impreciso('1980-01-01','1982-12-31'). É apenas um termo.
paciente(p4, 'João Incerto', impreciso('1980-01-01','1982-12-31'), masculino, 'Porto').

% consultas: consulta(ID, Data, PacienteID, Idade, Diastolica, Sistolica, Pulsacao)
% - consulta com valores exactos
consulta(c1, '2025-10-10', p1, 40, 78, 118, 72).

% - consulta com pressões imprecisas (sistolica imprecisa entre 120 e 130)
consulta(c2, '2025-10-11', p1, 40, 82, impreciso(120,130), 75).

% - consulta para paciente com campo interdito (deverá funcionar mas não revelar a morada)
consulta(c3, '2025-09-01', p3, 46, 95, 145, 80).

% tensao_arterial/6 (opcional: tabela de referencia)
% tensao_arterial(ID, Classe, SistMin, SistMax, DiastMin, DiastMax)
tensao_arterial(t_normal, normal, 0, 119, 0, 79).
tensao_arterial(t_elevada, elevado, 120, 139, 80, 89).
tensao_arterial(t_hipertensao, hipertensao, 140, 10000, 90, 10000).

% -------------------------------------------------------------------------
% (3) Invariantes (regras restritivas)
%    As invariantes são escritas usando a forma:
%      +Termo :: Condicao     (avaliada quando Termo é inserido / assertz)
%      -Termo :: Condicao     (avaliada quando Termo é removido / retract)
% -------------------------------------------------------------------------

% 3.1 ID de paciente único (após inserção só deve existir 1 fact com esse ID)
+paciente(ID, _, _, _, _) :: (
    findall(ID, paciente(ID, _, _, _, _), L),
    length(L, N),
    N =:= 1
).

% 3.2 Não permitir inserir paciente se existir negação forte desse paciente
+paciente(ID, _, _, _, _) :: (
    \+ '-paciente'(ID, _, _, _, _)
).

% 3.3 Não permitir inserir negação forte se já existe facto positivo
+'-paciente'(ID, _, _, _, _) :: (
    \+ paciente(ID, _, _, _, _)
).

% 3.4 Invariante que protege campos interditos:
%     Antes de inserir um novo paciente (com mesmo ID) certifica que não existe
%     já um paciente com esse ID cujo qualquer campo seja 'interdito' -- isso evita
%     revelar/substituir um valor interdito.
+paciente(ID, Nome, DataNasc, Sexo, Morada) :: (
    % procurar factos existentes para o mesmo ID que tenham 'interdito' em algum campo
    ( findall(_, (paciente(ID, N, D, Sx, M), (N == interdito ; D == interdito ; M == interdito)), L),
      length(L, C),
      C =:= 0
    )
).

% 3.5 Protecção na remoção: não permitir remover um paciente cujo algum campo é 'interdito'
- paciente(ID, Nome, DataNasc, Sexo, Morada) :: (
    \+ (Nome == interdito ; DataNasc == interdito ; Morada == interdito)
).

% 3.6 Invariantes para consulta: paciente deve existir (ou, pelo menos, não haver negação forte)
+consulta(_, _, P, _, _, _, _) :: (
    ( paciente(P, _, _, _, _) ; \+ '-paciente'(P, _, _, _, _) )
).

% 3.7 Coerência de leituras: quando sistólica e diastólica são números, syst > diast
+consulta(ID, Data, P, Idade, Diast, Syst, Puls) :: (
    ( number(Diast), number(Syst) -> Syst > Diast ; true )
).

% 3.8 Invariante para tensao_arterial (intervalos coerentes)
+tensao_arterial(_, _, Smin, Smax, Dmin, Dmax) :: (
    Smin =< Smax, Dmin =< Dmax
).

% -------------------------------------------------------------------------
% (4) Evolução / Involução (mecânica genérica com rollback em caso de falha)
% -------------------------------------------------------------------------
evolucao(Termo) :-
    findall(Inv, +Termo::Inv, ListaInv),     % recolhe invariantes para inserção
    assertz(Termo),                          % insere temporariamente
    ( teste(ListaInv) ->
        true
    ;
        % se falhar, faz rollback e falha
        retract(Termo),
        fail
    ).

involucao(Termo) :-
    findall(Inv, -Termo::Inv, ListaInv),     % invariantes para remoção
    retract(Termo),                           % tenta remover
    ( teste(ListaInv) ->
        true
    ;
        % rollback: re-insere e falha
        assertz(Termo),
        fail
    ).

% verifica lista de invariantes
teste([]).
teste([R|Rs]) :-
    R,
    teste(Rs).

% -------------------------------------------------------------------------
% (5) Predicados utilitários para inserir/remover (interface)
% -------------------------------------------------------------------------
inserir_paciente(ID, Nome, DataNasc, Sexo, Morada) :-
    evolucao(paciente(ID, Nome, DataNasc, Sexo, Morada)).

remover_paciente(ID) :-
    % vamos procurar o facto e invocar involucao
    paciente(ID, Nome, DataNasc, Sexo, Morada),
    involucao(paciente(ID, Nome, DataNasc, Sexo, Morada)).

inserir_consulta(ID, Data, PID, Idade, Diast, Syst, Puls) :-
    evolucao(consulta(ID, Data, PID, Idade, Diast, Syst, Puls)).

remover_consulta(ID) :-
    consulta(ID, Data, PID, Idade, Diast, Syst, Puls),
    involucao(consulta(ID, Data, PID, Idade, Diast, Syst, Puls)).

% -------------------------------------------------------------------------
% (6) Predicados para trabalhar com nulos/imprecisões
% -------------------------------------------------------------------------
% det_nulo(+Valor) -> true se Valor for um marcador de nulo
% usa a base nulo/1 para centralizar a definição de marcadores de nulos
det_nulo(Valor) :-
    % impreciso/2 é um marcador especial que unifica com impreciso(_,_)
    ( Valor = impreciso(_,_) -> true
    ; nulo(Valor)
    ).

% extrai_valor_numero(+V, -Num)
% se V for número devolve-o, caso contrário falha
valor_numerico(V, V) :- number(V).
valor_numerico(impreciso(_, _), _) :- !, fail.
valor_numerico(incerto, _) :- !, fail.
valor_numerico(interdito, _) :- !, fail.

% exemplo de avaliação segura: classificar apenas se conseguimos obter números
classifica_pressao(Syst, Diast, Class) :-
    valor_numerico(Syst, S),
    valor_numerico(Diast, D),
    !,
    ( S < 120, D < 80 -> Class = normal
    ; (S >= 140 ; D >= 90) -> Class = hipertensao
    ; Class = elevado
    ).
% caso haja imprecisão, devolve 'impreciso' com o termo
classifica_pressao(Syst, Diast, impreciso(Syst, Diast)) :-
    (det_nulo(Syst) ; det_nulo(Diast)).

% -------------------------------------------------------------------------
% (7) Exemplo de operações proibidas (demonstração)
% -------------------------------------------------------------------------
% demo_p3/0 executa testes automáticos mostrando o comportamento do modulo
demo_p3 :-
    writeln('--- Demo Módulo P3 ---'),
    writeln('Base inicial de pacientes:'),
    listar_pacientes_demo,
    nl,

    writeln('1) Tentativa de inserir paciente com ID repetido (deve falhar):'),
    ( inserir_paciente(p1, 'Outro', '1990-01-01', masculino, 'Lugar') ->
        writeln('Inserção (inesperada) bem sucedida!')
    ;
        writeln('Falha esperada: ID duplicado ou invariantes violados.')
    ),
    nl,

    writeln('2) Tentativa de substituir campo interdito (deve falhar):'),
    ( inserir_paciente(p3, 'Paciente X', '1979-03-03', feminino, 'NovaMorada') ->
        writeln('Inserção (inesperada) bem sucedida!')
    ;
        writeln('Falha esperada: não é possível substituir valor interdito.')
    ),
    nl,

    writeln('3) Inserir novo paciente válido (deve ter sucesso):'),
    ( inserir_paciente(px, 'Novo Paciente', '1995-05-05', masculino, 'Viana') ->
        writeln('Inserção bem sucedida: px'),
        listar_pacientes_demo
    ;
        writeln('Falha inesperada na inserção de px.')
    ),
    nl,

    writeln('4) Testar consulta com imprecisão:'),
    ( classifica_pressao(impreciso(120,130), 85, R) ->
        format('classifica_pressao devolveu: ~w~n', [R])
    ;
        writeln('classifica_pressao falhou (improvável).')
    ),
    nl,

    writeln('--- FIM Demo ---').

% listar pacientes para demo
listar_pacientes_demo :-
    findall((ID,Nome,Data,Sexo,Morada), paciente(ID,Nome,Data,Sexo,Morada), L),
    imprime_pacientes_demo(L).

imprime_pacientes_demo([]).
imprime_pacientes_demo([(ID,Nome,Data,Sexo,Morada)|T]) :-
    format('~w: ~w  Data: ~w  Sexo: ~w  Morada: ~w~n', [ID, Nome, Data, Sexo, Morada]),
    imprime_pacientes_demo(T).

% -------------------------------------------------------------------------
% (8) Opcional: predicado de utilidade para tentar "revelar" um interdito (deve falhar)
%    tenta_revelar_morada(PID, MoradaReal) apenas para demonstrar que substituição não é permitida
% -------------------------------------------------------------------------
tenta_revelar_morada(PID, MoradaReal) :-
    % tentativa de remover paciente com campo interdito e reinserir com morada revelada
    paciente(PID, Nome, DataNasc, Sexo, MoradaExistente),
    ( MoradaExistente == interdito ->
        format('Tentativa de revelar morada do ~w: proibido pelo invariante.~n', [PID]),
        fail
    ;
        % caso não seja interdito, permite substituição usual (apenas para demonstração)
        involucao(paciente(PID, Nome, DataNasc, Sexo, MoradaExistente)),
        evolucao(paciente(PID, Nome, DataNasc, Sexo, MoradaReal))
    ).

% -------------------------------------------------------------------------
% (9) API minima para exportar/integrar com o resto do projecto
%    (estes predicados já foram expostos no modulo)
% -------------------------------------------------------------------------
% =========================================================================
% (10) ANÁLISE INTERVALAR PARA TENSÃO ARTERIAL IMPRECISA
% =========================================================================
% Objetivo: raciocínio sobre imprecisão para determinar:
%   → hipertensao_certeza   : intervalo indica 100% hipertensão
%   → hipertensao_possivel  : pode ser hipertensão dependendo do valor real
%
% Classificação com base em intervalos sistólicos/diastólicos:
%   - hipertensao_certeza :
%         Lsyst >= 140
%         OU
%         Ldiast >= 90
%
%   - hipertensao_possivel :
%         Hisyst >= 140  (sistólica pode chegar a hipertensa)
%         OU
%         Hidiast >= 90
%
%   - caso contrário: indeterminado
% =========================================================================

% Decide o tipo de classificação intervalar para a pressão sanguínea
classificacao_intervalar(SystInt, DiastInt, Classe) :-

    % Verificar se imprecisão se aplica aos dois valores (impreciso/2)
    SystInt = impreciso(Smin, Smax),
    DiastInt = impreciso(Dmin, Dmax),
    !,
    (
      % 1) Certeza de hipertensão (todo o intervalo é hipertenso)
      (Smin >= 140 ; Dmin >= 90) ->
          Classe = hipertensao_certeza
      ;

      % (2) Possibilidade de hipertensão
      (Smax >= 140 ; Dmax >= 90) ->
          Classe = hipertensao_possivel

      % (3) Caso contrário: indeterminado
      ;
          Classe = indeterminado
    ).


% Permite intervalos apenas para SistInt ou DiastInt
classificacao_intervalar(SystInt, Diast, Classe) :-
    SystInt = impreciso(Smin, Smax),
    number(Diast),
    !,
    (
      (Smin >= 140 ; Diast >= 90) -> Classe = hipertensao_certeza
      ; (Smax >= 140 ; Diast >= 90) -> Classe = hipertensao_possivel
      ; Classe = indeterminado
    ).

classificacao_intervalar(Syst, DiastInt, Classe) :-
    DiastInt = impreciso(Dmin, Dmax),
    number(Syst),
    !,
    (
      (Syst >= 140 ; Dmin >= 90) -> Classe = hipertensao_certeza
      ; (Syst >= 140 ; Dmax >= 90) -> Classe = hipertensao_possivel
      ; Classe = indeterminado
    ).

% Caso sem imprecisão -> delega para classificação normal
classificacao_intervalar(Syst, Diast, Classe) :-
    classifica_pressao(Syst, Diast, Classe).

% Correção: Redefinir operador para melhor associatividade
:- op(100, xfx, ::).

% Adição: Novo tipo de nulo para probabilidade
nulo(probabilidade(_)).

% Correção: Melhorar evolucao/1 com catch para rollback robusto
evolucao(Termo) :-
    findall(Inv, +Termo::Inv, ListaInv),
    catch(
        (assertz(Termo), teste(ListaInv)),
        _Exception,
        (retract(Termo), fail)  % Rollback em caso de erro
    ).

% Adição: Predicado para inserir negação
inserir_negacao_paciente(ID, Nome, DataNasc, Sexo, Morada) :-
    evolucao('-paciente'(ID, Nome, DataNasc, Sexo, Morada)).

% Adição: Invariante para idade coerente (aproximada)
+paciente(ID, Nome, DataNasc, Sexo, Morada) :: (
    (DataNasc = impreciso(D1, D2) -> true ;  % Pular se impreciso
     calcular_idade_aprox(DataNasc, IdadeAprox), IdadeAprox >= 0)
).

% Adição: Inferência avançada
inferir_condicao(PID, cronico) :-
    findall(_, (consulta(_, _, PID, _, D, S, _), classifica_pressao(S, D, hipertensao)), L),
    length(L, N), N > 2.

% Adição: Relatório integrado
gerar_relatorio(PID) :-
    paciente(PID, Nome, _, _, _),
    findall(Classe, (consulta(_, _, PID, _, D, S, _), classificacao_intervalar(S, D, Classe)), Classes),
    format('Relatório para ~w (ID: ~w): Condições ~w~n', [Nome, PID, Classes]).