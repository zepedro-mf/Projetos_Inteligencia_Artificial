
% Definir os dynamic para cada um dos predicados
:-dynamic (utente/3).
:-dynamic (registo/7).
:-dynamic (imc/4).
:- op(900, xfy, '::').
:- dynamic ('-'/1).
:- op(900, xfy, 'e').
:- op(900, xfy, 'ou').


%                 Sistema de Inferência

% Extensao do meta-predicado SI: Questao,Resposta -> {V,F}
si(Questao,verdadeiro):-Questao.
si(Questao.falso):-Questao.
si(Questao,desconhecido):-nao(Questao),nao(-Questao).

% Negação por falha

% Extensao do meta-predicado nao: Questao -> {V,F}
nao(Questao):-Questao, !, fail.
nao(Questao).

%Extensão do predicado comprimento: lista,N -> {V,F}
comprimento([],0).
comprimento([_|L],N):-
    comprimento(L,N1),
    N is N1+1.



%               Pressuposto do Mundo fechado
-utente(Utente,Nome,Morada):- nao(utente(Utente,Nome,Morada)),
                            nao(excecao(utente(Utente,Nome,Morada))).
-registo(Registo,Data,Utente,Sexo,Idade,Altura,Peso):- nao(registo(Registo,Data,Utente,Sexo,Idade,Altura,Peso)),
                            nao(excecao(registo(Registo,Data,Utente,Sexo,Idade,Altura,Peso))).
-imc(Imc,Class,Linf,Lsup):- nao(imc(Imc,Class,Linf,Lsup)),
                            nao(excecao(imc(Imc,Class,Linf,Lsup))).




%            Representação do Conhecimento Positivo - Conhecimento Perfeito

% utente: #utente, nome, morada -> { V,F,D }
% resgisto: #registo, data, #utente, sexo, idade, altura, peso -> { V,F,D }
% imc: #imc, classificacao, limite inferior, limite superior -> { V,F,D }

% Extensao do predicado utente:  #utente, nome, morada -> { V,F,D }
utente(123456789,antonio,guimaraes).
utente(987654321,beatriz,braga).
utente(135246978,carlos,vila verde).

%Extensão do predicado resgisto: #registo, data, #utente, sexo, idade, altura, peso -> { V,F,D }
registo(gmr123,(30,09,2023),123456789,1,21,1.80,75.7).
resgisto(gmr234,(30,09,2023),987654321,2,35,1.61,55.3).
registo(gmr567,(30,09,2023),135246978,1,43,1.76,93.5).

%Extensão do predicado imc: #imc, classificacao, limite inferior, limite superior -> { V,F,D }
imc(imc02,magreza,10,18.5).
imc(imc04,normal,18.5,25.0).
imc(imc06,sobrepeso,25.0,30.0).
imc(imc08,obesidade,30,35).




%           Representação do Conhecimento Negativo

% O IMC apenas pode ser medido em utentes cuja idade seja entre os 18 e os 80
-registo(resgisto,data,utente,sexo,Idade,altura,peso):- Idade=<18, Idade>=80.





%     Representação de casos de Conhecimento Imperfeito, de todos os tipos estudados

% Conhecimento imperfeito INCERTO - no registo dos utentes a altura foi mal medida. Desta forma, este parâmetro não foi registado
registo(gmr789,(30,09,2023),159487123,1,50,desconhecido,70).

excecao(registo(Registo,Data,Utente,Sexo,Idade,Altura,Peso)):- registo(Registo,Data,Utente,Sexo,Idade,desconhecido,Peso).

% Conhecimento IMPRECISO - no registo dos utentes o carlos estava nervoso e o seu peso na balança oscilava 
excecao(registo(gmr567,(30,09,2023),135246978,1,43,1.76,93.4)).
excecao(registo(gmr567,(30,09,2023),135246978,1,43,1.76,93.1)).
excecao(registo(gmr567,(30,09,2023),135246978,1,43,1.76,93.7)).

% Conhecimento imperfeito INTERDITO - o hostorgio perdeu o CC e não sabe o seu número de utente
utente(num_desconhecido,hostorgio,leiria).

excecao(utente(Utente,Nome,Morada)):- utente(num_desconhecido,Nome,Morada).
interdito(num_desconhecido).

+utente(Utente,Nome,Morada)::(findall(Utente,utente(Utente,Nome,Morada),nao(interdito(Utente))),S),
                        comprimento(S,N),
                        N==1.




%               Evolução da base de conhecimento

evolucao(Termo):- findall(Invariante,+Termo::Invariante,Lista),
                    insercao(Termo),
                    validar(Lista).



insercao(Termo):- assert(Termo).
insercao(Termo):- retract(Termo),!,fail.

validar([]).
validar([R|LR]):- R, validar(LR).

%        Invariantes

% Invariante que impede a registo de utentes que não existam e o registo de pessoas com o mesmo número de utente
+registo(Registo,Data,Utente,Sexo,Idade,Altura,Peso) :: (findall(Utente,(utente(Utente,Nome,Morada)), S),
                                                        comprimento(S,N),
                                                        N==1).

% Invariantes que permitem alterar a base de conhecimento:
+utente(Utente,Nome,Morada) :: 
		(nao(interdito(Nome)),
		findall(Utente, utente(Utente,_,_), S),
		comprimento(S,N), 
		N ==1,
        insercao(utente(Utente,Nome,Morada))).

+registo(Registo,Data,Utente,Sexo,Idade,Altura,Peso) :: 
		(nao(interdito(Utente)),
		findall(Utente, excecao(utente(Utente,_,_)), S),
		comprimento(S,N), 
		atualiza_a(utente(Utente,Nome,Morada),N)).










%Cálculo do IMC
calcular_imc(Utente, IMC) :-
    registo(_, _, Utente, _, _, Altura, Peso),
    IMC is Peso / (Altura * Altura).



classificar_estado(IMC, Estado) :-
    IMC < 18.5,
    Estado = 'Magreza'.

classificar_estado(IMC, Estado) :-
    IMC >= 18.5,
    IMC < 25.0,
    Estado = 'Normal'.

classificar_estado(IMC, Estado) :-
    IMC >= 25.0,
    IMC < 30.0,
    Estado = 'Sobrepeso'.

classificar_estado(IMC, Estado) :-
    IMC >= 30.0,
    Estado = 'Obesidade'.



% imc(Utente,R):- 
        calcular_imc(Utente,IMC),
        IMC>=10,
        IMC=<18.5,
        R is magreza.
% imc(Utente,R):- 
        calcular_imc(Utente,IMC),
        IMC>=18.5,
        IMC=<25,
        R is normal.
% imc(Utente,R):- 
        calcular_imc(Utente,IMC),
        IMC>=25,
        IMC=<30,
        R is sobrepeso.
% imc(Utente,R):- 
        calcular_imc(Utente,IMC),
        IMC>=30,
        IMC=<35,
        R is obesidade.
