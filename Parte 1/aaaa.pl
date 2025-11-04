% Facts about father relationships
pai(joao, ana).
pai(jose, joao).
pai(manuel, jose).

% Rule to define son relationship (inverse of father)
filho(X, Y) :- pai(Y, X).

% Rule to define grandfather relationship
avo(X, Z) :- pai(X, Y), pai(Y, Z).