%?- [game,'./games/pd/specification','./games/pd/reasoning'].
%?- [game,'./games/gc/specification','./games/gc/reasoning'].

load0(X):-
	atomic_list_concat(['./',lib,'/', X,'/',specification], SpecX), 
	atomic_list_concat(['./',lib,'/', X,'/',reasoning], RsngX), 
	consult([game,SpecX,RsngX]).

load(X):-
	atomic_list_concat(['./',lib,'/', X,'/',specification], SpecX), 
	consult([game,SpecX]).
