/* bottom up compiler/evaluator */

prospective(S, R):-
	final(S),
	outcome(S, R).
prospective(S, R):- 
    	\+ final(S), 
    	legal(do(M,S)), 
    	prospective(do(M,S),R).


persists(F, S):- 
    initial(S),
    initially(F, S).
persists(F, do(M, S)):- 
    initiates(F, M, S).
persists(F, do(M, S)):- 
    persists(F, S), 
    \+ terminates(F, M, S).

/*
initial/1
new/2
final/1
initially/2
outcome/2
valid/2 (legal/2, possible/2, available/2)
effect/3
holds/2
abnormal/3

effect/3 and abnormal/3 is initiates/3 and terminates/3 or use causes/3. Essentially, slightly more sophisticated than C+
*/
