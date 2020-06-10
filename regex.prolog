% přiklad regex: ABAaba.*a*|b*9((abc)|(cba))
% přiklad podporovaných znaků: "Aa?+.|()[]\\1"
% regex(Regex, Automat)
% simple test:
% regex("AAA", Automat)

verify_regex(Regex) :- verify_regex_start(Regex), verify_regex_rest(Regex).

verify_regex_rest(Regex) :- verify_regex_rest(Regex, 0, 0, false).
% verify_regex_rest(Regex, LeftParanthesis, LeftBracket, Counter).
verify_regex_rest(['('|Rx], LeftParanthesis, LeftBracket, _) :- LP is LeftParanthesis + 1,!, verify_regex_rest(Rx, LP, LeftBracket, false),!.
verify_regex_rest([')'|Rx], LeftParanthesis, LeftBracket, _) :- LP is LeftParanthesis - 1, LeftParanthesis \= 0,!, verify_regex_rest(Rx, LP, LeftBracket, false),!.
verify_regex_rest(['['|Rx], LeftParanthesis, LeftBracket, _) :- LB is LeftBracket + 1,!, verify_regex_rest(Rx, LeftParanthesis, LB, false),!.
verify_regex_rest([']'|Rx], LeftParanthesis, LeftBracket, _) :- LB is LeftBracket - 1,!, LeftBracket \= 0,!, verify_regex_rest(Rx, LeftParanthesis, LB, false),!.
verify_regex_rest(['*'|Rx], LeftParanthesis, LeftBracket, Counter) :- !,Counter = false, verify_regex_rest(Rx, LeftParanthesis, LeftBracket, true),!.
verify_regex_rest(['+'|Rx], LeftParanthesis, LeftBracket, Counter) :- !,Counter = false, verify_regex_rest(Rx, LeftParanthesis, LeftBracket, true),!.
verify_regex_rest(['?'|Rx], LeftParanthesis, LeftBracket, Counter) :- !,Counter = false, verify_regex_rest(Rx, LeftParanthesis, LeftBracket, true),!.
verify_regex_rest([_|Rx], LeftParanthesis, LeftBracket, _) :- !,verify_regex_rest(Rx, LeftParanthesis, LeftBracket, false),!.
verify_regex_rest([], 0, 0, _) :- !.
verify_regex_start([R|_]) :- R \= '+', R \= '*', R \= '?', R \= '|'.

% Automat: (StarýStav, přechod, NovýStav)

regex_to_tree([X| Xs], Part, Rest).





% Tests


% test_regex_correctnes(Regex, Correctness).
test_regex_correctnes("Count (true)", Count) :- findall(_,testCase_regex(_,true),Result), length(Result, Count).
test_regex_correctnes(Regex, Correctness) :- testCase_regex(Regex, Correctness), verify_regex(Regex).

testCase_regex(['A', a, (+), ('.'), ('|'), '(', ')', '[', ']', (\), '1'], true).
testCase_regex([(?), a, (+), ('.'), ('|'), '(', ')', '[', ']', (\), '1'], false).
testCase_regex([('|'), a, (+), ('.'), ('|'), '(', ')', '[', ']', (\), '1'], false).
testCase_regex(['A', a, (+), (*), ('.'), ('|'), '(', ')', '[', ']', (\), '1'], false).
testCase_regex([(\), a, (+), ('.'), ('|'), '(', '[', ']', (\), '1'], false).
testCase_regex([(+), a, (+), ('.'), ('|'), '(', ')', '[', ']', (\), '1'], false).
testCase_regex(['*', a, (+), ('.'), ('|'), '(', ')', '[', ']', (\), '1'], false).
testCase_regex(['*', a, (+), ('.'), ('|'), '(', ')', '[', ']', (\), '1'], false).