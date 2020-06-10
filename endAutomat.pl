% přiklad regex: ABAaba.*a*|b*9((abc)|(cba))
% přiklad podporovaných znaků: "Aa?+.|()[]\\1"
% regex(Regex, Automat)
% simple test:
% regex("AAA", Automat)

regex(String, Automat) :- string(String), !, string_chars(String, Array), regex(Array, Automat).
regex(Array, Automat) :- makeRegexTree(Array, Automat).

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

makeRegexTree(Array, ClearTree):- makeRegexTree(Array, [], Tree), removeEmpty(Tree, ClearTree).

makeRegexTree([], [], []) :- !.
makeRegexTree([], LastState, [('&', LastState)]):- !.
makeRegexTree(['?'|Xs], [LastState|[]], [('?', LastState)| Tree]) :- makeRegexTree(Xs, Tree), !.
makeRegexTree(['?'|Xs], LastState, [('&', Head), ('?', Tail)| Tree]) :- splitLast(LastState, Head, Tail), makeRegexTree(Xs, Tree), !.
makeRegexTree(['('|Xs], LastState, [('&', LastState),('&', InnerTree)|Tree]) :- matchUntil('(',')', Xs, Matched, Rest), makeRegexTree(Matched, InnerTree), !, makeRegexTree(Rest, Tree), !.
makeRegexTree(['['|Xs], LastState, [('&', LastState),('&', RulledTree)|Tree]) :- matchUntil('[',']', Xs, Matched, Rest), makeRegexTree(Matched, InnerTree), overRull('|', InnerTree, RulledTree), !, makeRegexTree(Rest, Tree), !.
makeRegexTree([X|Xs], LastState, Tree) :- append(LastState, [X], Next), !, makeRegexTree(Xs, Next, Tree), !.

splitLast([X|[]], [], X).
splitLast([X,Xs|Xss], [X|Head], Tail) :- splitLast([Xs|Xss], Head, Tail).
    

matchUntil(LeftSymbol,RightSymbol, Array, Matched, Rest) :- matchUntil(LeftSymbol,RightSymbol, Array, Matched, Rest, 1).
matchUntil(LeftSymbol,RightSymbol, [LeftSymbol| Xs], [LeftSymbol|Matched], Rest, Count) :- Next is Count + 1,!, matchUntil(LeftSymbol, RightSymbol, Xs, Matched, Rest, Next).
matchUntil(_,RightSymbol, [RightSymbol| Xs], [], Xs, 1) :- !.
matchUntil(LeftSymbol,RightSymbol, [RightSymbol| Xs], [RightSymbol|Matched], Rest, Count) :- Next is Count - 1,!, matchUntil(LeftSymbol, RightSymbol, Xs, Matched, Rest, Next).
matchUntil(LeftSymbol,RightSymbol, [X|Xs], [X|Matched], Rest, Count) :- matchUntil(LeftSymbol,RightSymbol, Xs, Matched, Rest, Count).

overRull(_, [],[]).
overRull(Symbol, [(_,X)|Xs], [(Symbol,X)|Rest]):- overRull(Symbol, Xs, Rest), !.

removeEmpty([],[]).
removeEmpty([(_, [])| Tree], Clear) :- removeEmpty(Tree, Clear), !.
removeEmpty([(Symbol, InnerNode) |Tree], [(Symbol, ClearInnerNode) |Clear]) :- removeEmpty(InnerNode, ClearInnerNode), !, removeEmpty(Tree, Clear), !.
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