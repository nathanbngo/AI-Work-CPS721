% Enter the names of your group members below.
% If you only have 2 group members, leave the last space blank
%
%%%%%
%%%%% NAME: Nathan Ngo
%%%%% NAME: Kevin Shao
%%%%% NAME:
%
% Add the required rules in the corresponding sections. 
% If you put the rules in the wrong sections, you will lose marks.
%
% You may add additional comments as you choose but DO NOT MODIFY the comment lines below
%

%%%%% SECTION: robocup_setup
%%%%%
%%%%% These lines allow you to write statements for a predicate that are not consecutive in your program
%%%%% Doing so enables the specification of an initial state in another file
%%%%% DO NOT MODIFY THE CODE IN THIS SECTION
:- dynamic hasBall/2.
:- dynamic robotLoc/4.
:- dynamic scored/1.

%%%%% This line loads the generic planner code from the file "planner.pl"
%%%%% Just ensure that that the planner.pl file is in the same directory as this one
:- [planner].

%%%%% SECTION: init_robocup
%%%%% Loads the initial state from either robocupInit1.pl or robocupInit2.pl
%%%%% Just leave whichever one uncommented that you want to test on
%%%%% NOTE, you can only uncomment one at a time
%%%%% HINT: You can create other files with other initial states to more easily test individual actions
%%%%%       To do so, just replace the line below with one loading in the file with your initial state
:- [robocupInit1].
%:- [robocupInit2].

%%%%% SECTION: goal_states_robocup
%%%%% Below we define different goal states, each with a different ID
%%%%% HINT: It may be useful to define "easier" goals when starting your program or when debugging
%%%%%       You can do so by adding more goal states below
%%%%% Goal XY should be read as goal Y for problem X

%% Goal states for robocupInit1
goal_state(11, S) :- robotLoc(r1, 0, 1, S).
goal_state(12, S) :- hasBall(r2, S).
goal_state(13, S) :- hasBall(r3, S).
goal_state(14, S) :- scored(S). 
goal_state(15, S) :- robotLoc(r1, 2, 2, S).
goal_state(16, S) :- robotLoc(r1, 3, 2, S).

%% Goal states for robocupInit1
goal_state(21, S) :- scored(S). 
goal_state(22, S) :- robotLoc(r1, 2, 4, S).

%%%%% SECTION: precondition_axioms_robocup
%%%%% Write precondition axioms for all actions in your domain. Recall that to avoid
%%%%% potential problems with negation in Prolog, you should not start bodies of your
%%%%% rules with negated predicates. Make sure that all variables in a predicate 
%%%%% are instantiated by constants before you apply negation to the predicate that 
%%%%% mentions these variables. 

% actions:
% move
poss(move(Robot, Row1, Col1, Row2, Col2), S) :-
    robot(Robot),
    robotLoc(Robot, Row1, Col1, S),
    adjacent(Row1, Col1, Row2, Col2),
    not opponentAt(Row2, Col2),
    not robotLoc(_,Row2,Col2,S).

% pass
poss(pass(Robot1, Robot2), S) :-
    robot(Robot1), robot(Robot2),
    hasBall(Robot1, S),
    robotLoc(Robot1, Row1, Col1, S),
    robotLoc(Robot2, Row2, Col2, S),
    isSameRowOrCol(Row1, Col1, Row2, Col2),
    canPass(Row1, Col1, Row2, Col2),
    not Robot1 = Robot2.

% shoot
poss(shoot(Robot), S) :-
    hasBall(Robot, S),
    robotLoc(Robot, Row, Col, S),
    goalCol(Col),
    numRows(MaxRows),
    MaxRows2 is MaxRows + 1,
    canPass(Row,Col, MaxRows2, Col),
    not opponentAt(MaxRows2, Col).

% checks is space is adjacent to each other
adjacent(Row1, Col1, Row2, Col2) :-
    Row1 = Row2,
    Col2 is Col1 + 1,
    valid_position(Row2, Col2). % Right
adjacent(Row1, Col1, Row2, Col2) :-
    Row1 = Row2,
    Col2 is Col1 - 1,
    valid_position(Row2, Col2). % Left
adjacent(Row1, Col1, Row2, Col2) :-
    Col1 = Col2,
    Row2 is Row1 + 1,
    valid_position(Row2, Col2). % Down
adjacent(Row1, Col1, Row2, Col2) :-
    Col1 = Col2,
    Row2 is Row1 - 1,
    valid_position(Row2, Col2). % Up

% Ensure positions are within bounds
valid_position(Row, Col) :-
    numRows(MaxRows),
    numCols(MaxCols),
    Row >= 0, Row < MaxRows,
    Col >= 0, Col < MaxCols.

% Check if two cells are in the same row or column
isSameRowOrCol(Row1, _, Row2, _) :-
    Row1 = Row2.
isSameRowOrCol(_, Col1, _, Col2) :-
    Col1 = Col2.

% Check if path is clear
canPass(Row1, Col1, Row2, Col2) :-
    (Row1 = Row2, horizontalClear(Row1, Col1, Col2)).
canPass(Row1, Col1, Row2, Col2) :-
    (Col1 = Col2, verticalClear(Col1, Row1, Row2)).

horizontalClear(_, Col, Col).
horizontalClear(Row, Col1, Col2):-
    Col1 < Col2,
    not opponentAt(Row, Col1),
    Col is Col1 + 1,
    horizontalClear(Row, Col, Col2).
horizontalClear(Row, Col1, Col2):-
    Col1 > Col2,
    not opponentAt(Row, Col1),
    Col is Col1 - 1,
    horizontalClear(Row, Col, Col2).

verticalClear(_, Row, Row).
verticalClear(Col, Row1, Row2):-
    Row1 < Row2,
    Row is Row1 + 1,
    not opponentAt(Row1, Col),
    verticalClear(Col, Row, Row2).

verticalClear(Col, Row1, Row2):-
    Row1 > Row2,
    Row is Row1 - 1,
    not opponentAt(Row1, Col),
    verticalClear(Col, Row, Row2).

%%%%% SECTION: successor_state_axioms_robocup
%%%%% Write successor-state axioms that characterize how the truth value of all 
%%%%% fluents change from the current situation S to the next situation [A | S]. 
%%%%% You will need two types of rules for each fluent: 
%%%%% 	(a) rules that characterize when a fluent becomes true in the next situation 
%%%%%	as a result of the last action, and
%%%%%	(b) rules that characterize when a fluent remains true in the next situation,
%%%%%	unless the most recent action changes it to false.
%%%%% When you write successor state axioms, you can sometimes start bodies of rules 
%%%%% with negation of a predicate, e.g., with negation of equality. This can help 
%%%%% to make them a bit more efficient.
%%%%%
%%%%% Write your successor state rules here: you have to write brief comments %

robotLoc(Robot, Row, Col, [move(Robot, Row2, Col2, Row, Col) | S]).
robotLoc(Robot, Row, Col, [A | S]) :-
    not A = move(Robot,Row,Col,_,_),
    robotLoc(Robot, Row, Col, S).

hasBall(Robot, [pass(Robot2, Robot) | S]) :- hasBall(Robot2, S).

hasBall(Robot, [A | S]) :-
    not A = pass(_,_),
    hasBall(Robot, S).

scored([shoot(_) | S]).

scored([A | S]) :-
    not A = shoot(_),
    scored(S).

%%%%% SECTION: declarative_heuristics_robocup
%%%%% The predicate useless(A,ListOfPastActions) is true if an action A is useless
%%%%% given the list of previously performed actions. The predicate useless(A,ListOfPastActions)
%%%%% helps to solve the planning problem by providing declarative heuristics to 
%%%%% the planner. If this predicate is correctly defined using a few rules, then it 
%%%%% helps to speed-up the search that your program is doing to find a list of actions
%%%%% that solves a planning problem. Write as many rules that define this predicate
%%%%% as you can: think about useless repetitions you would like to avoid, and about 
%%%%% order of execution (i.e., use common sense properties of the application domain). 
%%%%% Your rules have to be general enough to be applicable to any problem in your domain,
%%%%% in other words, they have to help in solving a planning problem for any instance
%%%%% (i.e., any initial and goal states).
%%%%%	
%%%%% write your rules implementing the predicate  useless(Action,History) here. %

% 1: useless moving to same spot
useless(move(_, Row, Col, Row, Col), _).

% 2: useless repeating the same move
useless(move(R, Row1, Col1, Row2, Col2), [move(R,Row2,Col2,Row1,Col1)|_]).

% 3: useless passing to the same robot
useless(pass(R2, R1), [pass(R1, R2) | _]).

% 4: useless shooting if not aligned with goal
useless(shoot(Robot), S) :-
    robotLoc(Robot, _, Col, S),
    goalCol(GoalCol),
    not Col = GoalCol.

% 5: useless shooting if just shot
useless(shoot(Robot), [shoot(Robot) | _]).

% 6: useless passing if just shot
useless(pass(_,_),[shoot(_)|_]).

% 7: useless moving if just shot
useless(move(_,_,_,_,_),[shoot(_)|_]).

% 8: useless shooting if scored
useless(shoot(_), S) :-
    scored(S).
