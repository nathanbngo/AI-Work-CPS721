
% Enter the names of your group members below.
% If you only have 2 group members, leave the last space blank
%
%%%%%
%%%%% NAME: Nathan Ngo 501090210
%%%%% NAME: Kevin Shao 501042805
%%%%% NAME:
%
% Add the required rules in the corresponding sections. 
% If you put the rules in the wrong sections, you will lose marks.
%
% You may add additional comments as you choose but DO NOT MODIFY the comment lines below
%


%%%%% SECTION: database
%%%%% Put statements for account, created, lives, location and gender below

% account(AccountID, Name, Bank, Balance)
account(1, drake, 'rbc', 1200).
account(2, taylor, 'cibc', 3000).
account(3, taylor, 'cibc', 2500).   
account(4, ed, 'td', 500).
account(5, justin, 'bmo', 15000).
account(6, ariana, 'cibc', 2000).
account(7, theweeknd, 'bmo', 25000).
account(8, selena, 'mcu', 600).
account(9, shawn, 'td', 900).
account(10, rihanna, 'rbc', 5400).
account(11, bruno, 'rbc', 800).
account(12, kanye, 'rbc', 1500).
account(13, kanye, 'mcu', 700).
account(14, drake, 'mcu', 80000).
account(15, beyonce, 'mcu', 8000).
account(16, rihanna, 'cibc', 200).
account(17, rihanna, 'td', 12000).
account(18, adele, 'td', 16000).

% created(AccountID, Name, Bank, Month, Year)
created(1, drake, 'rbc', 5, 2023).
created(2, taylor, 'cibc', 11, 2021).
created(3, taylor, 'cibc', 10, 2020).  
created(4, ed, 'td', 6, 2020).
created(5, justin, 'bmo', 4, 2024).
created(6, ariana, 'cibc', 10, 2023).
created(7, theweeknd, 'bmo', 9, 2019).
created(8, selena, 'mcu', 12, 2022).
created(9, shawn, 'td', 7, 2021).
created(10, rihanna, 'rbc', 3, 2024).
created(11, bruno, 'rbc', 1, 2021).
created(12, kanye, 'rbc', 5, 2022).
created(13, kanye, 'mcu', 7, 2023).
created(14, drake, 'mcu', 1, 2024).
created(15, beyonce, 'mcu', 3, 2015).
created(16, rihanna, 'cibc', 10, 2021).
created(17, rihanna, 'td', 10, 2024).
created(18, adele, 'td', 10, 2024).

% lives(Name, City)
lives(drake, richmondHill).
lives(taylor, markham).
lives(ed, ottawa).
lives(beyonce, markham).
lives(justin, scarborough).
lives(ariana, montreal).
lives(theweeknd, toronto).
lives(selena, scarborough).
lives(shawn, sanFrancisco).
lives(rihanna, losAngeles).
lives(bruno, losAngeles).
lives(kanye, losAngeles).
lives(adele, london).

% location(X, Y) for cities and banks
location('bmo', montreal).
location('cibc', toronto).
location('td', ottawa).
location('rbc', toronto).
location('mcu', losAngeles).
location(toronto, canada).
location(markham, canada).
location(richmondHill, canada).
location(scarborough, canada).
location(montreal, canada).
location(ottawa, canada).
location(newYork, usa).
location(losAngeles, usa).
location(sanFrancisco, usa).
location(london, uk).

% gender(Name, Gender)
gender(drake, man).
gender(taylor, woman).
gender(ed, man).
gender(beyonce, woman).
gender(justin, man).
gender(ariana, woman).
gender(theweeknd, man).
gender(selena, woman).
gender(shawn, man).
gender(rihanna, woman).
gender(bruno, man).
gender(kanye, man).
gender(beyonce, woman).
gender(adele, woman).

%%%%% SECTION: lexicon
%%%%% Put the rules/statements defining articles, adjectives, proper nouns, common nouns,
%%%%% and prepositions in this section.
%%%%% You should also put your lexicon helpers in this section
%%%%% Your helpers should include at least the following:
%%%%%       bank(X), person(X), man(X), woman(X), city(X), country(X)
%%%%% You may introduce others as you see fit
%%%%% DO NOT INCLUDE ANY statements for account, created, lives, location and gender 
%%%%%     in this section

/* initializing */

% Articles
article(a).
article(an).
article(the).
article(any).

% Common nouns
common_noun(bank, X) :- account(_,_,X,_).
common_noun(city, X) :- location(X, canada).
common_noun(city, X) :- location(X, usa).
common_noun(man, X) :- gender(X, man).
common_noun(woman, X) :- gender(X, woman).
common_noun(person, X) :- gender(X,_).
common_noun(account, X) :- account(X, _, _, _).
common_noun(account, X) :- account(_, X, _, _).
common_noun(balance, X) :- account(_, _, _, X).
common_noun(owner, X) :- account(_, X, _, _).
common_noun(country,X):- location(_,X), not location(X, _).
common_noun(american, X) :- lives(X,City),location(City, usa).
common_noun(american, X) :- location(X,City),location(City, usa).
common_noun(canadian, X) :- lives(X,City),location(City, canada).
common_noun(canadian, X) :- location(X,City),location(City, canada).

% Proper nouns
proper_noun(drake).
proper_noun(taylor).
proper_noun(ed).
proper_noun(beyonce).
proper_noun(justin).
proper_noun(ariana).
proper_noun(theweeknd).
proper_noun(selena).
proper_noun(shawn).
proper_noun(rihanna).
proper_noun('bmo').
proper_noun('cibc').
proper_noun('td').
proper_noun('rbc').
proper_noun('mcu').
proper_noun(toronto).
proper_noun(markham).
proper_noun(richmondHill).
proper_noun(scarborough).
proper_noun(montreal).
proper_noun(ottawa).
proper_noun(newYork).
proper_noun(losAngeles).
proper_noun(sanFrancisco).
proper_noun(london).
proper_noun(canada).
proper_noun(usa).
proper_noun(uk).


% Adjectives
adjective(canadian, X) :- lives(X, City), location(City, canada).
adjective(canadian, X) :- location(X,City),location(City, canada).

adjective(british, X) :- lives(X, City), location(City, uk).
adjective(british, X) :- location(X, City), location(City, uk).

adjective(american, X) :- lives(X, City), location(City, usa).
adjective(american, X) :- location(X,City),location(City, usa).

adjective(foreign, X) :- lives(X, City), location(City, usa).
adjective(foreign, X) :- location(X, City), location(City, usa).

adjective(small, X) :- account(X, _, _, Balance), Balance < 1000.
adjective(small, X) :- account(_, X, _, Balance), Balance < 1000.
adjective(medium, X) :- account(X, _, _, Balance), Balance >= 1000, Balance =< 10000.
adjective(large, X) :- account(_, _, _, X), X > 10000.
adjective(large, X) :- account(_, X, _, Y), Y > 10000.
adjective(large, X) :- account(X, _, _, Y), Y > 10000.

adjective(male, X) :- gender(X, man).
adjective(female, X) :- gender(X, woman).

adjective(local, X) :- location(X, canada).
adjective(local, X) :- location(X, Y), location(Y,canada).
adjective(local, X) :- account(_,X, Bank, _),location(Bank, Y), location(Y, canada).
adjective(local, X) :- account(X,_, Bank, _),location(Bank, Y), location(Y, canada).

adjective(new, X) :- created(X,_,_,_,Year), Year >= 2024.
adjective(old, X) :- created(X,_,_,_,Year), Year < 2024.

adjective(oldest, X) :- created(X, _, _, _, Year), not((created(_, _, _, _, OtherYear), OtherYear < Year)).
adjective(largest, X) :- account(X, _, _, Balance), not((account(_, _, _, OtherBalance), OtherBalance > Balance)).

% Prepositions
preposition(of, X, Y) :- account(Y, _, _, X).
preposition(of, X, Y) :- account(Y, X, _,_).
preposition(of, X, Y) :- account(X, Y, _,_).
preposition(of, X, Y) :- location(Y, X).

preposition(from, X, Y) :- account(_,User,X,_),lives(User,City),location(City,Y).
preposition(from, X, Y) :- lives(X,City),location(City,Y).
preposition(from, X, Y) :- lives(X,Y).
preposition(from, X, Y) :- account(X,User,_,_),lives(User,City),location(City,Y).

preposition(with, X, Y) :- account(Y, X, _, _).
preposition(with, X, Y) :- account(Y, _, X, _).
preposition(with, X, Y) :- account(X, Name, _, _), account(Y,Name,_,_), not X = Y.

preposition(in, X, Y) :- location(X, Y).
preposition(in, X, Y) :- location(X, Z), location(Z, Y).
preposition(in, X, Y) :- account(Y,X,_,_).
preposition(in, X, Y) :- account(X,_,Y,_).

%%%%% SECTION: parser
%%%%% For testing your lexicon for question 3, we will use the default parser initially given to you.
%%%%% ALL QUERIES IN QUESTION 3 and 4 SHOULD WORK WHEN USING THE DEFAULT PARSER
%%%%% For testing your answers for question 5, we will use your parser below
%%%%% You may include helper predicates for Question 5 here, but they
%%%%% should not be needed for Question 3
%%%%% DO NOT INCLUDE ANY statements for account, created, lives, location and gender 
%%%%%     in this section


what([_, largest, account, from,_,X], Ref) :-
	account(Ref, Name1, _, Balance), 
	gender(Name1, X),
	not((account(_, Name2, _, OtherBalance), 
	OtherBalance > Balance, gender(Name2,X))),
	mods(Rest,Ref).

what([_,balance, between, Low, and, High|Rest], Ref) :-
    number(Low),
    number(High),
    account(_, _, _, Ref),
    Ref >= Low,
	Ref =< High,
	mods(Rest,Ref).

what(Words, Ref) :- np(Words, Ref).

/* Noun phrase can be a proper name or can start with an article */
np([Name], Name) :- proper_noun(Name).
np([Art|Rest], What) :- article(Art), np2(Rest, What).

np([the | Rest], What) :- np3(Rest, What).

np3(Rest, What) :-
    np2(Rest, What),
    not((np2(Rest, Other), not Other = What)).

/* If a noun phrase starts with an article, then it must be followed
   by another noun phrase that starts either with an adjective
   or with a common noun. */
np2([Adj|Rest], What) :- adjective(Adj, What), np2(Rest, What).
np2([Noun|Rest], What) :- common_noun(Noun, What), mods(Rest, What).


/* Modifier(s) provide additional specific info about nouns.
   A modifier can be a prepositional phrase followed by none, one, or more
   additional modifiers. */
mods([], _).
mods(Words, What) :-
    appendLists(Start, End, Words),
    prepPhrase(Start, What), mods(End, What).

prepPhrase([Prep | Rest], What) :-
    preposition(Prep, What, Ref), np(Rest, Ref).

appendLists([], L, L).
appendLists([H | L1], L2, [H | L3]) :- appendLists(L1, L2, L3).




