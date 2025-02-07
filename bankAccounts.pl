% Enter the names of your group members below.
% If you only have 2 group members, leave the last space blank
%
%%%%%
%%%%% NAME: Eliya Farhat
%%%%% NAME: Manar Elbeshbishy
%%%%% NAME: Tahir Mahmood
%
% Add the required rules in the corresponding sections. 
% If you put the rules in the wrong sections, you will lose marks.
%
% You may add additional comments as you choose but DO NOT MODIFY the comment lines below
%


%%%%% SECTION: database
%%%%% Put statements for account, created, lives, location and gender below

account(1, alexander, td, 4700).                 account(2, emma, scotia, 2350).
account(3, sophia, capitalOne, 11420).           account(4, ethan, cibc, 2340).
account(5, lucas, rbc, 900).                     account(6, isaac, bmo, 7850).
account(7, chloe, scotia, 3200).                 account(8, nathan, td, 1500).
account(9, oliver, citi, 12250).                 account(10, noah, rbc, 2800).
account(11, jacob, capitalOne, 6100).            account(12, david, bmo, 12675).
account(13, david, citi, 2300).                  account(14, samuel, cibc, 500).
account(15, lucas, capitalOne, 300).             account(16, ethan, metro_credit_union, 3500).
account(17, emma, metro_credit_union, 2230).     account(18, emma, citi, 22000).
account(19, noah, citi, 100).                    account(20, rebecca, citi, 1010101).
account(21, mary, metro_credit_union, 232).
  
created(1, alexander, td, 7, 2019).              created(2, emma, scotia, 3, 2011).
created(3, sophia, capitalOne, 6, 2018).         created(4, ethan, cibc, 5, 2016).
created(5, lucas, rbc, 10, 2005).                created(6, isaac, bmo, 3, 2024).
created(7, chloe, scotia, 4, 2021).              created(8, nathan, td, 1, 2017).
created(9, oliver, citi, 12, 2013).              created(10, noah, rbc, 2, 2009).
created(11, jacob, capitalOne, 8, 2014).         created(12, david, bmo, 11, 2007).
created(13, david, citi, 9, 2010).               created(14, samuel, cibc, 6, 2018).
created(15, lucas, metro_credit_union, 5, 2023). created(16, ethan, metro_credit_union, 7, 2022). 
created(17, emma, metro_credit_union, 10, 2009). created(18, emma, citi, 11, 2024).
created(19, noah, citi, 11, 2023).               created(20, rebecca, citi, 11, 2022).
created(21, mary, metro_credit_union, 11, 2008).

lives(nathan, mississauga).                      lives(oliver, mississauga).
lives(noah, losAngeles).                         lives(jacob, oakville).
lives(david, toronto).                           lives(samuel, scarborough).
lives(alexander, mississauga).                   lives(emma, markham).
lives(sophia, oakville).                         lives(ethan, losAngeles).
lives(lucas, losAngeles).                        lives(isaac, scarborough).
lives(chloe, madrid).                            lives(rebecca, london).
lives(mary, markham).

location(losAngeles, usa).                       location(madrid, spain).
location(toronto, canada).                       location(brampton, canada).
location(mississauga, canada).                   location(scarborough, canada).
location(oakville, canada).                      location(markham, canada).
location(london, uk).

location(capitalOne, losAngeles).                location(scotia, toronto).
location(rbc, toronto).                          location(td, toronto).
location(cibc, toronto).                         location(citi, london).
location(bmo, toronto).                          location(metro_credit_union, markham).

gender(chloe, woman).                            gender(nathan, man).
gender(oliver, man).                             gender(noah, man).
gender(jacob, man).                              gender(david, man).
gender(samuel, man).                             gender(alexander, man).
gender(emma, woman).                             gender(sophia, woman).
gender(ethan, man).                              gender(lucas, man).
gender(isaac, man).                              gender(rebecca, woman).
gender(mary, woman).


%%%%% SECTION: lexicon
%%%%% Put the rules/statements defining articles, adjectives, proper nouns, common nouns,
%%%%% and prepositions in this section.
%%%%% You should also put your lexicon helpers in this section
%%%%% Your helpers should include at least the following:
%%%%%       bank(X), person(X), man(X), woman(X), city(X), country(X)
%%%%% You may introduce others as you see fit
%%%%% DO NOT INCLUDE ANY statements for account, created, lives, location and gender 
%%%%%     in this section

% helpers
bank(X) :-
   location(X, City),
   not city(X).

person(X) :-
   account(ID, X, Bank, Balance).

man(X) :-
   gender(X, man).

woman(X) :-
   gender(X, woman).

city(X) :-
   location(X, Country),
   not location(Country, Y).

country(X) :-
   location(City, X),
   not location(X, Y).

% articles
article(a).
article(an).
article(any).
article(the).

% common nouns
common_noun(bank, X) :- account(ID, Name, X, Balance).
common_noun(city, X) :- city(X).
common_noun(country, X) :- country(X).
common_noun(american, X) :- adjective(american, X), person(X).
common_noun(canadian, X) :- adjective(canadian, X), person(X).
common_noun(british, X) :- adjective(british, X), person(X).
common_noun(man, X) :- man(X).
common_noun(woman, X) :- woman(X).
common_noun(owner, X) :- created(ID, X, Bank, Month, Year).
common_noun(person, X) :- person(X).
common_noun(account, X) :- account(X, Name, Bank, Balance).
common_noun(balance, X) :- account(ID, Name, Bank, X).

% prepositions

% BALANCE OF ACCOUNT 
preposition(of, X, Y) :- account(Y, Name, Bank, X).
% ACCOUNT OF PERSON 
preposition(of, X, Y) :- account(X, Y, Bank, Balance), person(Y).
% OWNER OF ACCOUNT 
preposition(of, X, Y) :- account(Y, X, Bank, Balance), common_noun(owner, X).
% COUNTRY OF ACCOUNT 
preposition(of, X, Y) :- country(X), account(Y, Name, Bank, Balance), location(Bank, City), location(City, X).
% ACCOUNT OF MAN
preposition(of, X, Y) :- account(X, Y, Bank, Balance), man(Y).
% ACCOUNT OF WOMAN
preposition(of, X, Y) :- account(X, Y, Bank, Balance), woman(Y).

% PERSON FROM ACCOUNT 
preposition(from, X, Y) :- person(X), account(Y, X, Bank, Balance).
% PERSON FROM CITY 
preposition(from, X, Y) :- person(X), city(Y), lives(X, Y).
% OWNER FROM COUNTRY 
preposition(from, X, Y) :- common_noun(owner, X), lives(X, Z), location(Z, Y).
% BALANCE FROM ACCOUNT 
preposition(from, X, Y) :- account(Y, Name, Bank, X).
% WOMAN FROM COUNTRY 
preposition(from, X, Y) :- woman(X), lives(X, Z), location(Z, Y).
% MAN FROM COUNTRY 
preposition(from, X, Y) :- man(X), lives(X, Z), location(Z, Y).
% ACCOUNT FROM COUNTRY 
preposition(from, X, Y) :- account(X, Name, Bank, Balance), bank(Bank), location(Bank, City), location(City, Y).

% ACCOUNT IN BANK 
preposition(in, X, Y) :- account(X, Name, Y, Balance).
% ACCOUNT IN LOCAL BANK 
preposition(in, X, Y) :- account(X, Name, Y, Balance), location(Y, City), location(City, Canada). 
% CITY IN COUNTRY 
preposition(in, X, Y) :- city(X), country(Y), location(X, Y).
% BANK IN CITY 
preposition(in, X, Y) :- bank(X), city(Y), location(X, Y).
% BANK IN COUNTRY 
preposition(in, X, Y) :- bank(X), country(Y), location(X, City), location(City, Y).
% BALANCE IN ACCOUNT 
preposition(in, X, Y) :- account(Y, Name, Bank, X).



% BANK WITH ACCOUNT 
preposition(with, X, Y) :- bank(X), account(Y, Name, X, Balance).
preposition(with, X, Y) :- bank(X), account(ID, Y, X, Balance).
preposition(with, Y, X) :- account(ID, Y, X, Balance).
preposition(with, X, Y) :- account(X, Name, Bank, Balance), account(Y, Name2, Bank, Balance2), not X = Y.
% CITY WITH ACCOUNT 
preposition(with, X, Y) :- city(X), account(Y, Name, Bank, Balance), location(Bank, City).
% PERSON WITH ACCOUNT 
preposition(with, X, Y) :- person(X), account(Y, X, Bank, Balance).
% MALE WITH ACCOUNT 
preposition(with, X, Y) :- gender(X, man), account(Y, X, Bank, Balance).
% FEMALE WITH ACCOUNT 
preposition(with, X, Y) :- gender(X, woman), account(Y, X, Bank, Balance).
% OWNER WITH ACCOUNT 
preposition(with, X, Y) :- common_noun(owner, X), account(Y, X, Bank, Balance).
% ACCOUNT WITH BANK 
preposition(with, X, Y) :- account(X, Name, Y, Balance).
% BANK WITH BALANCE 
preposition(with, X, Y) :- bank(X), account(ID, Name, X, Y).
% ACCOUNT WITH BALANCE 
preposition(with, X, Y) :- account(X, Name, Bank, Y).

preposition(between, What, Lower, Upper) :- number(What), What > Lower, What < Upper.
preposition(between, What, Lower, Upper) :- account(What, Name, Bank, Balance), Balance > Lower, Balance < Upper.
preposition(between, What, Lower, Upper) :- account(ID, What, Bank, Balance), Balance > Lower, Balance < Upper.
preposition(between, What, Lower, Upper) :- account(ID, Name, What, Balance), Balance > Lower, Balance < Upper.
preposition(between, What, Lower, Upper) :- account(ID, Name, Bank, What), What > Lower, What < Upper.


% proper nouns
proper_noun(City) :- city(City).
proper_noun(Bank) :- bank(Bank).
proper_noun(Country) :- country(Country).
proper_noun(Person) :- person(Person).
proper_noun(Number) :- number(Number).
proper_noun(AccNumber) :- account(AccNumber, Name, Bank, Balance).

% adjectives
adjective(small, X) :- account(X, Name, Bank, Balance), Balance < 1000.
adjective(medium, X) :- account(X, Name, Bank, Balance), Balance >= 1000, Balance =< 10000.
adjective(large, X) :- account(X, Name, Bank, Balance), Balance > 10000.
adjective(canadian, X) :- lives(X, City), location(City, canada).
adjective(canadian, X) :- location(X, canada).
adjective(canadian, X) :- location(X, City), location(City, canada).
adjective(canadian, X) :- account(X, Name, Bank, Balance), adjective(canadian, Bank).
adjective(american, X) :- lives(X, City), location(City, usa).
adjective(american, X) :- location(X, usa).
adjective(american, X) :- location(X, City), location(City, usa).
adjective(american, X) :- account(X, Name, Bank, Balance), adjective(usa, Bank).
adjective(british, X) :- lives(X, City), location(City, uk).
adjective(british, X) :- location(X, uk).
adjective(british, X) :- location(X, City), location(City, uk).
adjective(british, X) :- account(X, Name, Bank, Balance), adjective(uk, Bank).
adjective(female, X) :- woman(X).
adjective(male, X) :- man(X).
adjective(local, X) :- adjective(canadian, X).
adjective(foreign, X) :- adjective(american, X).
adjective(foreign, X) :- adjective(british, X).
adjective(new, X) :- account(X, Name, Bank, Balance), created(X, Name, Bank, Month, Year), Year = 2024.
adjective(old, X) :- account(X, Name, Bank, Balance), created(X, Name, Bank, Month, Year), Year < 2024.
adjective(recent, X) :- adjective(new, X). % same as new



%%%%% SECTION: parser
%%%%% For testing your lexicon for question 3, we will use the default parser initially given to you.
%%%%% ALL QUERIES IN QUESTION 3 and 4 SHOULD WORK WHEN USING THE DEFAULT PARSER
%%%%% For testing your answers for question 5, we will use your parser below
%%%%% You may include helper predicates for Question 5 here, but they
%%%%% should not be needed for Question 3
%%%%% DO NOT INCLUDE ANY statements for account, created, lives, location and gender 
%%%%%     in this section


what(Words, Ref) :- np(Words, Ref).

/* Noun phrase can be a proper name or can start with an article */

np([Name],Name) :- proper_noun(Name).
np([the|Rest], What) :- article(the), np2(Rest, X), not (np2(Rest, Y), not X = Y), What = X.
np([Art|Rest], What) :- not Art = the, article(Art), np2(Rest, What).


/* If a noun phrase starts with an article, then it must be followed
   by another noun phrase that starts either with an adjective
   or with a common noun. */

np2([largest|Rest],What) :- np2(Rest, What), not (np2(Rest,Y), not What = Y, account(What, Name, Bank, Balance), account(Y, Name2, Bank2, Balance2), Balance < Balance2).
np2([oldest|Rest],What) :- np2(Rest, What), not (np2(Rest,Y), not What = Y, created(What, Name, Bank, Month, Year), 
   created(Y, Name2, Bank2, Month2, Year2), 
   (Year2 < Year, (not Year2 = Year, not Month2 < Month))).
np2([Adj|Rest],What) :- not Adj = largest, not Adj = oldest, adjective(Adj,What), np2(Rest, What).
np2([Noun|Rest], What) :- common_noun(Noun, What), mods(Rest,What).

/* Modifier(s) provide an additional specific info about nouns.
   Modifier can be a prepositional phrase followed by none, one or more
   additional modifiers.  */

mods([], _).
mods(Words, What) :-
	appendLists(Start, End, Words),
	prepPhrase(Start, What),	mods(End, What).

prepPhrase([Prep | Rest], What) :-
	preposition(Prep, What, Ref), np(Rest, Ref).

prepPhrase([between, Lower, and, Upper], What) :- number(Lower), number(Upper), preposition(between, What, Lower, Upper). % between can be at the end (no rest)
prepPhrase([between, Lower, and, Upper | Rest], What) :- number(Lower), number(Upper), preposition(between, What, Lower, Upper), np(Rest, What). % or before the end 


appendLists([], L, L).
appendLists([H | L1], L2, [H | L3]) :-  appendLists(L1, L2, L3).