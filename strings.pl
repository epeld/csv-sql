%
% This module contains string parsing predicates
%
:- module(strings, [string//1, string//2, match_like/2]).
:- set_prolog_flag(double_quotes, codes).


string(Content) --> string(Content, _).

string(Content, QuoteChar) -->
  quote_char(QuoteChar),
  content(Content, QuoteChar),
  quote_char(QuoteChar).


content([C | Rest], Q) -->
  {
    quote_char(Q),
    between(0, 255, C),
    C \= Q,
    [C] \= "\\"
  },
  [C],
  content(Rest, Q).

content([ Char | Rest], Q) -->
  "\\", 
  [Char],
  content(Rest, Q).

content([], _) --> [].


quote_char(Char) -->
  {
    quote_char(Char)
  },
  [Char].


quote_char(Char) :-
  member(Char, "'\"").


%
% This predicate performs a SQL-LIKE-like matching.
% The argument can contain _ and % which act like wildcards
%
match_like(Pattern, String) :-
  once(
    phrase(like(Pattern), String)
  ).

like([]) --> [].

like([C | Rest]) -->
  [_],
  {
    [C] = "_"
  },
  like(Rest).

like([C | Rest]) -->
  {
    [C] = "%"
  },
  anything_then_like(Rest).

like([C | Rest]) -->
  [C],
  {
    [C] \= "%",
    [C] \= "_"
  },
  like(Rest).


anything_then_like(Rest) --> like(Rest).
anything_then_like(Rest) --> [_], anything_then_like(Rest).
