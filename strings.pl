%
% This module contains string parsing predicates
%
:- module(strings, [string//1, string//2]).
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
