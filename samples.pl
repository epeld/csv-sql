
:- module(samples, [sample_csv/1]).
:- set_prolog_flag(double_quotes, codes).

sample_csv([Header, Row1, Row2]) :-
  Header = row(a, foo, bar),
  Row1 = row(33,44,55),
  Row2 = row(1,17,3).
