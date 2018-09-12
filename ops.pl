:- module(ops, [select_fields/3, filter_rows/3]).
:- set_prolog_flag(double_quotes, codes).

%%
%% FILTER
%%
filter_rows([ Header | Rows], Filter, [ Header | RowsOut ]) :-
  include(satisfies_filter(Header, Filter), Rows, RowsOut).


satisfies_filter(_Header, nothing, _Row).
satisfies_filter(Header, like(Column, Codes), Row) :-
  column_index(Header, Column, Ix),
  column_index(Row, Value, Ix),

  like(Value, Codes).


like(A, B) :-
  format(codes(ACodes), "~w", [A]),
  % TODO support % and _ using dcg
  append([B, _2], ACodes).


%%
%% SELECT
%%

select_fields([ Header | Rows], Fields, [ HeaderOut | RowsOut ]) :-
  one_two_three_etc(Fields, Range),
  same_length(Rows, RowsOut),  
  length(Fields, NumColumns),
  rows([HeaderOut | RowsOut], NumColumns),
  column_indices(Header, Fields, Indices),
  column_indices(HeaderOut, Fields, Range),
  maplist(column_indices_1(Indices), Rows, RowFields),
  maplist(column_indices_1(Range), RowsOut, RowFields).


rows([Term | Rest], Arity) :-
  functor(Term, row, Arity),
  rows(Rest, Arity).

rows([], _).


column_indices(HeaderRow, Fields, Indices) :-
  maplist(column_index(HeaderRow), Fields, Indices).


column_indices_1(Indices, HeaderRow, Fields) :-
  column_indices(HeaderRow, Fields, Indices).


column_index(Row, Field, Index) :-
  arg(Index, Row, Field).


one_two_three_etc(List, Out) :-
  one_two_three_etc(List, 1, Out).

one_two_three_etc([_ | Rest], N, [N | OutRest]) :-
  N1 is N + 1,
  one_two_three_etc(Rest, N1, OutRest).

one_two_three_etc([], _, []).
