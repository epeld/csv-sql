:- module(ops, [select_fields/3, filter_rows/3, order_rows/3]).
:- use_module(strings, [match_like/2]).
:- set_prolog_flag(double_quotes, codes).

%%
%% ORDER
%%
order_rows(R, nothing, R).
order_rows([ Header | Rows], OrderBy, [ Header | RowsOut]) :-
  predsort(row_compare(Header, OrderBy), Rows, RowsOut).

row_compare(Header, [O], Delta, Row1, Row2) :-
  field_name(O, Name),
  column_index(Header, Name, Ix),
  column_index(Row1, Value1, Ix),
  column_index(Row2, Value2, Ix),

  field_compare(O, Delta, Value1, Value2).


field_compare(asc(_), Order, Value1, Value2) :-
  mycompare(Order, Value1, Value2).

field_compare(desc(_), Order, Value1, Value2) :-
  mycompare(Order, Value2, Value1).


mycompare((<), Value1, Value2) :-
  compare(Delta, Value1, Value2),
  (
    Delta = (<) ; Delta = (=)
  ).

mycompare((>), Value1, Value2) :-
  compare((>), Value1, Value2).

field_name(asc(Name), Name).
field_name(desc(Name), Name).


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
  match_like(B, ACodes).


%%
%% SELECT
%%

select_fields([ Header | Rows], all, [ Header | Rows ]).

select_fields([ Header | Rows], Fields, [ HeaderOut | RowsOut ]) :-
  Fields \= all,
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
