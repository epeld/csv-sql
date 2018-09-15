:- module(sql, [parse_query/4]).
:- use_module(strings, [string//1]).
:- set_prolog_flag(double_quotes, codes).

% This parses the query and 'pretty-prints' the result
% which means, "converts codes to atoms"
parse_query(Text, AtomFields, Where, OrderBy) :-
  phrase(select_query(Fields, UglyWhere, OrderByCodes), Text),
  pretty_where(UglyWhere, Where),
  pretty_fields(Fields, AtomFields),
  pretty_order_by(OrderByCodes, OrderBy).


pretty_fields(all, all).
pretty_fields(Fields, AtomFields) :-
  maplist(atom_codes, AtomFields, Fields).

pretty_where(like(ColName, Codes), like(Atom, Codes)) :-
  atom_codes(Atom, ColName).

pretty_where(nothing, nothing).


pretty_order_by(nothing, nothing).

pretty_order_by([asc(F)], [asc(A)]) :-
  atom_codes(A, F).

pretty_order_by([desc(F)], [desc(A)]) :-
  atom_codes(A, F).


%
% SQL query DCG
%
select_query(Fields, Where, OrderBy) -->
  "select",
  space,
  comma_fields(Fields),
  space,
  "from",
  space,
  "stdin",
  optional_where_clause(Where),
  optional_order_by(OrderBy).


optional_order_by(nothing) --> [].
optional_order_by([Field]) --> space, order_by([Field]).


order_by([Field]) -->
  "order by",
  space,
  asc_desc_field(Field).


asc_desc_field(desc(Field)) -->
  field(Field),
  space,
  "desc".


asc_desc_field(asc(Field)) -->
  field(Field) ;
  (
    field(Field), space, "asc"
  ).


optional_where_clause(nothing) --> [].
optional_where_clause(Where) --> space, where_clause(Where).


where_clause(Where) -->
  "where",
  space,
  where_condition(Where).


where_condition(like(ColName, String)) -->
  field(ColName),
  space,
  "like",
  space,
  string(String).

where_condition(like(ColName, String)) -->
  string(String),
  space,
  "like",
  space,
  field(ColName).
  


space --> [32].

optional_space --> space ; [].


comma_fields(all) --> "*".
comma_fields([Field | Rest]) -->
  field(Field),
  comma_fields_1(Rest).


comma_fields_1([Field | Rest]) -->
  ",",
  optional_space,
  field(Field),
  comma_fields_1(Rest).

comma_fields_1([]) --> [].


field([Char | Rest]) -->
  alpha(Char),
  field_1(Rest).


field_1([Char | Rest]) -->
  alpha(Char),
  field_1(Rest).

field_1([]) -->
  [].


alpha(Char) -->
  {
    alpha_char(Char)
  },
  [Char].


alpha_char(Char) :-
  member(Char, "ABCDEFGHIJKLMNOPQRSTUVWXYZ").

alpha_char(Char) :-
  member(Char, "abcdefghijklmnopqrstuvwxyz").
