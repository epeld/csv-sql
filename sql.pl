:- module(sql, [parse_query/5]).
:- use_module(strings, [string//1]).
:- set_prolog_flag(double_quotes, codes).

% This parses the query and 'pretty-prints' the result
% which means, "converts codes to atoms"
parse_query(Text, AtomFields, Where, OrderBy, Limit) :-
  phrase(select_query(Fields, UglyWhere, OrderByCodes, Limit), Text),
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
select_query(Fields, Where, OrderBy, Limit) -->
  keyword_string("select"),
  space_plus,
  comma_fields(Fields),
  space_plus,
  keyword_string("from"),
  space_plus,
  keyword_string("stdin"),
  optional_where_clause(Where),
  optional_order_by(OrderBy),
  optional_limit(Limit).


optional_limit(nothing) --> [].
optional_limit(n(Limit)) --> space_plus, limit(Limit).

limit(Limit) -->
  keyword_string("limit"),
  space_plus,
  number(Limit).


number(N) --> digits(D), { number_codes(N, D) }.

digits([D | Rest]) --> [D], { member(D, "0123456789") }, digits1(Rest).

digits1([]) --> [].
digits1([D | Rest]) --> [D], { member(D, "0123456789") }, digits1(Rest).



optional_order_by(nothing) --> [].
optional_order_by([Field]) --> space_plus, order_by([Field]).


order_by([Field]) -->
  keyword_string("order by"),
  space_plus,
  asc_desc_field(Field).


asc_desc_field(desc(Field)) -->
  field(Field),
  space_plus,
  keyword_string("desc").


asc_desc_field(asc(Field)) -->
  field(Field) ;
  (
    field(Field), space_plus, keyword_string("asc")
  ).


optional_where_clause(nothing) --> [].
optional_where_clause(Where) --> space_plus, where_clause(Where).


where_clause(Where) -->
  keyword_string("where"),
  space_plus,
  where_condition(Where).


where_condition(like(ColName, String)) -->
  field(ColName),
  space_plus,
  keyword_string("like"),
  space_plus,
  string(String).

where_condition(like(ColName, String)) -->
  string(String),
  space_plus,
  keyword_string("like"),
  space_plus,
  field(ColName).
  


space_plus --> [32], optional_space_plus.

optional_space_plus --> space_plus ; [].


comma_fields(all) --> "*".
comma_fields([Field | Rest]) -->
  field(Field),
  optional_space_plus,
  comma_fields_1(Rest).


comma_fields_1([Field | Rest]) -->
  ",",
  optional_space_plus,
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


keyword_string(Codes) -->
  {
    atom_codes(Atom, Codes),
    downcase_atom(Atom, Down),
    upcase_atom(Atom, Up),
    atom_codes(Down, DownCodes),
    atom_codes(Up, UpCodes)
  },
  (DownCodes ; UpCodes).
