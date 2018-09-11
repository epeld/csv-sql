:- module(sql, [parse_query/3]).
:- use_module(strings, [string//1]).
:- set_prolog_flag(double_quotes, codes).

parse_query(Text, AtomFields, Where) :-
  phrase(select_query(Fields, UglyWhere), Text),
  pretty_where(UglyWhere, Where),
  maplist(atom_codes, AtomFields, Fields).


pretty_where(like(ColName, Codes), like(Atom, Codes)) :-
  atom_codes(ColName, Atom).


select_query(Fields, Where) -->
  "select",
  space,
  comma_fields(Fields),
  space,
  "from",
  space,
  "stdin",
  optional_where_clause(Where).


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
