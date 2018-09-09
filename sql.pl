:- module(sql, []).
:- set_prolog_flag(double_quotes, codes).

select_query(Fields) -->
  "select",
  space,
  comma_fields(Fields),
  space,
  "from",
  space,
  "stdin".


space --> [32].


comma_fields(all) --> "*".
comma_fields([Field | Rest]) -->
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
