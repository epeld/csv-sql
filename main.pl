
:- module(main, [main/0, main/1]).
:- set_prolog_flag(double_quotes, codes).

:- use_module(csv_util, [stream_csv/2]).
:- use_module(ops, [select_fields/3, filter_rows/3, order_rows/3]).
:- use_module(sql, [parse_query/4]).

main :-
  current_prolog_flag(argv, Argv),
  main(Argv).

main(Argv) :-
  setup_call_cleanup(
    (
      input_stream(In),
      % Put output in 'non-interactive' mode before parsing to avoid printing prompts
      set_stream(user_output, tty(false))
    ),
    once(
      main(In, Argv)
    ),
    (
      set_stream(user_output, tty(true)),
      close(In)
    )
  ).

main(Input, Argv) :-
  

  % Process CSV
  stream_csv(Input, Csv),
  [First | _] = Argv,

  atom_codes(First, CFirst),
  parse_query(CFirst, Fields, Filter, OrderBy),
  filter_rows(Csv, Filter, CsvFiltered),
  order_rows(CsvFiltered, OrderBy, CsvOrdered),
  select_fields(CsvOrdered, Fields, CsvOut),

  % Output
  output_options(Options),
  csv_write_stream(user_output, CsvOut, Options).


output_options([separator(Tab)]) :-
  [Tab] = "\t".


input_stream(user_input) :- !.

input_stream(In) :-
  open('abc.csv', read, In).
