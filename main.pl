
:- module(main, [main/0]).
:- set_prolog_flag(double_quotes, codes).

:- use_module(csv_util, [stream_csv/2]).
:- use_module(ops, [select_fields/3, filter_rows/3]).
:- use_module(sql, [parse_query/3]).

main :-
  current_prolog_flag(argv, Argv),
  catch(
    main(user_input, Argv),
    Error,
    format("Error: ~w~n", [Error])
  ).

main(Input, Argv) :-
  % Put output in 'non-interactive' mode before parsing to avoid printing prompts
  set_stream(user_output, tty(false)),

  % Process CSV
  stream_csv(Input, Csv),
  [First | _] = Argv,

  atom_codes(First, CFirst),
  parse_query(CFirst, Fields, Filter),
  filter_rows(Csv, Filter, CsvFiltered),
  select_fields(CsvFiltered, Fields, CsvOut),

  % Output
  output_options(Options),
  csv_write_stream(user_output, CsvOut, Options).


output_options([separator(Tab)]) :-
  [Tab] = "\t".
