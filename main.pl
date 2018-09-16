
:- module(main, [main/0, main/1]).
:- set_prolog_flag(double_quotes, codes).

:- use_module(csv_util, [stream_csv/2]).
:- use_module(options, [parse_argv/3]).
:- use_module(ops, [select_fields/3, filter_rows/3, order_rows/3]).
:- use_module(sql, [parse_query/4]).

main :-
  current_prolog_flag(argv, Argv),
  main(Argv).

main(Argv) :-
  parse_argv(Argv, Options, Query),
  setup_call_cleanup(
    (
      input_stream(In),
      % Put output in 'non-interactive' mode before parsing to avoid printing prompts
      set_stream(user_output, tty(false))
    ),
    once(
      main(In, Options, Query)
    ),
    (
      set_stream(user_output, tty(true)),
      close(In)
    )
  ).

main(Input, CmdLineOptions, Query) :-

  % Process CSV
  stream_csv(Input, Csv),

  atom_codes(Query, CQuery),
  parse_query(CQuery, Fields, Filter, OrderBy),
  filter_rows(Csv, Filter, CsvFiltered),
  order_rows(CsvFiltered, OrderBy, CsvOrdered),
  select_fields(CsvOrdered, Fields, CsvOut),

  % Output
  output_csv(user_output, CsvOut, CmdLineOptions).

output_csv(Stream, Csv, Options) :-
  option(quiet(Quiet), Options, quiet(false)),
  quiet_csv(Quiet, Csv, Csv1),
  output_options(CsvOptions),
  csv_write_stream(Stream, Csv1, CsvOptions).


quiet_csv(true, [ _H | Rest ], Rest).
quiet_csv(false, Csv, Csv).

output_options([separator(Tab)]) :-
  [Tab] = "\t".


% input_stream(user_input) :- !.

input_stream(In) :-
  open('abc.csv', read, In).
