
:- module(main, [main/0, main/1]).
:- set_prolog_flag(double_quotes, codes).

:- use_module(csv_util, [stream_csv/2]).
:- use_module(options, [parse_argv/3]).
:- use_module(ops, [select_fields/3, filter_rows/3, order_rows/3]).
:- use_module(sql, [parse_query/5]).

main :-
  current_prolog_flag(argv, Argv),
  main(Argv).

main(Argv) :-
  parse_argv(Argv, Options, Query),
  stream_property(user_output, tty(Tty)),
  setup_call_cleanup(
    % Put output in 'non-interactive' mode before parsing to avoid printing prompts
    set_stream(user_output, tty(false)),
    once(
      main(Options, Query)
    ),
    set_stream(user_output, tty(Tty))
  ).

main(CmdLineOptions, Query) :-
  atom_codes(Query, CQuery),
  parse_query(CQuery, Fields, Filter, OrderBy, Limit),

  csv_from_input(stdin, Csv),

  filter_rows(Csv, Filter, CsvFiltered),
  order_rows(CsvFiltered, OrderBy, CsvOrdered),
  select_fields(CsvOrdered, Fields, CsvOut),
  limit(Limit, CsvOut, CsvLimited),

  % Output
  output_csv(user_output, CsvLimited, CmdLineOptions).


csv_from_input(stdin, Csv) :-
  stream_csv(Csv).

csv_from_input(file(Name), Csv) :-
  setup_call_cleanup(
    open(Name, read, Input),
    stream_csv(Input, Csv),
    close(Input)
  ).



output_csv(Stream, Csv, Options) :-
  option(quiet(Quiet), Options, quiet(false)),
  quiet_csv(Quiet, Csv, Csv1),
  output_options(CsvOptions),
  csv_write_stream(Stream, Csv1, CsvOptions).


limit(nothing, L, L).
limit(n(N), [ H | L ], [ H | L0]) :-
  number(N),
  min_member(N0, [Length, N0]),
  length(L, Length),
  length(L0, N0),

  append(L0, _, L).


quiet_csv(true, [ _H | Rest ], Rest).
quiet_csv(false, Csv, Csv).

output_options([separator(Tab)]) :-
  [Tab] = "\t".

