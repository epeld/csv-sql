
:- module(main, [main/0]).
:- set_prolog_flag(double_quotes, codes).

:- use_module(csv_util, [stream_csv/2]).
:- use_module(sql, [select_fields/3]).

main :-
  main(user_input).

main(Input) :-
  % Put output in 'non-interactive' mode before parsing to avoid printing prompts
  set_stream(user_output, tty(false)),

  % Process CSV
  stream_csv(Input, Csv),
  select_fields(Csv, [c,b,a], CsvOut),

  % Output
  output_options(Options),
  csv_write_stream(user_output, CsvOut, Options).

output_options([separator(Tab)]) :-
  [Tab] = "\t".
