
:- module(main, [main/0]).
:- set_prolog_flag(double_quotes, codes).

:- use_module(csv_util, [stream_csv/2]).
:- use_module(sql, [select_fields/3]).


main :-
  set_stream(user_output, tty(false)),
  stream_csv(user_input, Csv),
  select_fields(Csv, [c,b,a], CsvOut),
  csv_write_stream(user_output, CsvOut, []),
  format("Done~n").
