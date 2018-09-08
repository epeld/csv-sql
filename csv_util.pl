
:- module(csv_util, [stream_csv/2, csv_from_file/2]).
:- set_prolog_flag(double_quotes, codes).

stream_csv(Stream, Csv) :-
  [Tab] = " ",
  Options = [
    separator(Tab),
    strip(true)
  ],
  phrase_from_stream(csv(Csv, Options), Stream).

csv_from_file(FileName, Csv) :-
  setup_call_cleanup(
    open(FileName, read, Stream, []),
    stream_csv(Stream, Csv),
    close(Stream)
  ).

