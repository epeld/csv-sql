:- module(options, [parse_argv/3]).
:- set_prolog_flag(double_quotes, codes).

program_options(Options) :-
  Options = [
    [opt(quiet), type(boolean), default(false),
     shortflags([q]), longflags(['quiet']),
     help(['Suppress printing of headers in output CSV'])]
  ].


parse_argv(Argv, ParsedOptions, Query):-
  program_options(Options),
  opt_parse(Options, Argv, Parsed, Positional),
  Positional = [Query].
