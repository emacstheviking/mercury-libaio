%-----------------------------------------------------------------------------%
%
% File: aio.m
% Main author:
% Date: Fri 23 Aug 22:23 -- made into a lib (Sun Dec 12 08:34:21 2021)
%
% ANSI Colour and colour IO support.
% Also contains some fancy pants things for ticks, crosses etc.
% I have also added basic colour selection, cursor control and a few others
% for clearing the screen, moving to a specific position etc.
%-----------------------------------------------------------------------------%
:- module aio.

:- interface.

% Comment out for library build!
% :- pred main(io::di, io::uo) is det.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module string.

    %--------------------------------------------------%
    % API: stdout/stderror output.

:- func chromakey(string::in) = (string::out) is det.
:- pred enable_colours(bool::in, io::di, io::uo) is det.

:- pred error(string::in, list(poly_type)::in, io::di, io::uo) is det.
:- pragma format_call(pred(error/4), format_string_values(1, 2)).

:- func format(string::in, list(poly_type)::in) = (string::out) is det.
:- pred format(string::in, list(poly_type)::in, io::di, io::uo) is det.
:- pragma format_call(func(format/2), format_string_values(1, 2)).
:- pragma format_call(pred(format/4), format_string_values(1, 2)).

    %--------------------------------------------------%
    % API: simple colour management

:- pred black(io::di, io::uo) is det.
:- pred red(io::di, io::uo) is det.
:- pred green(io::di, io::uo) is det.
:- pred yellow(io::di, io::uo) is det.
:- pred blue(io::di, io::uo) is det.
:- pred magenta(io::di, io::uo) is det.
:- pred cyan(io::di, io::uo) is det.
:- pred white(io::di, io::uo) is det.
:- pred default(io::di, io::uo) is det.
:- pred blackbg(io::di, io::uo) is det.
:- pred redbg(io::di, io::uo) is det.
:- pred greenbg(io::di, io::uo) is det.
:- pred yellowbg(io::di, io::uo) is det.
:- pred bluebg(io::di, io::uo) is det.
:- pred magentabg(io::di, io::uo) is det.
:- pred cyanbg(io::di, io::uo) is det.
:- pred whitebg(io::di, io::uo) is det.
:- pred defaultbg(io::di, io::uo) is det.

    %--------------------------------------------------%
    % API: cursor positioning, other functions.

:- pred at_xy(int::in, int::in, io::di, io::uo) is det.
:- pred clr(io::di, io::uo) is det.
:- pred reset(io::di, io::uo) is det.


:- implementation.

%----------------------------------------------------------------------------%
% INTERNAL STATE -- controlling the output mode.

:- type aio_state
    --->    aio_state(
                f_colour :: bool,    % Colour or monochrome output?
                f_quiet  :: bool     % All output suppressed?
            ).

:- mutable(astate, aio_state, aio_state(yes, no), ground, [untrailed, attach_to_io_state]).

%----------------------------------------------------------------------------%

    % Enable or disable colour output.
    %
enable_colours(Mode, !IO) :-
    get_astate(aio_state(_, Quiet), !IO),
    set_astate(aio_state(Mode, Quiet), !IO).

%----------------------------------------------------------------------------%

    % Format a string with embedded color signals to stdout.
    %
format(Format, Args, !IO) :-
    format_to(io.stdout_stream, Format, Args, !IO).

    % Format a string with embedded color signals to stderr.
    %
error(Format, Args, !IO) :-
    format_to(io.stderr_stream, Format, Args, !IO).



:- pragma format_call(pred(format_to/5), format_string_values(2, 3)).
:- pragma promise_pure(format_to/5).

:- pred format_to(output_stream::in, string::in, list(poly_type)::in,
    io::di, io::uo) is det.

format_to(Stream, Format, Args, !IO) :-
    semipure get_astate(S),
    ( if f_colour(S) = yes then
        disable_warning [unknown_format_calls] (
            io.format(Stream, chromakey(Format), Args, !IO)
        )
    else
        disable_warning [unknown_format_calls] (
            io.format(Stream, monokey(Format), Args, !IO)
        )
    ).


:- pragma promise_pure(format/2).

format(Format, Args) = Str :-
    semipure get_astate(S),
    ( if f_colour(S) = yes then
        disable_warning [unknown_format_calls] (
            Str = string.format(chromakey(Format), Args)
        )
    else
        disable_warning [unknown_format_calls] (
            Str = string.format(monokey(Format), Args)
        )
    ).

%----------------------------------------------------------------------------%
% SCREEN CONTROL AND COLOUR SELECTION

:- pragma inline(reset/2).
:- pragma inline(clr/2).
:- pragma inline(at_xy/4).

reset(!IO) :- io.format("\u001b[0m]", [], !IO).
clr(!IO) :- io.format("\u001b[2J", [], !IO).
at_xy(X, Y, !IO) :- io.format("\u001b[%i;%iH", [ i(Y), i(X)], !IO).

black(!IO) :- cout(30, !IO).
red(!IO) :- cout(31, !IO).
green(!IO) :- cout(32, !IO).
yellow(!IO) :- cout(33, !IO).
blue(!IO) :- cout(34, !IO).
magenta(!IO) :- cout(35, !IO).
cyan(!IO) :- cout(36, !IO).
white(!IO) :- cout(37, !IO).
default(!IO) :- cout(39, !IO).

blackbg(!IO) :- cout(40, !IO).
redbg(!IO) :- cout(41, !IO).
greenbg(!IO) :- cout(42, !IO).
yellowbg(!IO) :- cout(43, !IO).
bluebg(!IO) :- cout(44, !IO).
magentabg(!IO) :- cout(45, !IO).
cyanbg(!IO) :- cout(46, !IO).
whitebg(!IO) :- cout(47, !IO).
defaultbg(!IO) :- cout(49, !IO).


:- pred cout(int::in, io::di, io::uo) is det.
cout(Colour, !IO) :-
    io.format("\u001b[%im", [ i(Colour)], !IO).

%----------------------------------------------------------------------------%

    % Replace magic string tokens with ANSII escape sequences.
    % Quick and dirty, probablyreally I should parse the string.
    %
chromakey(In) = Out :-
    P = (pred({Key, Val}::in, S0::in, S::out) is det :-
        string.replace_all(S0, Key, Val, S)
    ),
    list.foldl(P,
        [ {"@bl:", "\u001b[30m"} % black
        , {"@rd:", "\u001b[31m"} % ReD
        , {"@gr:", "\u001b[32m"} % GReen
        , {"@yl:", "\u001b[33m"} % YeLlow
        , {"@bu:", "\u001b[34m"} % BlUe
        , {"@mg:", "\u001b[35m"} % MaGenta
        , {"@cy:", "\u001b[36m"} % CYan
        , {"@wh:", "\u001b[37m"} % WHite
        , {"@+",   "\u001b[1m"}  % Bold On
        , {"@-",   "\u001b[0m"}  % Bold Off/Reset
            % Icons
        , {"@bp:", "\u2022"}     % Bullet Point
        , {"@tk:", "\u2713"}     % a TicK symbol
        , {"@cr:", "\u2717"}     % a CRoss symbol
        , {"@:)",  "\U0001f600"} % smiling face
        , {"@:(",  "\U0001F641"} % frowning face
        , {"@:|",  "\U0001F610"} % MEH!
        ], In, Out
    ).

%----------------------------------------------------------------------------%

    % A boring black and white version, but sometimes this is
    % required e.b. Emacs buffers in command mode.
    %
:- func monokey(string::in) = (string::out) is det.

monokey(In) = Out :-
    P = (pred({Key,Val}::in, S0::in, S::out) is det :-
        string.replace_all(S0,  Key, Val, S)
    ),
    list.foldl(P,
        [ {"@bl:", ""}          % BLack
        , {"@rd:", ""}          % ReD
        , {"@gr:", ""}          % GReen
        , {"@yl:", ""}          % YeLlow
        , {"@bu:", ""}          % BlUe
        , {"@mg:", ""}          % MaGenta
        , {"@cy:", ""}          % CYan
        , {"@wh:", ""}          % WHite
        , {"@+",   ""}          % Bold On
        , {"@-",   "\u001b[0m"} % Bold Off/Reset
            % Icons
        , {"@bp:", "*"}         % bullet point
        , {"@tk:", "/"}         % a TicK symbol
        , {"@cr:", "x"}         % a CRoss symbol
        , {"@:)",  ":)"}        % smiling face
        , {"@:(",  ":("}        % frowning face
        , {"@:|",  ":|"}        % MEH!
        ], In, Out
    ).

%----------------------------------------------------------------------------%
% MAIN -- for development.
% Uncomment this and the interface declaration for local dev:
%     $ mmc --make aio

% main(!IO) :-
%     aio.format("@bp: @tk: @cr: @:) @:| @:( \n", [], !IO),
%     enable_colours(no, !IO),
%     aio.format("@bp: @tk: @cr: @:) @:| @:( \n", [], !IO).


%----------------------------------------------------------------------------%
:- end_module aio.
%----------------------------------------------------------------------------%

