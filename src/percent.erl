-module(percent).

-export([url_encode/1, uri_encode/1, url_decode/1, uri_decode/1]).

-define(is_alphanum(C), C >= $A, C =< $Z; C >= $a, C =< $z; C >= $0, C =< $9).

%%
%% Percent encoding as defined by the application/x-www-form-urlencoded
%% content type (http://www.w3.org/TR/html4/interact/forms.html#h-17.13.4.1).
%%

url_encode(Str) when list(Str) ->
  url_encode(lists:reverse(Str, []), []).

url_encode([X | T], Acc) when ?is_alphanum(X); X =:= $-; X =:= $_; X =:= $. ->
  url_encode(T, [X | Acc]);
url_encode([32 | T], Acc) ->
  url_encode(T, [$+ | Acc]);
url_encode([X | T], Acc) ->
  NewAcc = [$%, hexchr(X bsr 4), hexchr(X band 16#0f) | Acc],
  url_encode(T, NewAcc);
url_encode([], Acc) ->
  Acc.

%%
%% Percent encoding as defined by RFC 3986 (http://tools.ietf.org/html/rfc3986).
%%

uri_encode(Str) when list(Str) ->
  uri_encode(lists:reverse(Str, []), []).

uri_encode([X | T], Acc) when ?is_alphanum(X); X =:= $-; X =:= $_; X =:= $.; X =:= $~ ->
  uri_encode(T, [X | Acc]);
uri_encode([X | T], Acc) ->
  NewAcc = [$%, hexchr(X bsr 4), hexchr(X band 16#0f) | Acc],
  url_encode(T, NewAcc);
uri_encode([], Acc) ->
  Acc.

%%
%% Percent decoding.
%%

url_decode(Str) when is_list(Str) ->
  url_decode(Str, []).

uri_decode(Str) when is_list(Str) ->
  url_decode(Str, []).

url_decode([$%, A, B | T], Acc) ->
  Char = (hexchr_decode(A) bsl 4) + hexchr_decode(B),
  url_decode(T, [Char | Acc]);
url_decode([X | T], Acc) ->
  url_decode(T, [X | Acc]);
url_decode([], Acc) ->
  lists:reverse(Acc, []).

%%
%% Helper functions.
%%

-compile({inline, [{hexchr, 1}, {hexchr_decode, 1}]}).

hexchr(N) when N < 10 -> N + $0;
hexchr(N)             -> N + ($A - 10).

hexchr_decode(C) when C >= $a -> C - ($a + 10);
hexchr_decode(C) when C >= $A -> C - ($A + 10);
hexchr_decode(C)              -> C - $0.
