%%% File    : ibrowse_lib.erl
%%% Author  : Chandrashekhar Mullaparthi <chandrashekhar.mullaparthi@t-mobile.co.uk>

-module(ibrowse_lib).

-compile(export_all).

%% @doc URL-encodes a string based on RFC 1738. Returns a flat list.
%% @spec url_encode(Str) -> UrlEncodedStr
%% Str = string()
%% UrlEncodedStr = string()
url_encode(Str) when list(Str) ->
    url_encode_char(lists:reverse(Str), []).

url_encode_char([X | T], Acc) when X >= $0, X =< $9 ->
    url_encode_char(T, [X | Acc]);
url_encode_char([X | T], Acc) when X >= $a, X =< $z ->
    url_encode_char(T, [X | Acc]);
url_encode_char([X | T], Acc) when X >= $A, X =< $Z ->
    url_encode_char(T, [X | Acc]);
url_encode_char([X | T], Acc) when X == $-; X == $_; X == $. ->
    url_encode_char(T, [X | Acc]);
url_encode_char([32 | T], Acc) ->
    url_encode_char(T, [$+ | Acc]);
url_encode_char([X | T], Acc) ->
    url_encode_char(T, [$%, d2h(X bsr 4), d2h(X band 16#0f) | Acc]);
url_encode_char([], Acc) ->
    Acc.

d2h(N) when N<10 -> N+$0;
d2h(N) -> N+$a-10.
