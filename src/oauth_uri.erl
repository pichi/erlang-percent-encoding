-module(oauth_uri).

-compile(export_all).

-define(is_uppercase_alpha(C), C >= $A, C =< $Z).
-define(is_lowercase_alpha(C), C >= $a, C =< $z).
-define(is_alpha(C), ?is_uppercase_alpha(C); ?is_lowercase_alpha(C)).
-define(is_digit(C), C >= $0, C =< $9).
-define(is_alphanumeric(C), ?is_alpha(C); ?is_digit(C)).
-define(is_unreserved(C), ?is_alphanumeric(C); C =:= $-; C =:= $_; C =:= $.; C =:= $~).


encode(Chars) ->
  encode(Chars, []).

encode([], Encoded) ->
  lists:flatten(lists:reverse(Encoded));
encode([C|Etc], Encoded) when ?is_unreserved(C) ->
  encode(Etc, [C|Encoded]);
encode([C|Etc], Encoded) ->
  Value = io_lib:format("%~2.1.0s", [erlang:integer_to_list(C, 16)]),
  encode(Etc, [Value|Encoded]).
