-module(percent_unit).

-compile(export_all).


test() ->
  % These fail both sets of test cases:
  test(yaws_api, url_encode, form_urlencoded_test_cases()),
  test(mochiweb_util, quote_plus, form_urlencoded_test_cases()),
  % These pass:
  test(ibrowse_lib, url_encode, form_urlencoded_test_cases()),
  test(percent, url_encode, form_urlencoded_test_cases()),
  test(oauth_uri, encode, rfc3986_test_cases()),
  test(percent, uri_encode, rfc3986_test_cases()).

test(M, F, TestCases) when is_list(TestCases) ->
  lists:foreach(fun(TestCase) -> test(M, F, TestCase) end, TestCases);
test(M, F, {Out, In}) ->
  case apply(M, F, [In]) of
    Out -> pass;
    Term ->
      case downcased_hex(Out) of
        Term -> pass;
        _ -> io:format("FAIL: ~p:~p(~p) was ~p not ~p~n", [M, F, In, Term, Out])
      end
  end.

test_decode() ->
  lists:foreach(fun({In, Out}) -> Out = percent:uri_decode(In) end, test_cases([])).

downcased_hex(Str) when is_list(Str) ->
  [downcased_hex(Chr) || Chr <- Str];
downcased_hex(Chr) when Chr >= $A, Chr =< $F ->
  Chr + 32;
downcased_hex(Chr) ->
  Chr.

form_urlencoded_test_cases() ->
  test_cases([
    {"%7E-._", [$~|"-._"]},
    {"+", [16#0020]}
  ]).

rfc3986_test_cases() ->
  test_cases([
    {[$~|"-._"], [$~|"-._"]},
    {"%20", [16#0020]}
  ]).

test_cases(Extra) ->
  Extra ++ [ % cf. http://wiki.oauth.net/TestCases
    {"abcABC123", "abcABC123"},
    {"%0D%0A", "\r\n"},
    {"%25", "%"},
    {"%2B", "+"},
    {"%26%3D%2A", "&=*"},
    {"%0A", [16#000A]},
    {"%7F", [16#007F]},
    {"%C2%80", xmerl_ucs:to_utf8(16#0080)},
    {"%E3%80%81", xmerl_ucs:to_utf8(16#3001)}
  ].
