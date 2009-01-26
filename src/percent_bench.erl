-module(percent_bench).

-compile(export_all).


test() ->
  test(ibrowse_lib, url_encode, percent_unit:form_urlencoded_test_cases()),
  test(mochiweb_util, quote_plus, percent_unit:form_urlencoded_test_cases()),
  test(oauth_uri, encode, percent_unit:rfc3986_test_cases()),
  test(yaws_api, url_encode, percent_unit:form_urlencoded_test_cases()),
  test(percent, url_encode, percent_unit:form_urlencoded_test_cases()),
  test(percent, uri_encode, percent_unit:form_urlencoded_test_cases()).

test(M, F, TestCases) ->
  statistics(wall_clock),
  repeat(100000, fun() -> test_run(M, F, TestCases) end),
  {_, Time} = statistics(wall_clock),
  io:format("~p:~p - ~p milliseconds~n", [M, F, Time]).

repeat(0, _) -> ok;
repeat(N, F) -> F(), repeat(N - 1, F).

test_run(M, F, TestCases) ->
  lists:foreach(fun({_, In}) -> apply(M, F, [In]) end, TestCases).
