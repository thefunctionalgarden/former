-module(former_test).

-export([start/0]).
-export([concat_the_params/3]).
-export([map_k_v/2]).
-export([list_the_params/2]).



start() ->
    TestForm1 = #{submit => #{module => former_test, function => concat_the_params}},
    TestForm2 = #{submit => #{module => former_test, function => map_k_v}},
    TestForm3 = #{submit => #{module => former_test, function => list_the_params}},
    former:start([TestForm1, TestForm2, TestForm3], 10100),
    ok.

concat_the_params(P1, P2, P3) ->
    io:format("~p:~p - ############ TEST Params:~p~n", [?MODULE, ?LINE, [P1, P2, P3]]),
    <<P1/bitstring, P2/bitstring, P3/bitstring>>.

map_k_v(P1, P2) ->
    io:format("~p:~p - ############ TEST Params:~p~n", [?MODULE, ?LINE, [P1, P2]]),
    #{P1 => P2}.

list_the_params(P1, P2) ->
    io:format("~p:~p - ############ TEST Params:~p~n", [?MODULE, ?LINE, [P1, P2]]),
    [P1, P2].

% -- -- -- -- -- -- -- -- -- -- --
