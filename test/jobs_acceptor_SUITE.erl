-module(jobs_acceptor_SUITE).

-include_lib("common_test/include/ct.hrl").

-define(TIMEOUT, 5000).

%% common_test api

-export([all/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

%% test cases

-export([ask/1]).

%% common_test api

all() ->
    [ask].

suite() ->
    [{timetrap, {seconds, 15}}].

init_per_suite(Config) ->
    {ok, Started} = application:ensure_all_started(jobs_acceptor),
    [{started, Started} | Config].

end_per_suite(Config) ->
    Started = ?config(started, Config),
    _ = [application:stop(App) || App <- Started],
    ok.

init_per_testcase(TestCase, Config) ->
    ok = jobs:add_queue(TestCase, [{standard_rate,1}]),
    Opts = [{active, false}, {packet, 4}],
    {ok, LSock} = gen_tcp:listen(0, Opts),
    {ok, Port} = inet:port(LSock),
    {ok, Pool} = jobs_acceptor_test:start_link(TestCase),
    {ok, Ref} = acceptor_pool:accept_socket(Pool, LSock, 1),
    Connect = fun() -> gen_tcp:connect("localhost", Port, Opts, ?TIMEOUT) end,
    [{connect, Connect}, {pool, Pool}, {ref, Ref}, {socket, LSock} | Config].

end_per_testcase(_TestCase, _Config) ->
    ok.

%% test cases

ask(Config) ->
    Connect = ?config(connect, Config),

    {ok, ClientA} = Connect(),
    ok = gen_tcp:send(ClientA, "hello"),
    {ok, "hello"} = gen_tcp:recv(ClientA, 0, ?TIMEOUT),
    ok = gen_tcp:close(ClientA),

    ok.
