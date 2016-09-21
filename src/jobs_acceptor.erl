%%-------------------------------------------------------------------
%%
%% Copyright (c) 2016, James Fish <james@fishcakez.com>
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License. You may obtain
%% a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%%-------------------------------------------------------------------
%% @doc This module provides `jobs' rate limiting of an `acceptor' in an
%% `acceptor_pool'. It is an `acceptor' with arguments: `{Queue, Mod, Arg}',
%% where `Queue' is the `jobs' queue. `Mod' and `Arg' is the `acceptor' callback
%% module and its argument to be regulated. The job lasts for the life time of
%% the `acceptor' process to allow `jobs' regulators to limit concurrency of
%% open connections.
-module(jobs_acceptor).

-behaviour(acceptor).

%% acceptor api

-export([acceptor_init/3]).
-export([acceptor_continue/3]).
-export([acceptor_terminate/2]).

%% acceptor api

acceptor_init(SockName, Sock, {Queue, Mod, Args}) ->
    case jobs:ask(Queue) of
        {ok, Ref}  -> init(SockName, Sock, Ref, Mod, Args);
        {error, _} -> ignore
    end.

acceptor_continue(PeerName, Sock, {_Ref, Mod, State}) ->
    Mod:acceptor_continue(PeerName, Sock, State).

acceptor_terminate(Reason, {Ref, Mod, State}) ->
    try
        jobs:done(Ref)
    after
        Mod:acceptor_terminate(Reason, State)
    end.

%% internal

init(SockName, Sock, Ref, Mod, Args) ->
    try Mod:acceptor_init(SockName, Sock, Args) of
        Result       -> handle_init(Result, Ref, Mod)
    catch
        throw:Result -> handle_init(Result, Ref, Mod)
    end.

handle_init({ok, State}, Ref, Mod)          -> {ok, {Ref, Mod, State}};
handle_init({ok, State, Timeout}, Ref, Mod) -> {ok, {Ref, Mod, State}, Timeout};
handle_init(Other, _, _)                    -> Other.
