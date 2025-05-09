%% 2013-2014 (c) Mega Yu <yuhg2310@gmail.com>
%% 2013-2014 (c) huaban.com <www.huaban.com>
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%    http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(erlzk_conn_sup).
-behaviour(supervisor).

-export([start_conn/1, start_conn/2]).
-export([start_link/0]).
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

start_conn(Args) ->
  start_conn(temporary, Args).

start_conn(Type, Args) ->
  io:format("[~p]STARTING CONNECTION: ~p~n", [Type, Args]),
  supervisor:start_child(?SUPERVISOR, #{
    id => erlzk_conn,
    start => {erlzk_conn, start_link, Args},
    restart => Type,
    shutdown => 5000,
    type => worker,
    modules => [erlzk_conn]
  }).

start_link() ->
  supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

init([]) ->
  {ok, {#{
    strategy => one_for_one,
    intensity => 10,
    period => 5
  }, []}}.
