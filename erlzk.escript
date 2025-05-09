#!/usr/bin/env escript
%%! -pa _build/default/lib/erlzk/ebin -pa _build/default/lib/erlzk/include
%% -*- erlang -*-

% -include("erlzk.hrl").

-define(DEFAULT_ZK_PORT, 2181).
-define(DEFAULT_ZK_HOSTS, [
]).

main(Args) ->
    application:ensure_all_started(erlzk),
    case Args of
        [NodesString] ->
            Nodes = string:tokens(NodesString, ","),
            start_zookeeper(Nodes);
        [] ->
            Nodes = ?DEFAULT_ZK_HOSTS,
            start_zookeeper(Nodes);
        _ ->
            io:format("Usage: erlzk.escript \"host1:port1,host2:port2,...\"\n")
    end.

start_zookeeper(Nodes) ->
    ParsedHosts = lists:map(fun (Host) ->
        case string:tokens(Host, ":") of
            [HostName] -> {HostName, ?DEFAULT_ZK_PORT};
            [HostName, PortString] ->
                {HostName, list_to_integer(PortString)}
        end
    end, Nodes),
%     io:format("~p", [erlzk:module_info()]),
    case erlzk:connect(ParsedHosts, 3000, [{monitor, self()}]) of
        {ok, Pid} ->
            io:format("Connected to ZooKeeper nodes: ~p~n", [Nodes]),
            erlzk:exists(Pid, "/", self()),
            wait_forever();
       {error, Reason} ->
            io:format("Failed to connect to ZooKeeper nodes: ~p, reason: ~p~n", [Nodes, Reason]),
            exit(-1)
    end.


wait_forever() ->
    receive
        {Event, Path} ->
            io:format("Received event: ~p on path: ~p~n", [Event, Path]),
            wait_forever()
    end.