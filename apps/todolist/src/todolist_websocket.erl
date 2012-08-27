%% Websocket connection that sends periodic stats about the VM
%%
-module(todolist_websocket).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3, websocket_terminate/3,
				 websocket_info/3]).
-include("todolist.hrl").

init({_Any, http}, Req, []) ->
	case cowboy_http_req:header('Upgrade', Req) of
		{undefined, Req2} -> {ok, Req2, undefined};
		{<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
		{<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
	end.

handle(_Req, _State) ->
    exit(websockets_only).

terminate(_Req, _State) ->
    exit(websockets_only).

%%each client subscribes to the pubsubhub to get all updates!
websocket_init(_TransportName, Req, _Opts) ->
    todolist_pubsubhub:register_client(self()),
    {ok, Req, undefined_state}.

%%Receives all messages and delivers them to midleman that decides what happens next!
websocket_handle({text, Msg}, Req, State) ->
  io:format("MSG: ~p~n",[Msg]),
  [{<<"command">>,Command},{<<"data">>,Data}] = jsx:json_to_term(Msg),
  case middleman(Command,Data) of
  {reply,NewCommand,NewData} ->
    Reply = jsx:term_to_json([{command,NewCommand},{package,NewData}]),
	  {reply, {text, Reply}, Req, State, hibernate};
	broadcast->
	  {ok, Req, State}
	end;

websocket_handle(_Any, Req, State) ->
	{ok, Req, State}.

%% BROADCASTING to all clients
websocket_info({todolist,{NewCommand,NewData} }, Req, State) ->
    Reply = jsx:term_to_json([{command,NewCommand},{package,NewData}]),
    {reply, {text, Reply}, Req, State, hibernate};

websocket_info(Any, Req, State) ->
    io:format("Unhandled msg to ~p ~p\n", [?MODULE, Any]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.


%%%% MIDDLEMAN COMUNICATION PROTOCOL %%%%%

middleman(<<"create_todolist">> = Key,Data)->
  Reply = todolist_srv:create_todolist(
    proplists:get_value(<<"name">>,Data)
  ),
  Package = reply_to_json(Reply),
  io:format("K : ~p~n Reply:~p~n , Package: ~p~n",[Key,Reply,Package]),
  todolist_pubsubhub:notify({Key,Package}),
  broadcast;

middleman(<<"view_todolist_all">>=Key,_Data)->
  Reply = todolist_srv:view_todolist_all(),
  Package = reply_to_json(Reply),
  {reply,Key,Package};

middleman(<<"view_todolist">> = Key,Data)->
  Reply = todolist_srv:view_todolist(proplists:get_value(<<"id">>,Data)),
  Package = reply_to_json(Reply),
  io:format("K : ~p~n Reply:~p~n , Package: ~p~n",[Key,Reply,Package]),
  {reply,Key,Package};

middleman(<<"delete_todolist">> = Key,Data)->
  Reply = todolist_srv:delete_todolist(proplists:get_value(<<"id">>,Data)),
  Package = reply_to_json(Reply),
  io:format("K : ~p~n Reply:~p~n , Package: ~p~n",[Key,Reply,Package]),
  todolist_pubsubhub:notify({Key,Package}),
  broadcast;

middleman(<<"todolist_add_task">> = Key,Data)->
  Reply = todolist_srv:todolist_add_task(
      proplists:get_value(<<"listId">>,Data)
    , proplists:get_value(<<"text">>,Data)
    , proplists:get_value(<<"isDone">>,Data)
  ),
  Package = reply_to_json(Reply),
  io:format("K : ~p~n Reply:~p~n , Package: ~p~n",[Key,Reply,Package]),
  todolist_pubsubhub:notify({Key,Package}),
  broadcast;

middleman(<<"todolist_delete_task">> = Key,Data)->
  Reply = todolist_srv:todolist_delete_task(
      proplists:get_value(<<"listId">>,Data)
    , proplists:get_value(<<"id">>,Data)
  ),
  Package = reply_to_json(Reply),
  io:format("K : ~p~n Reply:~p~n , Package: ~p~n",[Key,Reply,Package]),
  todolist_pubsubhub:notify({Key,Package}),
  broadcast;

middleman(<<"todolist_set_status_task">> = Key ,Data)->
  Reply = todolist_srv:todolist_set_status_task(
      proplists:get_value(<<"listId">>,Data)
    , proplists:get_value(<<"id">>,Data)
    , proplists:get_value(<<"isDone">>,Data)
  ),
  Package = reply_to_json(Reply),
  io:format("K : ~p~n Reply:~p~n , Package: ~p~n",[Key,Reply,Package]),
  todolist_pubsubhub:notify({Key,Package}),
  broadcast;

%% testing
middleman(<<"listen">>,Data)->
  todolist_pubsubhub:notify({<<"listen">>,Data}),
  broadcast.


%% transformation schema
reply_to_json({Status,Data})->
  [{status,Status},{data,transform_data(Data)}].

transform_data(Data) when is_binary(Data)->
  Data;

transform_data(Data) when is_list(Data)->
  [transform_data(List) || List <- Data];


transform_data(Data) when is_record(Data,todolist)->
  [
    {id,Data#todolist.id}
  , {name,Data#todolist.name}
  , {tasks, [ transform_data(Task) || Task <- Data#todolist.tasks ]}
  ];

transform_data(Data) when is_record(Data,todolist_task)->
  [
      {id,Data#todolist_task.id}
    , {listId,Data#todolist_task.list_id}
    , {text,Data#todolist_task.text}
    , {isDone,Data#todolist_task.is_done}
  ].

