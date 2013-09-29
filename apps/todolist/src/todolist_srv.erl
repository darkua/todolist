%%%-------------------------------------------------------------------
%%% File:      todolist_srv.erl
%%% @author    Sergio Veiga <> []
%%% @copyright 2012 Sergio Veiga
%%% @doc
%%%
%%% @end
%%%
%%% @since 2012-08-22 by Sergio Veiga
%%%-------------------------------------------------------------------
-module(todolist_srv).
-author('').

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  create_todolist/1,
  view_todolist_all/0,
  view_todolist/1,
  delete_todolist/1,
  todolist_add_task/3,
  todolist_delete_task/2,
  todolist_set_status_task/3
  ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-include("todolist.hrl").
-record(state, {todolists}).
%%====================================================================
%% API
%%====================================================================


%%--------------------------------------------------------------------
%% @spec create_todolist(Name::string()) -> {ok,record(todolist,{})}
%% @doc Creates a new todolist
%% @end
%%--------------------------------------------------------------------
create_todolist(ListName)->
  gen_server:call(?SERVER,{create_todolist,ListName}).


%%--------------------------------------------------------------------
%% @spec view_todolist_all() -> {ok,[record(todolist,{})]}
%% @doc Retrieves all todolists
%% @end
%%--------------------------------------------------------------------
view_todolist_all()->
  gen_server:call(?SERVER,view_todolist_all).

%%--------------------------------------------------------------------
%% @spec view_todolist(Id::number()) -> {ok,record(todolist,{})} | {error,Message}
%% @doc Retrieves a specific list if it exists
%% @end
%%--------------------------------------------------------------------
view_todolist(Id)->
  gen_server:call(?SERVER,{view_todolist,Id}).


%%--------------------------------------------------------------------
%% @spec delete_todolist(Id::number()) -> ok
%% @doc Deletes a specific list
%% @end
%%--------------------------------------------------------------------
delete_todolist(Id)->
  gen_server:call(?SERVER,{delete_todolist,Id}).


%%--------------------------------------------------------------------
%% @spec todolist_add_task(ListId::number(), TaskText::string(), TaskStatus::string()) ->
%%                                        {ok,record(todolist_task,{})} | {error,Msg}
%% @doc adds a task to a list, if the list does not exist return error
%% @end
%%--------------------------------------------------------------------
todolist_add_task(ListId,TaskText,TaskStatus)->
  gen_server:call(?SERVER,{todolist_add_task,ListId,TaskText,TaskStatus}).


%%--------------------------------------------------------------------
%% @spec todolist_delete_task(ListId::number(), TaskId::number()) -> {ok,record(todolist_task,{})} | {error,Msg}
%% @doc deletes a task from the list if the list does not exist return error
%% @end
%%--------------------------------------------------------------------
todolist_delete_task(ListId,TaskId)->
  gen_server:call(?SERVER,{todolist_delete_task,ListId,TaskId}).


%%--------------------------------------------------------------------
%% @spec todolist_set_status_task(ListId::number(), TaskId::number(), Status::atom()) -> {ok,record(todolist_task,{})} | {error,Msg}
%% @doc Sets a task as done or not done, if the list does not exist return error
%% @end
%%--------------------------------------------------------------------
todolist_set_status_task(ListId,TaskId,Status)->
  gen_server:call(?SERVER,{todolist_set_status_task,ListId,TaskId,Status}).


%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @doc Initiates the server
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Dispatch = [
  		{'_', [
  		        {[],cowboy_http_static, [
  		          {directory, {priv_dir, todolist, [<<"html">>]}},
  		          {file, <<"index.html">>},
  		          {mimetypes, [{<<".html">>, [<<"text/html">>]}]}
  		        ]},
              {[<<"static">>, '...'], cowboy_http_static, [
                {directory, {priv_dir, todolist, []} },
                {mimetypes, [
                  {<<".css">>, [<<"text/css">>]},
                  {<<".js">>, [<<"application/javascript">>]}]
                }
              ]},
              {[<<"stream">>],todolist_websocket, []},
              {'_', default_handler, []}
  		]}
  	],
  	cowboy:start_listener(my_http_listener, 100,
  		cowboy_tcp_transport, [{port, ?WS_PORT}],
  		cowboy_http_protocol, [{dispatch, Dispatch}]
  	),
    {ok, #state{todolists=[]}}.

%%--------------------------------------------------------------------
%% @spec
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call({create_todolist,ListName}, _From, State) ->
    List = #todolist{id=generate_unique_id(), name=ListName, tasks=[]},
    {reply, {ok,List}, State#state{todolists=[List | State#state.todolists ]}};

handle_call(view_todolist_all, _From, State) ->
    {reply, {ok,State#state.todolists}, State};

handle_call({view_todolist,ListId}, _From, State) ->
    io:format("LIST ID~p~n",[ListId]),
    Reply = case lists:keyfind(ListId,#todolist.id,State#state.todolists) of
      false->
        {error,<<"sorry, list seems to not exist!">>};
      List->
        {ok,List}
    end,
    {reply, Reply, State};

handle_call({delete_todolist,ListId}, _From, State) ->
    case lists:keyfind(ListId,#todolist.id,State#state.todolists) of
      false->
        {reply, {error,<<"sorry, list seems to not exist!">>}, State};
      List->
        {reply, {ok,List}, State#state{todolists=lists:delete(List,State#state.todolists)}}
    end;

handle_call({todolist_add_task,ListId,TaskText,Status}, _From, State) ->
    case lists:keyfind(ListId,#todolist.id,State#state.todolists) of
      false->
        {reply, {error,<<"sorry, list seems to not exist!">>}, State};
      List->
        NewTask = #todolist_task{id=generate_unique_id(),list_id=ListId, text=TaskText, is_done=Status},
        NewList = List#todolist{tasks=List#todolist.tasks++[NewTask]},
        {reply, {ok, NewTask}, State#state{todolists=lists:keyreplace(ListId,#todolist.id,State#state.todolists,NewList)}}
    end;

handle_call({todolist_delete_task,ListId,TaskId}, _From, State) ->
    case lists:keyfind(ListId,#todolist.id,State#state.todolists) of
      false->
        {reply, {error,<<"sorry, list seems to not exist!">>}, State};
      List->
        NewList = List#todolist{tasks=lists:keydelete(TaskId,#todolist_task.id,List#todolist.tasks)},
        {reply, {ok,#todolist_task{id=TaskId,list_id=ListId}}, State#state{todolists=lists:keyreplace(ListId,#todolist.id,State#state.todolists,NewList)}}
    end;

handle_call({todolist_set_status_task,ListId,TaskId,Status}, _From, State) ->
    case lists:keyfind(ListId,#todolist.id,State#state.todolists) of
      false->
        {reply, {error,<<"sorry, list seems to not exist!">>}, State};
      List->
        case lists:keyfind(TaskId,#todolist_task.id, List#todolist.tasks) of
          false->
            {reply, {error,<<"sorry, task seems to not exist!">>}, State};
          Task->
            NewTask = Task#todolist_task{is_done=Status},
            NewList = List#todolist{tasks=lists:keyreplace(TaskId,#todolist_task.id,List#todolist.tasks,NewTask)},
            {reply, {ok, NewTask}, State#state{todolists=lists:keyreplace(ListId,#todolist.id,State#state.todolists,NewList)}}
        end
    end;


handle_call(_Msg, _From, State) ->
    {reply, State, State}.
%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% A simple solution for unique objects based on time, not reliable for destribution,
%% just use it here for simplicity and complience with DOM elements
generate_unique_id()->
  {A,B,C} = now(),
  erlang:list_to_binary(integer_to_list(A)++integer_to_list(B)++integer_to_list(C)).
