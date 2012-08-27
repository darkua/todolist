-module(todolist_srv_test).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("todolist.hrl").

-ifdef(TEST).
  app_start_test() ->
    ok = application:start(cowboy),
    ok = application:start(todolist),
    ?assertNot(undefined == whereis(todolist_sup)).

  todolist_test()->
    {ok,List} = todolist_srv:create_todolist("list1"),
    ?assert("list1" == List#todolist.name).

  view_todolist_test()->
    {ok,List} = todolist_srv:create_todolist("list2"),
    {ok,List1} = todolist_srv:view_todolist(List#todolist.id),
    ?assert(List == List1).

  delete_todolist_test()->
    {ok,List} = todolist_srv:create_todolist("list3"),
    {ok,List1} = todolist_srv:delete_todolist(List#todolist.id),
    ?assert(List == List1).

  todolist_add_task_test()->
    {ok,List} = todolist_srv:create_todolist("list4"),
    {ok,Task} = todolist_srv:todolist_add_task(List#todolist.id,"task1",false),
    {ok,List1} = todolist_srv:view_todolist(List#todolist.id),
    ?assert(true ==lists:member(Task,List1#todolist.tasks)).

  todolist_delete_task_test()->
    {ok,List} = todolist_srv:create_todolist("list5"),
    {ok,Task1} = todolist_srv:todolist_add_task(List#todolist.id,"task1",false),
    {ok,Task2} = todolist_srv:todolist_delete_task(Task1#todolist_task.list_id,Task1#todolist_task.id),
    ?assert(false == lists:keymember(Task2#todolist_task.id,#todolist_task.id,List#todolist.tasks)).

  todolist_set_status_task_test()->
    {ok,List} = todolist_srv:create_todolist("list5"),
    {ok,Task1} = todolist_srv:todolist_add_task(List#todolist.id,"task1",false),
    {ok,Task2} = todolist_srv:todolist_set_status_task(Task1#todolist_task.list_id,Task1#todolist_task.id,true),
    ?assert(true == Task2#todolist_task.is_done).

  app_stop_test()->
    ok = application:stop(todolist).
-endif.