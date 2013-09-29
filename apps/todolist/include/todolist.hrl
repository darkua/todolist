-define(WS_PORT,8082). %%cowboy port
-record(todolist_task,{id,list_id,text,is_done=false}).
-record(todolist,{id,name,tasks=[]}).

