TodoList = function(DataObj){
  this.id= DataObj.id;
  this.name = ko.observable(DataObj.name);
  this.link = ko.computed(function() {
      return '#list/' + this.id ;
  }, this);
  this.newTaskText = ko.observable();
  this.tasks = ko.observableArray([]);
  DataObj.tasks.forEach(function(taskObj){
    this.tasks.push(new TodoListTask(taskObj));
  },this);
  createTask = function (){
    TodoListApp.ws.send('todolist_add_task', {listId:this.id, text:this.newTaskText(), isDone:false});
    this.newTaskText("");
  };
  deleteList = function(){
    TodoListApp.ws.send('delete_todolist', {id:this.id});
  }
};
TodoList.prototype.findTask = function(id){
  var tasks = this.tasks();
  for(i=0;i<tasks.length;i++)
      if (tasks[i].id == id)
        return tasks[i];
};
TodoList.prototype.removeTask = function(id){
  var task = this.findTask(id);
  this.tasks.remove(task);
}
TodoList.prototype.setTaskStatus = function(id,isDone){
  var task = this.findTask(id);
  task.isDone(isDone);
}

TodoListTask = function(TaskObj){
  this.id= TaskObj.id;
  this.listId = TaskObj.listId;
  this.text = ko.observable(TaskObj.text);
  this.isDone = ko.observable(TaskObj.isDone);
  this.setStatus = function() {
      TodoListApp.ws.send('todolist_set_status_task', {listId:this.listId, id:this.id, isDone:!this.isDone()});
  };
  this.deleteTask = function(){
    TodoListApp.ws.send('todolist_delete_task', {id:this.id, listId:this.listId});
  }

};

TodoListApp = {
  ws: null,
  connect : function(channel){
    this.ws = $.websocket("ws://"+document.location.host+"/"+channel,{
      open : function (){TodoListApp.viewModel.addAlert("info","Websocket Live and Kicking!!!"); TodoListApp.routes.run() },
      close : function (){TodoListApp.viewModel.addAlert("error","Oopps! We lost connection to the matrix...") },
      events: {
                  create_todolist: TodoListApp.callbacks.create_todolist
                , delete_todolist: TodoListApp.callbacks.delete_todolist
                , view_todolist: TodoListApp.callbacks.view_todolist
                , view_todolist_all: TodoListApp.callbacks.view_todolist_all
                , todolist_add_task : TodoListApp.callbacks.todolist_add_task
                , todolist_delete_task : TodoListApp.callbacks.todolist_delete_task
                , todolist_set_status_task : TodoListApp.callbacks.todolist_set_status_task
              }
    });
  },
  check_error: function(package){
    switch(package.status){
      case "error": TodoListApp.viewModel.addAlert("error",package.data); return true;
      case "ok": return false;
    }
  },
  callbacks : {
    create_todolist : function(e){
      if(!TodoListApp.check_error(e.package))
        TodoListApp.viewModel.todoLists.unshift(new TodoList(e.package.data));
        $($('.new-task input')[0]).focus();
    },
    delete_todolist : function(e){
      if(!TodoListApp.check_error(e.package)){
        var list = new TodoList(e.package.data)
        TodoListApp.viewModel.todoLists.pop(list);
        TodoListApp.viewModel.addAlert('success',"Youpi! The List '"+list.name()+"' is gone forever!!!");
      }
    },
    view_todolist_all : function(e){
      if(!TodoListApp.check_error(e.package)){
        TodoListApp.viewModel.activeList(null);
        TodoListApp.viewModel.todoLists([]);
        e.package.data.forEach(function(listObj){
          TodoListApp.viewModel.todoLists.push(new TodoList(listObj));
        });
      }
    },
    view_todolist : function(e){
      if(!TodoListApp.check_error(e.package)){
        var list = new TodoList(e.package.data)
        TodoListApp.viewModel.activeList(list.id);
        TodoListApp.viewModel.todoLists([]);
        TodoListApp.viewModel.todoLists.push(list);
      }
    },
    todolist_add_task : function(e){
      if(!TodoListApp.check_error(e.package)){
        var task = new TodoListTask(e.package.data);
        var list = TodoListApp.viewModel.findList(task.listId);
        list.tasks.push(task);
      }
    },
    todolist_delete_task : function(e){
      if(!TodoListApp.check_error(e.package)){
        var task = new TodoListTask(e.package.data);
        var list = TodoListApp.viewModel.findList(task.listId);
        list.removeTask(task.id);
      }
    },
    todolist_set_status_task : function(e){
      if(!TodoListApp.check_error(e.package)){
        var task = new TodoListTask(e.package.data);
        var list = TodoListApp.viewModel.findList(task.listId);
        list.setTaskStatus(task.id,task.isDone())
      }
    }

  },
  viewModel : {
    alerts : ko.observableArray([]),
    todoLists : ko.observableArray([]),
    newListName : ko.observable(),
    activeList : ko.observable(),
    findList : function(id){
      var lists = this.todoLists();
      for(i=0;i<lists.length;i++)
          if (lists[i].id == id)
            return lists[i];
    },
    createList : function(){
      TodoListApp.ws.send('create_todolist', {name:this.newListName()});
      this.newListName("");
    },
    getAll : function(){
      TodoListApp.ws.send('view_todolist_all',{});
    },
    getList : function(listId){
      TodoListApp.ws.send('view_todolist', {id:listId});
    },
    addAlert : function(type,message){
      this.alerts.push({type:type,message:message});
    },
    removeAlert: function(){
      console.log(this,alert);
      TodoListApp.viewModel.alerts.pop(this);
    }
  },
  routes : Sammy(function() {

            this._checkFormSubmission = function(form){
              return false;
            }
            this.get('#lists', function() {
              TodoListApp.viewModel.getAll();
            });
            this.get('#list/:listId', function() {
              TodoListApp.viewModel.getList(this.params.listId);
            });
            this.get('', function() {this.app.runRoute('get', '#lists') });
      })
};
ko.applyBindings(TodoListApp.viewModel);

TodoListApp.connect("stream");