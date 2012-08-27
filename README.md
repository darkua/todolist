# TodoList
Multi-user todolist app using Cowboy + Knockout.js

#Install & Test
```git clone git@github.com:darkua/todolist.git

cd todolist

rebar get-deps

rebar compile

rebar compile eunit doc skip_deps=true```

With this you will compile the app, generate the doc, and run the tests.
Now if you have erlang R15 installed just run the script ./start for the dev environment
./start.sh

And its up and running, go to your browser and open 2 or more tabs to see the multi-user magic
http://localhost:8080

#Release
To deploy the app just run
make release
and start the app
./rel/todolist/bin/todolist start