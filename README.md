# TodoList
Multi-user todolist app using Cowboy + Knockout.js

#Install & Test
<code>git clone git@github.com:darkua/todolist.git</code>
<code>cd todolist</code>
<code>rebar get-deps</code>
<code>rebar compile</code>
<code>rebar compile eunit doc skip_deps=true</code>

With this you will compile the app, generate the doc, and run the tests.
Now if you have erlang R15 installed just run the script ./start for the dev environment
<code>./start.sh</code>

And its up and running, go to your browser and open 2 or more tabs to see the multi-user magic
<a href="http://localhost:8080">http://localhost:8080"</a>

#Release
To deploy the app just run
<code>make release</code>
and start the app
<code>./rel/todolist/bin/todolist start</code>

#Missing
More tests and extension documentation would be good :)