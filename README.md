# TodoList
Multi-user todolist app using Cowboy + Knockout.js with websocket communication!

#Install & Test
<p><code>git clone git@github.com:darkua/todolist.git</code></p>
<p><code>cd todolist</code></p>
<p><code>./rebar get-deps</code></p>
<p><code>./rebar compile</code></p>
<p><code>./rebar compile eunit doc skip_deps=true</code></p>

With this you will compile the app, generate the doc, and run the tests.
Now if you have erlang R15 installed just run the script ./start for the dev environment
<code>./start.sh</code>

And its up and running, go to your browser and open 2 or more tabs to see the websocket magic :)
<a href="http://localhost:8082">http://localhost:8082</a>

If need to change the port you can do it in the include file.

#Release
To deploy the app just run
<code>make release</code>
and start the app
<code>./rel/todolist/bin/todolist start</code>

#Demo	
<p>A dummy demo <a href="http://todolist.sergioveiga.com:8082">here</a></p>
