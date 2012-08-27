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

And its up and running, go to your browser and open 2 or more tabs to see the multi-user magic
<a href="http://localhost:8080">http://localhost:8080</a>

#Release
To deploy the app just run
<code>make release</code>
and start the app
<code>./rel/todolist/bin/todolist start</code>

#Missing
<p>More tests and documentation would be good</p>
<p>Input form validation for the ones that want to create void! </p>