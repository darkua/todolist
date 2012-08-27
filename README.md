# Fraud server
The fraud server does all fraud decisions for sumup.
To do this it keeps lots sum of purchases and a state per merchant.
It uses riak to store its data. Riak is an eventual consistency database.
It uses a project calls statebox to make the development on top of an
eventual consistency database easy.
