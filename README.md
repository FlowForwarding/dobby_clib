Dobby Client Library
====================
Dobby is a map server modeled to a certain degree after the IF-Map
server.  Unlike the IF-Map server, Dobby is intended to be a more
general purpose graph database that applications can then use for
whatever purpose they require.  Dobby Client Library provides functions
to communicate with the server.

This is an open source project sponsored by Infoblox.

# Requirements
- Erlang R17+
- Dobby server

# Building
```
% rebar get-deps
% rebar compile
```

# Running
```
erl -pa ebin -setcookie dobby -name client@127.0.0.1
...
1> net_adm:ping('dobby@127.0.0.1`).
pong
2> dby:install(dby).
{module, dby}
3> dby:publish(...).
```

# Examples
Prerequisites:
```
Pubid = <<"mypublisherid">>.
'''

Adding an identifier:
```
dby:publish(Pubid, {<<"id1">>, [{<<"key1">>, <<"meta1">>}]}, [persistent]).
'''

Adding a link:
```
dby:publish(Pubid, {<<"id1">>, <<"id2">, [{<<"linkkey1">>, <<"linkmeta1">>}]}, [persistent]).
'''

Adding list of identifiers and links.  This operation is atomic:
```
dby:publish(Pubid, [
<<"A">>, % identifier with no metadata
{<<"B">>, [{<<"bkey1">>, #{<<"hash1">> => <<"vhash1">>}}]}, % map as metadata
{<<"B">>, <<"C">>, [{<<"linkkeyAC1">>, <<"linkmetaAC1">>}]}, % link with metadata
{{<<"D">>, [{<<"dkey1">>, true}]}, <<"C">>, []}, % identifier metadata and link
{<<"F">>, <<"D">>, delete}, % delete link
{<<"G">>, delete} % delete identifier
], [persistent]).
'''

List identifiers some distance from a starting point:
```
dby:identifiers(<<"A">>, 2). % identifiers within two links of <<"A">>
'''

Link metadata of links from a starting point:
```
dby:links(<<"B">>).
'''

# Origin of Name
The Infoblox OpenFlow controller projects follow a fabric theme.
The overall project is called LOOM.  Dobby fabric is a type of
fabric.  Coincidentally it is also the name of a character in the
Harry Potter franchise.
