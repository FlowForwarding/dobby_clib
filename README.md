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
2> dby:publish(...).
```

# Origin of Name
The Infoblox OpenFlow controller projects follow a fabric theme.
The overall project is called LOOM.  Dobby fabric is a type of
fabric.  Coincidentally it is also the name of a character in the
Harry Potter franchise.
