% an identifier
-type dby_identifier() :: binary().

% system type
% subscription
-type system_type() :: subscription.

% Identifier type can be a system type (internal usage) or user (user data)
-type identifier_type() :: system | user.

% system identifiers and links have system_metadata, not jsonable()
-type system_metadata() :: #{system => system_type(),
                             atom() => term()}.

% JSONable datatypes for metadata
-type jsonable() :: integer() |
                    float() |
                    list(jsonable()) |
                    binary() |
                    #{binary() => jsonable()}.

% metadata as an argument to functions
-type metadata() :: jsonable() |
                    fun((jsonable()) -> jsonable()) |
                    nochange |
                    delete.

% identifier as an argument to functions
-type dby_endpoint() :: dby_identifier() | {dby_identifier(), metadata()}.

% link as an argument to functions
-type link() :: {dby_endpoint(), dby_endpoint(), metadata()}.

% publish options
-type publish_type() :: 'persistent' | 'message'.
-type publish_option() :: publish_type().

% error reasons
-type reason() :: term().

% search function
-type search_fun() :: fun((dby_identifier(),
                           IdMetadata :: jsonable() | system_metadata(),
                           Path :: [{dby_identifier(), jsonable() | system_metadata(), jsonable() | system_metadata()}],
                           Acc0 :: term()) ->
                            {search_control(), Acc1 :: term()} |
                            {search_control(), search_fun(), Acc1 :: term()}).

% delta function
-type delta_fun() :: fun((term(), term()) -> {delta, term()} | stop | nodelta).

% delivery function
-type delivery_fun() :: fun((term()) -> ok | stop).

% search control
-type search_control() :: continue | skip | stop.

% search options
-type search_algorithm() :: breadth | depth.
-type loop_detection() :: none | identifier | link.
-type search_options() :: search_algorithm() |
                          {max_depth, non_neg_integer()} |
                          {loop, loop_detection()}.

% subscription id
-type subscription_id() :: binary().

% subscribe options
-type subscribe_options() :: search_options() | {delta, delta_fun()} | persistent | message | {delivery, delivery_fun()}.
