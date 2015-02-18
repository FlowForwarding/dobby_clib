% transaction
-type transaction() :: term().

% an identifier
-type idenfier() :: binary().

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
-type endpoint() :: identifier() | {identifier(), metadata()}.

% link as an argument to functions
-type link() :: {endpoint(), endpoint(), metadata()}.

% publish options
-type publish_option() :: 'persistent' | 'message'.

% error reasons
-type reason() :: term().

% subscription id
-type subscription() :: term().

% search function
-type search_fun() :: fun((identifier(),
                           IdMetadata :: jsonable(),
                           LinkMetadata :: jsonable(),
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
-type search_options() :: breadth | depth | {max_depth, non_neg_integer()}.

% subscribe options
-type subscribe_options() :: search_options() | {delta, delta_fun()} | persistent | message | {delivery, delivery_fun()}.
