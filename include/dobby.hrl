% an identifier
-type dby_identifier() :: binary().

% identifier type
%   system - used internally by dobby
%   user - user data
-type identifier_type() :: user | system.

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
-type publish_option() :: 'persistent' | 'message' | identifier_type().

% error reasons
-type reason() :: term().

% search function
-type search_fun() :: fun((dby_identifier(),
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
-type loop_detection() :: none | identifier | link.
-type search_options() :: breadth |
                          depth |
                          {max_depth, non_neg_integer()} |
                          {loop, loop_detection()}.

% subscribe options
-type subscribe_options() :: search_options() | {delta, delta_fun()} | persistent | message | {delivery, delivery_fun()}.
