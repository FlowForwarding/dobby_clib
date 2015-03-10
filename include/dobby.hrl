% an identifier
-type dby_identifier() :: binary().

% publisher id
-type publisher_id() :: binary().

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

% metadata as an argument to publish
-type metadata_value() :: jsonable() | delete.
-type metadata_proplist() :: [{binary(), metadata_value()}].
-type metadata_arg() :: metadata_proplist() |
                        delete |
                        nochange |
                        fun((metadata_proplist()) -> metadata_proplist()).

% metadata delivered to search
-type metadata_info() :: #{binary() => #{value => jsonable(),
                                         publisher_id => publisher_id(),
                                         datetime => binary()}
                        }.

% identifier as an argument to functions
-type dby_endpoint() :: dby_identifier() |
                        {dby_identifier(), metadata_arg()}.

% link as an argument to functions
-type link() :: {dby_endpoint(), dby_endpoint(), metadata_arg()}.

% publish options
-type publish_type() :: 'persistent' | 'message'.
-type publish_option() :: publish_type().

% error reasons
-type reason() :: term().

% search function
-type all_metadata() :: metadata_info() | system_metadata().
-type search_path() :: [{dby_identifier(), all_metadata(), all_metadata()}].
-type search_fun() :: fun((dby_identifier(),
                           IdMetadata :: all_metadata(),
                           Path :: search_path(),
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
                          identifier_type() |
                          {max_depth, non_neg_integer()} |
                          {loop, loop_detection()}.

% subscription id
-type subscription_id() :: binary().

% subscribe options
-type subscribe_options() :: search_options() | {delta, delta_fun()} | persistent | message | {delivery, delivery_fun()}.
