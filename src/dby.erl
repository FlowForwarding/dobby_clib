%% @doc
%% dobby API
%% @end
-module(dby).

-export([install/1,
         publish/3,
         publish/5,
         search/4,
         subscribe/4,
         unsubscribe/1,
         identifiers/2,
         identifier/1,
         subscriptions/1,
         subscription/1,
         link_metadata/2,
         links/1,
         identifiers/4,
         links/4,
         identifier_detail/4,
         subscriptions_for_identifier/4,
         subscription_detail/4,
         link_search/1
        ]).

-include_lib("dobby_clib/include/dobby.hrl").

% =============================================================================
% API functions
% =============================================================================

%% @doc
%% `install/1' installs a module containing user functions into the
%% server so they can be called properly.
%% @end
-spec install(atom()) -> {module, atom()} | {error, term()}.
install(Module) ->
    case code:get_object_code(Module) of
        error -> error;
        ModuleBinary -> call(install_code, ModuleBinary)
    end.

%% @equiv publish(PublisherId, [{Endpoint1, Endpoint2, LinkMetadata}], Options)
-spec publish(publisher_id(), dby_endpoint(), dby_endpoint(), metadata_proplist(), [publish_option()]) -> ok | {error, reason()}.
publish(PublisherId, Endpoint1, Endpoint2, LinkMetadata, Options) ->
    publish(PublisherId, [{Endpoint1, Endpoint2, LinkMetadata}], Options).

%% @doc
%% `publish/3' adds or modifies identifiers and/or links, or sends a
%% message via the identifiers and/or links. The PublisherId identifies
%% the publisher.  The endpoints of a link are specified using an
%% identifier or a tuple with the identifier and metadata.  If the
%% endpoint is specified as an identifier, the metadata for that
%% identifier is not changed. If the metadata is nochange the metadata
%% for the endpoint is not changed.  If the endpoint is a tuple with
%% the second element delete, the identifier is deleted from Dobby and
%% all links to the identifier are also deleted.  If LinkMetadata is
%% not specified or is nochange the link metadata is not changed. If
%% link is a tuple with third element  delete the link between the two
%% identifiers is deleted.  The identifiers themselves are otherwise
%% not affected. Deleting an identifier deletes all links to the deleted
%% identifier. If publish creates a new identifier or link and no
%% metadata is provided, the metadata is set to null. Metadata may be
%% any Erlang term that can be represented as a JSON. If the metadata
%% key already exists, the metadata is replaced. If the metadata key
%% does not exist the key and value is added. If the value is delete
%% the metadata key is deleted. If the metadata is specified as a
%% function, the function is called with the old metadata and the
%% function is expected to return updated metadata for that link or
%% identifier. The metadata is in the form of a property list. The
%% return of the function is handled as if it was passed in directly
%% as the property list. Fun should be a pure function as it may be
%% called more than once.  The persistent option means all changes are
%% persisted in Dobby.  The changes are made atomically.  That is, all
%% the changes are made together or none are made. The message option
%% means metadata changes are communicated to subscriptions but the
%% changes are not persisted in dobby.  message is the default behavior.
%% You can only message publish to existing endpoints and links.  You
%% cannot use delete with message publish.
%%
%% Returns `badarg' for `message' publish if one of the endppoints or the
%% link between them does not exist.
%%
%% `publish/2' may also be called with an endpoint.  This is a convenience
%% for adding, removing, or modifying a single identifier.
-spec publish(publisher_id(), [dby_endpoint() | link()] | dby_endpoint(), [publish_option()]) -> ok | {error, reason()}.
publish(PublisherId, Endpoint, Options) when is_tuple(Endpoint); is_binary(Endpoint) ->
    publish(PublisherId, [Endpoint], Options);
publish(PublisherId, Data, Options) ->
    call(dby_publish, [PublisherId, Data, Options]).

%% @doc
%% search/4 performs a fold over the graph beginning with the identifier
%% in StartIdentifier. The options breadth and depth control how the
%% graph is traversed.  For breadth, all the links to StartIdentifier
%% are traversed first, followed by all the links of StartIdentifier’s
%% neighbors, etc.  For depth, one link of StartIdentifier is traversed,
%% followed by one link of that neighbor identifier, etc.  If neither
%% is specified, breadth is used.  Acc is the initial accumulator
%% value.  Fun is called for every Identifier traversed by search. It
%% controls the graph traversal and may also transform the result.
%% Identifier is the current identifier. IdMetadataInfo is the metadata
%% info for the identifier. Path is the list of identifiers with their
%% metadatainfo and link metadatainfo that is the path from StartIdentifier
%% to Identifier. Acc0 is the current accumulator. The first identifier
%% in the Path list is immediate neighbor of Identifier that lead to
%% Identifier. Metadatainfo is a map holding the metadata set with
%% publish, the publisher identifier set during publish, and the
%% datetime the metadata value was created or last updated. Fun returns
%% a status that controls the next step of the navigation and the new
%% accumulator.  The possible control values are: continue to continue
%% the search, skip to continue the search but skip navigating to any
%% neighbors of this identifier, stop to stop the search with this
%% identifier.
%% 
%% The option max_depth controls how far to navigate away from the
%% starting identifier.  max_depth of 0 means no navigation is performed.
%% max_depth of one means search only navigates to the immediate
%% neighbors of the starting identifier.  If max_depth is not provided,
%% 0 is used.
%% 
%% The loop option specifies the loop detection algorithm.  none means
%% there is no loop detection and Fun, Fun1 may see the same identifier
%% more than once.  link means that a link is traversed only once, but
%% if there is more than one link to an identifier, Fun, Fun1 may see
%% the same identifier more than once.  identifier means that an
%% identifier is traversed only once, so Fun, Fun1 will never see the
%% same identifier more than once.  If loop is not provided, identifier
%% loop detection is used.
-spec search(Fun :: search_fun(), Acc :: term(), StartIdentifier :: dby_identifier(), [search_options()]) -> term() | {error, reason()}.
search(Fun, Acc, StartIdentifier, Options) ->
    call(dby_search, [Fun, Acc, StartIdentifier, Options]).

%% @doc
%% `subscribe/4' creates a subscription and when successful returns a
%% subscription id. A subscription is a standing search and many of
%% the parameters for subscribe are the same as they are for search.
%% A subscription may be on publishing of persistent data or messages,
%% or both.  The subscription may provide a delta function, `DFun', that
%% computes the delta from previous search `Acc' to the new search `Acc'.
%% This function is only called `LastAcc' and `NewAcc' are different.  `DFun'
%% returns the computed `Delta', 'stop' to delete the subscription and no
%% further processing is performed on this subscription, or `nodelta'
%% to indicate that there was no delta.  If no `DFun' is not provided
%% in the options, Dobby uses `NewAcc' as the delta.  The subscription
%% may provide a delivery function `SFun'.  `SFun' is only called if there
%% is a delta in the subscription’s search result, that is, if `DFun' returns
%% a delta.
%% If `DFun' returns a delta, the `SFun' is called
%% with the delta.  If `DFun' returns nodelta, `SFun' is not called.  If
%% no `DFun' is provided, `SFun' is called with NewAcc.  `SFun' may return
%% `stop' to delete the subscription, otherwise it should return `ok'.  If
%% no `SFun' is provided, no deltas are delivered.
-spec subscribe(Fun :: search_fun(), Acc :: term(), StartIdentifier :: dby_identifier(), [subscribe_options()]) -> {ok, subscription_id()} | {error, reason()}.
subscribe(Fun, Acc, StartIdentifier, Options) ->
    call(dby_subscribe, [Fun, Acc, StartIdentifier, Options]).

%% @doc
%% `unsubscribe/1' deletes a subscription.  Attempts to delete an invalid
%% or already deleted subscription are ignored.
-spec unsubscribe(subscription_id()) -> ok.
unsubscribe(SubscriptionId) ->
    call(dby_unsubscribe, SubscriptionId).

%% @doc
%% List all identifiers starting at `StartIdentifier'
%% to a max depth of `Depth'.
-spec identifiers(dby_identifier(), non_neg_integer()) -> list().
identifiers(StartIdentifier, Depth) ->
    dby:search(fun identifiers/4, [], StartIdentifier, [{max_depth, Depth}]).

%% @doc
%% Identifier metadata for `Identifier'.
-spec identifier(dby_identifier()) -> metadata_info().
identifier(Identifier) ->
    dby:search(fun identifier_detail/4, [], Identifier, [{max_depth, 0}]).

%% @doc
%% List all subscriptions that are triggered by changes to  `Identifier'.
-spec subscriptions(dby_identifier()) -> [dby_identifier()].
subscriptions(Identifier) ->
    dby:search(fun subscriptions_for_identifier/4, [], Identifier, [{max_depth, 1}, system]).

%% @doc
%% Subscription metadata `SubscriptionId'.
-spec subscription(dby_identifier()) -> system_metadata().
subscription(SubscriptionId) ->
    dby:search(fun subscription_detail/4, [], SubscriptionId, [{max_depth, 0}, system]).

%% @doc
%% Links and metadata for links to `Identifier'.
-spec links(dby_identifier()) -> [{dby_identifier(), metadata_info()}].
links(Identifier) ->
    dby:search(fun links/4, [], Identifier, [{max_depth, 1}]).

%% @doc
%% metadata for a link between `Identifier1` and `Identifier2`.
-spec link_metadata(dby_identifier(), dby_identifier()) -> metadata_info().
link_metadata(Identifier1, Identifier2) ->
    SearchFn = link_search(Identifier2),
    dby:search(SearchFn, [], Identifier1, [{max_depth, 1}]).

%% @doc
%% When used as the function for `dby:search/4', returns the list of
%% identifiers traversed in the search as tuples containing the
%% identifier, the identifier's metadata, and the link's metadata.
%% Acc0 = [].
-spec identifiers(dby_identifier(), metadata_info(), search_path(), list()) -> {continue, list()}.
identifiers(Identifier, IdMetadata, _, Acc) ->
    {continue, [{Identifier, IdMetadata} | Acc]}.

%% @doc
%% When used as the function for `dby:search/4', returns
%% all metadata for an identifier.
%% StartIdentifier = Identifier,
%% Acc0 = ignored,
%% Options = [{max_depth, 0}]
-spec identifier_detail(dby_identifier(), all_metadata(), search_path(), undefined | metadata_info()) -> {stop, undefined | metadata_info()}.
identifier_detail(_, Metadata, _, _) ->
    {stop, Metadata}.

%% @doc
%% When used as the function for `dby:search/4', returns
%% all subscriptions for an identifier
%% use as the function to search.
%% StartIdentifier = SubscriptionId,
%% Acc0 = [],
%% Options = [{max_depth, 1}, system].
-spec subscriptions_for_identifier(dby_identifier(), all_metadata(), search_path(), list()) -> {continue, list()}.
subscriptions_for_identifier(SubscriptionId, _, [{_, _, #{system := subscription}} | _], Acc0) ->
    {continue, [SubscriptionId | Acc0]};
subscriptions_for_identifier(_, _, _, Acc0) ->
    {continue, Acc0}.

%% @doc
%% When used as the function for `dby:search/4', returns
%% all metadata for a subscription.
%% StartIdentifier = SubscriptionId,
%% Acc0 = ignored,
%% Options = [{max_depth, 0}, system].
-spec subscription_detail(dby_identifier(), all_metadata(), search_path(), undefined | system_metadata()) -> {stop, undefined | system_metadata()}.
subscription_detail(_, Metadata = #{system := subscription}, _, _) ->
    {stop, Metadata};
subscription_detail(_, _, _, _) ->
    {stop, undefined}.

%% @doc
%% When used as the function for `dby:search/4', returns the list of
%% Links and link metadata from the starting identifier.
%% Acc0 = [].
-spec links(dby_identifier(), metadata_info(), search_path(), list()) -> {continue, list()}.
links(Identifier, _, [{_, _, LinkMetadata} | _], Acc) ->
    {continue, [{Identifier, LinkMetadata} | Acc]};
links(_, _, _, Acc) ->
    {continue, Acc}.

%% @doc
%% Generates a function that when used as the function for `dby:search/4`
%% returns the metadata for the link between the starting identifier
%% and `Identifier`.
%% Acc0 = [].
%% Options = [{max_depth 1}].
-spec link_search(dby_identifier()) -> fun().
link_search(NeighborIdentifier) ->
    fun(NIdentifier, _, [{_, _, LinkMetadata} | _], _)
                                    when NeighborIdentifier == NIdentifier ->
        {stop, LinkMetadata};
       (_, _, _, Acc) ->
        {continue, Acc}
    end.

% =============================================================================
% Local functions
% =============================================================================

% call the dobby server
call(Op, Args) ->
    gen_server:call({global, dobby}, {Op, Args}).
