-module(dby).

% @doc
% dobby API
% @end

-export([publish/3,
         publish/5,
         search/4,
         subscribe/4,
         unsubscribe/1,
         identifiers/4
        ]).

-include_lib("dobby_clib/include/dobby.hrl").

% =============================================================================
% API functions
% =============================================================================

% @equiv publish(PublisherId, [{Endpoint1, Endpoint2, LinkMetadata}], Options).
-spec publish(publisher_id(), dby_endpoint(), dby_endpoint(), metadata_proplist(), [publish_option()]) -> ok | {error, reason()}.
publish(PublisherId, Endpoint1, Endpoint2, LinkMetadata, Options) ->
    publish(PublisherId, [{Endpoint1, Endpoint2, LinkMetadata}], Options).

% @doc
% publish/3 adds or modifies identifiers and/or links, or sends a
% message via the identifiers and/or links. The PublisherId identifies
% the publisher.  The endpoints of a link are specified using an
% identifier or a tuple with the identifier and metadata.  If the
% endpoint is specified as an identifier, the metadata for that
% identifier is not changed. If the metadata is nochange the metadata
% for the endpoint is not changed.  If the endpoint is a tuple with
% the second element delete, the identifier is deleted from Dobby and
% all links to the identifier are also deleted.  If LinkMetadata is
% not specified or is nochange the link metadata is not changed. If
% link is a tuple with third element  delete the link between the two
% identifiers is deleted.  The identifiers themselves are otherwise
% not affected. Deleting an identifier deletes all links to the deleted
% identifier. If publish creates a new identifier or link and no
% metadata is provided, the metadata is set to null. Metadata may be
% any Erlang term that can be represented as a JSON. If the metadata
% key already exists, the metadata is replaced. If the metadata key
% does not exist the key and value is added. If the value is delete
% the metadata key is deleted. If the metadata is specified as a
% function, the function is called with the old metadata and the
% function is expected to return updated metadata for that link or
% identifier. The metadata is in the form of a property list. The
% return of the function is handled as if it was passed in directly
% as the property list. Fun should be a pure function as it may be
% called more than once.  The persistent option means all changes are
% persisted in Dobby.  The changes are made atomically.  That is, all
% the changes are made together or none are made. The message option
% means metadata changes are communicated to subscriptions but the
% changes are not persisted in dobby.  message is the default behavior.
% You can only message publish to existing endpoints and links.  You
% cannot use delete with message publish.
%
% Returns `badarg' for `message' publish if one of the endppoints or the
% link between them does not exist.
%
% `publish/2' may also be called with an endpoint.  This is a convenience
% for adding, removing, or modifying a single identifier.
% @end
-spec publish(publisher_id(), [dby_endpoint() | link()] | dby_endpoint(), [publish_option()]) -> ok | {error, reason()}.
publish(PublisherId, Endpoint, Options) when is_tuple(Endpoint); is_binary(Endpoint) ->
    publish(PublisherId, [Endpoint], Options);
publish(PublisherId, Data, Options) ->
    call(dby_publish, [PublisherId, Data, Options]).

% @doc
% search/4 performs a fold over the graph beginning with the identifier
% in StartIdentifier. The options breadth and depth control how the
% graph is traversed.  For breadth, all the links to StartIdentifier
% are traversed first, followed by all the links of StartIdentifier’s
% neighbors, etc.  For depth, one link of StartIdentifier is traversed,
% followed by one link of that neighbor identifier, etc.  If neither
% is specified, breadth is used.  Acc is the initial accumulator
% value.  Fun is called for every Identifier traversed by search. It
% controls the graph traversal and may also transform the result.
% Identifier is the current identifier. IdMetadataInfo is the metadata
% info for the identifier. Path is the list of identifiers with their
% metadatainfo and link metadatainfo that is the path from StartIdentifier
% to Identifier. Acc0 is the current accumulator. The first identifier
% in the Path list is immediate neighbor of Identifier that lead to
% Identifier. Metadatainfo is a map holding the metadata set with
% publish, the publisher identifier set during publish, and the
% datetime the metadata value was created or last updated. Fun returns
% a status that controls the next step of the navigation and the new
% accumulator.  The possible control values are: continue to continue
% the search, skip to continue the search but skip navigating to any
% neighbors of this identifier, stop to stop the search with this
% identifier.
% 
% The option max_depth controls how far to navigate away from the
% starting identifier.  max_depth of 0 means no navigation is performed.
% max_depth of one means search only navigates to the immediate
% neighbors of the starting identifier.  If max_depth is not provided,
% 0 is used.
% 
% The loop option specifies the loop detection algorithm.  none means
% there is no loop detection and Fun, Fun1 may see the same identifier
% more than once.  link means that a link is traversed only once, but
% if there is more than one link to an identifier, Fun, Fun1 may see
% the same identifier more than once.  identifier means that an
% identifier is traversed only once, so Fun, Fun1 will never see the
% same identifier more than once.  If loop is not provided, identifier
% loop detection is used.
% @end
-spec search(Fun :: search_fun(), Acc :: term(), StartIdentifier :: dby_identifier(), [search_options()]) -> term() | {error, reason()}.
search(Fun, Acc, StartIdentifier, Options) ->
    call(dby_search, [Fun, Acc, StartIdentifier, Options]).

% @doc
% `subscribe/4' creates a subscription and when successful returns a
% subscription id. A subscription is a standing search and many of
% the parameters for subscribe are the same as they are for search.
% A subscription may be on publishing of persistent data or messages,
% or both.  The subscription may provide a delta function, `DFun', that
% computes the delta from previous search `Acc' to the new search `Acc'.
% This function is only called `LastAcc' and `NewAcc' are different.  `DFun'
% returns the computed `Delta', 'stop' to delete the subscription and no
% further processing is performed on this subscription, or `nodelta'
% to indicate that there was no delta.  If no `DFun' is not provided
% in the options, Dobby uses `NewAcc' as the delta.  The subscription
% may provide a delivery function `SFun'.  `SFun' is only called if there
% is a delta in the subscription’s search result, that is, if `DFun' returns
% a delta.
% If `DFun' returns a delta, the `SFun' is called
% with the delta.  If `DFun' returns nodelta, `SFun' is not called.  If
% no `DFun' is provided, `SFun' is called with NewAcc.  `SFun' may return
% `stop' to delete the subscription, otherwise it should return `ok'.  If
% no `SFun' is provided, no deltas are delivered.
% @end
-spec subscribe(Fun :: search_fun(), Acc :: term(), StartIdentifier :: dby_identifier(), [subscribe_options()]) -> {ok, subscription_id()} | {error, reason()}.
subscribe(Fun, Acc, StartIdentifier, Options) ->
    call(dby_subscribe, [Fun, Acc, StartIdentifier, Options]).

% @doc
% `unsubscribe/1' deletes a subscription.  Attempts to delete an invalid
% or already deleted subscription are ignored.
% @end
-spec unsubscribe(subscription_id()) -> ok.
unsubscribe(SubscriptionId) ->
    call(dby_unsubscribe, SubscriptionId).

% @doc
% When used as the function for `dby:search/4', returns the list of
% identifiers traversed in the search as tuples containing the
% identifier, the identifier's metadata, and the link's metadata.
% @end
-spec identifiers(dby_identifier(), metadata_info(), search_path(), list()) -> {continue, list()}.
identifiers(Identifier, IdMetadata, [], Acc) ->
    {continue, [{Identifier, IdMetadata, undefined} | Acc]};
identifiers(Identifier, IdMetadata, [{_ ,_, LinkMetadata}], Acc) ->
    {continue, [{Identifier, IdMetadata, LinkMetadata} | Acc]}.

% =============================================================================
% Local functions
% =============================================================================

% call the dobby server
call(Op, Args) ->
    gen_server:call({global, dobby}, {Op, Args}).
