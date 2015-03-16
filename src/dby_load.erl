-module(dby_load).

% load identifiers and links from files.

-export([load/2,
         load/3,
         dirload/2,
         dirload/3]).

% ------------------------------------------------------------------------------
% API functions
% ------------------------------------------------------------------------------

% @equiv load(Publisher, Filename, infinity).
-spec load(binary(), string()) -> ok | {error, term()}.
load(Publisher, Filename) ->
    load(Publisher, Filename, infinity).

% @doc
% `load/3' reads `Filename' and calls `dby:publish/3' with the contents
% of the file using `Publisher' as te publisher.
% The maximum batch size
% is `BatchSize'.  If `BatchSize' is `infinity' the contents of the file
% are not batched.
% @end
-spec load(binary(), string(), inifinity | non_neg_integer()) -> ok | {error, term()}.
load(Publisher, Filename, BatchSize) ->
    case file:consult(Filename) of
        {ok, [Data]} ->
            process_file(Publisher, Data, BatchSize);
        Err ->
            Err
    end.

% @equiv dirload(Publisher, Dirname, infinity).
-spec dirload(binary(), string()) -> ok | {error, term()}.
dirload(Publisher, Dirname) ->
    dirload(Publisher, Dirname, infinity).

% @doc
% `dirload/3' finds all the files in `Dirname' with the
% `.dobby' extension and calls `load/2' on each one. Processing
% aborts if there is an error. Each file is loaded as a separate
% transaction, so an abort will result in a partial load of the data.
% The maximum batch size
% is `BatchSize'.  If `BatchSize' is `infinity' the contents of the file
% are not batched.
% @end
-spec dirload(binary(), string(), infinity | non_neg_integer()) -> ok | {error, term()}.
dirload(Publisher, Dirname, BatchSize) ->
    case file:list_dir(Dirname) of
        {ok, Filenames} ->
            process_files(Publisher, Dirname, Filenames, BatchSize);
        Err ->
            Err
    end.

% ------------------------------------------------------------------------------
% internal functions
% ------------------------------------------------------------------------------

process_files(Publisher, Dirname, Filenames, BatchSize) ->
    try
        lists:foreach(
            fun(Filename) ->
                case filename:extension(Filename) of
                    ".dobby" ->
                        case load(Publisher, filename:join(Dirname, Filename), BatchSize) of
                            ok ->
                                ok;
                            Err ->
                                throw({Filename, Err})
                        end;
                    _ ->
                        ok
                end
            end, Filenames),
        ok
    catch
        throw:{Filename, Err} ->
            {error, {Filename, Err}}
    end.

process_file(Publisher, Data, BatchSize) ->
    Batches = split(Data, BatchSize),
    try
        lists:foreach(
            fun(Batch) ->
                case dby:publish(Publisher, Batch, [persistent]) of
                    ok ->
                        ok;
                    Err ->
                        throw(Err)
                end
            end, Batches),
        ok
    catch
        throw:Error ->
            Error
    end.

split(Data, infinity) ->
    [Data];
split(Data, BatchSize) ->
    split(Data, BatchSize, []).

split([], _, Acc) ->
    Acc;
split(Data, BatchSize, Acc) when length(Data) < BatchSize ->
    [Data | Acc];
split(Data, BatchSize, Acc) ->
    {Batch, Rest} = lists:split(BatchSize, Data),
    split(Rest, BatchSize, [Batch | Acc]).
