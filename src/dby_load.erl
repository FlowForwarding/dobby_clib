-module(dby_load).

% load identifiers and links from files.

-export([load/2,
         dirload/2]).

% ------------------------------------------------------------------------------
% API functions
% ------------------------------------------------------------------------------

% @doc
% `load/2' reads `Filename' and calls `dby:publish/3' with the contents
% of the file using `Publisher' as te publisher.
% @end
-spec load(binary(), string()) -> ok | {error, term()}.
load(Publisher, Filename) ->
    case file:consult(Filename) of
        {ok, [Data]} ->
            dby:publish(Publisher, Data, [persistent]);
        Err ->
            Err
    end.

% @doc
% `dirload/2' finds all the files in `Dirname' with the
% `.dobby' extension and calls `load/2' on each one. Processing
% aborts if there is an error. Each file is loaded as a separate
% transaction, so an abort will result in a partial load of the data
% @end
-spec dirload(binary(), string()) -> ok | {error, term()}.
dirload(Publisher, Dirname) ->
    case file:list_dir(Dirname) of
        {ok, Filenames} ->
            process_files(Publisher, Dirname, Filenames);
        Err ->
            Err
    end.

% ------------------------------------------------------------------------------
% internal functions
% ------------------------------------------------------------------------------

process_files(Publisher, Dirname, Filenames) ->
    try
        lists:foreach(
            fun(Filename) ->
                case filename:extension(Filename) of
                    ".dobby" ->
                        case load(Publisher, filename:join(Dirname, Filename)) of
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
