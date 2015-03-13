-module(dby_load).

% load identifiers and links from files.

-export([load/2,
         dirload/2]).

-spec load(binary(), string()) -> ok | {error, term()}.
load(Publisher, Filename) ->
    case file:consult(Filename) of
        {ok, [Data]} ->
            dby:publish(Publisher, Data, [persistent]);
        Err ->
            Err
    end.

-spec dirload(binary(), string()) -> ok | {error, term()}.
dirload(Publisher, Dirname) ->
    case file:list_dir(Dirname) of
        {ok, Filenames} ->
            process_files(Publisher, Dirname, Filenames);
        Err ->
            Err
    end.

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
