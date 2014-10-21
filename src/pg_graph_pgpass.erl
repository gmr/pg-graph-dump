%%%-------------------------------------------------------------------
%%% @author Gavin M. Roy <gavinr@aweber.com>
%%% @copyright (C) 2014, AWeber Communications
%%% @doc Read and parse the .pgpass file
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pg_graph_pgpass).
-author("gavinr").

%% API
-export([get_password/4]).


get_password(Host, Port, DBName, User) ->
  case read_pgpass_file() of
    none    -> none;
    Entries -> get_password(Host, Port, DBName, User, Entries)
  end.


get_password(Host, Port, DBName, User, [Entry|Entries]) ->
  case match_entry(Host, Port, DBName, User, Entry) of
    false    -> get_password(Host, Port, DBName, User, Entries);
    Password -> Password
  end;

get_password(_, _, _, _, []) ->
  none.


match_entry(Host, Port, DBName, User, {EHost, EPort, EDBName, EUser, Password}) ->
  case [match_entry_value(Host, EHost),
        match_entry_value(Port, EPort),
        match_entry_value(DBName, EDBName),
        match_entry_value(User, EUser)] of
    [true, true, true, true] -> Password;
    _ -> false
  end.


match_entry_value(Desired, Entry) ->
  case Entry of
    "*" -> true;
    _   -> Desired =:= Entry
  end.


read_pgpass_file() ->
  Filename = pgpass_file(),
  case file:open(Filename, [read]) of
    {error, Reason} ->
      io:format("Could not open pgpass file ~p~n", [Reason]),
      none;
    {ok, Device} ->
      Entries = read_lines(Device, []),
      file:close(Device),
      [Entry || Entry <- Entries, Entry =/= []]
  end.


read_line(Device) ->
  case io:get_line(Device, "") of
    eof -> eof;
    Line -> parse_line(Line)
end.


read_lines(Device, Lines) ->
  case read_line(Device) of
    eof -> Lines;
    Entry ->
      read_lines(Device, lists:append(Lines, Entry))
  end.


parse_line(Line) ->
  case string:tokens(Line, ":\n") of
    [_, _, _, _] -> [];
    [Host, Port, DBName, User, Password] ->
      case string:str(Host, "#") of
        0 -> [{Host, list_to_integer(Port), DBName, User, Password}];
        _ -> []
      end;
    _ -> io:format("Could not parse line~n", []), []
  end.


pgpass_file() ->
  case os:getenv("PGPASSFILE") of
    false -> home_directory() ++ "/.pgpass";
    Value -> Value
  end.


home_directory() ->
  {ok,[[Home]]} = init:get_argument(home),
  Home.
