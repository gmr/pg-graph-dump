%%=============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2014 AWeber Communications
%% @end
%%=============================================================================
-module(pg_graph_dump).

-export([main/1, on_error/1]).

-include("include/pg_graph_dump.hrl").

%% Command line argument specification
-define(SPEC,
        [{dbname,        $d, "dbname",       {list, os:getenv("USER")}, "database to dump"},
         {host,          $h, "host",         {list, "localhost"},       "database server host"},
         {port,          $p, "port",         {integer, 5432},           "database server port number"},
         {username,      $U, "username",     {list, os:getenv("USER")}, "connect as specified database user"},
         {no_pwd_prompt, $w, "no-password",  boolean,                   "never prompt for password"},
         {pwd_prompt,    $W, "password",     boolean,                   "force password prompt (should happen automatically)"},
         {help,          $h, "help",         boolean,                   "display this help and exit"}]).


%% @spec main(Args)
%% @where
%%       Args = list()
%% @doc Process the command line arguments, displaying help or running app
%% @end
%%
main(Args) ->
  case getopt:parse(?SPEC, Args) of
    {ok, {Opts, _}} ->
      case proplists:get_bool(help, Opts) of
        true  -> usage(0);
        false ->
          case process(Opts) of
            ok    -> ok;
            Error ->
              io:format("Error: ~p~n", [Error]),
              usage(1)
          end
      end;
    {error, _} -> usage(1)
 end.


%% @spec on_error(Msg)
%% @where
%%       Msg = atom()
%% @doc Display an error message and exit in an error state
%% @end
%%
on_error(Msg) ->
  io:format("ERROR: " ++ atom_to_list(Msg) ++ "~n"),
  erlang:halt(1).


%% @private
%% @spec usage(ExitCode)
%% @where
%%       ExitCode = integer()
%% @doc Display the cli help, exiting with the specified exit code
%% @end
%%
usage(ExitCode) ->
  getopt:usage(?SPEC, "pg_graph_dump", [], []),
  case ExitCode of
    0 -> io:format("If no database name is supplied, then the PGDATABASE environment~nvariable value is used.~n");
    _ -> ok
  end,
  erlang:halt(ExitCode).


%% @private
%% @spec process(Opts)
%% @where
%%       ExitCode = integer()
%% @doc Display the cli help, exiting with the specified exit code
%% @end
%%
process(Opts) ->
  Host = proplists:get_value(host, Opts),
  Port = proplists:get_value(port, Opts),
  DBName = proplists:get_value(dbname, Opts),
  User = proplists:get_value(username, Opts),

  %% Connect to PostgreSQL, if the connection fails, the app exits prior to return
  Conn = case proplists:get_bool(pwd_prompt, Opts) of
    true  ->
      connect(Host, Port, DBName, User, get_password());
    false ->
      connect(Host, Port, DBName, User, proplists:get_bool(no_pwd_prompt, Opts))
  end,

  State1 = #state{connection=Conn,
                  graph=digraph:new(),
                  version=pg_graph_db:version(Conn)},
  State2 = pg_graph_db:build_graph(State1),
  io:format("Sorted: ~p~n", [digraph_utils:postorder(State2#state.graph)]).


%% @private
%% @spec get_password -> list()
%% @doc Propmpt the user for their password
%% @todo This currently echos the password to stdout, bad.
%% @end
%%
get_password() ->
  %% This fails
  %%ok = io:setopts([{echo, false}]),
  Value = io:get_line("Password: "),
  %%ok = io:setopts([{echo, true}]),
  io:format("~n"),
  string:strip(string:strip(Value, both, 13), both, 10).


%% @private
%% @spec connect(Host, Port, DBName, User, NoPrompt) -> pid()
%% @where
%%       Host     = list()
%%       Port     = integer()
%%       DBName   = list()
%%       User     = list()
%%       NoPrompt = boolean()
%% @doc Connect to PostgreSQL without a password. If the connection fails due
%%      to an invalid_authorization_specification error and we can prompt for
%%      a password, do so and try again.
%% @end
%%
connect(Host, Port, DBName, User, NoPrompt) when is_boolean(NoPrompt) ->
  case pgsql:connect(Host, User, [{database, DBName}, {port, Port}]) of
    {ok, Conn} -> Conn;
    {error, invalid_authorization_specification} ->
        case NoPrompt of
          true  -> on_error(invalid_authorization_specification);
          false -> connect(Host, Port, DBName, User, get_password())
        end;
    {error, Error} -> on_error(Error)
  end;

%% @private
%% @spec connect(Host, Port, DBName, User, Password) -> pid()
%% @where
%%       Host     = list()
%%       Port     = integer()
%%       DBName   = list()
%%       User     = list()
%%       Password = list()
%% @doc Connect to PostgreSQL with a password. If the connection fails, exit
%% @end
%%
connect(Host, Port, DBName, User, Password) ->
  case pgsql:connect(Host, User, Password, [{database, DBName}, {port, Port}]) of
    {ok, Conn}     -> Conn;
    {error, Error} -> on_error(Error)
  end.
