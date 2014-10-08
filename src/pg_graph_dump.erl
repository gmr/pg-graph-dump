%%=============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2014 AWeber Communications
%% @end
%%=============================================================================
-module(pg_graph_dump).

-export([main/1]).

%% Command line argument specification
-define(SPEC,
        [{dbname,        $d, "dbname",       {list, os:getenv("USER")}, "database to dump"},
         {host,          $h, "host",         list, "database server host or socket directory"},
         {port,          $p, "port",         integer, "database server port number"},
         {username,      $U, "username",     {list, os:getenv("USER")}, "connect as specified database user"},
         {no_pwd_prompt, $w, "no-password",  boolean, "never prompt for password"},
         {pwd_prompt,    $W, "password",     boolean, "force password prompt (should happen automatically)"},
         {help,          $h, "help",         boolean, "display this help and exit"}]).

%% @spec main(Args)
%% @where
%%       Args = list()
%% @doc Process the command line arguments, displaying help or running app
%% @end
%%
main(Args) ->
  case getopt:parse(?SPEC, Args) of
    {ok, {Opts, DBName}} ->
      case proplists:get_bool(help, Opts) of
        true  -> usage(0);
        false ->
          case process(Opts, DBName) of
            ok    -> ok;
            Error ->
              io:format("Error: ~p~n", [Error]),
              usage(1)
          end
      end;
    {error, _} -> usage(1)
 end.


%% @private
%% @spec usage(ExitCode)
%% @where
%%       ExitCode = integer()
%% @doc Display the cli help, exiting with the specified exit code
%% @end
%%
usage(ExitCode) ->
  getopt:usage(?SPEC, "pg_graph_dump", "[DBNAME]", [{"DBNAME", "database to dump"}]),
  case ExitCode of
    0 -> io:format("If no database name is supplied, then the PGDATABASE environment~nvariable value is used.~n");
    _ -> ok
  end,
  erlang:halt(ExitCode).

%% @private
%% @spec process(Options, DBName)
%% @where
%%       ExitCode = integer()
%% @doc Display the cli help, exiting with the specified exit code
%% @end
%%
process(Opts, []) ->
  io:format("Options: ~p~n", [Opts]),
  ok;

process(Opts, DBName) ->
  io:format("Options: ~p~nDBName: ~p~n", [Opts, DBName]),
  ok.
