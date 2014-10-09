%%%-------------------------------------------------------------------
%%% @author Gavin M. Roy <gavinr@aweber.com>
%%% @copyright (C) 2014, AWeber Communications
%%% @doc Compartmentalize
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pg_graph_db).

-include("include/pg_graph_dump.hrl").

-export([build_graph/1,
         version/1]).


build_graph(State) ->
  State1 = State#state{roles=roles(State)},
  pg_graph_build:roles(State1#state.graph, State1#state.roles),
  State2 = State1#state{namespaces=namespaces(State1)},
  pg_graph_build:namespaces(State2#state.graph, State2#state.namespaces),
  State3 = State2#state{tables=tables(State2)},
  pg_graph_build:tables(State3#state.graph, State3#state.tables),
  State3.


version(Connection) ->
  case pgsql:squery(Connection, "SELECT setting FROM pg_settings WHERE name = 'server_version'") of
    {ok, _, [{Version}|_]} -> semver:parse(Version);
    {error, Error} -> pg_graph_dump:on_error(Error)
  end.


%% -----------------------------------------------------------------------------
%% Private Functions
%% -----------------------------------------------------------------------------

role({Name, SuperUser, Inherit, CreateRole, CreateDB, CatalogUpdate, CanLogin, Replication, ConnLimit, Password, ValidUntil, Config, Oid}) ->
  #pg_role{name=pg_graph_util:bin_to_list(Name),
           super_user=pg_graph_util:bool(SuperUser),
           inherit=pg_graph_util:bool(Inherit),
           create_role=pg_graph_util:bool(CreateRole),
           create_db=pg_graph_util:bool(CreateDB),
           catalog_update=pg_graph_util:bool(CatalogUpdate),
           can_login=pg_graph_util:bool(CanLogin),
           replication=pg_graph_util:bool(Replication),
           conn_limit=binary_to_integer(ConnLimit),
           password=pg_graph_util:bin_to_list(Password),
           valid_until=ValidUntil,
           config=Config,
           oid=binary_to_integer(Oid)}.


role_name(Roles, Oid) ->
  case lists:keysearch(Oid, #pg_role.oid, Roles) of
    {value, Role} -> Role#pg_role.name;
    false -> Oid
  end.


role_sql(Version) ->
  case semver:compare(Version, {semver, 9, 0, 0, undefined}) of
    -1 -> "SELECT rolname, rolsuper, rolinherit, rolcreaterole, rolcreatedb, rolcatupdate, rolcanlogin, rolconnlimit, rolpassword, rolvaliduntil, rolconfig, oid FROM pg_roles";
    _ -> "SELECT rolname, rolsuper, rolinherit, rolcreaterole, rolcreatedb, rolcatupdate, rolcanlogin, rolreplication, rolconnlimit, rolpassword, rolvaliduntil, rolconfig, oid FROM pg_roles"
  end.


roles(#state{connection=Connection, version=Version}) ->
  case pgsql:squery(Connection, role_sql(Version)) of
    {ok, _, Rows} -> [role(Row) || Row <- Rows];
    {error, Error} -> pg_graph_dump:on_error(Error)
  end.


namespace(State, {Name, Owner, ACL, Oid}) ->
  #pg_namespace{name=pg_graph_util:bin_to_list(Name),
                owner=role_name(State, binary_to_integer(Owner)),
                acls=pg_graph_util:parse_acls(pg_graph_util:bin_to_list(ACL)),
                oid=binary_to_integer(Oid)}.


namespaces(#state{connection=Connection, roles=Roles}) ->
  case pgsql:squery(Connection, "SELECT nspname, nspowner, nspacl, oid FROM pg_namespace WHERE nspname NOT LIKE 'pg_%' AND nspname != 'information_schema'") of
    {ok, _, Rows} ->
      [namespace(Roles, Row) || Row <- Rows];
    {error, Error} ->
      io:format("Error: " ++ atom_to_list(Error) ++ "~n"),
      error
  end.


table(State, Namespace, {Name, Owner, ACL, Options, Oid}) ->
  Role = role_name(State#state.roles, Owner),
  #pg_table{name=pg_graph_util:bin_to_list(Name),
            namespace=Namespace#pg_namespace.name,
            owner=role_name(State#state.roles, binary_to_integer(Role)),
            acls=pg_graph_util:parse_acls(pg_graph_util:bin_to_list(ACL)),
            options=pg_graph_util:bin_to_list(Options),
            oid=binary_to_integer(Oid)}.


tables(State) ->
  lists:flatten([tables(State, Namespace) || Namespace <- State#state.namespaces]).


tables(State, Namespace) ->
  case pgsql:equery(State#state.connection,
                    "SELECT relname, relowner, relacl, reloptions, oid FROM pg_class WHERE relnamespace=$1 AND relkind = 'r';",
                    [Namespace#pg_namespace.oid]) of
    {ok, _, Rows} ->
      [table(State, Namespace, Row) || Row <- Rows];
    {error, Error} ->
      io:format("Error: " ++ atom_to_list(Error) ++ "~n"),
      error
  end.

