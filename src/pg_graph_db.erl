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
  graph_roles(State1#state.graph, State1#state.roles),
  State2 = State1#state{namespaces=namespaces(State1)},
  graph_namespaces(State2#state.graph, State2#state.namespaces),
  State3 = State2#state{tables=tables(State2)},
  graph_tables(State3#state.graph, State3#state.tables),
  State3.


version(Connection) ->
  case pgsql:squery(Connection, "SELECT setting FROM pg_settings WHERE name = 'server_version'") of
    {ok, _, [{Version}|_]} -> semver:parse(Version);
    {error, Error} -> pg_graph_dump:on_error(Error)
  end.


%% -----------------------------------------------------------------------------
%% Private Functions
%% -----------------------------------------------------------------------------


graph_add_namespace(Graph, Namespace) ->
  digraph:add_vertex(Graph, {namespace, Namespace#pg_namespace.name}),
  graph_role_edge(Graph, {namespace, Namespace#pg_namespace.name}, Namespace#pg_namespace.owner),
  lists:foreach(fun(A) -> graph_role_edge(Graph, {namespace, Namespace#pg_namespace.name}, A#pg_acl.role) end,
                Namespace#pg_namespace.acls).


graph_namespaces(Graph, Namespaces) ->
  lists:foreach(fun(Namespace) ->  graph_add_namespace(Graph, Namespace) end, Namespaces).


graph_role_edge(Graph, Edge, Role) ->
  case Role of
    null  -> ok;
    Value -> digraph:add_edge(Graph, Edge, {role, Value})
  end.


graph_roles(Graph, Roles) ->
  lists:foreach(fun(V) -> digraph:add_vertex(Graph, {role, V#pg_role.name}) end, Roles).


graph_tables(Graph, Tables) ->
  lists:foreach(fun(T) -> graph_table(Graph, T) end, Tables).


graph_table(Graph, Table) ->
  digraph:add_vertex(Graph, {table, Table#pg_table.namespace, Table#pg_table.name}),
  digraph:add_edge(Graph,
                   {table, Table#pg_table.namespace, Table#pg_table.name},
                   {namespace, Table#pg_table.namespace}),
  digraph:add_edge(Graph,
                   {table, Table#pg_table.namespace, Table#pg_table.name},
                   {role, Table#pg_table.owner}),
  lists:foreach(fun(A) -> graph_role_edge(Graph,
                                          {table, Table#pg_table.namespace, Table#pg_table.name},
                                          A#pg_acl.role) end,
                Table#pg_table.acls).


role({Name, SuperUser, Inherit, CreateRole, CreateDB, CatalogUpdate, CanLogin, Replication, ConnLimit, Password, ValidUntil, Config, Oid}) ->
  #pg_role{name=bin_to_list(Name),
           super_user=bool(SuperUser),
           inherit=bool(Inherit),
           create_role=bool(CreateRole),
           create_db=bool(CreateDB),
           catalog_update=bool(CatalogUpdate),
           can_login=bool(CanLogin),
           replication=bool(Replication),
           conn_limit=binary_to_integer(ConnLimit),
           password=bin_to_list(Password),
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
  #pg_namespace{name=bin_to_list(Name),
                owner=role_name(State, binary_to_integer(Owner)),
                acls=parse_acls(bin_to_list(ACL)),
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
  #pg_table{name=bin_to_list(Name),
            namespace=Namespace#pg_namespace.name,
            owner=role_name(State#state.roles, binary_to_integer(Role)),
            acls=parse_acls(bin_to_list(ACL)),
            options=bin_to_list(Options),
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


bin_to_list(Value) ->
  case Value of
    null -> null;
    Bin -> binary_to_list(Bin)
  end.


bool(Value) ->
  case Value of
    <<"t">> -> true;
    "t"     -> true;
    <<"f">> -> false;
    "f"     -> false;
    null    -> null
  end.


create_acl(ACL) ->
  case string:tokens(ACL, "/=") of
    [Values, GrantedBy] -> create_acl(null, Values, GrantedBy);
    [Role, Values, GrantedBy] -> create_acl(Role, Values, GrantedBy)
  end.


create_acl(Role, Values, GrantedBy) ->
  #pg_acl{role=Role,
          granted_by=GrantedBy,
          select=has_permission($r, Values),
          usage=has_permission($U, Values),
          insert=has_permission($a, Values),
          update=has_permission($w, Values),
          delete=has_permission($d, Values),
          trigger=has_permission($t, Values),
          truncate=has_permission($D, Values),
          references=has_permission($x, Values),
          execute=has_permission($X, Values),
          create=has_permission($C, Values),
          connect=has_permission($c, Values),
          temporary=has_permission($T, Values)}.


has_permission(B, Bytes) ->
  lists:any(fun(C) -> B == C end, Bytes).


parse_acls(ACLs) ->
  case ACLs of
    null -> [];
    Values ->
      Tokenized = string:tokens(Values, ",{}"),
      [create_acl(A) || A <- Tokenized]
  end.
