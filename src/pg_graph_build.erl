%%%-------------------------------------------------------------------
%%% @author Gavin M. Roy <gavinr@aweber.com>
%%% @copyright (C) 2014, AWeber Communications
%%% @doc Compartmentalize
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pg_graph_build).

%% API
-export([namespaces/2,
         roles/2,
         tables/2]).

-include("include/pg_graph_dump.hrl").


namespace(Graph, Namespace) ->
  digraph:add_vertex(Graph, {namespace, Namespace#pg_namespace.name}),
  role_edge(Graph, {namespace, Namespace#pg_namespace.name}, Namespace#pg_namespace.owner),
  lists:foreach(fun(A) -> role_edge(Graph, {namespace, Namespace#pg_namespace.name}, A#pg_acl.role) end,
                Namespace#pg_namespace.acls).


namespaces(Graph, Namespaces) ->
  lists:foreach(fun(Namespace) ->  namespace(Graph, Namespace) end, Namespaces).


role_edge(Graph, Edge, Role) ->
  case Role of
    null  -> ok;
    Value -> digraph:add_edge(Graph, Edge, {role, Value})
  end.


roles(Graph, Roles) ->
  lists:foreach(fun(V) -> digraph:add_vertex(Graph, {role, V#pg_role.name}) end, Roles).


tables(Graph, Tables) ->
  lists:foreach(fun(T) -> table(Graph, T) end, Tables).


table(Graph, Table) ->
  digraph:add_vertex(Graph, {table, Table#pg_table.namespace, Table#pg_table.name}),
  digraph:add_edge(Graph,
                   {table, Table#pg_table.namespace, Table#pg_table.name},
                   {namespace, Table#pg_table.namespace}),
  digraph:add_edge(Graph,
                   {table, Table#pg_table.namespace, Table#pg_table.name},
                   {role, Table#pg_table.owner}),
  lists:foreach(fun(A) -> role_edge(Graph,
                                    {table, Table#pg_table.namespace, Table#pg_table.name},
                                    A#pg_acl.role) end,
                Table#pg_table.acls).