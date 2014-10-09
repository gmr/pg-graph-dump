%%=============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2014 AWeber Communications
%% @end
%%=============================================================================
-module(pg_graph_dot).

%% API
-export([save/3]).


save(Filename, Database, Graph) ->
  Lookup = build_lookup_list(Graph),
  case file:open(Filename, [write]) of
    {ok, FileId} ->
      io:fwrite(FileId, "digraph ~s {~n", [Database]),
      io:fwrite(FileId, "  node [fontname=Helvetica fontsize=12 style=filled shape=box fillcolor=white fontcolor=black];~n", []),
      io:fwrite(FileId, "  node [height=0.5 width=1 penwidth = 0.5];~n", []),
      io:fwrite(FileId, "  edge [penwidth=0.5 arrowsize=0.5];~n", []),
      io:fwrite(FileId, "  outputorder=edgesfirst;~n", []),
      io:fwrite(FileId, "  overlap=false;~n", []),
      io:fwrite(FileId, "  ranksep=1.25; nodesep=1;~n", []),
      io:fwrite(FileId, "  ratio=auto;~n", []),
      lists:foreach(fun(Row) -> write_layout(FileId, Row) end, Lookup),
      io:fwrite(FileId, "~n", []),
      lists:foreach(fun(V) -> write_associations(FileId, Lookup, Graph, V) end, digraph:vertices(Graph)),
      io:fwrite(FileId, "}~n", []),
      file:close(FileId);
    _ -> pg_graph_util:on_error(failed_to_write_file)
  end.


build_lookup_list(Graph) ->
  Lookup = lists:foldl(fun(V, L) -> maybe_add_vertex(Graph, L, V) end, [], digraph:vertices(Graph)),
  lists:foldl(fun(V, L) -> maybe_add_vertex_edges(Graph, L, V) end, Lookup, digraph:vertices(Graph)).


edge_vertex({_, _, Vertex, _}) ->
  Vertex.


get_vertex_id(Lookup, Vertex) ->
  case proplists:lookup(Vertex, Lookup) of
    {_, ID} -> ID;
    none -> none
  end.


maybe_add_vertex(Graph, Lookup, Vertex) ->
  case lists:member(Vertex, Lookup) of
    true  -> Lookup;
    false ->
      case digraph:edges(Graph, Vertex) of
        [] -> Lookup;
        _ -> lists:append(Lookup, [{Vertex, next_id(Lookup)}])
      end
  end.


maybe_add_vertex_edges(Graph, Lookup, Vertex) ->
  Edges = [edge_vertex(digraph:edge(Graph, E)) || E <- digraph:edges(Graph, Vertex)],
  Missing = lists:takewhile(fun(E) -> proplists:lookup(E, Lookup) == none end, Edges),
  lists:foldl(fun(V, L) -> maybe_add_vertex(Graph, L, V) end, Lookup, Missing).


next_id([]) -> 0;
next_id(Lookup) -> lists:max([V || {_, V} <- Lookup]) + 1.


node_label(Node) ->
  case Node of
    {namespace, Value} -> "Schema: " ++ Value;
    {role, Value} -> "Role: " ++ Value;
    {table, Prefix, Value} -> "Table: " ++ Prefix ++ "." ++ Value
  end.


node_style(Node) ->
  "label=\"" ++ node_label(Node) ++ "\", " ++ case Node of
    {namespace, _} -> "fillcolor=\"#8bb8f9\" fontcolor=white";
    {role, _}      -> "fillcolor=\"#ffff8e\"";
    {table, _, _}  -> "fillcolor=\"#eeeeee\""
  end.


get_edge_ids(Graph, Lookup, Vertex) ->
  Edges = [get_vertex_id(Lookup, E) || E <- [edge_vertex(digraph:edge(Graph, E)) || E <- digraph:edges(Graph, Vertex)]],
  VertexID = get_vertex_id(Lookup, Vertex),
  lists:usort(["i" ++ integer_to_list(E) || E <- lists:takewhile(fun(V) -> (V /= none) and (V /= VertexID) end, Edges)]).


write_layout(FileId, {Vertex, ID}) ->
  io:fwrite(FileId, "  i~p [~s];~n", [ID, node_style(Vertex)]).


write_associations(FileId, Lookup, Graph, Vertex) ->
  case get_vertex_id(Lookup, Vertex) of
    none -> ok;
    ID ->
      case get_edge_ids(Graph, Lookup, Vertex) of
        [] -> ok;
        IDs -> io:fwrite(FileId, "  i~p->{~s};~n", [ID, string:join(IDs, ",")])
      end
  end.
