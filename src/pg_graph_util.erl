%%%-------------------------------------------------------------------
%%% @author Gavin M. Roy <gavinr@aweber.com>
%%% @copyright (C) 2014, AWeber Communications
%%% @doc
%%%
%%% @end
%%% Created : 09. Oct 2014 12:40 AM
%%%-------------------------------------------------------------------
-module(pg_graph_util).

-include("include/pg_graph_dump.hrl").

%% API
-export([bin_to_list/1,
         bool/1,
         parse_acls/1,
         load_graph/1,
         save_graph/2]).


load_graph(Filename) ->
  {ok, [Data]} = file:consult(Filename),
  deserialize(Data).


deserialize({VL, EL, NL, B}) ->
  Graph = {digraph, V, E, N, B} = case B of
     true -> digraph:new();
     false -> digraph:new([acyclic])
  end,
  ets:delete_all_objects(V),
  ets:delete_all_objects(E),
  ets:delete_all_objects(N),
  ets:insert(V, VL),
  ets:insert(E, EL),
  ets:insert(N, NL),
  Graph.


save_graph(Filename, Graph) ->
  file:write_file(Filename, io_lib:fwrite("~p.\n",[serialize(Graph)])).


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


serialize({digraph, V, E, N, B}) ->
    {ets:tab2list(V),
     ets:tab2list(E),
     ets:tab2list(N),
     B}.
