%%=============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2014 AWeber Communications
%% @end
%%=============================================================================

-include_lib("epgsql/include/pgsql.hrl").


-record(state, {connection, graph, version, roles, namespaces, tables}).

-record(pg_acl, {role, granted_by, select, usage, insert, update, delete, trigger, truncate, references, execute, create, connect, temporary}).
-record(pg_namespace, {name, owner, acls, oid}).
-record(pg_role, {name, super_user, inherit, create_role, create_db, catalog_update, can_login, replication, conn_limit, password, valid_until, config, oid}).
-record(pg_table, {namespace, name, owner, acls, options, columns, constraints, oid}).


-record(pg_extension, {name, schema, comment, owner}).
-record(pg_column, {name, type, nullable}).
-record(pg_domain, {name}).

-record(pg_function, {name}).
-record(pg_cast, {name}).
-record(pg_rule, {name}).
-record(pg_trigger, {name}).
-record(pg_index, {name}).
-record(pg_constraint, {name}).
