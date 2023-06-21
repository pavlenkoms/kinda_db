-module(db).

-define(BACKEND, db_backend_dict_map).

-export([new/1, create/2, read/2, update/2, delete/2]).

-export_type([db_name/0, db_key/0, db_record/0, db_error/0, db_resp/0]).

-type db_name() :: nonempty_string().
-type db_key() :: integer().
-type db_record() ::
  {Key :: integer(), UserName :: nonempty_string(), City :: nonempty_string()}.
-type db_error() :: {error, Reason :: term()}.
-type db_resp() :: {ok, Record :: db_record()} | db_error().

-spec new(DbName :: db_name()) -> ok | db_error().
new(DbName) ->
  ?BACKEND:new(DbName).

-spec create(Record :: db_record(), DbName :: db_name()) -> db_resp().
create(Record, DbName) ->
  ?BACKEND:create(Record, DbName).

-spec read(Key :: db_key(), DbName :: db_name()) -> db_resp().
read(Key, DbName) ->
  ?BACKEND:read(Key, DbName).

-spec update(Record :: db_record(), DbName :: db_name()) -> db_resp().
update(Record, DbName) ->
  ?BACKEND:update(Record, DbName).

-spec delete(Record :: db_record(), DbName :: db_name()) -> db_resp().
delete(Record, DbName) ->
  ?BACKEND:delete(Record, DbName).
