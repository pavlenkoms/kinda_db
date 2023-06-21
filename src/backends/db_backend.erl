-module(db_backend).

-callback new(DbName :: db:db_name()) -> ok | db:db_error().
-callback create(Record :: db:db_record(), DbName :: db:db_name()) -> db:db_resp().
-callback read(Key :: db:db_key(), DbName :: db:db_name()) -> db:db_resp().
-callback update(Record :: db:db_record(), DbName :: db:db_name()) -> db:db_resp().
-callback delete(Record :: db:db_key(), DbName :: db:db_name()) -> db:db_resp().
