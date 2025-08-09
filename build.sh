luastatic brex.lua \
    src/init.lua \
    src/note.lua \
    src/task.lua \
    src/sql.lua \
    src/update.lua \
    src/vault_to_sql.lua \
    src/bx_utils.lua \
    src/help.lua \
    lua-utils/src/argparse.lua \
    lua-utils/src/database.lua \
    lua-utils/src/dataframes.lua \
    lua-utils/src/dates.lua \
    lua-utils/src/delimited_files.lua \
    lua-utils/src/paths.lua \
    lua-utils/src/prettyprint.lua \
    lua-utils/src/string_utils.lua \
    lua-utils/src/table_utils.lua \
    lua-utils/src/utils.lua \
    -I/usr/include/lua5.1 -llua5.1 -lm -ldl -lreadline -o bld/brex

mv brex.luastatic.c bld
