-- Define a module table
bx_utils = {}

const cli = require("cli")
const database = require("database")
const paths = require("paths")
const yaml = require("yaml")
config = require("config")
get_brain_path = config.get_brain_path
lfs = require("lfs")

function length(containable)
    if containable == nil then
        return 0
    end
    if type(containable) == "string" then
        return #containable
    end
    if type(containable) == "table" then
        cnt = 0
        for _, _ in pairs(containable) do
            cnt = cnt + 1
        end
        return cnt
    end
    return 0
end

function isempty(source)
    return source == nil or source == false or length(source) == 0
end

function readdir(dir_path)
    entries = {}
    if dir_path == nil or lfs.attributes(dir_path, "mode") != "directory" then
        return entries
    end
    for entry in lfs.dir(dir_path) do
        if entry != "." and entry != ".." then
            table.insert(entries, entry)
        end
    end
    table.sort(entries)
    return entries
end

function read_yaml(path)
    raw = paths.read_file(path)
    if raw == nil or raw == "" then
        return nil
    end
    ok, parsed = pcall(yaml.load, raw)
    if not ok then
        return nil
    end
    return parsed
end

function parse_cmd_args(cmd_args, flag_specs, help_string)
    opts, pos, err = cli.parse_flags(cmd_args, flag_specs)
    if err != nil then
        print(err)
        print("Usage: \t" .. tostring(cmd_args[0]))
        if help_string != nil then
            print(help_string)
        end
        return nil
    end
    if opts.help == true then
        print("Usage: \t" .. tostring(cmd_args[0]))
        if help_string != nil then
            print(help_string)
        end
        return nil
    end
    if pos != nil and #pos > 0 then
        print("Unknown argument: " .. tostring(pos[1]))
        print("Usage: \t" .. tostring(cmd_args[0]))
        if help_string != nil then
            print(help_string)
        end
        return nil
    end
    return opts
end

function is_id_unique(table_name, target_id, brain_file)
    if brain_file == nil then
        brain_file = get_brain_path()
    end
    query = string.format("SELECT COUNT(*) AS cnt FROM %s WHERE id = %s;", table_name, database.quote(target_id))
    res = database.query(brain_file, query)
    if res == nil or #res == 0 then
        return true
    end
    raw_count = res[1].cnt
    if raw_count == nil then
        raw_count = res[1][1]
    end
    count_val = tonumber(raw_count)
    if count_val == nil then
        count_val = 0
    end
    return count_val == 0
end

function generate_id(table_name, desired_length, seed, brain_file)
    if desired_length == nil then
        desired_length = 10
    end
    if seed == nil then
        seed = os.time()
    end
    math.randomseed(seed)

    id = ""
    id_unique = false
    while not id_unique do
        id = string.format("%0" .. desired_length .. "d", math.random(10 ^ (desired_length - 1)))
        id_unique = is_id_unique(table_name, id, brain_file)
    end

    return id
end

function is_timestamp(str)
    pattern = "^%d%d%d%d%-%d%d%-%d%d %d%d:%d%d:%d%d$"
    match = string.match(str, pattern)
    if match then
        return true
    else
        return false
    end
end

function is_sqlite_empty(brain_file, table_name)
    query = "SELECT COUNT(*) AS cnt FROM " .. table_name .. ";"
    res = database.query(brain_file, query)
    if res == nil or #res == 0 then
        return true
    end
    raw_count = res[1].cnt
    if raw_count == nil then
        raw_count = res[1][1]
    end
    count_val = tonumber(raw_count)
    if count_val == nil then
        count_val = 0
    end
    return count_val == 0
end

function ensure_dir(path)
    current = ""
    if string.sub(path, 1, 1) == "/" then
        current = "/"
    end
    for part in string.gmatch(path, "[^/]+") do
        if current == "/" or current == "" then
            current = current .. part
        else
            current = current .. "/" .. part
        end
        lfs.mkdir(current)
    end
end

function find_markdown_files(base_dir)
    file_list = {}
    queue = {""}
    q_index = 1
    
    while q_index <= #queue do
        current_dir = queue[q_index]
        q_index = q_index + 1
        
        path_to_scan = base_dir .. "/" .. current_dir
        if current_dir == "" then
            path_to_scan = base_dir
        end

        for file in lfs.dir(path_to_scan) do
            if file != "." and file != ".." then
                rel_path = current_dir .. "/" .. file
                if current_dir == "" then
                    rel_path = file
                end
                full_path = base_dir .. "/" .. rel_path
                attr = lfs.attributes(full_path)
                if attr != nil then
                    if attr.mode == "directory" then
                        table.insert(queue, rel_path)
                    elseif attr.mode == "file" and string.match(file, "%.md$") != nil then
                        table.insert(file_list, {
                            rel_path = rel_path,
                            file_name = file,
                            dir_path = current_dir
                        })
                    end
                end
            end
        end
    end
    
    return file_list
end

function strip(s)
    if s == nil then return "" end
    return string.match(s, "^%s*(.-)%s*$")
end

function parse_frontmatter(content)
    if content == nil then return {}, "" end
    metadata = {}
    body = content
    
    start_idx, end_idx = string.find(content, "^%-%-%-%s*[\r\n](.-)[\r\n]%-%-%-%s*[\r\n]")
    if start_idx != nil then
        frontmatter_text = string.match(content, "^%-%-%-%s*[\r\n](.-)[\r\n]%-%-%-%s*[\r\n]")
        body = string.sub(content, end_idx + 1)
        if frontmatter_text != nil then
            for line in string.gmatch(frontmatter_text, "[^\r\n]+") do
                k, v = string.match(line, "^%s*(%a[_%w]*)%s*:%s*(.-)%s*$")
                if k != nil and v != nil then
                    k = strip(k)
                    v = strip(v)
                    if string.sub(v, 1, 1) == "\"" and string.sub(v, -1) == "\"" then
                        v = string.sub(v, 2, -2)
                    elseif string.sub(v, 1, 1) == "'" and string.sub(v, -1) == "'" then
                        v = string.sub(v, 2, -2)
                    end
                    metadata[k] = v
                end
            end
        end
    end
    return metadata, body
end

function serialize_frontmatter(metadata)
    lines = {"---"}
    keys = {}
    for k, _ in pairs(metadata) do
        table.insert(keys, k)
    end
    table.sort(keys)
    for _, k in ipairs(keys) do
        v = metadata[k]
        if v != nil and v != "" then
            table.insert(lines, k .. ": " .. tostring(v))
        end
    end
    table.insert(lines, "---")
    table.insert(lines, "")
    return table.concat(lines, "\n")
end

bx_utils.length = length
bx_utils.isempty = isempty
bx_utils.readdir = readdir
bx_utils.read_yaml = read_yaml
bx_utils.parse_cmd_args = parse_cmd_args
bx_utils.is_id_unique = is_id_unique
bx_utils.generate_id = generate_id
bx_utils.is_sqlite_empty = is_sqlite_empty
bx_utils.is_timestamp = is_timestamp
bx_utils.strip = strip
bx_utils.parse_frontmatter = parse_frontmatter
bx_utils.serialize_frontmatter = serialize_frontmatter
bx_utils.ensure_dir = ensure_dir
bx_utils.find_markdown_files = find_markdown_files

-- Export the module
return bx_utils
