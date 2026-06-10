-- Define a module table
bx_utils = {}

database = require("database")
config = require("config")
get_brain_path = config.get_brain_path
lfs = require("lfs")

function is_id_unique(table_name, target_id, brain_file)
    brain_file = brain_file or get_brain_path()
    query = string.format("SELECT COUNT(*) AS cnt FROM %s WHERE id = '%s';", table_name, target_id)
    res = database.local_query(brain_file, query)
    if res == nil or #res == 0 then
        return true
    end
    count_val = tonumber(res[1].cnt or res[1][1]) or 0
    return count_val == 0
end

function generate_id(table_name, desired_length, seed, brain_file)
    desired_length = desired_length or 10
    seed = seed or os.time()
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
    res = database.local_query(brain_file, query)
    if res == nil or #res == 0 then
        return true
    end
    count_val = tonumber(res[1].cnt or res[1][1]) or 0
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
        
        path_to_scan = current_dir == "" and base_dir or base_dir .. "/" .. current_dir
        
        for file in lfs.dir(path_to_scan) do
            if file != "." and file != ".." then
                rel_path = current_dir == "" and file or (current_dir .. "/" .. file)
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
                    if string.sub(v, 1, 1) == '"' and string.sub(v, -1) == '"' then
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

function parse_session_body(body)
    messages = {}
    roles = {user = true, assistant = true, tool_result = true, compaction_summary = true, system = true}
    
    current_role = nil
    current_created_at = nil
    current_metadata = nil
    current_in_context = 1
    current_content_lines = {}
    
    lines = {}
    for line in string.gmatch(body, "[^\r\n]+") do
        table.insert(lines, line)
    end
    
    for _, line in ipairs(lines) do
        is_header = false
        matched_role = string.match(line, "^##%s+(.-)%s*$")
        if matched_role != nil then
            normalized_role = string.lower(string.gsub(matched_role, "%s+", "_"))
            if roles[normalized_role] then
                if current_role != nil then
                    table.insert(messages, {
                        role = current_role,
                        created_at = current_created_at,
                        metadata = current_metadata,
                        in_context = current_in_context,
                        content = strip(table.concat(current_content_lines, "\n"))
                    })
                end
                current_role = normalized_role
                current_created_at = nil
                current_metadata = nil
                current_in_context = 1
                current_content_lines = {}
                is_header = true
            end
        end
        
        if is_header == false and current_role != nil then
            created_at_match = string.match(line, "^%*Created%s+at:%s*(.-)%*%s*$")
            metadata_match = string.match(line, "^%*Metadata:%s*(.-)%*%s*$")
            in_context_match = string.match(line, "^%*In%s+Context:%s*(.-)%*%s*$")
            
            if created_at_match != nil then
                current_created_at = created_at_match
            elseif metadata_match != nil then
                current_metadata = metadata_match
            elseif in_context_match != nil then
                if in_context_match == "true" then
                    current_in_context = 1
                else
                    current_in_context = 0
                end
            else
                table.insert(current_content_lines, line)
            end
        end
    end
    
    if current_role != nil then
        table.insert(messages, {
            role = current_role,
            created_at = current_created_at,
            metadata = current_metadata,
            in_context = current_in_context,
            content = strip(table.concat(current_content_lines, "\n"))
        })
    end
    
    return messages
end

function serialize_session(session_meta, messages)
    sb = {}
    table.insert(sb, serialize_frontmatter(session_meta))
    table.insert(sb, "# Session: " .. (session_meta.name or "Unnamed Session"))
    table.insert(sb, "")
    
    for _, msg in ipairs(messages) do
        role_display = msg.role
        if msg.role == "user" then role_display = "User"
        elseif msg.role == "assistant" then role_display = "Assistant"
        elseif msg.role == "tool_result" then role_display = "Tool Result"
        elseif msg.role == "compaction_summary" then role_display = "Compaction Summary"
        elseif msg.role == "system" then role_display = "System"
        end
        
        table.insert(sb, "## " .. role_display)
        if msg.created_at != nil and msg.created_at != "" then
            table.insert(sb, "*Created at: " .. msg.created_at .. "*")
        end
        if msg.metadata != nil and msg.metadata != "" then
            table.insert(sb, "*Metadata: " .. msg.metadata .. "*")
        end
        if msg.in_context != nil then
            if msg.in_context == 1 or msg.in_context == true then
                table.insert(sb, "*In Context: true*")
            else
                table.insert(sb, "*In Context: false*")
            end
        end
        table.insert(sb, "")
        table.insert(sb, msg.content or "")
        table.insert(sb, "")
    end
    
    return table.concat(sb, "\n")
end

bx_utils.generate_id = generate_id
bx_utils.is_timestamp = is_timestamp
bx_utils.strip = strip
bx_utils.parse_frontmatter = parse_frontmatter
bx_utils.serialize_frontmatter = serialize_frontmatter
bx_utils.parse_session_body = parse_session_body
bx_utils.serialize_session = serialize_session
bx_utils.ensure_dir = ensure_dir
bx_utils.find_markdown_files = find_markdown_files

-- Export the module
return bx_utils
