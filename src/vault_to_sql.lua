-- update brain file from obsidian vault
local vault_update = {}

local sqlite = require("sqlite3")

local function filter_markdown_files(dir_content)
    local markdown_files = {}

    for _, entry in pairs(dir_content) do
        if match("%.md$", entry) then
            table.insert(markdown_files, entry)
        end
    end

    return markdown_files
end

local function filter_directories(dir_content)
    local directories = {}

    for _, entry in pairs(dir_content) do
        if not match("%.", entry) then
            table.insert(directories, entry)
        end
    end

    return directories
end

local function get_vault_files(vault_path)
    local vault_content = {}
    local dir_content = readdir(vault_path)
    vault_content["root"] = filter_markdown_files(dir_content)
    local vault_groups = filter_directories(dir_content)
    for _, group in pairs(vault_groups) do
        dir_content = readdir(vault_path .. "/" .. group)
        vault_content[group] = filter_markdown_files(dir_content)
    end
    return vault_content
end

local function read_note(vault_path, note)
    local note_path = vault_path .. "/" .. note
    local note_name = note:gsub("%.md$", "")
    local note_content = read(note_path)
    return {name = note_name, content = note_content}
end

local function read_vault(vault_path)
    local vault_files = get_vault_files(vault_path)
    local vault_content = {}
    for _, group in pairs(keys(vault_files)) do
        if group == "root" then
            vault_content["root"] = {}
            for _, note in pairs(vault_files["root"]) do
                local note_content = read_note(vault_path, note)
                if note_content then
                    table.insert(vault_content["root"], note_content)
                end
            end
        else
            vault_content[group] = {}
            for _, note in pairs(vault_files[group]) do
                table.insert(vault_content[group], read_note(vault_path .. "/" .. group, note))
            end
        end
    end

    return vault_content
end

local function get_lines(markdown_text)
    local lines = {}

    for line in match_all("[^\r\n]+", markdown_text) do
        table.insert(lines, line)
    end

    return lines
end

local function extract_links(line, link_found)
    link_found = link_found or {}
    for link in match_all("%[%[(.-)%]%]", line) do
        if not occursin(link, link_found) then
            table.insert(link_found, link)
        end
        line = line:gsub("%[%[" .. link .. "%]%]", "")
    end
    return line, link_found
end

local function process_content(content)
    local content_lines = get_lines(content)
    local processed_content = ""
    local link_found = {}
    for _, line in ipairs(content_lines) do
        line, link_found = extract_links(line, link_found)
        processed_content = processed_content .. "/n" .. line
    end
    return processed_content, link_found
end

function vault_to_sql(brain_file)
    io.write("Vault path: ")
    local vault_path = io.read()
    local vault_content = read_vault(vault_path)
    local db = sqlite.open(brain_file)
    local insert_statement = ""
    local content = ""
    local links = {}
    db:exec("BEGIN TRANSACTION;")
    for _, group in pairs(keys(vault_content)) do
        for _, note in pairs(vault_content[group]) do
            content, links = process_content(note.content)
            insert_notes = "INSERT INTO notes ('group', 'name', 'content') VALUES ('" .. group .. "', '" .. note.name .. "', '" .. content .. "');"
            insert_connections = "INSERT INTO connections ('source', 'target') VALUES "
            for _, link in pairs(links) do
                source = "('" .. note.name .. "', "
                target = "'" .. link .. "'), "
                insert_connections = insert_connections .. source .. target
            end
            insert_connections = slice(insert_connections, 1, length(insert_connections) - 2)
            insert_connections = insert_connections .. ";"

            db:exec(insert_notes)
            db:exec(insert_connections)
        end
    end
    db:exec("COMMIT;")
    db:close()
end

vault_update.vault_to_sql = vault_to_sql

-- Export the module
return vault_update