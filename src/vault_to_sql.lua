-- update brain file from obsidian vault
local vault_update = {}

require("utils").using("utils")
local sqlite = require("sqlite3")
local lfs = require("lfs")

local function get_last_update_time(file_path)
    local attr = lfs.attributes(file_path)
    if attr then
        return os.date("%Y-%m-%d %H:%M:%S", attr.modification)
    else
        error("Could not get attributes for file: " .. file_path)
    end
end

local function filter_markdown_files(dir_content)
    local markdown_files = {}

    for _, entry in pairs(dir_content) do
        if string.match(entry, "%.md$") then
            table.insert(markdown_files, entry)
        end
    end

    return markdown_files
end

local function filter_directories(dir_path, dir_content)
    local directories = {}

    for _, entry in pairs(dir_content) do
        local entry_path = dir_path .. '/' .. entry
        local attr = lfs.attributes(entry_path)
        if attr and attr.mode == "directory" then
            if string.sub(entry, 1, 1) ~= '.' then
                table.insert(directories, entry)
            end
        end
    end

    return directories
end

local function get_vault_files(vault_path)
    local vault_content = {}
    local dir_content = readdir(vault_path)
    vault_content["root"] = filter_markdown_files(dir_content)
    local vault_groups = filter_directories(vault_path, dir_content)
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

-- Function to remove all instances of the link pattern
local function remove_link(input_line, link)
    local link_pattern = "[[" .. link .. "]]"
    local output_line = replace(input_line, link_pattern, "")
    return output_line
end

local function extract_links(line, link_found)
    link_found = link_found or {}
    local processed_line = line
    for link in match_all("%[%[(.-)%]%]", line) do
        if not occursin(link, link_found) then
            table.insert(link_found, link)
        end
        processed_line = remove_link(processed_line, link)
    end
    return processed_line, link_found
end

local function process_content(content)
    local content_lines = get_lines(content)
    local processed_lines = {}
    local link_found = {}

    for _, line in ipairs(content_lines) do
        local processed_line, updated_link_found = extract_links(line, link_found)
        link_found = updated_link_found
        table.insert(processed_lines, processed_line)
    end

    local processed_content = table.concat(processed_lines, "\n")
    return processed_content, link_found
end

function vault_to_sql(vault_path, brain_file)
    local vault_content = read_vault(vault_path)
    local db = sqlite.open(brain_file)
    db:exec("BEGIN TRANSACTION;")
    for group, notes in pairs(vault_content) do
        for _, note in pairs(notes) do
            local content, links = process_content(note.content)
            local note_path = ""
            if group ~= "root" then
                note_path = vault_path .. "/" .. group .. "/" .. note.name .. ".md"
            else
                note_path = vault_path .. "/" .. note.name .. ".md"
            end
            local last_update_time = get_last_update_time(note_path)
            local insert_notes = "INSERT INTO notes ('time', 'group', 'name', 'content') VALUES ('" .. last_update_time .. "', '" .. group .. "', '" .. note.name .. "', '" .. content .. "');"
            db:exec(insert_notes)

            if length(links) > 0 then
                local insert_connections = "INSERT INTO connections ('source', 'target') VALUES "
                for _, link in pairs(links) do
                    local source = "('" .. note.name .. "', "
                    local target = "'" .. link .. "'), "
                    insert_connections = insert_connections .. source .. target
                end
                insert_connections = slice(insert_connections, 1, length(insert_connections) - 2)
                insert_connections = insert_connections .. ";"
                db:exec(insert_connections)
            end
        end
    end

    db:exec("COMMIT;")
    db:close()
end

vault_update.vault_to_sql = vault_to_sql

-- Export the module
return vault_update