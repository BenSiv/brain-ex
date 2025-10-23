-- update brain file from obsidian vault
local vault_update = {}

require("utils").using("utils")
local joinpath = require("paths").joinpath
local sqlite = require("sqlite3")
local lfs = require("lfs")

-- parse links string like "daily/note1,backend/note2,note3"
local function parse_links_str(links_str)
    if links_str == nil or links_str == "" then
        return {}
    end

    local links = {}
    for raw_link in links_str:gmatch("[^,]+") do
        raw_link = strip(raw_link)  -- remove extra spaces
        local parts = split(raw_link, "/")
        if #parts == 2 then
            table.insert(links, {subject=parts[1], title=parts[2]})
        else
            table.insert(links, {subject="", title=parts[1]})
        end
    end
    return links
end

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
    local vault_subjects = filter_directories(vault_path, dir_content)
    for _, subject in pairs(vault_subjects) do
        dir_content = readdir(vault_path .. "/" .. subject)
        vault_content[subject] = filter_markdown_files(dir_content)
    end
    return vault_content
end

local function read_note(vault_path, note)
    local note_path = joinpath(vault_path, note)
    local note_name = replace(note, ".md", "")
    local note_content = read(note_path)
    return {name = note_name, content = note_content}
end

local function read_vault(vault_path)
    local vault_files = get_vault_files(vault_path)
    local vault_content = {}
    local note_content
    for _, subject in pairs(keys(vault_files)) do
        if subject == "root" then
            vault_content["root"] = {}
            for _, note in pairs(vault_files["root"]) do
                note_content = read_note(vault_path, note)
                if note_content then
                    table.insert(vault_content["root"], note_content)
                end
            end
        else
            vault_content[subject] = {}
            for _, note in pairs(vault_files[subject]) do
                note_path = joinpath(vault_path, subject, note)
                note_content = read_note(vault_path .. "/" .. subject, note)
                table.insert(vault_content[subject], note_content)
            end
        end
    end

    return vault_content
end

local function get_lines(markdown_text)
    local lines = {}
    
    for line in match_all(markdown_text, "[^\r\n]+") do
        table.insert(lines, line)
    end

    return lines
end

-- Function to remove all instances of the link pattern
local function remove_link(input_line, link)
    local link_pattern = "[[" .. link .. "]]"
    local output_line = replace(input_line, link_pattern, "")
    if match(output_line, "^%s*$") then
    	output_line = ""
    end
    return output_line
end

local function extract_links(line, link_found)
    link_found = link_found or {}
    local processed_line = line

    for raw_link in match_all(line, "%[%[(.-)%]%]") do
        local parsed_links = parse_links_str(raw_link)
        for _, link in ipairs(parsed_links) do
            table.insert(link_found, link)
        end
        processed_line = remove_link(processed_line, raw_link)
    end

    return processed_line, link_found
end


local function clean_content(content)
    local cleaned_content = replace(content, "'", "")
    cleaned_content = replace(cleaned_content, '"', "")
    return cleaned_content
end

local function process_content(content)
    content = unescape_string(content)
    content = clean_content(content)
    local content_lines = get_lines(content)
    local processed_lines = {}
    local link_found = {}

    for _, line in ipairs(content_lines) do
        local processed_line, updated_link_found = extract_links(line, link_found)
        link_found = updated_link_found
        if processed_line ~= "" then
        	table.insert(processed_lines, processed_line)
        end
    end

    local processed_content = table.concat(processed_lines, "\n")
    return processed_content, link_found
end

function vault_to_sql(vault_path, brain_file)
    local vault_content = read_vault(vault_path)
    if not vault_content then
        print("Failed to read vault")
        return nil
    end

    local db = sqlite.open(brain_file)
    db:exec("BEGIN TRANSACTION;")

    for subject, notes in pairs(vault_content) do
        -- Treat "root" as empty subject
        local actual_subject = (subject == "root") and "" or subject

        for _, note in pairs(notes) do
            -- Extract cleaned content and parsed links
            local content, links = process_content(note.content)

            -- Resolve note file path
            local note_path
            if actual_subject ~= "" then
                note_path = joinpath(vault_path, actual_subject, note.name .. ".md")
            else
                note_path = joinpath(vault_path, note.name .. ".md")
            end

            local last_update_time = get_last_update_time(note_path)

            -- Insert note into notes table
            local insert_note = string.format(
                "INSERT INTO notes (time, subject, title, content) VALUES ('%s','%s','%s','%s');",
                last_update_time,
                actual_subject,
                note.name,
                content
            )
            db:exec(insert_note)

            -- Insert connections if any
            if length(links) > 0 then
                local insert_connections = "INSERT INTO connections (source_title, source_subject, target_title, target_subject) VALUES "

                for _, link in pairs(links) do
                    local statement_value = string.format(
                        "('%s','%s','%s','%s'), ",
                        note.name,
                        subject,
                        link.title,
                        link.subject or ""
                    )
                    insert_connections = insert_connections .. statement_value
                end

                -- Trim trailing comma and finalize
                insert_connections = insert_connections:sub(1, -3) .. ";"
                db:exec(insert_connections)
            end
        end
    end

    db:exec("COMMIT;")
    db:close()
    return "success"
end

vault_update.parse_links_str = parse_links_str
vault_update.process_content = process_content
vault_update.vault_to_sql = vault_to_sql

-- Export the module
return vault_update
