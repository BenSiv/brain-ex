-- update brain file from obsidian vault
vault_update = {}

utils = require("utils")
joinpath = require("paths").joinpath
database = require("database")
lfs = require("lfs")

-- parse links string like "daily/note1,backend/note2,note3"
function parse_links_str(links_str)
    if links_str == nil or links_str == "" then
        return {}
    end

    links = {}
    for raw_link in string.gmatch(links_str, "[^,]+") do
        raw_link = strip(raw_link)  -- remove extra spaces
        parts = split(raw_link, "/")
        if #parts == 2 then
            table.insert(links, {subject=parts[1], title=parts[2]})
        else
            table.insert(links, {subject="", title=parts[1]})
        end
    end
    return links
end

function get_file_attributes(file_path)
    attr = lfs.attributes(file_path)
    if attr != nil then
        return os.date("%Y-%m-%d %H:%M:%S", attr.modification), attr.size
    else
        error("Could not get attributes for file: " .. file_path)
    end
end

function filter_markdown_files(dir_content)
    markdown_files = {}

    for _, entry in pairs(dir_content) do
        if string.match(entry, "%.md$") != nil then
            table.insert(markdown_files, entry)
        end
    end

    return markdown_files
end

function filter_directories(dir_path, dir_content)
    directories = {}

    for _, entry in pairs(dir_content) do
        entry_path = dir_path .. '/' .. entry
        attr = lfs.attributes(entry_path)
        if attr != nil and attr.mode == "directory" then
            if string.sub(entry, 1, 1) != '.' then
                table.insert(directories, entry)
            end
        end
    end

    return directories
end

function get_vault_files(vault_path)
    vault_content = {}
    dir_content = utils.readdir(vault_path)
    vault_content["root"] = filter_markdown_files(dir_content)
    vault_subjects = filter_directories(vault_path, dir_content)
    for _, subject in pairs(vault_subjects) do
        dir_content = utils.readdir(vault_path .. "/" .. subject)
        vault_content[subject] = filter_markdown_files(dir_content)
    end
    return vault_content
end

function read_note(vault_path, note)
    if note == nil then
        return nil
    end
    note_path = joinpath(vault_path, note)
    note_name = string.gsub(note, "%.md$", "")
    note_content = utils.read(note_path)
    return {name = note_name, content = note_content}
end

function get_lines(markdown_text)
    lines = {}
    
    for line in utils.match_all(markdown_text, "[^\r\n]+") do
        table.insert(lines, line)
    end

    return lines
end

-- Function to remove all instances of the link pattern
function remove_link(input_line, link)
    if input_line == nil or input_line == "" then
        return ""
    end
    link_pattern = "%[%[" .. link .. "%]%]"
    output_line = utils.replace(input_line, link_pattern, "")
    if utils.match(output_line, "^%s*$") != nil then
    	output_line = ""
    end
    return output_line
end

function extract_links(line, link_found)
    if link_found == nil then
        link_found = {}
    end
    processed_line = line

    for raw_link in utils.match_all(line, "%[%[(.-)%]%]") do
        parsed_links = parse_links_str(raw_link)
        for _, link in ipairs(parsed_links) do
            table.insert(link_found, link)
        end
        processed_line = remove_link(processed_line, raw_link)
    end

    return processed_line, link_found
end


function clean_content(content)
    if content == nil then
        return ""
    end
    cleaned_content = string.gsub(content, "'", "")
    cleaned_content = string.gsub(cleaned_content, '"', "")
    return cleaned_content
end

function process_content(content)
    content = unescape_string(content)
    content = clean_content(content)
    content_lines = get_lines(content)
    processed_lines = {}
    link_found = {}

    for _, line in ipairs(content_lines) do
        processed_line, updated_link_found = extract_links(line, link_found)
        link_found = updated_link_found
        if processed_line != "" then
        	table.insert(processed_lines, processed_line)
        end
    end

    processed_content = table.concat(processed_lines, "\n")
    return processed_content, link_found
end

function vault_to_sql(vault_path, brain_file)
    -- Ensure size column exists (for upgrade path)
    pcall(database.sqlite_update, brain_file, "ALTER TABLE notes ADD COLUMN size INTEGER DEFAULT 0;") -- ignore error if already exists
    
    -- Load existing note metadata for incremental update
    existing_notes = {}
    query = "SELECT subject, title, time, size FROM notes;"
    rows_raw = database.sqlite_query(brain_file, query)
    rows = {}
    if rows_raw != nil then
        rows = rows_raw
    end
    for _, row in ipairs(rows) do
        -- Handle both named and numeric column access
        subject = ""
        if row[1] != nil then
            subject = row[1]
        end
        if row.subject != nil then
            subject = row.subject
        end

        title = ""
        if row[2] != nil then
            title = row[2]
        end
        if row.title != nil then
            title = row.title
        end

        time = ""
        if row[3] != nil then
            time = row[3]
        end
        if row.time != nil then
            time = row.time
        end

        size_field = 0
        if row[4] != nil then
            size_field = row[4]
        end
        if row.size != nil then
            size_field = row.size
        end
        size_num = tonumber(size_field)
        size = 0
        if size_num != nil then
            size = size_num
        end

        key = subject .. "||" .. title
        existing_notes[key] = {time = time, size = size}
    end

    vault_files = get_vault_files(vault_path)
    if vault_files == nil then
        print("Failed to read vault")
        return nil
    end

    sql_statements = {}
    seen_notes = {}
    updates_count = 0
    inserts_count = 0

    for subject, notes in pairs(vault_files) do
        actual_subject = subject
        if subject == "root" then
            actual_subject = ""
        end

        for _, note_file in pairs(notes) do
            note_name = string.gsub(note_file, "%.md$", "")
            note_key = actual_subject .. "||" .. note_name
            seen_notes[note_key] = true
            
            -- Resolve note file path
            note_path = nil
            if actual_subject != "" then
                note_path = joinpath(vault_path, actual_subject, note_file)
            else
                note_path = joinpath(vault_path, note_file)
            end

            last_update_time, file_size = get_file_attributes(note_path)
            
            existing = existing_notes[note_key]
            
            -- Absolute guarantee: update if time OR size differs
            needs_update = (existing == nil) or (existing.time != last_update_time) or (existing.size != file_size)

            if needs_update then
                note_content = utils.read(note_path)
                content, links = process_content(note_content)

                if existing != nil then
                    -- Update existing note
                    update_note = string.format(
                        "UPDATE notes SET time='%s', size=%d, content='%s' WHERE subject='%s' AND title='%s';",
                        last_update_time,
                        file_size,
                        content,
                        actual_subject,
                        note_name
                    )
                    table.insert(sql_statements, update_note)
                    -- Clear old connections
                    table.insert(sql_statements, string.format("DELETE FROM connections WHERE source_title='%s' AND source_subject='%s';", note_name, actual_subject))
                    updates_count = updates_count + 1
                else
                    -- Insert new note
                    insert_note = string.format(
                        "INSERT INTO notes (time, size, subject, title, content) VALUES ('%s', %d, '%s', '%s', '%s');",
                        last_update_time,
                        file_size,
                        actual_subject,
                        note_name,
                        content
                    )
                    table.insert(sql_statements, insert_note)
                    inserts_count = inserts_count + 1
                end

                -- Insert connections if any
                if utils.length(links) > 0 then
                    insert_connections = "INSERT INTO connections (source_title, source_subject, target_title, target_subject) VALUES "

                    for _, link in pairs(links) do
                        link_subject = ""
                        if link.subject != nil then
                            link_subject = link.subject
                        end
                        statement_value = string.format(
                            "('%s','%s','%s','%s'), ",
                            note_name,
                            actual_subject,
                            link.title,
                            link_subject
                        )
                        insert_connections = insert_connections .. statement_value
                    end

                    -- Trim trailing comma and finalize
                    insert_connections = string.sub(insert_connections, 1, -3) .. ";"
                    table.insert(sql_statements, insert_connections)
                end
            end
        end
    end

    -- Delete notes that are no longer in the vault
    deletes_count = 0
    for note_key, _ in pairs(existing_notes) do
        if seen_notes[note_key] == nil then
            parts = split(note_key, "||")
            subject = parts[1]
            title = parts[2]
            
            table.insert(sql_statements, string.format("DELETE FROM notes WHERE subject='%s' AND title='%s';", subject, title))
            table.insert(sql_statements, string.format("DELETE FROM connections WHERE source_title='%s' AND source_subject='%s';", title, subject))
            deletes_count = deletes_count + 1
        end
    end

    if #sql_statements > 0 then
        table.insert(sql_statements, 1, "BEGIN TRANSACTION;")
        table.insert(sql_statements, "COMMIT;")
        transaction_sql = table.concat(sql_statements, "\n")
        status = database.sqlite_update(brain_file, transaction_sql)
        if status == nil then
            return nil
        end
    end
    
    return "success"
end

vault_update.parse_links_str = parse_links_str
vault_update.process_content = process_content
vault_update.vault_to_sql = vault_to_sql

-- Export the module
return vault_update
