-- Define a module table
local note = {}

local lfs = require("lfs")
local get_vault_path = require("bx_utils").get_vault_path
local user = require("user")

local function insert_note(brain_file, subject, title, content)
    local insert_statement = "INSERT INTO notes ('subject', 'name', 'content') VALUES ('" .. subject .. "', '" .. title .. "', '" .. table.concat(content, "\n") .. "');"
    local status = local_update(brain_file, insert_statement)
    if not status then
        print("Failed to update database")
        return nil
    end
    return "success"
end

local function append_content(brain_file, subject, title, content)
    local query = string.format("SELECT content FROM notes WHERE name='%s' AND subject='%s';", title, subject)
    local result = local_query(brain_file, query)
    if not result then
        print("Failed to query note")
        return nil
    end
    local new_content = result[1].content .. "\n" .. table.concat(content, "\n")

    local update_statement = string.format("UPDATE notes SET content='%s' WHERE name='%s' AND subject='%s';", new_content, title, subject)

    local status = local_update(brain_file, update_statement)
    if not status then
        print("Failed to update database")
        return nil
    end
    return "success"
end

local function connect_notes(brain_file, source, links)
    local insert_statement = "INSERT OR IGNORE INTO connections (source, target) VALUES "
    for index, target in pairs(links) do
        local statement_value = "('" .. source .. "', '" .. target .. "'), "
        insert_statement = insert_statement .. statement_value
    end
    insert_statement = slice(insert_statement, 1, length(insert_statement)-2) .. ";"

    local status = local_update(brain_file, insert_statement)
    if not status then
        print("Failed to connect notes")
        return nil
    end
    return "success"
end

local function write_note(vault_dir, subject, title, content, links)
    local obsidian_links = {}
    for index, link in pairs(links) do
        table.insert(obsidian_links, "[[" .. link .. "]] ")
    end

    local note_dir = vault_dir .. "/" .. subject
    local note_path = note_dir .. "/" .. title .. ".md"

    -- Ensure the directory exists
    local mkdir_status = lfs.mkdir(note_dir)
    if not mkdir_status and not lfs.attributes(note_dir, "mode") then
        print("Could not create directory: " .. note_dir)
        return
    end
    
    local note_file = io.open(note_path, "a")
    if not note_file then
        print("Error: Could not open file: " .. note_path)
        return
    end

    local to_write = table.concat(content, "\n") .. "\n" .. table.concat(obsidian_links, "\n") .. "\n"
    note_file:write(to_write)
    note_file:close()
    return "success"
end

local function take_note(brain_file)
    -- get note info
    local subject = user.input("Subject: ")
    local title = user.input("Title: ")
    local content = user.inputs("Content: ")
    local links = user.inputs("Links: ")

    local vault_dir = get_vault_path()

    if not isempty(content) then
        local insert_status = insert_note(brain_file, subject, title, content)
        if not insert_status then
            print("Error: note insertion failed")
            return
        end
    end
    
    if not isempty(links) then
        local connect_status = connect_notes(brain_file, title, links)
        if not connect_status then
            print("Error: notes connection failed")
            return
        end
    end

    if vault_dir then
        local write_status = write_note(vault_dir, subject, title, content, links)
        if not write_status then
            print("Error: note writing to file failed")
            return
        end
    end

    return "success"
end

local function edit_note(brain_file)
    local subject = user.input("Subject: ")
    local title = user.input("Title: ")
    local default_editor = get_default_editor()
    local vault_path = get_vault_path()

	if subject == "" and title == "" then
    	subject = "daily"
    	title = os.date("%Y-%m-%d")
	end
	
    local editor = default_editor or "nano"

    local note_path = vault_path .. "/" .. subject .. "/" .. title .. ".md"
    if not lfs.attributes(note_path) then
        print("Note file does not exist: " .. note_path)
        return
    end

    local success = os.execute(string.format("'%s' '%s'", editor, note_path))
    if not success then
        print("Failed to open editor")
        return
    end

    success = update_note_from_file(brain_file, note_path)
    if not success then
        print("Failed to edit note in brain file")
        return
    end

    return "success"
end


-- local function update_note(brain_file)
--     -- get note info
--     local subject = user.input("Subject: ")
--     local title = user.input("Title: ")
--     local content = user.inputs("Content: ")
--     local links = user.inputs("Links: ")
-- 
--     local vault_dir = get_vault_path()
-- 
--     if not isempty(content) then
--         local append_status = append_content(brain_file, subject, title, content)
--         if not append_status then
--             print("Error: append content failed")
--             return
--         end
--     end
--     
--     if not isempty(links) then
--         local connect_status = connect_notes(brain_file, title, links)
--         if not connect_status then
--             print("Error: notes connection failed")
--             return
--         end
--     end
-- 
--     if vault_dir then
--         local write_status = write_note(vault_dir, subject, title, content, links)
--         if not write_status then
--             print("Error: note writing to file failed")
--             return
--         end
--     end
-- 
--     return "success"
-- end

local function last_notes(brain_file)
    local subject = user.input("Subject: ")
    local num = user.input("Number of entries: ")
    print("") -- new line

    if subject == "" then
        subject = "daily"
    end

    if num == "" then
        num = "5"
    end

    local query = string.format("SELECT name, content FROM notes WHERE subject='%s' ORDER BY name DESC LIMIT %s", subject, num)

    local notes_empty = is_sqlite_empty(brain_file, "notes")
    if notes_empty then
        print("No notes available")
        return
    end

    local result = local_query(brain_file, query)
    if length(result) > 0 then
        -- view(result)
        for i, note in pairs(result) do
            bold(note.name)
            print(note.content .. "\n")
        end
    else
        print("No notes available")
    end
end

local function todays_note(brain_file)
    -- Get today's date in the format "YYYY-MM-DD"
    local today_date = os.date("%Y-%m-%d")
    local subject = "daily"
    local title = today_date

    local content = user.inputs("Content: ")
    local links = user.inputs("Links: ")

    local vault_dir = get_vault_path()

    -- Check if the note exists
    local query = string.format("SELECT COUNT(*) AS count FROM notes WHERE name='%s' AND subject='%s';", title, subject)
    local result = local_query(brain_file, query)
    if not result then
        print("Failed to query note database")
        return
    end

    local note_exists = tonumber(result[1].count) > 0

    -- If the note exists, update it; otherwise, create a new one
    if not isempty(content) then
        if note_exists then
            local append_status = append_content(brain_file, subject, title, content)
            if not append_status then
                print("Error: append content failed")
                return
            end
        else
            local insert_status = insert_note(brain_file, subject, title, content)
            if not insert_status then
                print("Error: note insertion failed")
                return
            end
        end
    end

    if not isempty(links) then
        local connect_status = connect_notes(brain_file, title, links)
        if not connect_status then
            print("Error: notes connection failed")
            return
        end
    end

    if vault_dir then
        local write_status = write_note(vault_dir, subject, title, content, links)
        if not write_status then
            print("Error: note writing to file failed")
            return
        end
    end

    return "success"
end


note.take_note = take_note
-- note.update_note = update_note
note.edit_note = edit_note
note.last_notes = last_notes
note.todays_note = todays_note

-- Export the module
return note
