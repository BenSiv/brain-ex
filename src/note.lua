-- Define a module table
local note = {}

local lfs = require("lfs")

-- local function proccess_links_str(links_str)
--     links = split(links_str, ",")
--     for idx,link in pairs(links) do
--         links[idx] = strip(link)
--     end
--     return links
-- end

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
        
local function insert_note(brain_file, subject, title, content)
    local insert_statement = "INSERT INTO notes ('subject', 'title', 'content') VALUES ('" .. subject .. "', '" .. title .. "', '" .. content .. "');"
    local status = local_update(brain_file, insert_statement)
    if not status then
        print("Failed to update database")
        return nil
    end
    return "success"
end

local function append_content(brain_file, subject, title, content)
    local query = string.format("SELECT content FROM notes WHERE title='%s' AND subject='%s';", title, subject)
    local result = local_query(brain_file, query)
    if not result then
        print("Failed to query note")
        return nil
    end
    local new_content = result[1].content .. "\n" .. content

    local update_statement = string.format("UPDATE notes SET content='%s' WHERE title='%s' AND subject='%s';", new_content, title, subject)

    local status = local_update(brain_file, update_statement)
    if not status then
        print("Failed to update database")
        return nil
    end
    return "success"
end

local function connect_notes(brain_file, source_title, source_subject, links)
    if isempty(links) then
        return "success"
    end

    local insert_statement = "INSERT OR IGNORE INTO connections (source_title, source_subject, target_title, target_subject) VALUES "

    for _, link in pairs(links) do
        local target_title = link.title
        local target_subject = link.subject or ""

        local statement_value = string.format(
            "('%s','%s','%s','%s'), ",
            source_title,
            source_subject or "",
            target_title,
            target_subject
        )
        insert_statement = insert_statement .. statement_value
    end

    -- remove trailing comma + space and add semicolon
    insert_statement = insert_statement:sub(1, -3) .. ";"

    local status = local_update(brain_file, insert_statement)
    if not status then
        print("Failed to connect notes")
        return nil
    end
    return "success"
end

local function write_note(vault_dir, subject, title, content, links)
    local obsidian_links = {}
    for _, link in pairs(links) do
        -- each link is a table {title=..., subject=...}
        local link_path
        if link.subject ~= "" then
            link_path = link.subject .. "/" .. link.title
        else
            link_path = link.title
        end
        table.insert(obsidian_links, "[[" .. link_path .. "]]")
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

    local to_write = content .. "\n" .. table.concat(obsidian_links, "\n") .. "\n"
    note_file:write(to_write)
    note_file:close()
    return "success"
end

local function take_note(brain_file, args)
    local subject = args["subject"] or ""
    local title = args["title"] or ""
    local content = args["content"] or ""
    local links_str = args["links"] or ""
    local links = parse_links_str(links_str)

    local vault_path = get_vault_path()

    if title == "" then
        print("Must provide note title")
        return
    end

    if content == "" then
        print("Must provide note content")
        return
    end

    local insert_status = insert_note(brain_file, subject, title, content)
    if not insert_status then
        print("Error: note insertion failed")
        return
    end
    
    if not isempty(links) then
        local connect_status = connect_notes(brain_file, title, subject, links)
        if not connect_status then
            print("Error: notes connection failed")
            return
        end
    end

    if vault_path then
        local write_status = write_note(vault_path, subject, title, content, links)
        if not write_status then
            print("Error: note writing to file failed")
            return
        end
    end

    return "success"
end

local function edit_note(brain_file, args)
    local subject = args["subject"] or ""
    local title = args["title"] or ""
    local editor = get_default_editor()
    local vault_path = get_vault_path()

	if subject == "" and title == "" then
    	subject = "daily"
    	title = os.date("%Y-%m-%d")
	end
	
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

local function last_notes(brain_file, args)
    local subject = args["subject"] or "daily"
    local num = args["number"] or 5

    local query = string.format("SELECT title, content FROM notes WHERE subject='%s' ORDER BY title DESC LIMIT %s", subject, num)
    local result = local_query(brain_file, query)
    if not result then
        print("Failed to query notes")
        return
    end
    if length(result) > 0 then
        -- view(result)
        for i, note in pairs(result) do
            bold(note.title)
            print(note.content .. "\n")
        end
    else
        print("No notes available")
    end
end

local function todays_note(brain_file, args)
    -- Get today's date in the format "YYYY-MM-DD"
    local title = os.date("%Y-%m-%d")
    local subject = "daily"
    local content = args["content"] or ""
    local links_str = args["links"] or ""
    links = parse_links_str(links_str)

    local vault_path = get_vault_path()

    if content == "" then
        print("Must provide note content")
        return
    end

    -- Check if the note exists
    local query = string.format("SELECT COUNT(*) AS count FROM notes WHERE title='%s' AND subject='%s';", title, subject)
    local result = local_query(brain_file, query)
    if not result then
        print("Failed to query note database")
    end

    local note_exists = tonumber(result[1].count) > 0

    -- Insert or append content
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
        local connect_status = connect_notes(brain_file, title, subject, links)
        if not connect_status then
            print("Error: notes connection failed")
            return
        end
    end

    if vault_path then
        local write_status = write_note(vault_path, subject, title, content, links)
        if not write_status then
            print("Error: note writing to file failed")
            return
        end
    end

    return "success"
end

local function do_note_connect(brain_file, args)
    local title = args["title"] or os.date("%Y-%m-%d")
    local subject = args["subject"] or "daily"
    local links_str = args["links"] or ""
    local links = parse_links_str(links_str)

    if isempty(links) then
        print("No links provided to connect.")
        return
    end

    -- Connect in the database
    local status = connect_notes(brain_file, title, subject, links)
    if not status then
        print("Failed to connect notes")
        return
    end

    -- Also append links to the note file
    local vault_path = get_vault_path()
    if vault_path then
        local note_dir = vault_path .. "/" .. subject
        local note_path = note_dir .. "/" .. title .. ".md"

        -- Ensure directory exists
        if not lfs.attributes(note_dir, "mode") then
            lfs.mkdir(note_dir)
        end

        local note_file = io.open(note_path, "a")
        if note_file then
            local obsidian_links = {}
            for _, link in pairs(links) do
                if link.subject ~= "" then
                    table.insert(obsidian_links, "[[" .. link.subject .. "/" .. link.title .. "]]")
                else
                    table.insert(obsidian_links, "[[" .. link.title .. "]]")
                end
            end
            note_file:write(table.concat(obsidian_links, " ") .. "\n")
            note_file:close()
        else
            print("Failed to open note file: " .. note_path)
        end
    end

    return "success"
end

local function do_note(brain_file)
    local arg_string = [[
        -d --do arg string false
        -s --subject arg string false
        -t --title arg string false
        -c --content arg string false
        -l --links arg string false
        -n --number arg number false
    ]]

    local help_string = get_help_string(arg[0])
    local expected_args = def_args(arg_string)
    local args = parse_args(arg, expected_args, help_string)

    if args then
        if args["do"] == "add" then
            take_note(brain_file, args)
        elseif args["do"] == "edit" then
            edit_note(brain_file, args)
        elseif args["do"] == "last" then
            last_notes(brain_file, args)
        elseif args["do"] == "connect" then
            do_note_connect(brain_file, args)
        elseif not args["do"] then
            todays_note(brain_file, args)
        else
            print("Unknown subcommand: " .. args["do"])
            print("Available subcommands: add, edit, last")
        end
    end
end

note.do_note = do_note

if arg[0] == "note.lua" then
    do_note(brain_file)
else
    -- Export the module
    return note
end
