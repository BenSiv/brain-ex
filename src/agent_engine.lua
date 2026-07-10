-- src/agent_engine.lua
agent_engine = {}

config = require("config")
paths = require("paths")
joinpath = paths.joinpath
database = require("database")
bridge = require("agent_tools.bridge")
knowledge_pool = require("knowledge_pool")



function estimate_tokens(text)
    if text == nil then return 0 end
    return math.ceil(#text / 4)
end

function agent_engine.get_provider_config()
    return config.get_agent_config()
end

function agent_engine.run_agent(subagent, prompt, brain_file)
    provider_name, model_name = agent_engine.get_provider_config()
    
    status, provider = pcall(require, "agent_providers." .. provider_name)
    if status == false then
        status, provider = pcall(require, provider_name)
    end

    if status == false or provider == nil then
        print("Error: Could not load provider '" .. provider_name .. "': " .. tostring(provider))
        return "error"
    end
    
    system_prompt = ""
    if not (subagent == nil or subagent == "") then
        p_status, loaded_prompt = pcall(require, "agents." .. subagent)
        
        if p_status == false then
            p_status_fallback, loaded_prompt_fallback = pcall(require, subagent)
            if p_status_fallback then
                p_status = true
                loaded_prompt = loaded_prompt_fallback
            end
        end

        if p_status == true then
            system_prompt = loaded_prompt
        else
            missing_module = false
            if type(loaded_prompt) == "string" then
                missing_module = string.match(loaded_prompt, "module '.*' not found") != nil
            end
            if missing_module then
                print("Warning: subagent '" .. subagent .. "' not found.")
            else
                print("Error loading subagent '" .. subagent .. "': " .. tostring(loaded_prompt))
            end
            system_prompt = ""
        end
    end

    print("Running " .. subagent .. " via " .. provider_name .. " (" .. model_name .. ")...")

    -- Ensure tables and default session exist
    knowledge_pool.ensure_table(brain_file)
    check_session = database.local_query(brain_file, "SELECT id FROM agent_sessions WHERE id='default';")
    if check_session == nil or #check_session == 0 then
        database.local_update(brain_file, "INSERT INTO agent_sessions (id, name) VALUES ('default', 'Default Session');")
        agent_engine.backup_agent_data(brain_file)
    end

    -- Insert user prompt to DB
    database.local_update(brain_file, "INSERT INTO agent_messages (session_id, role, content, in_context) VALUES ('default', 'user', '%s', 1);", prompt)
    agent_engine.backup_agent_data(brain_file)

    -- Context token size estimation and compaction check
    active_messages = database.local_query(brain_file, "SELECT id, role, content FROM agent_messages WHERE session_id='default' AND in_context=1 ORDER BY id ASC;")
    if active_messages == nil then
        active_messages = {}
    end
    total_tokens = estimate_tokens(system_prompt)
    for _, msg in ipairs(active_messages) do
        msg_content = msg[3]
        if msg.content != nil then
            msg_content = msg.content
        end
        total_tokens = total_tokens + estimate_tokens(msg_content)
    end

    threshold = 4000
    compaction_threshold = config.get_compaction_threshold()
    if compaction_threshold != nil then
        threshold = compaction_threshold
    end
    if total_tokens > threshold and #active_messages > 4 then
        to_compact = {}
        for i = 1, #active_messages - 4 do
            table.insert(to_compact, active_messages[i])
        end

        summary_prompt = "You are a context compaction engine. Please summarize the following conversation history into a concise, structured Markdown summary of goals, key information established, and progress. Focus on preserving factual details and state, so that a future model invocation has all the necessary context. Keep the summary under 300 words.\n\nConversation to summarize:\n"
        for _, msg in ipairs(to_compact) do
            role = msg[2]
            if msg.role != nil then
                role = msg.role
            end
            content = msg[3]
            if msg.content != nil then
                content = msg.content
            end
            summary_prompt = summary_prompt .. string.upper(role) .. ": " .. content .. "\n"
        end

        print("[Compaction] Active token count (" .. total_tokens .. ") exceeds threshold (" .. threshold .. "). Compacting history...")
        compaction_summary, comp_err = provider.generate(model_name, "You are a concise summarizer.", summary_prompt)
        if compaction_summary != nil and comp_err == nil then
            -- Save summary
            database.local_update(brain_file, "INSERT INTO agent_messages (session_id, role, content, in_context) VALUES ('default', 'compaction_summary', '%s', 1);", compaction_summary)
            -- Mark old as out of context (zero-deletion)
            ids_to_update = {}
            for _, msg in ipairs(to_compact) do
                mid = msg[1]
                if msg.id != nil then
                    mid = msg.id
                end
                table.insert(ids_to_update, tostring(mid))
            end
            database.local_update(brain_file, "UPDATE agent_messages SET in_context=0 WHERE id IN (" .. table.concat(ids_to_update, ",") .. ");")
            print("[Compaction] History compacted successfully.")
            agent_engine.backup_agent_data(brain_file)
        else
            print("[Compaction Warning] Failed to generate compaction summary: " .. tostring(comp_err))
        end
    end

    -- Run multi-turn loops up to 10 turns
    for turn = 1, 10 do
        history_parts = {}
        current_active = database.local_query(brain_file, "SELECT role, content FROM agent_messages WHERE session_id='default' AND in_context=1 ORDER BY id ASC;")
        if current_active == nil then
            current_active = {}
        end
        for _, msg in ipairs(current_active) do
            role = msg[1]
            if msg.role != nil then
                role = msg.role
            end
            content = msg[2]
            if msg.content != nil then
                content = msg.content
            end
            if role == "compaction_summary" then
                table.insert(history_parts, "[COMPACTED HISTORY SUMMARY]:\n" .. content)
            elseif role == "user" then
                table.insert(history_parts, "User: " .. content)
            elseif role == "assistant" then
                table.insert(history_parts, "Assistant: " .. content)
            elseif role == "tool_result" then
                table.insert(history_parts, "Tool Output:\n" .. content)
            end
        end
        current_prompt = table.concat(history_parts, "\n\n")

        result, err = provider.generate(model_name, system_prompt, current_prompt)
        if err != nil then
            print("Error: " .. err)
            return "error"
        end

        if result == nil then
            print("Error: Agent returned no result.")
            return "error"
        end

        -- Record assistant reply in database
        database.local_update(brain_file, "INSERT INTO agent_messages (session_id, role, content, in_context) VALUES ('default', 'assistant', '%s', 1);", result)
        agent_engine.backup_agent_data(brain_file)

        done_message = string.match(result, "<done>%s*(.-)%s*</done>")
        if done_message != nil then
            print("Agent reply:\n" .. done_message)
            return "success"
        end

        tool_name = string.match(result, "<tool>%s*(.-)%s*</tool>")
        method_name = string.match(result, "<method>%s*(.-)%s*</method>")
        args_str = string.match(result, "<args>%s*(.-)%s*</args>")

        if tool_name == nil or method_name == nil then
            print("Agent reply:\n" .. result)
            return "success"
        end

        print("Agent requested tool: " .. tool_name .. "." .. method_name)
        args = {}
        if args_str != nil then
            for line in string.gmatch(args_str, "[^\r\n]+") do
                k, v = string.match(line, "^(.-)=(.*)$")
                if k != nil and v != nil then
                    args[k] = v
                end
            end
        end

        tool_result, tool_err = bridge.dispatch(brain_file, tool_name, method_name, args)

        tool_summary = tostring(tool_result)
        if tool_err != nil then
            tool_summary = "ERROR: " .. tostring(tool_err)
        elseif tool_result == true then
            tool_summary = "ok"
        end

        -- Record tool result in database
        database.local_update(brain_file, "INSERT INTO agent_messages (session_id, role, content, in_context) VALUES ('default', 'tool_result', '%s', 1);", tool_summary)
        agent_engine.backup_agent_data(brain_file)
    end

    print("Agent reply:\nUnable to complete tool-assisted run in 10 turns.")
    return "success"
end

function agent_engine.process_tasks(brain_file)
    query = "SELECT id, subject, content FROM tasks WHERE owner = 'agent' AND done IS NULL;"
    result = database.local_query(brain_file, query)
    if result == nil or #result == 0 then
        print("No pending tasks for agent.")
        return "success"
    end
    for _, task in ipairs(result) do
        task_id = task[1]
        if task.id != nil then
            task_id = task.id
        end
        task_subject = task[2]
        if task.subject != nil then
            task_subject = task.subject
        end
        task_content = ""
        if task[3] != nil then
            task_content = task[3]
        end
        if task.content != nil then
            task_content = task.content
        end
        prompt = "Please handle task. Subject: " .. task_subject .. "\nContent: " .. task_content
        print("Processing task " .. task_id .. "...")
        agent_engine.run_agent("worker", prompt, brain_file)
        database.local_update(brain_file, "UPDATE tasks SET done = datetime('now') WHERE id = '" .. task_id .. "';")
    end
    return "success"
end

function agent_engine.backup_agent_data(brain_file)
    vault_path = config.get_vault_path()
    if vault_path != nil then
        paths_mod = require("paths")
        sessions_dir = joinpath(vault_path, "agent_sessions")
        paths_mod.create_dir_if_not_exists(sessions_dir)
        
        all_sessions = database.local_query(brain_file, "SELECT id, name, created_at, updated_at FROM agent_sessions;")
        if all_sessions == nil then
            all_sessions = {}
        end

        seen_files = {}
        for _, sess in ipairs(all_sessions) do
            sess_id = sess[1]
            if sess.id != nil then
                sess_id = sess.id
            end
            sess_name = sess[2]
            if sess.name != nil then
                sess_name = sess.name
            end
            created_at = sess[3]
            if sess.created_at != nil then
                created_at = sess.created_at
            end
            updated_at = sess[4]
            if sess.updated_at != nil then
                updated_at = sess.updated_at
            end

            msgs = database.local_query(brain_file, string.format("SELECT role, content, metadata, in_context, created_at FROM agent_messages WHERE session_id='%s' ORDER BY id ASC;", sess_id))
            if msgs == nil then
                msgs = {}
            end
            
            session_meta = {
                id = sess_id,
                name = sess_name,
                created_at = created_at,
                updated_at = updated_at
            }
            
            bx_utils = require("bx_utils")
            markdown_content = bx_utils.serialize_session(session_meta, msgs)
            
            filename = sess_id .. ".md"
            file_path = joinpath(sessions_dir, filename)
            seen_files[filename] = true
            
            f = io.open(file_path, "w")
            if f != nil then
                io.write(f, markdown_content)
                io.close(f)
                
                database.local_update(brain_file, "CREATE TABLE IF NOT EXISTS sync_meta (key TEXT PRIMARY KEY, value TEXT);")
                lfs_mod = require("lfs")
                attr = lfs_mod.attributes(file_path)
                if attr != nil then
                    database.local_update(brain_file, string.format(
                        "INSERT OR REPLACE INTO sync_meta (key, value) VALUES ('session_file_mod_%s', '%s');",
                        filename,
                        tostring(attr.modification)
                    ))
                    database.local_update(brain_file, string.format(
                        "INSERT OR REPLACE INTO sync_meta (key, value) VALUES ('session_file_size_%s', '%s');",
                        filename,
                        tostring(attr.size)
                    ))
                    database.local_update(brain_file, string.format(
                        "INSERT OR REPLACE INTO sync_meta (key, value) VALUES ('session_id_for_%s', '%s');",
                        filename,
                        tostring(sess_id)
                    ))
                end
            end
        end
        
        files = readdir(sessions_dir)
        if files == nil then
            files = {}
        end
        for _, file in ipairs(files) do
            if string.match(file, "%.md$") != nil and seen_files[file] == nil then
                os.remove(joinpath(sessions_dir, file))
            end
        end
    end
    return true
end

return agent_engine
