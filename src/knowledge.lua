-- src/knowledge.lua
knowledge = {}

knowledge_pool = require("knowledge_pool")
dataframes = require("dataframes")

function knowledge.search(brain_file, query_str, limit)
    results, retrieval_id = knowledge_pool.search(brain_file, query_str, limit)
    view_rows = {}
    for _, row in ipairs(results) do
        table.insert(view_rows, {
            id = row.id,
            title = row.title,
            subject = row.subject,
            tier = row.tier,
            score = row.score,
            process = row.process_level,
            artifact = row.artifact_status,
            source = row.source_ref
        })
    end
    return view_rows, retrieval_id
end

function knowledge.retrieve(brain_file, query_str)
    results, _ = knowledge.search(brain_file, query_str, 5)
    return results or {}
end

function knowledge.sync(brain_file)
    return knowledge_pool.sync_notes(brain_file)
end

function knowledge.process(brain_file)
    return knowledge_pool.process_items(brain_file)
end

function knowledge.browse(brain_file, limit)
    return knowledge_pool.browse(brain_file, limit)
end

function knowledge.show(brain_file, id)
    item = knowledge_pool.get_item(brain_file, id)
    if item == nil then
        return {}
    end
    return {item}
end

function knowledge.history(brain_file, id)
    return knowledge_pool.history(brain_file, id)
end

function knowledge.queue(brain_file)
    return knowledge_pool.queue(brain_file)
end

function knowledge.promote(brain_file, id, target_tier, artifact_status)
    return knowledge_pool.promote(brain_file, id, target_tier, artifact_status)
end

function knowledge.get_option(args, name, short_name, default_value)
    for i = 1, #args do
        if args[i] == name or args[i] == short_name then
            if args[i + 1] != nil then
                return args[i + 1]
            end
        end
    end
    return default_value
end

function knowledge.query_from_args(args, start_index)
    parts = {}
    limit = 5
    i = start_index
    while i <= #args do
        if args[i] == "--limit" or args[i] == "-n" then
            if args[i + 1] != nil then
                limit = tonumber(args[i + 1]) or limit
            end
            i = i + 2
        else
            table.insert(parts, args[i])
            i = i + 1
        end
    end
    return table.concat(parts, " "), limit
end

function knowledge.print_usage()
    print("Usage: brex knowledge <search|sync|browse|show|history|process|queue|promote> [arguments]")
end

function knowledge.view(rows, columns)
    if rows == nil or #rows == 0 then
        print("Empty table")
        return
    end
    dataframes.view(rows, {columns = columns})
end

function knowledge.item_value(item, key, index, default_value)
    if item[key] != nil then
        return item[key]
    elseif item[index] != nil then
        return item[index]
    end
    return default_value or ""
end

function knowledge.print_item(item)
    if item == nil then
        print("Empty table")
        return
    end

    print("id: " .. tostring(knowledge.item_value(item, "id", 1, "")))
    print("title: " .. tostring(knowledge.item_value(item, "title", 2, "")))
    print("subject: " .. tostring(knowledge.item_value(item, "subject", 3, "")))
    print("tier: " .. tostring(knowledge.item_value(item, "tier", 4, "")))
    print("process_level: " .. tostring(knowledge.item_value(item, "process_level", 5, "")))
    print("retrieval_count: " .. tostring(knowledge.item_value(item, "retrieval_count", 6, "")))
    print("heat: " .. tostring(knowledge.item_value(item, "heat", 7, "")))
    print("source_ref: " .. tostring(knowledge.item_value(item, "source_ref", 9, "")))
    print("artifact_status: " .. tostring(knowledge.item_value(item, "artifact_status", 13, "")))
    print("promotion_status: " .. tostring(knowledge.item_value(item, "promotion_status", 14, "")))
    print("artifact_ref: " .. tostring(knowledge.item_value(item, "artifact_ref", 11, "")))
    print("duplicate_of: " .. tostring(knowledge.item_value(item, "duplicate_of", 15, "")))
    print("merged_into: " .. tostring(knowledge.item_value(item, "merged_into", 16, "")))
    print("content: " .. tostring(knowledge.item_value(item, "content", 17, "")))
end

function do_knowledge(brain_file, cmd_args)
    subcommand = cmd_args[1]

    if subcommand == nil or subcommand == "" or subcommand == "-h" or subcommand == "--help" then
        knowledge.print_usage()
        return "success"
    end

    if subcommand == "search" then
        query_str, limit = knowledge.query_from_args(cmd_args, 2)
        if query_str == nil or query_str == "" then
            print("Usage: brex knowledge search <query> [--limit N]")
            return "error"
        end
        results, retrieval_id = knowledge.search(brain_file, query_str, limit)
        if retrieval_id != nil and retrieval_id > 0 then
            print("retrieval_id: " .. tostring(retrieval_id))
        end
        knowledge.view(results, {"id", "title", "subject", "tier", "score", "process", "artifact", "source"})
    elseif subcommand == "sync" then
        count = knowledge.sync(brain_file)
        print("Synced " .. tostring(count) .. " knowledge items")
    elseif subcommand == "browse" then
        limit = knowledge.get_option(cmd_args, "--limit", "-n", 20)
        results = knowledge.browse(brain_file, limit)
        knowledge.view(results, {"id", "title", "tier", "promotion_status", "source_ref"})
    elseif subcommand == "show" then
        id = cmd_args[2]
        if id == nil then
            print("Usage: brex knowledge show <id>")
            return "error"
        end
        results = knowledge.show(brain_file, id)
        if results == nil or #results == 0 then
            print("Empty table")
        else
            knowledge.print_item(results[1])
        end
    elseif subcommand == "history" then
        id = cmd_args[2]
        results = knowledge.history(brain_file, id)
        if id != nil then
            knowledge.view(results, {"retrieval_id", "query_text", "created_at", "rank", "score"})
        else
            knowledge.view(results, {"id", "query_text", "created_at"})
        end
    elseif subcommand == "process" then
        results = knowledge.process(brain_file)
        knowledge.view(results, {"synced", "duplicate_groups", "reviewed", "ready"})
    elseif subcommand == "queue" then
        results = knowledge.queue(brain_file)
        knowledge.view(results, {"id", "title", "promotion_status", "retrieval_count", "duplicate_of"})
    elseif subcommand == "promote" then
        id = cmd_args[2]
        target_tier = knowledge.get_option(cmd_args, "--tier", "-t", cmd_args[3])
        artifact_status = knowledge.get_option(cmd_args, "--status", "-s", nil)
        artifact_path, err = knowledge.promote(brain_file, id, target_tier, artifact_status)
        if artifact_path == nil then
            print(err or "Promotion failed")
            return "error"
        end
        print("Promoted " .. tostring(id) .. " to " .. artifact_path)
    else
        knowledge.print_usage()
    end

    return "success"
end

knowledge.do_knowledge = do_knowledge

return knowledge
