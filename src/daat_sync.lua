-- Incremental two-way synchronization between a local brex SQLite brain
-- and a daat deployment (/api/v1/document and /api/v1/task).
--
-- Mapping:
--   brex `subject` (non-empty) <-> top-level daat folder document (title = subject)
--   daat "Knowledge Pool" folder <-> brex subject "knowledge-pool" (curated/synthesized/distilled docs)
--   brex `notes` <-> daat `document`
--   brex `tasks` <-> daat `task` (attached via task.document = document.id)
--   brex `connections` <-> `[[...]]` wikilinks in document.content

const bx_utils = require("bx_utils")
const config = require("config")
const database = require("database")
const db = database
const dkjson = require("dkjson")
const ext = require("external_tool")
const help = require("help")
const hmac = require("hmac")
const knowledge_pool = require("knowledge_pool")
const paths = require("paths")
const sql_schema = require("sql_schema")
const task_mod = require("task")

const daat_sync = {}

const PAGE_SIZE = 200
const BATCH_SIZE = 100
const KNOWLEDGE_POOL_FOLDER = "Knowledge Pool"
const KNOWLEDGE_POOL_SUBJECT = "knowledge-pool"

const SYNC_SCHEMA_SQL = """
CREATE TABLE IF NOT EXISTS daat_sync_folders (
    subject TEXT PRIMARY KEY,
    daat_doc_id INTEGER NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS daat_sync_items (
    brex_note_id INTEGER PRIMARY KEY,
    daat_doc_id INTEGER NOT NULL UNIQUE,
    daat_task_id INTEGER,
    synced_doc_hash TEXT NOT NULL,
    synced_task_hash TEXT,
    last_synced_at TEXT
);
"""

function coalesce(a, b)
    if a != nil then
        return a
    end
    return b
end

function task_id_of(task_row)
    if task_row != nil then
        return task_row.id
    end
    return nil
end

function shell_quote(s)
    return "'" .. string.gsub(tostring(s), "'", "'\\''") .. "'"
end

function trim(s)
    if s == nil then
        return ""
    end
    return string.trim(tostring(s))
end

function row_val(row, key, idx, default_val)
    if row == nil then
        return default_val
    end
    if row[key] != nil then
        return row[key]
    end
    if idx != nil and row[idx] != nil then
        return row[idx]
    end
    return default_val
end

function clamp_priority(val)
    num = tonumber(val)
    if num == nil then
        return 1
    end
    floored = math.floor(num)
    if floored < 1 then
        return 1
    end
    if floored > 5 then
        return 5
    end
    return floored
end

function normalize_due_for_daat(due_val)
    if due_val == nil then
        return ""
    end
    str = trim(due_val)
    if str == "" or str == "NULL" then
        return ""
    end
    ymd = string.match(str, "^(%d%d%d%d%-%d%d%-%d%d)")
    if ymd != nil then
        return ymd
    end
    return str
end

function normalize_due_for_brex(due_val)
    ymd = normalize_due_for_daat(due_val)
    if ymd == "" then
        return nil
    end
    if string.match(ymd, "^%d%d%d%d%-%d%d%-%d%d$") != nil then
        return ymd .. " 00:00:00"
    end
    return ymd
end

function compute_overdue(due_for_brex, is_done)
    if is_done == true or due_for_brex == nil or due_for_brex == "" then
        return 0
    end
    today_ymd = os.date("%Y-%m-%d")
    due_ymd = string.sub(due_for_brex, 1, 10)
    if due_ymd < today_ymd then
        return 1
    end
    return 0
end

function doc_hash(subject, title, content)
    subj = coalesce(subject, "")
    ttl = coalesce(title, "")
    body = coalesce(content, "")
    canonical = subj .. "\0" .. ttl .. "\0" .. body
    return hmac.sha256("brex_daat_doc_v1", canonical)
end

function task_hash(task_info)
    if task_info == nil then
        return "none"
    end
    subj = coalesce(task_info.subject, "")
    due_ymd = normalize_due_for_daat(task_info.due_to)
    status = task_info.status
    if status == nil or status == "" then
        if task_info.done != nil and trim(task_info.done) != "" and trim(task_info.done) != "NULL" then
            status = "done"
        else
            status = "open"
        end
    end
    owner = task_info.owner
    if owner == nil or owner == "NULL" then
        owner = ""
    end
    imp = clamp_priority(task_info.importance)
    urg = clamp_priority(task_info.urgency)
    canonical = table.concat({
        status,
        subj,
        due_ymd,
        trim(owner),
        tostring(imp),
        tostring(urg)
    }, "\0")
    return hmac.sha256("brex_daat_task_v1", canonical)
end

function subject_to_folder_title(subject)
    if subject == nil or subject == "" then
        return nil
    end
    if subject == KNOWLEDGE_POOL_SUBJECT then
        return KNOWLEDGE_POOL_FOLDER
    end
    return subject
end

function folder_title_to_subject(folder_title)
    if folder_title == nil or folder_title == "" then
        return ""
    end
    if folder_title == KNOWLEDGE_POOL_FOLDER then
        return KNOWLEDGE_POOL_SUBJECT
    end
    return folder_title
end

function extract_wikilinks(content)
    if content == nil or content == "" then
        return {}
    end
    links = {}
    seen = {}
    in_fence = false
    for line in string.gmatch(content .. "\n", "(.-)\n") do
        if string.match(line, "^%s*```") != nil then
            in_fence = not in_fence
        elseif not in_fence then
            for raw_ref in string.gmatch(line, "%[%[([^%[%]\n]+)%]%]") do
                cleaned = trim(raw_ref)
                if cleaned != "" and seen[cleaned] == nil then
                    seen[cleaned] = true
                    slash_pos = string.find(cleaned, "/", 1, true)
                    if slash_pos != nil then
                        table.insert(links, {
                            subject = string.sub(cleaned, 1, slash_pos - 1),
                            title = string.sub(cleaned, slash_pos + 1),
                            raw = cleaned
                        })
                    else
                        table.insert(links, {
                            subject = "",
                            title = cleaned,
                            raw = cleaned
                        })
                    end
                end
            end
        end
    end
    return links
end

function render_content_with_connections(content, conn_list)
    base = coalesce(content, "")
    if conn_list == nil or #conn_list == 0 then
        return base
    end

    present = {}
    for _, existing in ipairs(extract_wikilinks(base)) do
        present[existing.raw] = true
        if existing.subject == "" then
            present[existing.title] = true
        end
    end

    missing_tags = {}
    for _, conn in ipairs(conn_list) do
        t_sub = coalesce(conn.subject, "")
        t_title = conn.title
        if t_title != nil and t_title != "" then
            full_ref = t_title
            if t_sub != "" then
                full_ref = t_sub .. "/" .. t_title
            end
            if present[full_ref] == nil and present[t_title] == nil then
                present[full_ref] = true
                table.insert(missing_tags, "[[" .. full_ref .. "]]")
            end
        end
    end

    if #missing_tags == 0 then
        return base
    end
    suffix = table.concat(missing_tags, " ")
    if base == "" then
        return suffix
    end
    return base .. "\n\n" .. suffix
end

function replace_note_connections(brain_file, title, subject, content)
    subj = coalesce(subject, "")
    database.exec(
        brain_file,
        string.format("DELETE FROM connections WHERE source_title=%s AND source_subject=%s;", db.quote(title), db.quote(subj))
    )
    links = extract_wikilinks(content)
    if #links == 0 then
        return
    end
    values_sql = {}
    for _, link in ipairs(links) do
        table.insert(values_sql, string.format(
            "(%s, %s, %s, %s)",
            db.quote(title),
            db.quote(subj),
            db.quote(link.title),
            db.quote(coalesce(link.subject, ""))
        ))
    end
    database.exec(
        brain_file,
        "INSERT OR IGNORE INTO connections (source_title, source_subject, target_title, target_subject) VALUES "
            .. table.concat(values_sql, ", ") .. ";"
    )
end

function api_request(client, method, path, query, body_obj)
    payload_str = nil
    if body_obj != nil then
        payload_str = dkjson.encode(body_obj)
    end

    if client.cgi_bin != nil and client.cgi_bin != "" then
        in_tmp = os.tmpname()
        if payload_str != nil then
            paths.write_file(in_tmp, payload_str)
        else
            paths.write_file(in_tmp, "")
        end
        q_str = coalesce(query, "")
        cmd = string.format(
            "cd %s && GATEWAY_INTERFACE='CGI/1.1' REQUEST_METHOD=%s PATH_INFO=%s QUERY_STRING=%s HTTP_X_API_KEY=%s CONTENT_TYPE='application/json' %s < %s",
            shell_quote(client.cgi_dir),
            shell_quote(method),
            shell_quote(path),
            shell_quote(q_str),
            shell_quote(client.api_key),
            shell_quote(client.cgi_bin),
            shell_quote(in_tmp)
        )
        raw_out, ok = ext.exec(cmd)
        os.remove(in_tmp)
        if ok == false or raw_out == nil or raw_out == "" then
            return nil, "CGI invocation failed for " .. method .. " " .. path, 0
        end
        hdr_end_s, hdr_end_e = string.find(raw_out, "\r?\n\r?\n")
        headers_part = ""
        body_part = raw_out
        if hdr_end_s != nil then
            headers_part = string.sub(raw_out, 1, hdr_end_s - 1)
            body_part = string.sub(raw_out, hdr_end_e + 1)
        end
        status_code = 200
        matched_code = string.match(headers_part, "Status:%s*(%d+)")
        if matched_code != nil then
            status_code = tonumber(matched_code)
        end
        decoded, _, jerr = dkjson.decode(body_part)
        if status_code == 404 then
            return nil, "not_found", 404
        end
        if status_code < 200 or status_code >= 300 then
            err_msg = body_part
            if type(decoded) == "table" and decoded.error != nil then
                err_msg = tostring(decoded.error)
            end
            return nil, string.format("HTTP %d on %s %s: %s", status_code, method, path, err_msg), status_code
        end
        if decoded == nil then
            return nil, "Invalid JSON from CGI " .. path .. ": " .. tostring(jerr), status_code
        end
        return decoded, nil, status_code
    end

    full_url = client.base_url .. path
    if query != nil and query != "" then
        full_url = full_url .. "?" .. query
    end

    cfg_tmp = os.tmpname()
    body_tmp = nil
    os.execute("chmod 600 " .. shell_quote(cfg_tmp))
    escaped_key = string.gsub(client.api_key, "\\", "\\\\")
    escaped_key = string.gsub(escaped_key, "\"", "\\\"")
    cfg_lines = {
        string.format("url = %q", full_url),
        string.format("request = %q", method),
        string.format("header = \"X-Api-Key: %s\"", escaped_key),
    }
    if payload_str != nil then
        body_tmp = os.tmpname()
        paths.write_file(body_tmp, payload_str)
        table.insert(cfg_lines, "header = \"Content-Type: application/json\"")
        table.insert(cfg_lines, string.format("data-binary = %q", "@" .. body_tmp))
    elseif method == "POST" or method == "PATCH" then
        table.insert(cfg_lines, "data-binary = \"\"")
    end
    paths.write_file(cfg_tmp, table.concat(cfg_lines, "\n") .. "\n")

    curl_cmd = string.format("curl -sS --max-time 60 -w '\\n%%{http_code}' -K %s", shell_quote(cfg_tmp))
    raw_resp, curl_ok = ext.exec(curl_cmd)
    os.remove(cfg_tmp)
    if body_tmp != nil then
        os.remove(body_tmp)
    end

    if curl_ok == false or raw_resp == nil or raw_resp == "" then
        return nil, "Network error calling " .. method .. " " .. path, 0
    end

    last_nl = nil
    search_pos = 1
    while true do
        nl_idx = string.find(raw_resp, "\n", search_pos, true)
        if nl_idx == nil then
            break
        end
        last_nl = nl_idx
        search_pos = nl_idx + 1
    end

    http_code = 0
    resp_body = raw_resp
    if last_nl != nil then
        parsed_code = tonumber(trim(string.sub(raw_resp, last_nl + 1)))
        http_code = coalesce(parsed_code, 0)
        resp_body = string.sub(raw_resp, 1, last_nl - 1)
    end

    decoded_resp, _, decode_err = dkjson.decode(resp_body)
    if http_code == 404 then
        return nil, "not_found", 404
    end
    if http_code < 200 or http_code >= 300 then
        msg = resp_body
        if type(decoded_resp) == "table" and decoded_resp.error != nil then
            msg = tostring(decoded_resp.error)
        end
        return nil, string.format("HTTP %d on %s %s: %s", http_code, method, path, msg), http_code
    end
    if decoded_resp == nil then
        return nil, "Invalid JSON from " .. path .. ": " .. tostring(decode_err), http_code
    end
    return decoded_resp, nil, http_code
end

function format_api_issues(issues)
    if type(issues) != "table" or #issues == 0 then
        return "validation failed"
    end
    parts = {}
    for _, iss in ipairs(issues) do
        prefix = ""
        if iss.field != nil and iss.field != "" then
            prefix = tostring(iss.field) .. ": "
        end
        if iss.index != nil then
            prefix = "[#" .. tostring(iss.index) .. "] " .. prefix
        end
        table.insert(parts, prefix .. tostring(coalesce(iss.message, "error")))
    end
    return table.concat(parts, "; ")
end

function fetch_all_active(client, entity_type, allow_missing_type)
    all_rows = {}
    offset = 0
    while true do
        query = string.format("limit=%d&offset=%d", PAGE_SIZE, offset)
        res, err, code = api_request(client, "GET", "/api/v1/" .. entity_type, query, nil)
        if res == nil then
            if allow_missing_type == true and code == 404 then
                return {}, false
            end
            return nil, err
        end
        rows = coalesce(res.rows, {})
        for _, r in ipairs(rows) do
            table.insert(all_rows, r)
        end
        total = coalesce(tonumber(res.total), #all_rows)
        offset = offset + #rows
        if #rows == 0 or offset >= total then
            break
        end
    end
    return all_rows, true
end

function ensure_sync_schema(brain_file)
    database.exec(brain_file, sql_schema.sql_init)
    task_mod.ensure_schema(brain_file)
    database.exec(brain_file, SYNC_SCHEMA_SQL)

    missing_rows = database.query(brain_file, "SELECT rowid, subject, title FROM notes WHERE id IS NULL;")
    if missing_rows != nil then
        for _, mr in ipairs(missing_rows) do
            rid = row_val(mr, "rowid", 1, nil)
            new_id = bx_utils.generate_id("notes", nil, nil, brain_file)
            database.exec(
                brain_file,
                string.format("UPDATE notes SET id=%s WHERE rowid=%s;", db.quote(new_id), tostring(rid))
            )
        end
    end
end

function load_local_state(brain_file)
    notes_by_id = {}
    notes_by_key = {}

    n_rows = coalesce(database.query(brain_file, "SELECT id, time, subject, title, content, size FROM notes ORDER BY id ASC;"), {})
    for _, nr in ipairs(n_rows) do
        nid = tonumber(row_val(nr, "id", 1, nil))
        subj = row_val(nr, "subject", 3, "")
        if subj == nil or subj == "NULL" then
            subj = ""
        end
        ttl = row_val(nr, "title", 4, "")
        body = row_val(nr, "content", 5, "")
        if body == nil or body == "NULL" then
            body = ""
        end
        tm = row_val(nr, "time", 2, "")
        item = {
            id = nid,
            time = tm,
            subject = subj,
            title = ttl,
            content = body,
            task = nil,
            connections = {}
        }
        notes_by_id[nid] = item
        notes_by_key[subj .. "\0" .. ttl] = item
    end

    c_rows = coalesce(database.query(brain_file, "SELECT source_title, source_subject, target_title, target_subject FROM connections;"), {})
    for _, cr in ipairs(c_rows) do
        s_ttl = row_val(cr, "source_title", 1, "")
        s_sub = row_val(cr, "source_subject", 2, "")
        if s_sub == nil or s_sub == "NULL" then
            s_sub = ""
        end
        t_ttl = row_val(cr, "target_title", 3, "")
        t_sub = row_val(cr, "target_subject", 4, "")
        if t_sub == nil or t_sub == "NULL" then
            t_sub = ""
        end
        owner_note = notes_by_key[s_sub .. "\0" .. s_ttl]
        if owner_note != nil then
            table.insert(owner_note.connections, {title = t_ttl, subject = t_sub})
        end
    end

    t_rows = coalesce(database.query(brain_file, "SELECT item_id, due_to, overdue, done, owner, importance, urgency FROM tasks;"), {})
    for _, tr in ipairs(t_rows) do
        tid = tonumber(row_val(tr, "item_id", 1, nil))
        owner_note = notes_by_id[tid]
        if owner_note != nil then
            done_raw = row_val(tr, "done", 4, nil)
            if done_raw != nil and (trim(done_raw) == "" or trim(done_raw) == "NULL") then
                done_raw = nil
            end
            due_raw = row_val(tr, "due_to", 2, nil)
            if due_raw != nil and (trim(due_raw) == "" or trim(due_raw) == "NULL") then
                due_raw = nil
            end
            own_raw = row_val(tr, "owner", 5, nil)
            if own_raw != nil and (trim(own_raw) == "" or trim(own_raw) == "NULL") then
                own_raw = nil
            end
            st = "open"
            if done_raw != nil then
                st = "done"
            end
            owner_note.task = {
                due_to = due_raw,
                overdue = coalesce(tonumber(row_val(tr, "overdue", 3, 0)), 0),
                done = done_raw,
                status = st,
                owner = own_raw,
                importance = clamp_priority(row_val(tr, "importance", 6, 1)),
                urgency = clamp_priority(row_val(tr, "urgency", 7, 1)),
                subject = owner_note.subject
            }
        end
    end

    sync_folders = {}
    folder_doc_to_subj = {}
    f_rows = coalesce(database.query(brain_file, "SELECT subject, daat_doc_id FROM daat_sync_folders;"), {})
    for _, fr in ipairs(f_rows) do
        f_sub = row_val(fr, "subject", 1, "")
        f_did = tonumber(row_val(fr, "daat_doc_id", 2, nil))
        if f_sub != "" and f_did != nil then
            sync_folders[f_sub] = f_did
            folder_doc_to_subj[f_did] = f_sub
        end
    end

    sync_items = {}
    s_rows = coalesce(database.query(
        brain_file,
        "SELECT brex_note_id, daat_doc_id, daat_task_id, synced_doc_hash, synced_task_hash FROM daat_sync_items;"
    ), {})
    for _, sr in ipairs(s_rows) do
        bnid = tonumber(row_val(sr, "brex_note_id", 1, nil))
        ddid = tonumber(row_val(sr, "daat_doc_id", 2, nil))
        dtid_raw = row_val(sr, "daat_task_id", 3, nil)
        dtid = nil
        if dtid_raw != nil and tostring(dtid_raw) != "" and tostring(dtid_raw) != "NULL" then
            dtid = tonumber(dtid_raw)
        end
        table.insert(sync_items, {
            brex_note_id = bnid,
            daat_doc_id = ddid,
            daat_task_id = dtid,
            synced_doc_hash = row_val(sr, "synced_doc_hash", 4, ""),
            synced_task_hash = row_val(sr, "synced_task_hash", 5, "none")
        })
    end

    return {
        notes_by_id = notes_by_id,
        notes_by_key = notes_by_key,
        sync_folders = sync_folders,
        folder_doc_to_subj = folder_doc_to_subj,
        sync_items = sync_items
    }
end

function save_sync_folder(brain_file, subject, daat_doc_id)
    database.exec(
        brain_file,
        string.format(
            "INSERT INTO daat_sync_folders (subject, daat_doc_id) VALUES (%s, %d) "
            .. "ON CONFLICT(subject) DO UPDATE SET daat_doc_id = excluded.daat_doc_id;",
            db.quote(subject), tonumber(daat_doc_id)
        )
    )
end

function save_sync_item(brain_file, brex_note_id, daat_doc_id, daat_task_id, d_hash, t_hash)
    task_id_sql = "NULL"
    if daat_task_id != nil then
        task_id_sql = tostring(tonumber(daat_task_id))
    end
    database.exec(
        brain_file,
        string.format(
            "INSERT INTO daat_sync_items (brex_note_id, daat_doc_id, daat_task_id, synced_doc_hash, synced_task_hash, last_synced_at) "
            .. "VALUES (%d, %d, %s, %s, %s, datetime('now', 'localtime')) "
            .. "ON CONFLICT(brex_note_id) DO UPDATE SET "
            .. "daat_doc_id = excluded.daat_doc_id, "
            .. "daat_task_id = excluded.daat_task_id, "
            .. "synced_doc_hash = excluded.synced_doc_hash, "
            .. "synced_task_hash = excluded.synced_task_hash, "
            .. "last_synced_at = excluded.last_synced_at;",
            tonumber(brex_note_id), tonumber(daat_doc_id), task_id_sql, db.quote(d_hash), db.quote(coalesce(t_hash, "none"))
        )
    )
end

function delete_sync_item(brain_file, brex_note_id)
    database.exec(
        brain_file,
        string.format("DELETE FROM daat_sync_items WHERE brex_note_id = %d;", tonumber(brex_note_id))
    )
end

function disambiguate_local_title(brain_file, subject, desired_title, own_note_id, fallback_suffix)
    candidate = desired_title
    if candidate == nil or candidate == "" then
        candidate = "(untitled " .. tostring(fallback_suffix) .. ")"
    end
    check_sql = nil
    if own_note_id != nil then
        check_sql = string.format(
            "SELECT COUNT(*) AS cnt FROM notes WHERE subject=%s AND title=%s AND id != %d;",
            db.quote(subject), db.quote(candidate), tonumber(own_note_id)
        )
    else
        check_sql = string.format(
            "SELECT COUNT(*) AS cnt FROM notes WHERE subject=%s AND title=%s;",
            db.quote(subject), db.quote(candidate)
        )
    end
    rows = database.query(brain_file, check_sql)
    cnt = 0
    if rows != nil and rows[1] != nil then
        cnt = coalesce(tonumber(row_val(rows[1], "cnt", 1, 0)), 0)
    end
    if cnt > 0 then
        candidate = candidate .. " [" .. tostring(fallback_suffix) .. "]"
    end
    return candidate
end

function apply_remote_note_to_brex(brain_file, existing_note, subject, title, content)
    body = coalesce(content, "")
    if existing_note != nil then
        safe_title = disambiguate_local_title(brain_file, subject, title, existing_note.id, existing_note.id)
        if existing_note.title != safe_title or existing_note.subject != subject then
            database.exec(
                brain_file,
                string.format(
                    "DELETE FROM connections WHERE source_title=%s AND source_subject=%s;",
                    db.quote(existing_note.title), db.quote(existing_note.subject)
                )
            )
        end
        database.exec(
            brain_file,
            string.format(
                "UPDATE notes SET subject=%s, title=%s, content=%s, size=%d, time=datetime('now', 'localtime') WHERE id=%d;",
                db.quote(subject), db.quote(safe_title), db.quote(body), string.len(body), tonumber(existing_note.id)
            )
        )
        replace_note_connections(brain_file, safe_title, subject, body)
        return existing_note.id, safe_title
    end

    new_id = tonumber(bx_utils.generate_id("notes", nil, nil, brain_file))
    safe_title = disambiguate_local_title(brain_file, subject, title, nil, new_id)
    database.exec(
        brain_file,
        string.format(
            "INSERT INTO notes (id, time, subject, title, content, size) VALUES (%d, datetime('now', 'localtime'), %s, %s, %s, %d);",
            new_id, db.quote(subject), db.quote(safe_title), db.quote(body), string.len(body)
        )
    )
    replace_note_connections(brain_file, safe_title, subject, body)
    return new_id, safe_title
end

function apply_remote_task_to_brex(brain_file, brex_note_id, existing_task, remote_task)
    if remote_task == nil then
        if existing_task != nil then
            database.exec(
                brain_file,
                string.format("DELETE FROM tasks WHERE item_id=%d;", tonumber(brex_note_id))
            )
        end
        return
    end

    due_brex = normalize_due_for_brex(remote_task.due_to)
    is_done = (remote_task.status == "done")
    done_val = nil
    if is_done then
        if existing_task != nil and existing_task.done != nil then
            done_val = existing_task.done
        else
            done_val = os.date("%Y-%m-%d %H:%M:%S")
        end
    end
    overdue_val = compute_overdue(due_brex, is_done)
    owner_val = remote_task.owner
    if owner_val != nil and trim(owner_val) == "" then
        owner_val = nil
    end
    imp_val = clamp_priority(remote_task.importance)
    urg_val = clamp_priority(remote_task.urgency)

    database.exec(
        brain_file,
        string.format(
            "INSERT INTO tasks (item_id, due_to, overdue, done, owner, importance, urgency) "
            .. "VALUES (%d, %s, %d, %s, %s, %d, %d) "
            .. "ON CONFLICT(item_id) DO UPDATE SET "
            .. "due_to = excluded.due_to, "
            .. "overdue = excluded.overdue, "
            .. "done = excluded.done, "
            .. "owner = excluded.owner, "
            .. "importance = excluded.importance, "
            .. "urgency = excluded.urgency;",
            tonumber(brex_note_id),
            db.literal(due_brex),
            overdue_val,
            db.literal(done_val),
            db.literal(owner_val),
            imp_val,
            urg_val
        )
    )
end

function delete_local_item(brain_file, note_item)
    database.exec(
        brain_file,
        string.format("DELETE FROM tasks WHERE item_id=%d;", tonumber(note_item.id))
    )
    database.exec(
        brain_file,
        string.format(
            "DELETE FROM connections WHERE source_title=%s AND source_subject=%s;",
            db.quote(note_item.title), db.quote(note_item.subject)
        )
    )
    database.exec(
        brain_file,
        string.format("DELETE FROM notes WHERE id=%d;", tonumber(note_item.id))
    )
end

function build_daat_task_payload(doc_id, subject, local_task, is_patch)
    payload = {
        importance = clamp_priority(local_task.importance),
        urgency = clamp_priority(local_task.urgency),
        status = coalesce(local_task.status, "open")
    }
    if not is_patch then
        payload.document = tonumber(doc_id)
    end
    subj = coalesce(subject, "")
    due_ymd = normalize_due_for_daat(local_task.due_to)
    own = trim(local_task.owner)
    if is_patch then
        payload.subject = subj
        payload.due_to = due_ymd
        payload.owner = own
    else
        if subj != "" then
            payload.subject = subj
        end
        if due_ymd != "" then
            payload.due_to = due_ymd
        end
        if own != "" then
            payload.owner = own
        end
    end
    return payload
end

function ensure_folders_exist(brain_file, client, state, subjects_needed, dry_run, stats)
    missing_subjects = {}
    seen = {}
    for _, subj in ipairs(subjects_needed) do
        if subj != nil and subj != "" and state.sync_folders[subj] == nil and seen[subj] == nil then
            seen[subj] = true
            table.insert(missing_subjects, subj)
        end
    end
    if #missing_subjects == 0 then
        return true, nil
    end
    if dry_run then
        stats.folders_created = stats.folders_created + #missing_subjects
        return true, nil
    end

    idx = 1
    while idx <= #missing_subjects do
        batch = {}
        batch_subjs = {}
        limit_idx = math.min(idx + BATCH_SIZE - 1, #missing_subjects)
        for j = idx, limit_idx do
            s = missing_subjects[j]
            f_title = subject_to_folder_title(s)
            table.insert(batch, {title = f_title})
            table.insert(batch_subjs, s)
        end
        res, err = api_request(client, "POST", "/api/v1/document", nil, batch)
        if res == nil then
            return nil, err
        end
        if res.success != true then
            return nil, "Failed creating subject folders: " .. format_api_issues(res.issues)
        end
        created_ids = res.created_ids
        if created_ids == nil and res.created_id != nil then
            created_ids = {res.created_id}
        end
        for k, fid in ipairs(coalesce(created_ids, {})) do
            s = batch_subjs[k]
            num_fid = tonumber(fid)
            state.sync_folders[s] = num_fid
            state.folder_doc_to_subj[num_fid] = s
            save_sync_folder(brain_file, s, num_fid)
            stats.folders_created = stats.folders_created + 1
        end
        idx = limit_idx + 1
    end
    return true, nil
end

function sync_remote_task_for_item(client, doc_id, subject, local_task, current_daat_task_id, remote_task, task_type_registered, dry_run, stats)
    if task_type_registered != true then
        return current_daat_task_id, nil
    end

    active_tid = current_daat_task_id
    if remote_task != nil then
        active_tid = tonumber(remote_task.id)
    end

    if local_task == nil then
        if remote_task != nil then
            if not dry_run then
                res, err = api_request(client, "POST", "/api/v1/task/" .. tostring(remote_task.id) .. "/archive", "reason=brex+sync", nil)
                if res == nil then
                    return nil, err
                end
            end
            stats.tasks_pushed = stats.tasks_pushed + 1
        end
        return nil, nil
    end

    if remote_task != nil then
        if not dry_run then
            patch_body = build_daat_task_payload(doc_id, subject, local_task, true)
            res, err = api_request(client, "PATCH", "/api/v1/task/" .. tostring(remote_task.id), "reason=brex+sync", patch_body)
            if res == nil then
                return nil, err
            end
            if res.success != true then
                return nil, "Task update failed: " .. format_api_issues(res.issues)
            end
        end
        stats.tasks_pushed = stats.tasks_pushed + 1
        return tonumber(remote_task.id), nil
    end

    if dry_run then
        stats.tasks_pushed = stats.tasks_pushed + 1
        return active_tid, nil
    end

    create_body = build_daat_task_payload(doc_id, subject, local_task, false)
    res, err = api_request(client, "POST", "/api/v1/task", nil, create_body)
    if res == nil then
        return nil, err
    end
    if res.success != true then
        return nil, "Task create failed: " .. format_api_issues(res.issues)
    end
    stats.tasks_pushed = stats.tasks_pushed + 1
    return tonumber(res.created_id), nil
end

function daat_sync.run(brain_file, opts)
    opts = coalesce(opts, {})
    dry_run = (opts.dry_run == true)

    cgi_bin = os.getenv("BREX_DAAT_CGI_BIN")
    cgi_dir = os.getenv("BREX_DAAT_CGI_DIR")
    base_url = coalesce(opts.url, config.get_daat_url())
    if (cgi_bin == nil or cgi_bin == "") and (base_url == nil or base_url == "") then
        return nil, "No daat_url configured (set daat_url in ~/.config/brain-ex/config.yaml or BREX_DAAT_URL)."
    end

    api_key = coalesce(opts.api_key, config.get_daat_api_key())
    if api_key == nil or api_key == "" then
        return nil, "No daat API key found (set daat_api_key_file in config.yaml or BREX_DAAT_API_KEY)."
    end

    ensure_sync_schema(brain_file)

    client = {
        base_url = base_url,
        api_key = api_key,
        cgi_bin = cgi_bin,
        cgi_dir = cgi_dir
    }

    remote_docs, docs_err = fetch_all_active(client, "document", false)
    if remote_docs == nil then
        return nil, docs_err
    end

    remote_tasks, task_type_registered = fetch_all_active(client, "task", true)
    if remote_tasks == nil then
        return nil, task_type_registered
    end

    state = load_local_state(brain_file)

    doc_by_id = {}
    has_children = {}
    for _, d in ipairs(remote_docs) do
        did = tonumber(d.id)
        d.id = did
        if d.parent_id != nil and tostring(d.parent_id) != "" then
            pid = tonumber(d.parent_id)
            d.parent_id = pid
            has_children[pid] = true
        else
            d.parent_id = nil
        end
        if d.content == nil then
            d.content = ""
        end
        doc_by_id[did] = d
    end

    task_by_doc_id = {}
    for _, t in ipairs(remote_tasks) do
        t.id = tonumber(t.id)
        tdoc = tonumber(t.document)
        t.document = tdoc
        if tdoc != nil and task_by_doc_id[tdoc] == nil then
            task_by_doc_id[tdoc] = t
        end
    end

    is_folder_doc = {}
    for subj, f_did in pairs(state.sync_folders) do
        if doc_by_id[f_did] != nil then
            is_folder_doc[f_did] = true
        else
            state.sync_folders[subj] = nil
            state.folder_doc_to_subj[f_did] = nil
            if not dry_run then
                database.exec(
                    brain_file,
                    string.format("DELETE FROM daat_sync_folders WHERE subject=%s;", db.quote(subj))
                )
            end
        end
    end

    for _, d in ipairs(remote_docs) do
        if d.parent_id == nil and task_by_doc_id[d.id] == nil then
            is_kp = (d.title == KNOWLEDGE_POOL_FOLDER)
            is_empty_parent = (has_children[d.id] == true and trim(d.content) == "")
            if is_kp or is_empty_parent or is_folder_doc[d.id] == true then
                is_folder_doc[d.id] = true
                mapped_subj = folder_title_to_subject(d.title)
                if mapped_subj != "" and state.sync_folders[mapped_subj] == nil then
                    state.sync_folders[mapped_subj] = d.id
                    state.folder_doc_to_subj[d.id] = mapped_subj
                    if not dry_run then
                        save_sync_folder(brain_file, mapped_subj, d.id)
                    end
                end
            end
        end
    end

    function remote_doc_subject(d)
        if d.parent_id == nil then
            return ""
        end
        mapped = state.folder_doc_to_subj[d.parent_id]
        if mapped != nil then
            return mapped
        end
        parent_doc = doc_by_id[d.parent_id]
        if parent_doc != nil then
            return folder_title_to_subject(parent_doc.title)
        end
        return ""
    end

    stats = {
        folders_created = 0,
        docs_pushed = 0,
        docs_pulled = 0,
        docs_archived = 0,
        docs_deleted_local = 0,
        tasks_pushed = 0,
        tasks_pulled = 0,
        conflicts_merged = 0
    }

    handled_local_ids = {}
    handled_remote_doc_ids = {}
    pulled_any_notes = false

    for _, link_row in ipairs(state.sync_items) do
        b_id = link_row.brex_note_id
        d_id = link_row.daat_doc_id
        local_note = state.notes_by_id[b_id]
        remote_doc = doc_by_id[d_id]
        remote_task = task_by_doc_id[d_id]

        if local_note != nil then
            handled_local_ids[b_id] = true
        end
        if remote_doc != nil then
            handled_remote_doc_ids[d_id] = true
        end

        prev_sync_thash = coalesce(link_row.synced_task_hash, "none")

        if local_note == nil and remote_doc == nil then
            if not dry_run then
                delete_sync_item(brain_file, b_id)
            end
        elseif local_note == nil and remote_doc != nil then
            r_subj = remote_doc_subject(remote_doc)
            r_doc_hash = doc_hash(r_subj, remote_doc.title, remote_doc.content)
            r_task_hash = task_hash(remote_task)
            remote_changed = (r_doc_hash != link_row.synced_doc_hash)
                or (task_type_registered == true and r_task_hash != prev_sync_thash)

            if remote_changed then
                if not dry_run then
                    restored_id, restored_title = apply_remote_note_to_brex(
                        brain_file, nil, r_subj, remote_doc.title, remote_doc.content
                    )
                    apply_remote_task_to_brex(brain_file, restored_id, nil, remote_task)
                    delete_sync_item(brain_file, b_id)
                    final_d_hash = doc_hash(r_subj, restored_title, remote_doc.content)
                    save_sync_item(
                        brain_file, restored_id, d_id,
                        task_id_of(remote_task),
                        final_d_hash, r_task_hash
                    )
                end
                pulled_any_notes = true
                stats.docs_pulled = stats.docs_pulled + 1
            else
                if not dry_run then
                    if remote_task != nil then
                        _, t_arch_err = api_request(client, "POST", "/api/v1/task/" .. tostring(remote_task.id) .. "/archive", "reason=brex+sync", nil)
                        if t_arch_err != nil then
                            return nil, t_arch_err
                        end
                    end
                    _, d_arch_err = api_request(client, "POST", "/api/v1/document/" .. tostring(d_id) .. "/archive", "reason=brex+sync", nil)
                    if d_arch_err != nil then
                        return nil, d_arch_err
                    end
                    delete_sync_item(brain_file, b_id)
                end
                stats.docs_archived = stats.docs_archived + 1
            end
        elseif local_note != nil and remote_doc == nil then
            single_res, single_err, single_code = api_request(client, "GET", "/api/v1/document/" .. tostring(d_id), nil, nil)
            if single_res == nil and single_code != 404 then
                return nil, single_err
            end
            rendered_local = render_content_with_connections(local_note.content, local_note.connections)
            l_doc_hash = doc_hash(local_note.subject, local_note.title, rendered_local)
            l_task_hash = task_hash(local_note.task)
            local_changed = (l_doc_hash != link_row.synced_doc_hash)
                or (task_type_registered == true and l_task_hash != prev_sync_thash)

            if local_changed and single_res != nil and single_res.row != nil then
                ok_f, f_err = ensure_folders_exist(brain_file, client, state, {local_note.subject}, dry_run, stats)
                if ok_f == nil then
                    return nil, f_err
                end
                if not dry_run then
                    _, un_err = api_request(client, "POST", "/api/v1/document/" .. tostring(d_id) .. "/unarchive", "reason=brex+sync", nil)
                    if un_err != nil then
                        return nil, un_err
                    end
                    target_pid = ""
                    if local_note.subject != "" and state.sync_folders[local_note.subject] != nil then
                        target_pid = state.sync_folders[local_note.subject]
                    end
                    patch_res, patch_err = api_request(client, "PATCH", "/api/v1/document/" .. tostring(d_id), "reason=brex+sync", {
                        title = local_note.title,
                        content = rendered_local,
                        parent_id = target_pid
                    })
                    if patch_res == nil then
                        return nil, patch_err
                    end
                    if patch_res.success != true then
                        return nil, "Document update failed: " .. format_api_issues(patch_res.issues)
                    end
                    new_tid, t_sync_err = sync_remote_task_for_item(
                        client, d_id, local_note.subject, local_note.task, link_row.daat_task_id, nil, task_type_registered, false, stats
                    )
                    if t_sync_err != nil then
                        return nil, t_sync_err
                    end
                    save_sync_item(brain_file, b_id, d_id, new_tid, l_doc_hash, l_task_hash)
                end
                stats.docs_pushed = stats.docs_pushed + 1
            else
                if not dry_run then
                    delete_local_item(brain_file, local_note)
                    delete_sync_item(brain_file, b_id)
                end
                stats.docs_deleted_local = stats.docs_deleted_local + 1
            end
        else
            rendered_local = render_content_with_connections(local_note.content, local_note.connections)
            l_doc_hash = doc_hash(local_note.subject, local_note.title, rendered_local)
            r_subj = remote_doc_subject(remote_doc)
            r_doc_hash = doc_hash(r_subj, remote_doc.title, remote_doc.content)

            doc_local_dirty = (l_doc_hash != link_row.synced_doc_hash)
            doc_remote_dirty = (r_doc_hash != link_row.synced_doc_hash)

            final_doc_hash = l_doc_hash
            final_subject = local_note.subject

            if l_doc_hash == r_doc_hash then
                final_doc_hash = l_doc_hash
            elseif doc_local_dirty and not doc_remote_dirty then
                ok_f, f_err = ensure_folders_exist(brain_file, client, state, {local_note.subject}, dry_run, stats)
                if ok_f == nil then
                    return nil, f_err
                end
                if not dry_run then
                    target_pid = ""
                    if local_note.subject != "" and state.sync_folders[local_note.subject] != nil then
                        target_pid = state.sync_folders[local_note.subject]
                    end
                    patch_res, patch_err = api_request(client, "PATCH", "/api/v1/document/" .. tostring(d_id), "reason=brex+sync", {
                        title = local_note.title,
                        content = rendered_local,
                        parent_id = target_pid
                    })
                    if patch_res == nil then
                        return nil, patch_err
                    end
                    if patch_res.success != true then
                        return nil, "Document update failed: " .. format_api_issues(patch_res.issues)
                    end
                end
                stats.docs_pushed = stats.docs_pushed + 1
                final_doc_hash = l_doc_hash
            elseif doc_remote_dirty and not doc_local_dirty then
                final_subject = r_subj
                if not dry_run then
                    _, saved_title = apply_remote_note_to_brex(
                        brain_file, local_note, r_subj, remote_doc.title, remote_doc.content
                    )
                    if saved_title != remote_doc.title then
                        api_request(client, "PATCH", "/api/v1/document/" .. tostring(d_id), "reason=brex+sync", {title = saved_title})
                    end
                    final_doc_hash = doc_hash(r_subj, saved_title, remote_doc.content)
                else
                    final_doc_hash = r_doc_hash
                end
                pulled_any_notes = true
                stats.docs_pulled = stats.docs_pulled + 1
            else
                merged_body = rendered_local
                if trim(remote_doc.content) != "" and trim(remote_doc.content) != trim(rendered_local) then
                    stamp = os.date("%Y-%m-%d %H:%M:%S")
                    if trim(merged_body) == "" then
                        merged_body = remote_doc.content
                    else
                        merged_body = merged_body .. "\n\n---\n" .. stamp .. " (merged from daat)\n" .. remote_doc.content
                    end
                end
                merged_subj = local_note.subject
                merged_title = local_note.title
                ok_f, f_err = ensure_folders_exist(brain_file, client, state, {merged_subj}, dry_run, stats)
                if ok_f == nil then
                    return nil, f_err
                end
                if not dry_run then
                    _, saved_title = apply_remote_note_to_brex(
                        brain_file, local_note, merged_subj, merged_title, merged_body
                    )
                    target_pid = ""
                    if merged_subj != "" and state.sync_folders[merged_subj] != nil then
                        target_pid = state.sync_folders[merged_subj]
                    end
                    patch_res, patch_err = api_request(client, "PATCH", "/api/v1/document/" .. tostring(d_id), "reason=brex+sync+merge", {
                        title = saved_title,
                        content = merged_body,
                        parent_id = target_pid
                    })
                    if patch_res == nil then
                        return nil, patch_err
                    end
                    final_doc_hash = doc_hash(merged_subj, saved_title, merged_body)
                else
                    final_doc_hash = doc_hash(merged_subj, merged_title, merged_body)
                end
                pulled_any_notes = true
                stats.conflicts_merged = stats.conflicts_merged + 1
            end

            prev_t_hash = prev_sync_thash
            local_t_for_hash = local_note.task
            if local_t_for_hash != nil then
                local_t_for_hash.subject = final_subject
            end
            l_task_hash = task_hash(local_t_for_hash)
            r_task_hash = task_hash(remote_task)

            final_task_hash = prev_t_hash
            final_daat_task_id = coalesce(task_id_of(remote_task), link_row.daat_task_id)

            if task_type_registered == true then
                t_local_dirty = (l_task_hash != prev_t_hash)
                t_remote_dirty = (r_task_hash != prev_t_hash)

                if l_task_hash == r_task_hash then
                    final_task_hash = l_task_hash
                    final_daat_task_id = task_id_of(remote_task)
                elseif t_local_dirty and not t_remote_dirty then
                    new_tid, t_err = sync_remote_task_for_item(
                        client, d_id, final_subject, local_t_for_hash, link_row.daat_task_id, remote_task, true, dry_run, stats
                    )
                    if t_err != nil then
                        return nil, t_err
                    end
                    final_daat_task_id = new_tid
                    final_task_hash = l_task_hash
                elseif t_remote_dirty and not t_local_dirty then
                    if not dry_run then
                        apply_remote_task_to_brex(brain_file, b_id, local_note.task, remote_task)
                    end
                    stats.tasks_pulled = stats.tasks_pulled + 1
                    final_daat_task_id = task_id_of(remote_task)
                    final_task_hash = r_task_hash
                else
                    if local_t_for_hash != nil and local_t_for_hash.status == "done" and (remote_task == nil or remote_task.status != "done") then
                        new_tid, t_err = sync_remote_task_for_item(
                            client, d_id, final_subject, local_t_for_hash, link_row.daat_task_id, remote_task, true, dry_run, stats
                        )
                        if t_err != nil then
                            return nil, t_err
                        end
                        final_daat_task_id = new_tid
                        final_task_hash = l_task_hash
                    else
                        if not dry_run then
                            apply_remote_task_to_brex(brain_file, b_id, local_note.task, remote_task)
                        end
                        stats.tasks_pulled = stats.tasks_pulled + 1
                        final_daat_task_id = task_id_of(remote_task)
                        final_task_hash = r_task_hash
                    end
                end
            end

            if not dry_run and (final_doc_hash != link_row.synced_doc_hash
                    or final_task_hash != prev_t_hash
                    or final_daat_task_id != link_row.daat_task_id) then
                save_sync_item(brain_file, b_id, d_id, final_daat_task_id, final_doc_hash, final_task_hash)
            end
        end
    end

    untracked_remote_by_key = {}
    untracked_remote_list = {}
    for _, d in ipairs(remote_docs) do
        if handled_remote_doc_ids[d.id] == nil
                and is_folder_doc[d.id] != true
                and d.source_type != "chat_session" then
            r_subj = remote_doc_subject(d)
            r_key = r_subj .. "\0" .. tostring(d.title)
            if untracked_remote_by_key[r_key] == nil then
                untracked_remote_by_key[r_key] = d
            end
            table.insert(untracked_remote_list, d)
        end
    end

    new_local_notes = {}
    local_ids_sorted = {}
    for nid, _ in pairs(state.notes_by_id) do
        if handled_local_ids[nid] == nil then
            table.insert(local_ids_sorted, nid)
        end
    end
    table.sort(local_ids_sorted)

    matched_remote_ids = {}
    for _, nid in ipairs(local_ids_sorted) do
        ln = state.notes_by_id[nid]
        l_key = ln.subject .. "\0" .. ln.title
        matching_remote = untracked_remote_by_key[l_key]
        if matching_remote != nil and matched_remote_ids[matching_remote.id] == nil then
            matched_remote_ids[matching_remote.id] = true
            rt = task_by_doc_id[matching_remote.id]
            rendered_local = render_content_with_connections(ln.content, ln.connections)
            if trim(matching_remote.content) != trim(rendered_local) then
                if not dry_run then
                    api_request(client, "PATCH", "/api/v1/document/" .. tostring(matching_remote.id), "reason=brex+sync", {
                        content = rendered_local
                    })
                end
                stats.docs_pushed = stats.docs_pushed + 1
            end
            final_tid = task_id_of(rt)
            final_thash = task_hash(rt)
            if task_type_registered == true then
                if ln.task != nil and rt == nil then
                    created_tid, t_err = sync_remote_task_for_item(
                        client, matching_remote.id, ln.subject, ln.task, nil, nil, true, dry_run, stats
                    )
                    if t_err != nil then
                        return nil, t_err
                    end
                    final_tid = created_tid
                    final_thash = task_hash(ln.task)
                elseif ln.task == nil and rt != nil then
                    if not dry_run then
                        apply_remote_task_to_brex(brain_file, ln.id, nil, rt)
                    end
                    stats.tasks_pulled = stats.tasks_pulled + 1
                end
            end
            if not dry_run then
                save_sync_item(
                    brain_file, ln.id, matching_remote.id, final_tid,
                    doc_hash(ln.subject, ln.title, rendered_local), final_thash
                )
            end
        else
            table.insert(new_local_notes, ln)
        end
    end

    if #new_local_notes > 0 then
        needed_subjects = {}
        for _, ln in ipairs(new_local_notes) do
            if ln.subject != "" then
                table.insert(needed_subjects, ln.subject)
            end
        end
        ok_f, f_err = ensure_folders_exist(brain_file, client, state, needed_subjects, dry_run, stats)
        if ok_f == nil then
            return nil, f_err
        end

        if dry_run then
            stats.docs_pushed = stats.docs_pushed + #new_local_notes
            for _, ln in ipairs(new_local_notes) do
                if ln.task != nil and task_type_registered == true then
                    stats.tasks_pushed = stats.tasks_pushed + 1
                end
            end
        else
            idx = 1
            while idx <= #new_local_notes do
                doc_batch = {}
                batch_notes = {}
                limit_idx = math.min(idx + BATCH_SIZE - 1, #new_local_notes)
                for j = idx, limit_idx do
                    ln = new_local_notes[j]
                    rendered = render_content_with_connections(ln.content, ln.connections)
                    doc_entry = {
                        title = ln.title,
                        content = rendered
                    }
                    if ln.subject != "" and state.sync_folders[ln.subject] != nil then
                        doc_entry.parent_id = state.sync_folders[ln.subject]
                    end
                    table.insert(doc_batch, doc_entry)
                    table.insert(batch_notes, {note = ln, rendered = rendered})
                end

                doc_res, doc_err = api_request(client, "POST", "/api/v1/document", nil, doc_batch)
                if doc_res == nil then
                    return nil, doc_err
                end
                if doc_res.success != true then
                    return nil, "Batch document create failed: " .. format_api_issues(doc_res.issues)
                end
                created_doc_ids = doc_res.created_ids
                if created_doc_ids == nil and doc_res.created_id != nil then
                    created_doc_ids = {doc_res.created_id}
                end

                task_batch = {}
                task_owners = {}
                doc_id_by_k = {}
                for k, item in ipairs(batch_notes) do
                    ddid = tonumber(created_doc_ids[k])
                    doc_id_by_k[k] = ddid
                    stats.docs_pushed = stats.docs_pushed + 1
                    if task_type_registered == true and item.note.task != nil then
                        table.insert(task_batch, build_daat_task_payload(ddid, item.note.subject, item.note.task, false))
                        table.insert(task_owners, k)
                    end
                end

                created_task_by_k = {}
                if #task_batch > 0 then
                    t_res, t_err = api_request(client, "POST", "/api/v1/task", nil, task_batch)
                    if t_res == nil then
                        return nil, t_err
                    end
                    if t_res.success != true then
                        return nil, "Batch task create failed: " .. format_api_issues(t_res.issues)
                    end
                    t_ids = t_res.created_ids
                    if t_ids == nil and t_res.created_id != nil then
                        t_ids = {t_res.created_id}
                    end
                    for tk_idx, owner_k in ipairs(task_owners) do
                        created_task_by_k[owner_k] = tonumber(t_ids[tk_idx])
                        stats.tasks_pushed = stats.tasks_pushed + 1
                    end
                end

                for k, item in ipairs(batch_notes) do
                    ln = item.note
                    ddid = doc_id_by_k[k]
                    dtid = created_task_by_k[k]
                    thash = "none"
                    if task_type_registered == true and ln.task != nil then
                        thash = task_hash(ln.task)
                    end
                    save_sync_item(
                        brain_file, ln.id, ddid, dtid,
                        doc_hash(ln.subject, ln.title, item.rendered), thash
                    )
                end

                idx = limit_idx + 1
            end
        end
    end

    for _, d in ipairs(untracked_remote_list) do
        if matched_remote_ids[d.id] == nil then
            r_subj = remote_doc_subject(d)
            rt = task_by_doc_id[d.id]
            if r_subj == "" and rt != nil and rt.subject != nil and trim(rt.subject) != "" then
                r_subj = trim(rt.subject)
            end
            if dry_run then
                stats.docs_pulled = stats.docs_pulled + 1
                if rt != nil then
                    stats.tasks_pulled = stats.tasks_pulled + 1
                end
            else
                new_nid, saved_title = apply_remote_note_to_brex(brain_file, nil, r_subj, d.title, d.content)
                if rt != nil then
                    apply_remote_task_to_brex(brain_file, new_nid, nil, rt)
                    stats.tasks_pulled = stats.tasks_pulled + 1
                end
                save_sync_item(
                    brain_file, new_nid, d.id, task_id_of(rt),
                    doc_hash(r_subj, saved_title, d.content), task_hash(rt)
                )
                pulled_any_notes = true
                stats.docs_pulled = stats.docs_pulled + 1
            end
        end
    end

    if pulled_any_notes and not dry_run then
        knowledge_pool.sync_notes(brain_file)
    end

    return stats, nil
end

function do_sync(brain_file, cmd_args)
    flag_specs = {
        {short = "n", long = "dry-run", key = "dry_run", kind = "boolean"},
        {short = "u", long = "url", kind = "string"},
    }
    help_string = help.get_help_string(arg[0])
    parsed = bx_utils.parse_cmd_args(cmd_args, flag_specs, help_string)
    if parsed == nil then
        return "success"
    end

    stats, err = daat_sync.run(brain_file, {
        dry_run = (parsed.dry_run == true),
        url = parsed.url
    })
    if stats == nil then
        print("Sync failed: " .. tostring(err))
        return "error"
    end

    prefix = "Synced with daat"
    if parsed.dry_run == true then
        prefix = "Dry-run sync with daat"
    end
    print(string.format(
        "%s: pushed %d doc(s)/%d task(s), pulled %d doc(s)/%d task(s), archived %d, deleted_local %d, folders_created %d, merged %d.",
        prefix,
        stats.docs_pushed, stats.tasks_pushed,
        stats.docs_pulled, stats.tasks_pulled,
        stats.docs_archived, stats.docs_deleted_local,
        stats.folders_created, stats.conflicts_merged
    ))
    return "success"
end

daat_sync.do_sync = do_sync
return daat_sync
