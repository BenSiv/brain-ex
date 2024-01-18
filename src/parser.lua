-- parser for markdown source file to a workable form
local function print_help()
    print("Usage: parse <file>")
end

function parse_markdown(markdown_text)
    local result = {}
    local lines = get_lines(markdown_text)
    local current_title = nil
    local free_text = false

    for _, line in ipairs(lines) do
        local level, title = match("^(#+)%s(.*)$", line)
        local bullet_point = match("^%- (.*)$", line)
        local body = nil

        if level then
            current_title = { level = length(level), title = title, body = {} }
            table.insert(result, current_title)
            free_text = false
        elseif bullet_point then
            bullet_point, links = extract_links(bullet_point)
            body = {text = bullet_point, link = links}
            table.insert(current_title.body, body)
            free_text = false
        else
            if free_text then
                line, links = extract_links(line, current_title.body.link)
                last_element = length(current_title.body)
                current_title.body[last_element].text = current_title.body[last_element].text .. "\n" ..line
                current_title.body[last_element].link = links
            else
                line, links = extract_links(line)
                body = {text = line, link = links}
                table.insert(current_title.body, body)
                free_text = true
            end
        end
    end

    return result
end

function get_lines(markdown_text)
    local lines = {}

    for line in match_all("[^\r\n]+", markdown_text) do
        table.insert(lines, line)
    end

    return lines
end

function extract_links(line, link_found)
    link_found = link_found or {}
    for link in match_all("%[%[(.-)%]%]", line) do
        if not occursin(link, link_found) then
            table.insert(link_found, link)
        end
        line = line:gsub("%[%[" .. link .. "%]%]", "")
    end
    return line, link_found
end

-- function flat_to_nested(flat_table)
--     local nested_table = {}
--     nested_table[0] = {level = 0}

--     local last_element = {}
--     local current_level = 0
--     for key, value in pairs(flat_table) do
--         last_element = nested_table[length(nested_table)]
--         current_level = last_element["level"]
--         if value["level"] > current_level then
--             if last_element["body"] then
--                 table.insert(last_element["body"], flat_table[key])
--             else
--                 last_element["body"] = flat_table[key]
--             end
--         end
--     end

--     return nested_table
-- end


function main()
    if length(arg) ~= 1 then
        print_help()
    else
        local file_path = arg[1]
        local file = io.open(file_path, "r")
        local content
        if file then
            content = file:read("*all")
            file:close()
        else
            print("Error opening the file.")
        end        
        -- local content = read_lines(file_path)
        local content_parsed = parse_markdown(content)
        show(content_parsed)
    end
end

-- run script
main()
