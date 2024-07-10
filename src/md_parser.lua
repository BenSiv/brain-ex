-- parser for markdown source file to a workable form

require("utils").using("utils")
using("paths")

script_path = debug.getinfo(1, "S").source:sub(2)
add_to_path(script_path, "../json.lua")

local json = require("json")

local function print_help()
    print("Usage: md_parser < input_file > < output_file >")
end

local function get_lines(markdown_text)
    local lines = {}

    for line in match_all(markdown_text, "[^\r\n]+") do
        table.insert(lines, line)
    end

    return lines
end

local function extract_links(line, link_found)
    link_found = link_found or {}
    for link in match_all(line, "%[%[(.-)%]%]") do
        if not occursin(link, link_found) then
            table.insert(link_found, link)
        end
        line = line:gsub("%[%[" .. link .. "%]%]", "")
    end
    return line, link_found
end

local function parse_markdown(markdown_text)
    local lines = get_lines(markdown_text)
    local free_text = false
    local current_title = { level = 0, title = "head", body = {} }
    local result = {}
    table.insert(result, current_title)

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

local function flat_to_nested(flat_table)
    local nested_table = {}
    nested_table[0] = {level = 0}

    local last_element = {}
    local current_level = 0
    for key, value in pairs(flat_table) do
        last_element = nested_table[length(nested_table)]
        current_level = last_element["level"]
        if value["level"] > current_level then
            if last_element["body"] then
                table.insert(last_element["body"], flat_table[key])
            else
                last_element["body"] = flat_table[key]
            end
        end
    end

    return nested_table
end

local function search_key_recursive(input_table, search_key, result_table)
    result_table = result_table or {}

    for key, value in pairs(input_table) do
        if key == search_key then
            table.insert(result_table, value)
        elseif type(value) == "table" then
            search_key_recursive(value, search_key, result_table)
        end
    end

    return result_table
end


local function main()
    if length(arg) ~= 2 then
        print_help()
    else
        local input_file_path = arg[1]
        local output_file_path = arg[2]
        local input_file = io.open(input_file_path, "r")
        local content
        if input_file then
            content = input_file:read("*all")
            input_file:close()
        else
            print("Error opening file: " .. input_file)
        end
        -- local content = read_lines(file_path)
        local content_parsed = parse_markdown(content)
        local content_json = json.encode(content_parsed)
        local output_file = io.open(output_file_path, "w")
        if output_file then
            output_file:write(content_json)
            output_file:close()
        else
            print("Error opening file: " .. output_file)
        end
    end
end

-- run script
main()