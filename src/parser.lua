-- parser for markdown source file to a workable form
local function print_help()
    print("Usage: parse <file>")
end

function parse_markdown(markdown_text)
    local result = {}

    local lines = {}
    
    for line in match_all("[^\r\n]+", markdown_text) do
        table.insert(lines, line)
    end

    local current_title = nil

    for _, line in ipairs(lines) do
        -- Extract links
        for link in match_all("%[%[(.-)%]%]", line) do
            if current_title and not occursin(link, current_title.links) then
                table.insert(current_title.links, link)
            end
        end
        
        -- Extract title levels
        local level, title = match("^(#+)%s(.*)$", line)
        if level then
            current_title = { level = length(level), title = title, body = "", links = {} }
            table.insert(result, current_title)
        elseif current_title then
            current_title["body"] = current_title["body"] .. "\n" ..line
        end
    end

    return result
end


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
