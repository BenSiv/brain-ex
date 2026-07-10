-- src/agent_providers/openai.lua
provider = {}
provider.name = "openai"

function provider.shell_escape(s)
    return "'" .. string.gsub(s, "'", "'\\''") .. "'"
end

function provider.generate(model, system_prompt, prompt)
    dkjson = require("dkjson")
    utils = require("utils")
    
    api_key = os.getenv("OPENAI_API_KEY")
    api_base = "https://api.openai.com/v1"
    if os.getenv("OPENAI_BASE_URL") != nil then
        api_base = os.getenv("OPENAI_BASE_URL")
    end
    if os.getenv("OPENAI_API_BASE") != nil then
        api_base = os.getenv("OPENAI_API_BASE")
    end
    
    if api_key == nil or api_key == "" then
        api_key = "local"
    end

    messages = {}
    if system_prompt != nil and system_prompt != "" then
        table.insert(messages, { role = "system", content = system_prompt })
    end
    table.insert(messages, { role = "user", content = prompt })

    payload = {
        model = model,
        messages = messages,
        temperature = 0.2
    }
    payload_json = dkjson.encode(payload)
    tmpfile = os.tmpname()
    utils.write(tmpfile, payload_json)

    command = string.format(
        "curl -s %s/chat/completions " ..
        "-H 'Content-Type: application/json' " ..
        "-H 'Authorization: Bearer %s' " ..
        "-d @%s", api_base, api_key, tmpfile
    )
    
    response, success = utils.exec_command(command)
    os.remove(tmpfile)

    if success == false or response == nil or response == "" then
        return nil, "Failed to connect to OpenAI endpoint."
    end

    res_obj, _, err = dkjson.decode(response)
    if err != nil then
        return nil, "Failed to decode response: " .. tostring(err) .. "\nResponse: " .. response
    end

    if res_obj.error != nil then
        error_detail = res_obj.error
        if res_obj.error.message != nil then
            error_detail = res_obj.error.message
        end
        return nil, "OpenAI API Error: " .. tostring(error_detail)
    end

    if res_obj.choices == nil or res_obj.choices[1] == nil or res_obj.choices[1].message == nil then
        return nil, "Invalid OpenAI response structure: " .. response
    end

    return res_obj.choices[1].message.content, nil
end

function provider.embeddings(model, input_text, command_tmpl)
    dkjson = require("dkjson")
    utils = require("utils")

    if command_tmpl != nil and command_tmpl != "" then
        command = string.gsub(command_tmpl, "%%m", model)
        if string.match(command, "%%t") != nil then
            command = string.gsub(command, "%%t", provider.shell_escape(input_text))
        end
        result, success = utils.exec_command(command)
        if success == true and result != nil and result != "" then
            embeddings = {}
            for num in string.gmatch(result, "([-?%d%.eE]+)") do
                table.insert(embeddings, tonumber(num))
            end
            if #embeddings > 0 then
                return embeddings, nil
            end
        end
        return nil, "Failed to get custom embeddings."
    end

    api_key = os.getenv("OPENAI_API_KEY")
    api_base = "https://api.openai.com/v1"
    if os.getenv("OPENAI_BASE_URL") != nil then
        api_base = os.getenv("OPENAI_BASE_URL")
    end
    if os.getenv("OPENAI_API_BASE") != nil then
        api_base = os.getenv("OPENAI_API_BASE")
    end
    
    if api_key == nil or api_key == "" then
        api_key = "local"
    end

    payload = {
        model = model,
        input = input_text
    }
    payload_json = dkjson.encode(payload)
    tmpfile = os.tmpname()
    utils.write(tmpfile, payload_json)

    command = string.format(
        "curl -s %s/embeddings " ..
        "-H 'Content-Type: application/json' " ..
        "-H 'Authorization: Bearer %s' " ..
        "-d @%s", api_base, api_key, tmpfile
    )
    
    response, success = utils.exec_command(command)
    os.remove(tmpfile)

    if success == false or response == nil or response == "" then
        return nil, "Failed to connect to OpenAI embeddings endpoint."
    end

    res_obj, _, err = dkjson.decode(response)
    if err != nil then
        return nil, "Failed to decode embeddings: " .. tostring(err) .. "\nResponse: " .. response
    end

    if res_obj.error != nil then
        error_detail = res_obj.error
        if res_obj.error.message != nil then
            error_detail = res_obj.error.message
        end
        return nil, "OpenAI API Error: " .. tostring(error_detail)
    end

    if res_obj.data == nil or res_obj.data[1] == nil or res_obj.data[1].embedding == nil then
        return nil, "Invalid embeddings response structure: " .. response
    end

    return res_obj.data[1].embedding, nil
end

return provider
