-- src/agent_providers/gemini.lua
provider = {}
provider.name = "gemini"

function provider.shell_escape(s)
    return "'" .. string.gsub(s, "'", "'\\''") .. "'"
end

function provider.generate(model, system_prompt, prompt)
    dkjson = require("dkjson")
    utils = require("utils")

    api_key = os.getenv("GEMINI_API_KEY")
    if api_key == nil or api_key == "" then
        return nil, "Missing GEMINI_API_KEY environment variable."
    end

    contents = {
        {
            role = "user",
            parts = {
                { text = "Task: " .. prompt }
            }
        }
    }

    payload = {
        contents = contents,
        generationConfig = {
            temperature = 0.2
        }
    }

    if system_prompt != nil and system_prompt != "" then
        payload.systemInstruction = {
            parts = {
                { text = system_prompt }
            }
        }
    end

    payload_json = dkjson.encode(payload)
    tmpfile = os.tmpname()
    utils.write(tmpfile, payload_json)

    command = string.format(
        "curl -s -X POST 'https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent?key=%s' " ..
        "-H 'Content-Type: application/json' " ..
        "-d @%s", model, api_key, tmpfile
    )

    response, success = utils.exec_command(command)
    os.remove(tmpfile)

    if success == false or response == nil or response == "" then
        return nil, "Failed to connect to Gemini API."
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
        return nil, "Gemini API Error: " .. tostring(error_detail)
    end

    if res_obj.candidates == nil or res_obj.candidates[1] == nil or res_obj.candidates[1].content == nil or res_obj.candidates[1].content.parts == nil or res_obj.candidates[1].content.parts[1] == nil then
        return nil, "Invalid Gemini response structure: " .. response
    end

    return res_obj.candidates[1].content.parts[1].text, nil
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

    api_key = os.getenv("GEMINI_API_KEY")
    if api_key == nil or api_key == "" then
        return nil, "Missing GEMINI_API_KEY environment variable."
    end

    payload = {
        content = {
            parts = {
                { text = input_text }
            }
        }
    }
    payload_json = dkjson.encode(payload)
    tmpfile = os.tmpname()
    utils.write(tmpfile, payload_json)

    command = string.format(
        "curl -s -X POST 'https://generativelanguage.googleapis.com/v1beta/models/%s:embedContent?key=%s' " ..
        "-H 'Content-Type: application/json' " ..
        "-d @%s", model, api_key, tmpfile
    )

    response, success = utils.exec_command(command)
    os.remove(tmpfile)

    if success == false or response == nil or response == "" then
        return nil, "Failed to connect to Gemini embeddings endpoint."
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
        return nil, "Gemini API Error: " .. tostring(error_detail)
    end

    if res_obj.embedding == nil or res_obj.embedding.values == nil then
        return nil, "Invalid embeddings response structure: " .. response
    end

    return res_obj.embedding.values, nil
end

return provider
