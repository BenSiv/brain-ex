provider = {}

provider.name = "mock"

call_count = 0

function provider.generate(model, system_prompt, prompt)
    call_count = call_count + 1
    key = "BREX_MOCK_RESPONSE_" .. tostring(call_count)
    response = "<done>mock reply</done>"
    if os.getenv("BREX_MOCK_RESPONSE") != nil then
        response = os.getenv("BREX_MOCK_RESPONSE")
    end
    if os.getenv(key) != nil then
        response = os.getenv(key)
    end
    return response, nil
end

function provider.embeddings(model, input_text, command_tmpl)
    return {0.0}, nil
end

return provider
