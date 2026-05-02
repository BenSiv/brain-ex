provider = {}

provider.name = "mock"

call_count = 0

function provider.generate(model, system_prompt, prompt)
    call_count = call_count + 1
    key = "BREX_MOCK_RESPONSE_" .. tostring(call_count)
    response = os.getenv(key) or os.getenv("BREX_MOCK_RESPONSE") or "<done>mock reply</done>"
    return response, nil
end

function provider.embeddings(model, input_text, command_tmpl)
    return {0.0}, nil
end

return provider
