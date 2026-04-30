# Agent Execution & Documentation

The agent component in `brain-ex` delegates database actions and queries seamlessly using an LLM.

## Setup
Configuration pulls the provider name and model seamlessly. In `~/.config/brain-ex/config.yaml`, add:
```yaml
agent_provider: "ollama"
agent_model: "qwen3.5:0.8b"
```

## Running Agents
A primary sub-agent acts as context templates mapping to system prompts located in `src/agents/*.md`, e.g., `scout`, `worker`.

- `brex agent run <subagent> <prompt>` - Streams the context and parses XML tools from output seamlessly.
- `brex agent process_tasks` - Gathers background tasks (`owner="agent"`) and executes the `worker` abstraction.

### Providers 
Currently `ollama` natively supported. Provide extensions via `src/agent_providers/<provider_name>.lua`. Must expose `provider.generate(model, system_prompt, prompt)`.
