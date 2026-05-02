# Agent Execution & Documentation

The agent component in `brain-ex` delegates database actions and queries seamlessly using an LLM.

## Setup
Configuration pulls the provider name and model seamlessly. In `~/.config/brain-ex/config.yaml`, add:
```yaml
agent_provider: "ollama"
agent_model: "qwen3.5:0.8b"
```

## Running Agents
The agent can be invoked for different purposes using specialized subcommands.

- `brex agent` (no arguments) - Defaults to `view`. Displays the agent log file (`agent.log`).
- `brex agent <prompt>` - Defaults to `ask`. Asks the general brain assistant.
- `brex agent ask <prompt>` - Asks the general brain assistant.
- `brex agent note <prompt>` - Runs the note-focused assistant.
- `brex agent task <prompt>` - Runs the task-focused assistant.
- `brex agent process_tasks` - Gathers background tasks (`owner="agent"`) and executes them.

### Sub-agents
Specialized templates are defined in `src/agent_prompts.lua`.
- `ask`: General assistant (uses `worker.md` style prompt)
- `note`: Note capture specialist
- `task`: Task management specialist
- `worker`: Used for background task processing
- `scout`: Fast information gathering agent

### Providers 
Currently `ollama` natively supported. Provide extensions via `src/agent_providers/<provider_name>.lua`. Must expose `provider.generate(model, system_prompt, prompt)`.
