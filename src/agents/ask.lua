-- src/agents/ask.lua
local common = require("agents.common")
return "You are a general brain assistant. Prefer reading notes or tasks before making claims about existing information.\n" .. common.tool_instructions
