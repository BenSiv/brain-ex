-- Declarative static build manifest for brex (brain-ex CLI).
return {
    bin = "brex",
    entry = "brex.lua",
    luam = "8c9bb857dd0b8c22b9f0dfad674130e0958ec240",
    with = {"sqlite3", "lfs", "yaml"},
    always_include = {"agent_providers", "agents"},
}
