

-- Example usage
local flat_table = {
    [1] = { level = 1 },
    [2] = { level = 2 },
    [3] = { level = 3 },
    [4] = { level = 2 },
}

local nested_table = flat_to_nested(flat_table)

-- Print the result
show(nested_table)
