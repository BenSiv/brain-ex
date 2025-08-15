1. **Configuration Access Anti-Pattern**
- Multiple functions across modules repeatedly call `get_config_path()`, `get_brain_path()`, `get_vault_path()`, etc.
- Each call re-reads the YAML config file from disk
- No caching mechanism, leading to inefficient I/O operations
- Inconsistent error handling for config access

### 3. **Database Connection Management Issues**
- Database connections are opened/closed repeatedly within single operations
- No connection pooling or reuse
- Transaction boundaries are unclear
- Scattered database error handling

### 4. **Code Duplication**
- Similar argument parsing patterns repeated in each command module
- Database connection patterns duplicated
- Error handling patterns scattered throughout