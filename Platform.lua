PLATFORM_SETTINGS                                       = {
    --- The log info level used in the system
    ---     1: Trace
    ---     2: Debug
    ---     3: Info
    ---     4: Warn
    ---     5: Error
    ---     6: Fatal
    ---
    --- @default 4(Warn)
    SYS_LOG_LEVEL                                       = 4,

    --- The log handler works like:
    ---     function LOG_HANDLER(message, ...)
    ---         -- message  : the log message
    ---         -- ...      : the log message's format
    ---     end
    ---
    --- @default print
    SYS_LOG_HANDLER                                     = print,

    --- Whether allow system use userdata type for namespace, if false, use table
    ---
    --- @default true
    SAFE_MODE                                           = true,
}
