do
    ----------------------------------------------------------------
    --- Copyright (c) 2016-2024 <mikuday0312@gmail.com>
    --- Prototype Lua environment-control tree system
    ---
    --- @_EC_Tree
    ----------------------------------------------------------------
    local cerror, cformat               = error, string.format
    local _EC_Tree                      = setmetatable(
        {
            --- Interface
            _G                          = _G,
            _VERSION                    = _VERSION and tonumber(_VERSION:match("[%d%.]+")) or 5.1,

            --- Weak Table
            WEAK_KEY                    = { __mode = "k",  __metatable = false },
            WEAK_VALUE                  = { __mode = "v",  __metatable = false },
            WEAK_ALL                    = { __mode = "kv", __metatable = false },

            --- Iterator
            pairs                       = pairs(_G),
            ipairs                      = ipairs(_G),
            next                        = next,
            select                      = select,

            --- String
            strformat                   = string.format,
            strfind                     = string.find,
            strsub                      = string.sub,
            strgsub                     = string.gsub,
            strmatch                    = string.match,
            strgmatch                   = string.gmatch,
            strsplit                    = string.split,
            strlower                    = string.lower,
            strupper                    = string.upper,

            --- Table
            tinsert                     = table.insert,
            tremove                     = table.remove,
            tconcat                     = table.concat,
            unpack                      = unpack,
            rawset                      = rawset,
            rawget                      = rawget,
            setmetatable                = setmetatable,
            getmetatable                = getmetatable,

            --- Math
            ceil                        = math.ceil,
            floor                       = math.floor,

            --- Type
            type                        = type,
            tonumber                    = tonumber,
            tostring                    = tostring,

            --- Safe
            newproxy                    = newproxy or false,
            error                       = error,
            pcall                       = pcall,

            --- Debug
            print                       = print,
            loadstring                  = loadstring,
            setfenv                     = setfenv,
            getfenv                     = getfenv,
            collectgarbage              = collectgarbage,

            --- Fake API
            fakefunc                    = function() end,
        }, {
            __index                     = function(self, k)
                local v                 = self.namespace.GetValue(self, k)
                if v ~= nil then return v end
                cerror(cformat("Global variable %q can't be found", k), 2)
            end,
            __metatable                 = true,
        }
    )
    _EC_Tree._EC_Tree                   = _EC_Tree
    if setfenv then setfenv(1, _EC_Tree) else _ENV = _EC_Tree end

    ----------------------------------------------------------------
    --- Platform settings
    ---
    --- @return PLATFORM_SETTINGS:table
    ----------------------------------------------------------------
    PLATFORM_SETTINGS                   = (function(default)
        local settings                  = _G.PLATFORM_SETTINGS
        if type(settings) == "table" then
            _G.PLATFORM_SETTINGS        = nil

            for k, v in pairs, default do
                local r                 = settings[k]
                if type(r) == type(v) then
                    default[k]          = r
                end
            end
        end
        return default
    end) {
        --- The log info level used in the system
        ---     1: Trace
        ---     2: Debug
        ---     3: Info
        ---     4: Warn
        ---     5: Error
        ---     6: Fatal
        ---
        --- @default 4(Warn)
        SYS_LOG_LEVEL                                   = 4,

        --- The log handler works like:
        ---     function LOG_HANDLER(message, ...)
        ---         -- message  : the log message
        ---         -- ...      : the log message's format
        ---     end
        ---
        --- @default print
        SYS_LOG_HANDLER                                 = print,

        --- Whether allow system use userdata type for namespace, if false, use table
        ---
        --- @default true
        SAFE_MODE                                       = true,
    }

    ----------------------------------------------------------------
    --- Share API
    ----------------------------------------------------------------
    wipe                                = function(self)        for k in pairs, self do self[k] = nil end return self end
    strtrim                             = function(s)           return s and s:gsub("^%s*(.-)%s*$", "%1") or "" end
    setstorage                          = function(self, k, v)  self[k] = v return self end
    getstorage                          = function(self, k)     return self[k] end
    safeget                             = function(self, k)     local ok, r = pcall(getstorage, self, k) if ok then return r end end
    readonly                            = function(self)        error(strformat("The %s can't be written", tostring(self)), 2) end
    writeonly                           = function(self)        error(strformat("The %s can't be read",    tostring(self)), 2) end

    ----------------------------------------------------------------
    --- Generate zero-size userdata meta-table
    ---
    --- @return userdata/table
    ----------------------------------------------------------------
    newproxy                            = PLATFORM_SETTINGS.SAFE_MODE and newproxy or (function()
        local falsemeta                 = { __metatable = false }
        local proxymap                  = setmetatable({}, WEAK_ALL)

        return function(prototype)
            if prototype == true then
                local meta              = {}
                prototype               = setmetatable({}, meta)
                proxymap[prototype]     = meta
                return prototype
            elseif proxymap[prototype] then
                return setmetatable({}, proxymap[prototype])
            else
                return setmetatable({}, falsemeta)
            end
        end
    end)()

    ----------------------------------------------------------------
    --- Generate log handler
    ---
    --- @return PLATFORM_SETTINGS.SYS_LOG_HANDLER
    ----------------------------------------------------------------
    GenerateLogger                      = function(prefix, loglvl)
        local handler                   = PLATFORM_SETTINGS.SYS_LOG_HANDLER
        return loglvl and fakefunc or function(msg, ...)
            return handler(prefix .. (... and strformat(msg, ...) or msg))
        end
    end

    Trace                               = GenerateLogger("[Trace]", PLATFORM_SETTINGS.SYS_LOG_LEVEL > 1)
    Debug                               = GenerateLogger("[Debug]", PLATFORM_SETTINGS.SYS_LOG_LEVEL > 2)
    Info                                = GenerateLogger("[ Info]", PLATFORM_SETTINGS.SYS_LOG_LEVEL > 3)
    Warn                                = GenerateLogger("[ Warn]", PLATFORM_SETTINGS.SYS_LOG_LEVEL > 4)
    Error                               = GenerateLogger("[Error]", PLATFORM_SETTINGS.SYS_LOG_LEVEL > 5)
    Fatal                               = GenerateLogger("[Fatal]", PLATFORM_SETTINGS.SYS_LOG_LEVEL > 6)

    ----------------------------------------------------------------
    --- Cache for recycling table
    ---
    --- @return table
    ----------------------------------------------------------------
    TCache                              = setmetatable({}, {
        __mode                          = "kv",
        __call                          = function(self, tbl)
            if tbl then tinsert(self, wipe(tbl)) else return tremove(self) or {} end
        end,
    })

    ----------------------------------------------------------------
    --- Deep clone the source to target table
    ---
    --- @return table
    ----------------------------------------------------------------
    TDeepClone                          = function(src, tar, override, cache)
        if cache then cache[src]        = tar end

        for k, v in pairs, src do
            if override or tar[k] == nil then
                if cache and cache[v] then
                    tar[k]              = cache[v]
                elseif type(v) == "table" and getmetatable(v) == nil then
                    tar[k]              = TDeepClone(v, {}, override, cache)
                else
                    tar[k]              = v
                end
            elseif type(v) == "table" and type(tar[k]) == "table" and getmetatable(v) == nil and getmetatable(tar[k]) == nil then
                TDeepClone(v, tar[k], nil, cache)
            end
        end
        return tar
    end

    ----------------------------------------------------------------
    --- Clone the source to target table
    ---
    --- @return table
    ----------------------------------------------------------------
    TClone                              = function(src, tar, deep, override, safe)
        if deep then
            safe                        = safe and TCache()
            TDeepClone(src, tar, override, safe) -- if safe true, cache for duplicated table
            if safe then TCache(safe) end
        else
            for k, v in pairs, src do
                if override or tar[k] == nil then tar[k] = v end
            end
        end
        return tar
    end

    ----------------------------------------------------------------
    --- Loading a chunk string and return it as a function
    ---
    --- @param  chunk:string
    --- @param  err:string
    --- @param  env                     default _G
    --- @return function
    ----------------------------------------------------------------
    LoadChunk                           = (function()
        local body                      = TCache()
        local apis                      = TCache()

        --- External local variable
        tinsert(body, "")

        --- Generate handler
        tinsert(body, [[
            return function(chunk, err, env)
                err                     = err or "anonymous"
        ]])

        --- Log
        if Debug ~= fakefunc then
            tinsert(apis, "Debug")
            tinsert(body, [[
                Debug("[LoadChunk] %s ==> created", err)
            ]])
        end

        if Trace ~= fakefunc then
            tinsert(apis, "Trace")
            tinsert(body, [[
                Trace(chunk)
                Trace("[LoadChunk] %s <== complete", err)
            ]])
        end

        --- Return handler to environment
        tinsert(apis, "loadstring")
        tinsert(apis, "setfenv")
        tinsert(apis, "_G")
        tinsert(body, [[
                chunk, err              = loadstring(chunk, err)
                if chunk then
                    setfenv(chunk, env or _G)
                end
                return chunk, err
            end
        ]])

        if #apis > 0 then
            local declare               = tconcat(apis, ", ")
            body[1]                     = strformat("local %s = %s", declare, declare)
        end

        local handler                   = setfenv(loadstring(tconcat(body, "\n"), "LoadChunk"), _EC_Tree)()
        TCache(body) TCache(apis)

        return handler
    end)()

    ----------------------------------------------------------------
    --- The prototype provided a system to build prototypes of others,
    --- the environment prototype used to generate environments used as type builders or modules
    ---
    --- @param  super
    --- @param  meta
    --- @param  nodeepclone:bool
    --- @return prototype
    ----------------------------------------------------------------
    prototype                           = (function()
        local head                      = TCache()
        local body                      = TCache()
        local upval                     = TCache()
        local apis                      = TCache()

        --- Prototype map
        tinsert(head, "_PrototypeMap")
        tinsert(upval, setmetatable({}, WEAK_ALL))

        --- External local variable
        tinsert(body, "")
        tinsert(body, "")

        --- Generate handler
        tinsert(body, [[
            local function _To(...)
                local super, meta, nodeepclone
        ]])

        --- Parse param
        tinsert(apis, "select")
        tinsert(apis, "type")
        tinsert(apis, "getmetatable")
        tinsert(apis, "newproxy")
        tinsert(body, [[
            for i = 1, select("#", ...) do
                local v                 = select(i, ...)
                local vt                = type(v)

                if vt == "table" then
                    if getmetatable(v) == nil then
                        meta            = v
                    elseif _PrototypeMap[v] then
                        super           = v
                    end
                elseif vt == "userdata" and _PrototypeMap[v] then
                    super               = v
                elseif vt == "boolean" then
                    nodeepclone         = v
                end
            end

            local keyword
            local prototype             = newproxy(true)
            local pmeta                 = getmetatable(prototype)
            _PrototypeMap[prototype]    = pmeta
        ]])

        --- Clone meta-table
        tinsert(apis, "TClone")
        tinsert(body, [[
            if meta then TClone(meta, pmeta, not nodeepclone, true, true) end
        ]])

        --- Default
        tinsert(body, [[
            if type(pmeta.__tostring) == "string" then keyword, pmeta.__tostring = pmeta.__tostring, function() return keyword end end
            if      pmeta.__metatable == nil      then pmeta.__metatable         = prototype end
        ]])

        --- Inherit
        tinsert(body, [[
            if super then TClone(_PrototypeMap[super], pmeta, true, false, true) end
        ]])

        --- Log
        if Debug ~= fakefunc then
            tinsert(apis, "Debug")
            tinsert(body, [[Debug("[prototype] %s ==> created", keyword or "anonymous")]])
        end

        --- Return handler to environment
        tinsert(apis, "setmetatable")
        tinsert(apis, "readonly")
        tinsert(body, [[
                return prototype
            end

            return _To {
                __tostring              = "prototype",
                __index                 = {
                    ["NewProxy"]        = newproxy;
                    ["NewObject"]       = function(self, tbl) return setmetatable(tbl or {}, _PrototypeMap[self]) end;
                    ["Validate"]        = function(self) return _PrototypeMap[self] and self end;
                },
                __newindex              = readonly,
                __call                  = function(self, ...) return _To(...) end,
            }
        ]])

        if #head > 0 then
            body[1]                     = "local " .. tconcat(head, ", ") .. " = ..."
        end

        if #apis > 0 then
            local declare               = tconcat(apis, ", ")
            body[2]                     = strformat("local %s = %s", declare, declare)
        end

        local handler                   = LoadChunk(tconcat(body, "\n"), "prototype", _EC_Tree)(unpack(upval))
        TCache(head) TCache(body) TCache(upval) TCache(apis)

        return handler
    end)()
end

do
    ----------------------------------------------------------------
    --- Environment private constants
    ----------------------------------------------------------------
    local _ENV_NS_OWNER                 = "__ENV_OWNER"
    local _ENV_NS_IMPORT                = "__ENV_IMPORT"
    local _ENV_NS_PARENT                = "__ENV_PARENT"
    local _ENV_NS_DEFINE                = "__ENV_DEFINE_MODE"

    ----------------------------------------------------------------
    --- Environment private storage
    ----------------------------------------------------------------
    local _NSTree                       = setmetatable({}, WEAK_KEY)
    local _NSPath                       = setmetatable({}, WEAK_KEY)
    local _GlobalNS                     = {}    -- global namespaces
    local _GlobalKeywords               = {}    -- global keywords

    local _KeyVisitor                           -- environment for next keyword access
    local _AccessKey                            -- keyword for the next access
    local _NextNSForType                        -- namespace for next generated type

    ----------------------------------------------------------------
    --- Environment private method
    ----------------------------------------------------------------
    local getNamespace                  = function(root, path)
        if type(root) == "string" then
            root, path                  = ROOT_NAMESPACE, root
        elseif root == nil then
            root                        = ROOT_NAMESPACE
        end

        if _NSPath[root] ~= nil and type(path) == "string" then
            path                        = path:gsub("%s+", "")
            local iter                  = path:gmatch("[%P_]+")
            local subname               = iter()

            while subname do
                local nodes             = _NSTree[root]
                root                    = nodes and nodes[subname]
                if not root then return end

                local nxt               = iter()
                if not nxt  then return root end

                subname                 = nxt
            end
        end
    end

    local getNSPath                     = function(ns, only)
        ns                              = _NSPath[ns]
        return ns and (only == nil and ns or ns:match(only and "[%P_]+$" or "^(.*)%.%w+$"))
    end

    local getValidatedNS                = function(tar)
        return _NSPath[tar] ~= nil and tar or type(tar) == "string" and getNamespace(tar) or nil
    end

    local exportToEnv                   = function(env, key, value)
        local kt                        = type(key)
        if kt == "number" then
            kt                          = type(value)
            if kt == "string" then
                rawset(env, value, namespace.GetValue(env, value))
            elseif getValidatedNS(value) then
                rawset(env, getNSPath(value, true), value)
            end
        elseif kt == "string" then
            if value ~= nil then
                rawset(env, key, value)
            else
                rawset(env, key, namespace.GetValue(env, key))
            end
        elseif getValidatedNS(key) then
            rawset(env, getNSPath(key, true), key)
        end
    end

    local saveNamespaceTree             = function(root, path, subns) setstorage(_NSTree, root, setstorage(_NSTree[root] or {}, path, subns)) end
    local saveNamespacePath             = setstorage

    ----------------------------------------------------------------
    --- Register prototype.namespace
    ----------------------------------------------------------------
    namespace                           = prototype {
        __tostring                      = "namespace",
        __index                         = {
            --- Apply the environment to the function or stack
            ---
            --- @param  env
            --- @param  func:function
            --- @return env
            ["Apply"]                   = function(env, func, ...)
                if type(func) == "function" then
                    namespace.SetDefineMode(env, true)
                    setfenv(func, env)(env, ...)
                    namespace.SetDefineMode(env, false)
                elseif func == nil or type(func) == "number" then
                    setfenv((func or 1) + 1, env)
                end

                return env
            end;

            --- Register a namespace as global namespace, so it can be accessed
            ---
            --- @param  namespace
            ["RegisterGlobal"]          = function(ns)
                ns                      = getValidatedNS(ns)
                if ns then
                    for _, g_ns in ipairs, _GlobalNS, 0 do if g_ns == ns then return end end
                    tinsert(_GlobalNS, ns)
                end
            end;

            --- Unregister a registered global namespace
            ---
            --- @param  namespace
            ["UnregisterGlobal"]        = function(ns)
                ns                      = getValidatedNS(ns)
                if ns then
                    for i, g_ns in ipairs, _GlobalNS, 0 do
                        if g_ns == ns then tremove(__GlobalNS, i) return end
                    end
                end
            end;

            --- Register a global keyword key or collection
            ---
            --- @param  key
            --- @param  keyword
            ["RegisterGlobalKeyword"]   = function(key, keyword)
                local keywords          = _GlobalKeywords
                if type(key) == "table" then
                    for k, v in pairs, key do
                        if not keywords[k] and v then keywords[k] = v end
                    end
                elseif key and keyword and not keywords[key] then
                    keywords[key]       = keyword
                end
            end;

            --- Unregister a registered global keyword key or collection
            ---
            --- @param  key
            ["UnregisterGlobalKeyword"] = function(key)
                local keywords          = _GlobalKeywords
                if type(key) == "table" then
                    for _, k in ipairs, key, 0 do
                        if keywords[k] then keywords[k] = nil end
                    end
                elseif key and keywords[key] then
                    keywords[key]       = nil
                end
            end;

            --- Get the environment that visit the given keyword
            ---
            --- @param  keyword
            --- @return visitor env
            ["GetKeywordVisitor"]       = function(keyword)
                local visitor
                if keyword == _AccessKey then visitor = _KeyVisitor end
                _KeyVisitor, _AccessKey = nil, nil
                return visitor
            end;

            --- Import namespace to environment
            ---
            --- @param  env
            --- @param  namespace
            ["ImportNamespace"]         = function(env, ns)
                ns                      = getValidatedNS(ns)
                if not ns then error("namespace.ImportNamespace(env, ns) - The namespace is not provided", 2) end

                local imp               = rawget(env, _ENV_NS_IMPORT)
                if type(imp) == "table" then
                    for _, v in ipairs, imp, 0 do if v == ns then return end end
                else
                    imp                 = setmetatable({}, WEAK_VALUE)
                    rawset(env, _ENV_NS_IMPORT, imp)
                end

                tinsert(imp, ns)
            end;

            --- Export namespace and its children to environment
            ---
            --- @param  env
            --- @param  namespace
            --- @param  override:boolean
            ["ExportNamespace"]         = function(env, ns, override)
                ns                      = getValidatedNS(ns)
                if not ns then error("namespace.ExportNamespace(env, ns, override) - The namespace is not provided", 2) end

                local nodes             = _NSTree[ns]
                if nodes then
                    for k, imp in pairs, nodes do
                        if override or rawget(env, k) == nil then rawset(env, k, imp) end
                    end
                end

                nodes                   = _NSPath[ns]
                if nodes then
                    nodes               = nodes:match("[%P_]+$")
                    if override or rawget(env, nodes) == nil then rawset(env, nodes, ns) end
                end
            end;

            --- Export variables to environment
            ---
            --- @param  env
            --- @param  key
            ["ExportVariables"]         = function(env, key)
                if type(key) == "table" and getmetatable(key) == nil then
                    for k, v in pairs, key do
                        exportToEnv(env, k, v)
                    end
                else
                    exportToEnv(env, key)
                end
            end;

            --- Save the anonymous namespace, anonymous namespace can be collected as garbage
            ---
            --- @param  field:table/userdata
            ["SaveAnonymousNamespace"]  = function(field)
                if _NSPath[field] then error("namespace.SaveAnonymousNamespace(field) - The field already registered as " .. (_NSPath[field] or "anonymous"), 2) end
                saveNamespacePath(_NSPath, field, false)
            end;

            --- Whether the target is anonymous namespace
            ---
            --- @param  namespace
            --- @return boolean
            ["IsAnonymousNamespace"]    = function(ns)
                return _NSPath[ns] == false
            end;

            --- Save field to the target namespace
            ---
            --- @param  root
            --- @param  path
            --- @param  field:table/userdata
            --- @return field
            ["SaveNamespace"]           = function(root, path, field)
                if type(root) == "string" then
                    root, path, field   = ROOT_NAMESPACE, root, path
                elseif root == nil then
                    root                = ROOT_NAMESPACE
                else
                    root                = _NSPath[root] ~= nil and root
                end

                --- Check equal path
                if _NSPath[field] ~= nil then
                    local e_path        = TCache()
                    if _NSPath[root] then tinsert(e_path, _NSPath[root]) end
                    path:gsub("[%P_]+", function(name) tinsert(e_path, name) end)
                    if tconcat(e_path, ".") ~= _NSPath[field] then
                        error("namespace.SaveNamespace(root, path, field) - Already registered as " .. (_NSPath[field] or "anonymous"), 2)
                    end

                    Cache(e_path)
                    return field
                end

                local iter              = path:gmatch("[%P_]+")
                local subname           = iter()

                while subname do
                    local nodes         = _NSTree[root]
                    local subns         = nodes and nodes[subname]
                    local nxt           = iter()

                    if not nxt then
                        if subns then
                            if subns == field then return field end
                            error("namespace.SaveNamespace(root, path, field) - The namespace path has already be used by others", 2)
                        end

                        saveNamespacePath(_NSPath, field, _NSPath[root] and (_NSPath[root] .. "." .. subname) or subname)
                        saveNamespaceTree(root, subname, field)
                    elseif not subns then
                        subns           = prototype.NewProxy(tnamespace)

                        saveNamespacePath(_NSPath, subns,  _NSPath[root] and (_NSPath[root] .. "." .. subname) or subname)
                        saveNamespaceTree(root, subname, subns)
                    end

                    root, subname       = subns, nxt
                end

                return field
            end;

            --- Get the value from the environment
            --- When an environment try to the fetched value from the namespace system, it'll follow those orders:
            ---     1. system registered keywords
            ---     2. current namespace
            ---     3. imported namespaces
            ---     4. namespace parent
            ---     5. root namespace & global namespaces & _G
            ---
            --- @param  env
            --- @param  key
            --- @return value
            ["GetValue"]                = (function()
                local head              = TCache()
                local body              = TCache()
                local upval             = TCache()
                local apis              = TCache()

                --- External local variable
                tinsert(head, "_GlobalNS")
                tinsert(upval, _GlobalNS)

                tinsert(head, "_GlobalKeywords")
                tinsert(upval, _GlobalKeywords)

                tinsert(head, "getNamespace")
                tinsert(upval, getNamespace)

                tinsert(head, "getValidatedNS")
                tinsert(upval, getValidatedNS)

                tinsert(head, "getNSPath")
                tinsert(upval, getNSPath)

                tinsert(head, "registerKeyVisitor")
                tinsert(upval, function(visitor, keyword) _KeyVisitor, _AccessKey = visitor, keyword return keyword end)

                tinsert(body, "")
                tinsert(body, "")

                --- Generate meta-method __index handler
                tinsert(body, [[
                    local function _To(env, key)
                        local value
                ]])

                --- Check current namespace
                tinsert(apis, "rawget")
                tinsert(body, [[
                    local outer         = getValidatedNS(rawget(env, "]] .. _ENV_NS_OWNER .. [["))
                    if outer then
                        if key == getNSPath(outer, true) then return outer end
                        value           = outer[key]
                        if value ~= nil then return value end
                    end
                ]])

                --- Check imported namespaces
                tinsert(apis, "type")
                tinsert(apis, "ipairs")
                tinsert(body, [[
                    outer               = rawget(env, "]] .. _ENV_NS_IMPORT .. [[")
                    if type(outer) == "table" then
                        for _, ns in ipairs, outer, 0 do
                            ns          = getValidatedNS(ns)
                            if ns then
                                if key == getNSPath(ns, true) then return ns end
                                value   = ns[key]
                                if value ~= nil then return value end
                            end
                        end
                    end
                ]])

                --- Check namespace parent
                tinsert(apis, "_G")
                tinsert(body, [[
                    outer               = rawget(env, "]] .. _ENV_NS_PARENT .. [[")
                    if type(outer) == "table" and outer ~= _G then
                        if key == getNSPath(outer, true) then return outer end
                        value           = rawget(outer, key)
                        if value == nil then
                            value       = _To(outer, key)
                        end
                    end
                ]])

                tinsert(body, [[
                        return value
                    end

                    return function(env, key)
                        local value
                ]])

                --- Check keywords
                tinsert(body, [[
                    value               = _GlobalKeywords[key]
                    if value then return registerKeyVisitor(env, value) end
                ]])

                --- Check root namespace & global namespaces & _G
                tinsert(body, [[
                    value               = _To(env, key) or getNamespace(env, key) or getNamespace(key)
                    if value == nil then
                        for _, ns in ipairs, _GlobalNS, 0 do
                            value       = ns[key]
                            if value ~= nil then break end
                        end

                        if value == nil then
                            value       = _G[key]
                        end
                    end
                ]])

                --- Auto-Cache
                tinsert(apis, "rawset")
                tinsert(body, [[
                    if value ~= nil and not rawget(env, "]] .. _ENV_NS_DEFINE .. [[") then
                        rawset(env, key, value)
                    end
                ]])

                --- Return result to environment
                tinsert(body, [[
                        return value
                    end
                ]])

                if #head > 0 then
                    body[1]             = "local " .. tconcat(head, ", ") .. " = ..."
                end

                if #apis > 0 then
                    local declare       = tconcat(apis, ", ")
                    body[2]             = strformat("local %s = %s", declare, declare)
                end

                local handler           = LoadChunk(tconcat(body, "\n"), "namespace.GetValue", _EC_Tree)(unpack(upval))
                TCache(head) TCache(body) TCache(upval) TCache(apis)

                return handler
            end)();

            --- Set the namespace for next generated type
            ---
            --- @param  namesapce
            ["SetNamespaceForNext"]     = function(ns)
                local path              = ns
                ns                      = getValidatedNS(path)
                if not ns and type(path) == "string" then
                    ns                  = prototype.NewProxy(tnamespace)
                    namespace.SaveNamespace(path, ns)
                end

                _NextNSForType          = ns
            end;

            --- Get the namespace for next generated type
            ---
            --- @return namesapce
            ["GetNamespaceForNext"]     = function()
                local ns                = _NextNSForType
                _NextNSForType          = nil
                return ns
            end;

            --- Set the parent environment to the environment
            ---
            --- @param  env
            --- @param  parent
            ["SetParent"]                = function(env, parent)
                rawset(env, _ENV_NS_PARENT, parent)
            end;

            --- Get the parent environment from the environment
            ---
            --- @param  env
            --- @return parent
            ["GetParent"]                = function(env)
                return type(env) == "table" and rawget(env, _ENV_NS_PARENT) or nil
            end;

            --- Set the namespace owner from the environment
            ---
            --- @param  env
            --- @param  namespace
            ["SetOwner"]                = function(env, ns)
                rawset(env, _ENV_NS_OWNER, getValidatedNS(ns))
            end;

            --- Get the namespace owner from the environment
            ---
            --- @param  env
            --- @return owner
            ["GetOwner"]                = function(env)
                return rawget(env, _ENV_NS_OWNER)
            end;

            --- Turn on/off the definition mode for an environment, the value won't be auto-cached to the environment in definition mode
            ---
            --- @param  env
            --- @param  mode:boolean
            ["SetDefineMode"]           = function(env, mode)
                rawset(env, _ENV_NS_DEFINE, mode and true or nil)
            end;

            --- Get the namespace by path
            ---
            --- @param  root
            --- @param  path
            --- @return namespace
            ["GetNamespace"]            = getNamespace;

            --- Get the namespace path
            ---
            --- @param  path
            --- @param  only:boolean
            --- @return path
            ["GetPath"]                 = getNSPath;

            --- Get the validated namespace
            ---
            --- @param  tar
            --- @return namespace
            ["Validate"]                = getValidatedNS;
        },
        __newindex                      = readonly,
        __call                          = function(self, field)
            if field then
                if type(field) == "string" then
                    local path          = field
                    field               = namespace.GetNamespace(path)

                    if not field then
                        field           = prototype.NewObject(tnamespace)
                        namespace.SaveNamespace(path, field)
                    end
                else
                    field               = namespace.Validate(field)
                end
            end

            namespace.SetOwner(namespace.GetKeywordVisitor(namespace), field)
            return field
        end,
    }

    ----------------------------------------------------------------
    --- Register prototype.struct
    ----------------------------------------------------------------
    struct                              = prototype {
        __tostring                      = "struct",
        __index                         = writeonly,
        __newindex                      = readonly,
        __call                          = function(self, field)
            if field then
                local env               = namespace.GetKeywordVisitor(struct)
                if type(field) == "string" then
                    local path          = field
                    local root          = path:find("[^%P_]+") and ROOT_NAMESPACE or namespace.GetNamespaceForNext() or namespace.GetOwner(env)
                    field               = namespace.GetNamespace(root, path)

                    if not field then
                        field           = prototype.NewObject(tenvironment)
                        namespace.SaveNamespace(root, path, field)
                    end
                end

                if not namespace.GetParent(field) then
                    local p_path        = namespace.GetPath(field, false)
                    if p_path then
                        namespace.SetParent(field, namespace.GetNamespace(p_path))
                    end
                end

                rawset(env, namespace.GetPath(field, true), field)
            else
                field                   = prototype.NewObject(tenvironment)
                namespace.SaveAnonymousNamespace(field)
            end

            return field
        end,
    }

    ----------------------------------------------------------------
    --- The default prototype meta-method template
    ----------------------------------------------------------------
    tnamespace                          = prototype {
        __tostring                      = namespace.GetPath,
        __index                         = namespace.GetNamespace,
        __newindex                      = readonly,
        __metatable                     = namespace,
        __call                          = function(self, field)
            local env                   = prototype.NewObject(tenvironment)
            namespace.SetOwner(env, self)
            if field then
                return env(field)
            else
                return env
            end
        end,
    }

    tenvironment                        = prototype {
        __tostring                      = namespace.GetPath,
        __index                         = namespace.GetValue,
        __call                          = namespace.Apply,
    }

    ROOT_NAMESPACE                      = prototype.NewProxy(tnamespace)
    namespace.SaveAnonymousNamespace(ROOT_NAMESPACE)

    ----------------------------------------------------------------
    --- Import namespace to environment
    ---
    --- @param  namespace
    ----------------------------------------------------------------
    import                              = function(ns)
        namespace.ImportNamespace(namespace.GetKeywordVisitor(import), ns)
    end

    ----------------------------------------------------------------
    --- Export variables to environment
    ---
    --- @param  define
    ----------------------------------------------------------------
    export                              = function(define)
        namespace.ExportVariables(namespace.GetKeywordVisitor(export), define)
    end

    ----------------------------------------------------------------
    --- Register prototype interface
    ----------------------------------------------------------------
    namespace.SaveNamespace("System.Platform",          prototype { __tostring = namespace.GetPath, __index = PLATFORM_SETTINGS })
    namespace.SaveNamespace("System.Prototype",         prototype (prototype, { __tostring = namespace.GetPath }))
    namespace.SaveNamespace("System.Namespace",         prototype (namespace, { __tostring = namespace.GetPath }))

    ----------------------------------------------------------------
    --- Toolset to provide several compatible apis
    ----------------------------------------------------------------
    namespace.SaveNamespace("System.Toolset",           prototype {
        __tostring                      = namespace.GetPath,
        __index                         = {
            --- Share API
            wipe                        = wipe,
            strtrim                     = strtrim,
            setstorage                  = setstorage,
            getstorage                  = getstorage,
            safeget                     = safeget,

            --- Log
            GenerateLogger              = GenerateLogger,

            --- Cache
            TCache                      = TCache,

            --- Clone
            TClone                      = TClone,

            --- Load string
            LoadChunk                   = LoadChunk,
        },
        __newindex                      = readonly,
    })

    ----------------------------------------------------------------
    --- Global keywords & namespace
    ----------------------------------------------------------------
    namespace.RegisterGlobalKeyword {
        prototype                       = prototype,
        namespace                       = namespace,
        struct                          = struct,
        import                          = import,
        export                          = export,
    }

    namespace.RegisterGlobal("System")

    ----------------------------------------------------------------
    --- _G keywords
    ----------------------------------------------------------------
    if not rawget(_G, "prototype") then rawset(_G, "prototype", prototype) end
    if not rawget(_G, "namespace") then rawset(_G, "namespace", namespace) end
    if not rawget(_G, "struct")    then rawset(_G, "struct",    struct)    end
    if not rawget(_G, "import")    then rawset(_G, "import",    import)    end
    if not rawget(_G, "export")    then rawset(_G, "export",    export)    end

    _G.Hash                             = ROOT_NAMESPACE
end

return ROOT_NAMESPACE
