@Ohm = {} if !('Ohm' of @) 

Ohm.call = (code) ->
  for f in code
    eval('(' + f[0] + ')').apply(@,f[1..])

if !('JSON' of @) || !('stringify' of @JSON) then do () -> 

  esc = /["\\\x00-\x1f\x7f-\x9f]/g
  lesc = 
      '\b': '\\b'
      '\t': '\\t'
      '\n': '\\n'
      '\f': '\\f'
      '\r': '\\r'
      '"' : '\\"'
      '\\': '\\\\'

  fesc = (a) -> 
    return lesc[a] if a of lesc 
    c = a.charCodeAt()
    return '\\u00' + Math.floor(c/16)/toString(16) + (c % 16).toString(16);

  escStr = (str) ->
    return '"' + (if str.match(esc) then str.replace(esc,fesc) else str) + '"'
   
  toJSON = (x) ->
    
    return 'null' if x is null

    type = $.type(x)
    switch type 
      when 'undefined' then return undefined
      when 'number', 'boolean' then return String(x)
      when 'string' then return escStr(x)
              	
    if $.isArray x 
      return '[' + ($.toJSON(v) || 'null' for v in x).join(",") + ']'

    if typeof x is 'object'  
      kvs = []
      for k,v of x       
        return if !Object.prototype.hasOwnProperty.call(x,k)
        switch typeof k 
          when 'number' then n = '"' + k + '"'
          when 'string' then n = escStr k
          else 
            continue
        switch typeof v
          when 'function', 'undefined'
          else
            kvs.push(n + ':' + toJSON v)
      return '{' + kvs.join(',') + '}'

    return undefined

  @JSON = {} if !('JSON' of @)     
  JSON.stringify = toJSON

Ohm.toJSON = JSON.stringify

Ohm.post = (endpoint,data,callback) ->
  if typeof endpoint is 'string'
    send = (data,callback) -> 
      $.ajax
         url: endpoint
         data: Ohm.toJSON data
         type: 'POST'
         contentType: 'application/json'
         success: callback 
         dataType: 'json'
  else
    send = (data,callback) -> 
      f = eval('('+endpoint[0]+')')
      a = endpoint[1..]
      a.push data, callback
      f.apply @, a
  send data, callback
