# Ohm is © 2012 Victor Nicollet

window.joy = (($) ->

  select = ($where,selector) ->
    return $where if selector is ''
    $where.find selector

  execute = (ctx,$where,code) ->
    c = () ->
      @$ = $where
      return this
    c.prototype = ctx
    $.each code, (i,e) ->
      f = eval ('('+e[0]+')');
      ctx = new c()
      f.apply(ctx, e[1..])

  lastid = Math.floor(Math.random()*65536) 
  gen = () ->
    'form_id_'+(++lastid)

  # Form nodes are objects that implement the following interface:
  ## constructor(ctx,config)
  ## select
  ## identify($where)
  ## render($where)
  ## set(value)
  ## get()
  ## error(path,text)
  ## clear()
  ## remove()

  # Rendering HTML

  class node_html

    constructor: (@ctx,@config) ->
      @inner = recurse(@ctx,@config.i)

    select: ($where) -> 
      @$s = select($where,@config.s)

    render: ($where) ->
      @$inner = $ @config.h.html
      @$s.append(@$inner)
      @inner.identify @$inner
      execute @ctx, @$inner, @config.h.code
      @inner.select @$inner
      @inner.render @$inner

    identify: () ->

    set: (v) ->
      @inner.set v

    get: () ->
      do @inner.get

    remove: () ->
      do @$inner.remove

    error: (path,text) ->
      @inner.error path, text

    clear: () ->
      do @inner.clear

  # An array of nodes

  class node_array

    constructor: (@ctx,@config) ->
      @value = []

    append: (value) ->
      $item = $ @config.ih.html
      inner = recurse @ctx, @config.i
      @$list.append $item
      $item.data 'form', inner
      execute @ctx, $item, @config.ih.code
      inner.select $item
      inner.render $item
      inner.set value
      select($item,@config.rs).click () =>
        $item.remove()
        do @toggle
      do @toggle

    toggle: () ->
      show = !@config.max || @$list.children().length < @config.max
      @$add.toggle show

    identify: () ->

    select: ($where) ->
      @$list = select($where,@config.ls)
      @$add  = select($where,@config.as)

    render: ($where) ->
      @$add.click () =>
        @append null
      
    set: (v) ->
      @remove()
      $.each v, (i,e) =>
        @append e

    get: () ->
      out = []
      @$list.children().each () ->
        out.push $(@).data('form').get()
      out

    remove: () ->
      do @$list.children().remove

    clear: () ->
      @list.children().each () ->
        do $(@).data('form').clear

    error: (path,text) ->
      location = path.shift() || 0
      children = @$list.children()
      return if location < 0 || location >= children.length
      children.eq(location).data('form').error(path,text)

  # Concatenate two subnodes

  class node_concat

    constructor: (@ctx,config) ->
      @l = recurse @ctx, config[0]
      @r = recurse @ctx, config[1]

    identify: ($where) ->
      @l.identify $where
      @r.identify $where

    select: ($where) -> 
      @l.select $where
      @r.select $where

    render: ($where) ->
      @l.render $where
      @r.render $where

    set: (v) ->
      v = v || [null,null]
      @l.set(v[0] || null)
      @r.set(v[1] || null)

    get: () ->
      [ @l.get() , @r.get() ]

    remove: () ->
      do @l.remove
      do @r.remove

    error: (path,text) ->
      loc = path.shift()
      @l.error path, text if loc == 0
      @r.error path, text if loc == 1

    clear: () ->
      do @l.clear
      do @r.clear

  # Empty

  node_empty =
    set: () ->
    get: () -> null
    remove: () ->
    error: () ->
    clear: () ->
    select: () ->
    render: () ->
    identify: () ->

  # String field

  class node_string

    constructor: (@ctx,@config) ->
      @id = gen()

    select: () ->

    render: ($where) ->
      @$label = $ '<label/>'
      if 'ls' of @config
        $wrap = $where
        if 'lh' of @config
          $wrap = $ @config.lh.html
          select($where,@config.lhs).append $wrap
        @$label = select($wrap,@config.ls).attr('for',@id).text(@config.lt)
        if 'lh' of @config
          execute @ctx, $wrap, @config.lh.code
      @$error = $ '<label/>'
      if 'es' of @config
        $wrap = $where
        if 'eh' of @config
          $wrap = $ @config.eh.html
          select($where,@config.ehs).append $wrap
        @$error = select($wrap,@config.es).attr('for',@id)
        if 'eh' of @config
          execute @ctx, $wrap, @config.eh.code
      $wrap = $where
      if 'fh' of @config
        $wrap = $ @config.fh.html
        select($where,@config.fhs).append $wrap
      @$field = select $wrap, @config.s
      if 'fh' of @config
        @$field.attr('id',@id)
        execute @ctx, $wrap, @config.fh.code
      @$field.blur () -> 
        $(@).toggleClass '-filled', /\S/.test $(@).val()  
        
    identify: ($where) ->
      return if 'fh' of @config
      select($where,@config.s).attr {'id': @id, 'name': @id} 

    set: (v) ->
      do @$field.val(v || '').blur
      if @$field.data('onSet')
        @$field.data('onSet')(v)

    get: () ->
      if @$field.data('onGet')
        @$field.data('onGet')()
      @$field.val()

    remove: () ->
      do @$label.remove
      do @$error.remove
      do @$field.remove

    error: (path,text) ->
      return if path.length > 0
      @$error.text(text).closest('.error-root').addClass '-error'

    clear: () ->
      @$error.text('').closest('.error-root').removeClass '-error'

  # Choice (simple or multiple)

  class node_choice

    constructor: (@ctx,@config) ->
      @id = gen()

    select: () ->

    render: ($where) ->
      @$label = $ '<label/>'
      if 'ls' of @config
        $wrap = $where
        if 'lh' of @config
          $wrap = $ @config.lh.html
          select($where,@config.lhs).append $wrap
        @$label = select($wrap,@config.ls).attr('for',@id).text(@config.lt)
        if 'lh' of @config
          execute @ctx, $wrap, @config.lh.code
      @$error = $ '<label/>'
      if 'es' of @config
        $wrap = $where
        if 'eh' of @config
          $wrap = $ @config.eh.html
          select($where,@config.ehs).append $wrap
        @$error = select($wrap,@config.es).attr('for',@id)
        if 'eh' of @config
          execute @ctx, $wrap, @config.eh.code
      $wrap = $where
      if 'fh' of @config
        $wrap = $ @config.fh.html
        select($where,@config.fhs).append $wrap
      @$field = select $wrap, @config.s
      if 'fh' of @config
        @$field.attr('id',@id)
        execute @ctx, $wrap, @config.fh.code
      html = []
      for e,i in @config.src
        html.push '<label>'
        if @config.m
          html.push '<input type="checkbox" name="',
            @id + '-' + i,
            '" value="', i, '"/>'
        else
          html.push '<input type="radio" name="',
            @id,
            '" value="', i, '"/>'
        html.push e.html, '</label>'
      @$field.html html.join('')

    identify: () ->

    set: (v) ->
      v = v || []
      @$field.find('input:checked').attr('checked','')
      for e,i in @config.src
        if e.internal in v
          @$field.find('input[value='+i+']').attr('checked','checked')

    get: () ->
      out = []
      for e,i in @config.src
        if @$field.find('input[value='+i+']').is(':checked')
          out.push e.internal
      out

    remove: () ->
      do @$label.remove
      do @$error.remove
      do @$field.remove


    error: (path,text) ->
      return if path.length > 0
      @$error.text(text).closest('.error-root').addClass '-error'

    clear: () ->
      @$error.text('').closest('.error-root').removeClass '-error'

  # Select drop-down / auto-complete node

  class node_select

    constructor: (@ctx,@config) ->
      @id = gen()
      @current = null
      @string  = new node_string(@ctx,@config)
      @cache   = {}
      @lastXHR = null

    source: (request,response) ->
      term = request.term
      respond = (list) ->
        list.length = 10 if list.length > 10
        @cache[term] = list
        response(list)
        return respond @cache[term] if term of @cache

      matcher = new RegExp ('\\b' + term.replace(/[-[\]{}()*+?.,\\^$|#\s]/g, "\\$&")), 'i'

      local = []
      if @config.ss
        local = $.grep @config.ss, (value) ->
          matcher.test value.value
      if typeof @config.ds is 'string'
        @lastXHR = $.getJSON @config.ds, {complete:term}, (data, status, xhr) ->
          respond $.merge(data.list,local) if xhr is @lastXHR
      else
        respond local

    select: ($where) ->
      @string.select($where)

    render: ($where) ->
      @string.render($where)
      @string.$field.autocomplete
        minLength: 0
        source: (req,res) => @source req, res
        select: (event,ui) =>
          @apply(ui.item)
          false
      @string.$field.blur () =>
        return @apply(null) if @string.$field.val() is ''
        @apply(@current)
      @string.$field.addClass('form-select')
      @string.$field.focus () =>
        @$html.hide()
        @string.$field.css(@reset).autocomplete('search','')
      @string.$field.data('autocomplete')._renderItem = (ul,item) ->
        a = $ '<a></a>'
        a.append(item.html || item.label)
        a.toggleClass('joy-custom',!!item.html)
        li = $ '<li></li>'
        li.data 'item.autocomplete', item
        li.append a
        li.appendTo ul
        li

      @$html = $('<div class="joy-select-html"></div>');
      @$html.insertAdter(@string.field)
      @$html.click () =>
        do @string.$field.focus
      do @$html.hide

      @reset =
        position:  @string.$field.css('position')
        opacity:   @string.$field.css('opacity')
        'z-index': @string.$field.css('z-index')

    identify: ($where) ->
      @string.identify $where

    apply: (elem) ->
      if elem is null
        @current = null
        @string.field.val('').removeClass('joy-select-set').css(@reset)
      else
        @current = elem
        @string.$field.val(elem.value).addClass('joy-select-set');
        if elem.html
          z = @$html.html(elem.html).show().css('z-index')
          z = 1 if z is 'auto'
          @string.field.css
            position: absolute
            opacity: 0
            'z-index': z + 1
        else
          do @$html.hide
          @string.$field.css @reset

    set: (v) ->
      applied = false
      json = $.toJSON(value)

      find = (list) ->
        for elem, i in list
          return if applied
          if $.toJSON(elem.internal) is json
            @apply elem
            applied = true

      find(@config.ss || [])
      if (typeof @config.ds is 'string' && !applied)
        $.getJSON @config.ds, get: json, (data) ->
          find data.list

    get: () ->
      return @current.internal if @current
      null

    remove: () ->
      do @string.remove

    error: (path,text) ->
      @string.error path, text

    clear: () ->
      do @string.clear

  # The main function

  joy = (id,url,config) ->
    ctx     = @
    $hidden = $('#' + id)
    $form   = $hidden.parent()
    root    = recurse(ctx,config)
    init    = $.parseJSON($hidden.val())

    root.identify $form
    root.select   $form
    root.render   $form
    root.set      init

    $form.data 'ohmForm', 
      setUrl: (u) -> 
        url = u

    if typeof url is 'string'
      send = (data,callback) -> 
        $.ajax
          url: url
          data: $.toJSON data 
          type: 'POST'
          contentType: 'application/json'
          success: callback 
    else
      send = (data,callback) -> 
        f = eval('('+url[0]+')')
        a = url[1..]
        a.push data, callback
        f.apply @, a

    $form.submit () ->
      data = root.get()
      json = $.toJSON(data)
      $hidden.val json
      send data, (res) -> 
        root.set res.data if res.data
        if res.errors
          do root.clear
          root.error error[0], error[1] for error in res.errors
        execute ctx, $form, res.code if res.code
      false

  recurse = (ctx,config) ->
    return node_empty if config is null
    if 't' of config
      constructor = joy.nodes[config.t]
      return new constructor(ctx,config)
    return new node_concat(ctx,config)


  # Registering nodes to extend joy

  joy.nodes =
    html: node_html
    string: node_string
    select: node_select
    choice: node_choice
    array: node_array

  joy

)(jQuery)