class OhmBoxStack

  constructor: (ctx,sel,@url) -> 
  
    @$l = $('<div/>').css { position: 'relative', overflow: 'hidden', left: '0px' }
    @$c = $('<div/>').append(@$l).css { overflow: 'hidden' }
    @$  = ctx.$.find(sel).append(@$c)
    @w  = @$.width()

    @memory  = {}
    @current = null
    @prefix  = []   

    @rq  = 0
    @rp  = null
    
    @box = null

    @url = @url.substring(0,@url.length-1) if /\/$/.test(@url)

  # Fetch box data from a remote URL

  fetch: (url,cUrl,callback) -> 
    url = '/' + url if url[0] isnt '/'
    rq = ++@rq
    args = $.toJSON cUrl
    $.ajax
      url: @url + url
      contentType: 'application/json'
      type: 'POST'
      data: args
      success: (data) -> 
        callback data if rq is @rq 

  # Load a box with a certain url. 

  load: (url,force) -> 
    if @current && @current.re.test url 
 
      # The requested URL is intended to be managed by the current box.
      # Ignore all box metadata provided by server. 
      @fetch url, @prefix[@prefix.length-1], (data) => 
        @box = @current
        call @box, data.code
        @prefix[@prefix.length-1] = url

    else if force is false && @prefix.length > 1 && @prefix[@prefix.length-2] is url && url in @memory

      # We moved back to the previous page, which is stored in memory, 
      # and the reload is not being forced, so just move back. 
      prefix = @prefix.slice(0,@prefix.length-1) 
      box = @memory[url] 
      @replace(box,prefix)

    else

      # We're loading an unknown page, so load it before deciding.
      # All metadata from server is kept and used. 
      @fetch url, null, (data) => 
        box = {}
        box.$  = $('<div/>')
        box.$c = $('<div/>').append(box.$).css
          width: @w + 'px'
          float: 'left'
          overflow: 'hidden' 
        box.root = { $: box.$ } 
        box.re = new RegExp('^' + data.root)
        @add url, box
        @box = box
        call @box, data.code   
        data.prefix.push url 
        @replace box, data.prefix

  # Remove all <div>s inside @$c that do not match one of the
  # keys in the prefix. 

  clean: () -> 
    memory = {}
    for key of @memory 
      if key in @prefix
        memory[key] = @memory[key]
      else
        do @memory.$c.remove    
    @memory = memory

  # Add a new box with key to the list of available boxes
  # (it is auto-hidden if there is a current box)
  
  add: (key,box) -> 
    do box.$c.hide if @current isnt null
    do @memory[key].$c.remove if key of @memory 
    @memory[key] = box
    @$c.append box.$c    

  # Animation rules: 
  #  [a,b,c] -> [a,b]   : shift left
  #  [a,b,c] -> [a]     : shift left
  #  [a,b]   -> [a,b,c] : shift right
  #  anything else      : replace

  replace: (newB,newP) -> 

    # All changes and animations are queued
    return @rp.push [newB,newP] if @rp isnt null
    @rp = []

    oldP = @prefix
    @prefix = newP

    oldB = @current
    @current = newB

    # Define the three possible animations
    animReplace = (callback) -> 
      do oldB.$c.hide
      do newB.$c.show
      callback.call @

    animShiftRight = (callback) -> 
      oldB.$c.after newB.$c.show() 
      @$c.animate { left: (-@w) + 'px' }, 'fast', =>
        do oldB.$c.hide
        @c.css { left: 0 } 
        callback.call @

    animShiftLeft = (callback) -> 
      oldB.$c.before newB.$c.show()
      @$c.css { left: (-@w)+'px' } 
      @$c.animate { left: 0 }, 'fast', => 
        do oldB.$c.hide
        callback.call @

    # Select which animation to play
    anim = animReplace
    same = newP.length < oldP.length + 2 

    if same
      l = Math.max oldP.length newP.length
      for i in [0..l-1]
        same = oldP[i] is newP[i]
        break if !same

    if same 
      if oldP.length is newP.length - 1
        anim = animShiftRight
      if oldP.length > newP.length 
        anim = animShiftLeft

    # Play it, continue after it is done
    anim.call @, () -> 

      # Remove unnecessary boxes
      do @clean

      # Unqueue next animation
      return @rp = null if @rp.length is 0
      args = @rp.shift()
      @replace.apply @, args
    
ohmBoxStack = null

#>> ohmBox_init(url:string)

@ohmBox_init = (url) ->
  ctx = { $: $('body') }
  ohmBoxStack = new OhmBoxStack(ctx,'#box-root',url)
  $.address.change (event) -> 
    ohmBoxStack.load event.path, false

#>> ohmBox_declare(id:string,path:int list)

@ohmBox_declare = (id,path) ->
  box = ohmBoxStack.box 
  box = box[path.shift()] while path.length > 1 
  box[path[0]] = 
    $: $('#'+id)
    
#>> ohmBox_fill(path:int list,html:html)

@ohmBox_fill = (path,html) -> 
  box = ohmBoxStack.box
  box = box[path.shift()] while path.length > 0
  box.$.html(html.html)
  call box(html.code)

#>> ohmBox_call(url:string,name:int list,args:Json.t)