@Ohm = {} if !('Ohm' of @) 

Ohm.infinitePage = ($x,url) -> 

  s = "scroll"
  $w = $ window 

  f = () -> 
    if $x.is(":visible")
  
      dt = $w.scrollTop()
      db = dt + $w.height()
      et = here.$point.offset().top 
      eb = et + here.$point.height() 

      if (et <= db && eb >= dt)
        $w.unbind s, f
        Ohm.post url, null, (d) ->
          if d.more
            $x.before(d.more.html)
            call d.more.code
          call d.code
          $x.remove()

    else
      $w.unbind s, f

  $w.bind s, f

  do onScroll
  
