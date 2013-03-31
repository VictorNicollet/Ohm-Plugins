@Ohm = {} if !('Ohm' of @) 

Ohm.infinitePage = ($x,url) -> 

  s = "scroll"
  $w = $ window 

  f = () -> 
    if $x.is(":visible")
  
      dt = $w.scrollTop()
      db = dt + $w.height()
      et = $x.offset().top 
      eb = et + $x.height() 

      if (et <= db && eb >= dt)
        $w.unbind s, f
        Ohm.post url, null, (d) ->
          if d.more
            $x.before(d.more.html)
            Ohm.call d.more.code
          Ohm.call d.code
          $x.remove()

    else
      $w.unbind s, f

  $w.bind s, f

  do f
  
