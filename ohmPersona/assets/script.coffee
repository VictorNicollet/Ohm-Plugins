#>> ohmPersona(current:string option,login:string,logout:string)
@ohmPersona = (current,login,logout) ->
  success = (data) ->
    for f in data.code
      eval('(' + f[0] + ')').apply(@,f[1..])
  navigator.id.watch
    loggedInUser : current
    onLogin : (assertion) ->
      $.ajax
        url: login
        contentType: 'application/json'
        type: 'POST'
        data: $.toJSON assertion
        success: success
    onLogout : () ->
      $.ajax
        url: logout
        type: 'POST'
        success: success

@ohmPersonaLogin = () ->
  navigator.id.request()

#>> ohmPersonaLogout()
@ohmPersonaLogout = () ->
  navigator.id.logout() 