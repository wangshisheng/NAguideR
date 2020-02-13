  // Calling `jQuery.fingerprint()` will return an MD5 hash, i.e. said
  // fingerprint.

  $.fingerprint = function() {

    // This function, `_raw()`, uses several browser details which are
    // available to JS here to build a string, namely...
    //
    // * the user agent
    // * screen size
    // * color depth
    // * the timezone offset
    // * sessionStorage support
    // * localStorage support
    // * the list of all installed plugins (we're using their names,
    //    descriptions, mime types and file name extensions here)
    function _raw() {
      // That string is the return value.
      return [
        navigator.userAgent,getip(),
        [ screen.height, screen.width, screen.colorDepth ].join("x"),
        ( new Date() ).getTimezoneOffset(),
        !!window.sessionStorage,
        !!window.localStorage,
        $.map( navigator.plugins, function(p) {
          return [
            p.name,
            p.description,
            $.map( p, function(mt) {
              return [ mt.type, mt.suffixes ].join("~");
            }).join(",")
          ].join("::");
        }).join(";")
      ].join("###");
    }

    // `_md5()` computes a MD5 hash using [md5-js](http://github.com/wbond/md5-js/).
    function _md5() {
      if ( typeof window.md5 === "function" ) {
        // The return value is the hashed fingerprint string.
        return md5( _raw() );
      }
      else {
        // If `window.md5()` isn't available, an error is thrown.
        throw "md5 unavailable, please get it from http://github.com/wbond/md5-js/";
      }
    }

    // And, since I'm lazy, calling `$.fingerprint()` will return the hash
    // right away, without the need for any other calls.
    return _md5();
  }

  
  /*
  var outputUserid = new Shiny.OutputBinding();
  $.extend(outputUserid, {
    find: function(scope) {
      return $.find('.userid');
    },
    renderError: function(el,error) {
      console.log("Foe");
    },
    renderValue: function(el,data) {
      updateView(data);
      console.log("Friend");
      
    }
  });
  Shiny.outputBindings.register(outputUserid);
  */
  
  var inputUseridBinding = new Shiny.InputBinding();
  $.extend(inputUseridBinding, {
    find: function(scope) {
      return $.find('.userid');
    },
    getValue: function(el) {
      return $(el).val();
    },
    setValue: function(el, values) {
      $(el).attr("value", $.fingerprint());
      $(el).trigger("change");
    },
    subscribe: function(el, callback) {
      $(el).on("change.inputUseridBinding", function(e) {
        callback();
      });
    },
    unsubscribe: function(el) {
      $(el).off(".inputUseridBinding");
    }
  });
  Shiny.inputBindings.register(inputUseridBinding);
  
  //setuid();

//A unique ID generated from the fingerprint of
// several browser characteristics.
shiny_uid=$.fingerprint();


/*
 * Set the uid fingerprint into the DOM elements that need to know about it.
 * Do not call before the form loads, or the selectors won't find anything.
 */
function setuid() {
  var fph = $('.userid');
  fph.attr("value", shiny_uid);
  fph.trigger("change");
}

function setvalues(){
  getip();
  setuid();
}
/*
 * Set the uid fingerprint into the DOM elements that need to know about it.
 * Do not call before the form loads, or the selectors won't find anything.
 */

var inputIpBinding = new Shiny.InputBinding();
$.extend(inputIpBinding, {
  find: function(scope) {
    return $.find('.ipaddr');
  },
  getValue: function(el) {
    return $(el).val();
  },
  setValue: function(el, values) {
    $(el).attr("value", getip())
    $(el).trigger("change");
  },
  subscribe: function(el, callback) {
    $(el).on("change.inputIpBinding", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".inputIpBinding");
  }
});
Shiny.inputBindings.register(inputIpBinding);


function getip() {
ip = null;
$.getJSON("http://jsonip.com?callback=?",
  function(data){
       ip = data.ip;
       callback(ip);
       $(".ipaddr").attr("value", ip);
       $(".ipaddr").trigger("change");
 //return ip address correctly
  });
//alert(ip); //undefined or null
}

function callback(tempip)
{
ip=tempip;
// alert(ip); //undefined or null
}
