var submitForm = false;
$(document).ready(function(){    
    $('input[name=page_name]').keyup(function(evt){            
            var v = $('input[name=page_name]').attr("value");                
            v = v.replace(/[^.a-zA-Z0-9_-]/, '');            
            $('input[name=page_name]').attr("value", v);
            return true;
        })
        .focus();
    $('input[name=page_name]').keypress(function(evt){
        if (evt.keyCode == 13) {
            submitForm = false;
            return false;
        }
        return true;
    });
    $('#Save-btn, #Cancel-btn').click(function(){        
        submitForm = true;
    });
    
    $('#create-page-form').submit(function(evt, f){        
        return submitForm;
    });
});